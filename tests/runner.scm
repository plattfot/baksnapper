#! /usr/bin/env -S guile --no-auto-compile -e main -s
!#
; Script to run a baksnapper test

; SPDX-FileCopyrightText: 2025  Fredrik Salomonsson <plattfot@posteo.net>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 and-let-star)
             (ice-9 format)
             (ice-9 ftw)
             (ice-9 getopt-long)
             (ice-9 hash-table)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-9))

(define* (path-join #:key (separator file-name-separator-string) . paths)
  "Join PATHS into one path using `file-name-separator-string`."
  (string-join paths file-name-separator-string))

(define-record-type <snapshot>
  (make-snapshot id parent state tags)
  snapshot?
  (id snapshot-id)
  (parent snapshot-parent)
  (state snapshot-state)
  (tags snapshot-tags))

(define (make-snapshot-from input)
  "Parse INPUT and create a snapshot from it."
  (let* ((info (string-split input #\:))
         (id (car info))
         (metadata
          (map
           (lambda (metadatum)
             (match (string-split metadatum #\=)
               (("p" parent) (cons 'parent parent))
               (("s" state)
                (let ((state-sym (string->symbol state)))
                  (match state-sym
                    ((or 'valid 'empty 'no-info 'no-snapshot 'incomplete)
                     (cons 'state state-sym)))))
               (("t" tags) (cons 'tags tags))))
           (cdr info))))
    (make-snapshot
     id
     (match (assoc 'parent metadata)
       (('parent . p) p)
       (_ #f))
     (match (assoc 'state metadata)
       (('state . s) s)
       (_ 'valid))
     (match (assoc 'state metadata)
       (('tags . t) t)
       (_ #f)))))

(define* (create-snapshot path snapshot #:key valid?)
  "Create a dummy snapshot at PATH.

It will have the following structure:

PATH[__<tags>]
└── data

The file `data` will contain; snapshot=ID, where ID is the id from
SNAPSHOT; parent=PARENT, where PARAENT is the parent from SNAPSHOT.
If parent is #f it will skip it.

It will append the tags string to the path, separated by `__`, if that
does not evaluate to false.

It will also contain `ro=true` if VALID? evaluates to true."
(mkdir
 (match (snapshot-tags snapshot)
   (#f path)
   (tags (string-append path "__" tags))))

(let ((port (open-output-file
             (string-append path file-name-separator-string "data"))))
  (and-let* ((parent (snapshot-parent snapshot)))
    (format port "parent=~a~%" parent))
  (format port "snapshot=~a~%" (snapshot-id snapshot))
  (when valid?
    (format port "ro=true~%"))
  (close-port port)))

(define (create-snapper-snapshot path snapshot)
  "Create dummy snapper snapshot at PATH for SNAPSHOT.

A snapper snapshot is a normal directory which consist of a info.xml
file with metadata and a btrfs snapshot named `snapshot`.  This will
mock the structure with the following:

PATH
├── info.xml
└── snapshot
    └── data

Where the `info.xml` will just be an empty file.  The file `data` will
contain; snapshot=ID, where ID is the id from SNAPSHOT; parent=PARENT,
where PARAENT is the parent from SNAPSHOT.  If parent is #f it will
skip it.

The state in SNAPSHOT specify what state it should create the
snapshot, accepted values are:

- valid: a complete snapshot as described above.  With `ro=true` added
to the `data` file.

- empty: a broken snapshot that is just an empty directory.  No
`info.xml` file or `snapshot` directory.

- no-info: a broken snapshot which is missing the info.xml file, will
have the `snapshot` directory with the `data` file containing
`ro=true`.

- no-snapshot: A broken snapshot which is missing the `snapshot`
directory.  Will have the empty `info.xml`.

- incomplete: a broken snapshot which snapshot directory is
incomplete.  It will have both `info.xml` and `snapshot`.  But the
`ro=true` attribute will be missing in the `data` file."
  (let* ((snapshot-dir (string-append path file-name-separator-string "snapshot"))
         (create-info.xml (lambda ()
                           (close-port
                            (open-output-file
                             (string-append path "/info.xml"))))))
    (match (snapshot-state snapshot)
      ('valid
       (mkdir path)
       (create-info.xml)
       (create-snapshot snapshot-dir snapshot #:valid? #t))
      ('empty
       (mkdir path))
      ('no-info
       (mkdir path)
       (create-snapshot snapshot-dir snapshot #:valid? #t))
      ('no-snapshot
       (mkdir path)
       (create-info.xml))
      ('incomplete
       (mkdir path)
       (create-info.xml)
       (create-snapshot snapshot-dir snapshot #:valid? #f)))))

(define (check-snapshot-input value)
  "Verify the input for receiver or expected."
  (fold
   (lambda (snapshot prev-snapshot)
     (fold
      (lambda (metadata prev-metadata)
        (and (match (string-split metadata #\=)
               (("p" parent) #t)
               (("s" state)
                (match (string->symbol state)
                  ((or 'valid 'empty 'no-info 'no-snapshot 'incomplete) #t)
                  (_
                   (format (current-error-port) "unsupported state: ~a~%" state)
                   #f)))
               (("t" tags) #t)
               (_
                (format (current-error-port) "invalid snapshot syntax: ~a~%" metadata)
                #f))
             prev-metadata))
      prev-snapshot
      (cdr (string-split snapshot #\:))))
   #t
   (string-split value #\,)))

(define read-snapshots
  (match-lambda
    ((parent "info.xml" stat)
     (cons 'has-info #t))
    ((parent "latest" stat)
     (cons 'latest #t))
    ((parent "data" stat)
     (call-with-input-file (path-join parent "data")
       (lambda (port)
         (let parse-line ((line (get-line port)))
           (if (not (eof-object? line))
               (let ((key-value (string-split line #\=)))
                 (cons (cons (car key-value)
                             (match (cadr key-value)
                               ("" #f)
                               (value value)))
                       (parse-line (get-line port))))
               '())))))
    ((parent name stat children ...)
     (cons name
           (map
            (lambda (child)
              (read-snapshots (cons (path-join parent name) child)))
            children)))))

(define (make-snapshot-from-read data)
  "Create a snapshot from DATA.

Where DATA is in the format of what you get calling `read-snapshots'."
  (let* ((id (car data))
         (get (lambda (key alist default)
                (match (assoc key alist)
                  ((key . v) v)
                  (_ default))))
         (metadata (cdr data))
         (has-info (get 'has-info metadata #f))
         (snapshot-info (match (assoc "snapshot" metadata)
                          (("snapshot" . (v)) v)
                          (_ '())))
         (read-only (match (assoc "ro" snapshot-info)
                      (("ro" . "true") #t)
                      (_ #f)))
         (parent (get "parent" snapshot-info #f)))
    (make-snapshot
     id
     parent
     (match (list has-info snapshot-info read-only)
       ((#t (_ ...) #t)
        'valid)
       ((#f () _)
        'empty)
       ((#f (_ ...) #t)
        'no-info)
       ((#t () _)
        'no-snapshot)
       (_
        'incomplete))
     #f)))

(define (check-latest expected-latest receiver-dir)
  "Verify that EXPECTED-LATEST points to the expected snapshot.

If EXPECTED-LATEST is #f skip any check and simply return true."
  (let ((latest-path (path-join receiver-dir "latest")))
    (cond
     ((not expected-latest) #t)
     ((not (file-exists? latest-path))
      (format (current-error-port) "[FAIL] latest symlink does not exist~%")
      #f)
     ((not (equal? expected-latest (readlink latest-path)))
      (format (current-error-port) "[FAIL] latest points to ~a expected ~a~%"
              (read-link latest-path)
              expected-latest)
      #f)
     (#t #t))))

(define (configure-config-file configfile options test-dir sender-dir receiver-dir)
  "Copy CONFIGFILE and append source and dest options.

Where CONFIGFILE is either a path to a config file or #f if not set.

OPTIONS are cli options where it will look if src-config, dest-config
or path-config are set.

TEST-DIR where it should save the modified CONFIGFILE

SENDER-DIR is the path to the sender directory.

RECEIVER-DIR is the path to the receiver directory.

Return the path to the modified CONFIGFILE if it is defined otherwise #f."
  (if configfile
    (let* ((test-config-file (path-join test-dir (basename configfile)))
           (port (open-file test-config-file "w")))
      (call-with-input-file configfile
        (lambda (proc)
          (format port "~a" (get-string-all proc))))
      (when (option-ref options 'src-config #f)
        (format port "SOURCE=~a~%" sender-dir))
      (when (option-ref options 'dest-config #f)
        (format port "DEST=~a~%" receiver-dir))
      (when (option-ref options 'path-config #f)
        (format port "PATH=~a~%" (dirname receiver-dir)))
      (unless (option-ref options 'src-dest #f)
        (format port "CONFIG=~a~%" (option-ref options 'config "root")))
      (close-port port)
      (call-with-input-file test-config-file
        (lambda (proc)
          (format #t "-----config----~%~a~%--------------~%" (get-string-all proc))))
      test-config-file)
    #f))

(define (main args)
  (let* ((option-spec
          `((config (single-char #\c) (value #t))
            (sender (single-char #\s) (value #t) (predicate ,check-snapshot-input))
            (latest (single-char #\l) (value #t))
            (receiver (single-char #\r) (value #t) (predicate ,check-snapshot-input))
            (expected (single-char #\e) (value #t) (predicate ,check-snapshot-input))
            (expected-latest (single-char #\L) (value #t))
            (src-dest (value #f))
            (src-ssh (value #t))
            (dest-ssh (value #t))
            (configfile (value #t))
            (src-config (value #f))
            (dest-config (value #f))
            (path-config (value #f))
            (type (single-char #\t) (value #t))
            (help (single-char #\h) (value #f))))
         (options (getopt-long args option-spec))
         (exec-name (car args))
         (try-help (format #f "Try '~a --help' for more information.~%" exec-name)))
    (when (option-ref options 'help #f)
      (format #t "\
Usage: ~a [OPTION]... -- [COMMAND] [OPTION]...

Run a baksnapper test based on the options.  It has three sections.
The setup stage, running the baksnapper command and expected result.
If test succeeds it will clean up the setup.

By default it will generate the test setup in a temporary directory in
the temp directory.

Options:
  -c, --config   CONFIG     Name of the snapper config.
  -s, --sender   S0[,S1,…]  Snapshots at the source it should create.
  -l, --latest   R          The `latest` symlink at destination, pointing to R.
  -r, --receiver R0[:RM0][,R1[:RM1],…]  Snapshots at the destination.
  -e, --expected E0[:EM0][,E1[:EM1],…]  Snapshots expected after running baksnapper.
  -L, --expected-latest E   The expected `latest` snapshot after running baksnapper.
  -t, --type TYPE           Snapshot type, default is snapper.
      --src-dest            Give baksnapper source and dest locations.
      --src-ssh ADDRESS     Prepend ADDRESS: to source location.
      --dest-ssh ADDRESS    Prepend ADDRESS: to dest location.
      --configfile PATH     PATH to config file to pass on to COMMAND.
      --src-config          Append `SOURCE=<src>` to config file.
      --dest-config         Append `DEST=<dest>` to config file.
      --path-config         Append `PATH=<dest>` to config file.

Where SN, RN and EN are name of snapshots at the respective
location/stage.  RMN and EMN are metadata associated with a snapshot
for before and after after running baksnapper at the receiving
location.  Supported metadata are s=STATE which sets the state for the
snapshot.

It accept the following:
- valid: A complete snapshot.
- empty: A broken snapshot that's empty.
- no-snapshot: For snapper — Missing the snapshot subdirectory.
- no-info: For snapper — Missing the info.xml file.
- incomplete: An incomplete snapshot.

And p=PARENT, which list the parent of the snapshot.

The metadata are separate with :.

For example snapshot 10 with an incomplete state and parent snapshot
9.  Would be listed as `10:s=incomplete:p=9`.

Author:
Fredrik \"PlaTFooT\" Salomonsson
"
              exec-name)
      (exit #t))
    (let* ((parse-comma-option
            (lambda (input)
              (remove string-null? (string-split (option-ref options input "")  #\,))))
           (type (string->symbol (option-ref options 'type "snapper")))
           (config (option-ref options 'config "root"))
           (sender (parse-comma-option 'sender))
           (receiver (parse-comma-option 'receiver))
           (expected (parse-comma-option 'expected))
           (command (option-ref options '() '()))
           (test-dir (mkdtemp (path-join
                               (string-trim-right (or (getenv "TEMP") "/tmp") #\/)
                               "baksnapper-test-XXXXXX")))
           (sender-dir (path-join test-dir config ".snapshots"))
           (receiver-root-dir (path-join test-dir "receiver"))
           (receiver-dir (path-join receiver-root-dir config))
           (configfile (option-ref options 'configfile #f))
           (create-snapshot-func (match type
                                   ('denotebak create-denotebak-snapshot)
                                   (_ create-snapper-snapshot))))
      ;; Needed for nftw to be able to read the directory
      (chmod test-dir #o744)
      (format #t "Test ~a~%" test-dir)
      (format #t "  sender:   ~a~%" sender)
      (format #t "  receiver: ~a~%" receiver)
      (format #t "  expected: ~a~%" expected)
      (let ((exit-status 0)
            (sender-snapshots
             (map (lambda (input)
                    (make-snapshot-from input))
                  sender))
            (receiver-snapshots
             (map (lambda (input)
                    (make-snapshot-from input))
                  receiver))
            (expected-snapshots
             (alist->hash-table
              (map (lambda (input)
                     (let ((snapshot (make-snapshot-from input)))
                       (cons (snapshot-id snapshot) snapshot)))
                   expected))))
        ;; Setup
        ;; FIXME: write a proper mkdir -p
        (mkdir (path-join test-dir config))
        (mkdir sender-dir)
        (for-each
         (lambda (snapshot)
           (create-snapshot-func
            (path-join sender-dir (snapshot-id snapshot))
            snapshot))
         sender-snapshots)
        ;; FIXME: write a proper mkdir -p
        (mkdir (path-join test-dir "receiver"))
        (mkdir receiver-dir)
        (for-each
         (lambda (snapshot)
           (create-snapshot-func
            (path-join receiver-dir (snapshot-id snapshot))
            snapshot))
         receiver-snapshots)
        (and-let* ((latest (option-ref options 'latest #f)))
          (symlinkat receiver-dir latest "latest"))
        ;; Run command
        (let* ((append-ssh (lambda (address dir)
                             (if address
                                 (string-append address ":" dir)
                                 dir)))
               (if-set (lambda (check value)
                        (if check value '())))
               (prefix-if-set (lambda (check prefix value)
                                (if check (list prefix value) (list value))))
               (test-config-file (configure-config-file
                                  configfile
                                  options
                                  test-dir
                                  sender-dir
                                  receiver-dir))
               (command-with-path
                (append
                 command
                 (if-set test-config-file (list "--configfile" test-config-file))
                 (if (option-ref options 'src-dest #f)
                     (append
                      (if-set (not (option-ref options 'src-config #f))
                              (prefix-if-set
                               (option-ref options 'dest-config #f)
                               "--source"
                               (append-ssh (option-ref options 'src-ssh #f) sender-dir)))
                      (if-set (not (option-ref options 'dest-config #f))
                              (prefix-if-set
                               (option-ref options 'src-config #f)
                               "--dest"
                               (append-ssh (option-ref options 'dest-ssh #f) receiver-dir))))
                     (if-set
                      (not (option-ref options 'path-config #f))
                      (list (append-ssh (option-ref options 'dest-ssh #f) receiver-root-dir)))))))
          (format #t "Running command: ~a…~%" command-with-path)
          (setenv "BAKSNAPPER_TEST_RUNNER_SENDER_ROOT" test-dir)
          (let ((pipe (apply open-pipe* OPEN_READ command-with-path)))
            (format #t "~a~%" (get-string-all pipe))
            (set! exit-status (status:exit-val (close-pipe pipe)))))

        ;; Check
        (let* ((result-snapshots
                (alist->hash-table
                 (assq-remove!
                  (map (lambda (data)
                         (let ((snapshot (make-snapshot-from-read data)))
                           (cons (snapshot-id snapshot) snapshot)))
                       (cdr (read-snapshots
                             (cons (dirname receiver-dir)
                                   (file-system-tree receiver-dir)))))
                  'latest)))
               (mismatch-result
                (hash-fold
                 (lambda (id snapshot mismatch)
                   (match (hash-get-handle expected-snapshots id)
                     (#f (cons (cons snapshot #f) mismatch))
                     ((id . expected-snapshot)
                      (if (equal? snapshot expected-snapshot)
                          mismatch
                          (cons (cons snapshot expected-snapshot) mismatch)))))
                 '()
                 result-snapshots))
               (missing-expected
                (hash-fold
                 (lambda (id snapshot mismatch)
                   (if (hash-get-handle result-snapshots id)
                       mismatch
                       (cons snapshot mismatch)))
                 '()
                 expected-snapshots)))
          (for-each
           (match-lambda
             ((($ <snapshot> result-id result-parent result-state) . #f)
              (format (current-error-port)
                      "[FAIL] Snapshot ~a exists → p:~a s:~a~%"
                      result-id result-parent result-state))
             ((($ <snapshot> result-id result-parent result-state) .
               ($ <snapshot> expected-id expected-parent expected-state))
              (format (current-error-port)
                      "[FAIL] Snapshot `~a` differs~%" result-id)
              (when (not (equal? result-parent expected-parent))
                (format (current-error-port)
                        "  → parent is `~a` expected `~a`~%"
                        result-parent expected-parent))
              (when (not (equal? result-state expected-state))
                (format (current-error-port)
                        "  → state is `~a` expected `~a`~%"
                        result-state expected-state))))
           mismatch-result)
          (for-each
           (match-lambda
             (($ <snapshot> expected-id expected-parent expected-state)
              (format (current-error-port)
                       "[FAIL] Expected snapshot `~a` to exists with parent `~a` and state `~a`~%"
                       expected-id expected-parent expected-state)))
           missing-expected)
          (when (or (not (null-list? mismatch-result))
                    (not (null-list? missing-expected))
                    (not (check-latest
                          (option-ref options 'expected-latest #f)
                          receiver-dir)))
            (set! exit-status 1)))

        ;; Clean up
        (nftw test-dir
              (lambda (filename statinfo flag base level)
                (match flag
                  ('directory-processed (rmdir filename))
                  ((or 'regular symlink) (delete-file filename))
                  (_ (format
                      (current-error-port)
                      "cannot clean up file: ~a of type ~a~%"
                      filename
                      statinfo)))
                #t)
              'depth
              'physical)
        (exit exit-status)))))

