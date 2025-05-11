#! /usr/bin/env -S guile --no-auto-compile -e main -s
!#
; Script to run a baksnapper test

; SPDX-FileCopyrightText: 2025  Fredrik Salomonsson <plattfot@posteo.net>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 format)
             (ice-9 getopt-long)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-9))

(define* (path-join #:key (separator file-name-separator-string) . paths)
  "Join PATHS into one path using `file-name-separator-string`."
  (string-join paths file-name-separator-string))

(define-record-type <snapshot>
  (make-snapshot id parent state)
  snapshot?
  (id snapshot-id)
  (parent snapshot-parent)
  (state snapshot-state))

(define (main args)
  (let* ((option-spec
          `((config (single-char #\c) (value #t))
            (sender (single-char #\s) (value #t))
            (receiver (single-char #\r) (value #t))
            (expected (single-char #\e) (value #t))
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
  -r, --receiver R0[:RM0][,R1[:RM1],…]  Snapshots at the destination.
  -e, --expected E0[:EM0][,E1[:EM1],…]  Snapshots expected after running baksnapper.
  -t, --type TYPE           Snapshot type, default is snapper.

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
           (receiver-dir (path-join receiver-root-dir config)))
      (format #t "Test ~a~%" test-dir)
      (format #t "  sender:   ~a~%" sender)
      (format #t "  receiver: ~a~%" receiver)
      (format #t "  expected: ~a~%" expected)
      (format #t "  verify: ~a~%" (check-snapshot-input "10:s=valid:p=9"))
      ;; (mkdir sender-dir)
      ;; (for-each
      ;;  (lambda (snapshot)
      ;;    (create-snapper-snapshot (string-append sender-dir "/" snapshot) 'valid))
      ;;  sender)
      ;; TODO: parse state for receiver and expected snapshots
      ;; (rmdir test-dir)
      )))

