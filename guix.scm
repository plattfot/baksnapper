; SPDX-FileCopyrightText: 2025 Fredrik Salomonsson <plattfot@posteo.net>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules
  (gnu packages bash)
  (gnu packages base)
  (gnu packages linux)
  (gnu packages)
  ((guix licenses) #:prefix license:)
  (guix build-system meson)
  (guix download)
  (guix gexp)
  (guix git-download)
  (guix packages)
  (guix utils)
  (ice-9 popen)
  (ice-9 rdelim))

;; From the talk "Just build it with Guix" by Efraim Flashner
;; presented on the Guix days 2020
;; https://guix.gnu.org/en/blog/2020/online-guix-day-announce-2/
(define %source-dir (current-source-directory))

(define %git-commit
  (read-string (open-pipe "git show HEAD | head -1 | cut -d ' ' -f2" OPEN_READ)))

(define (skip-git-and-meson-artifacts file stat)
  "Skip git and autotools artifacts when collecting the sources."
  (let ((name (substring file (+ 1 (string-prefix-length %source-dir file)))))
    (not (or (string=? name ".envrc")
             (string-prefix? "./build" name)
             ))))

(define-public baksnapper
  (package
    (name "baksnapper")
    (version (git-version "2.3.0" "HEAD" %git-commit))
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? skip-git-and-meson-artifacts))
    (build-system meson-build-system)
    (inputs (list bash util-linux))
    (synopsis "Backup tool for snapper")
    (description
     "Baksnapper is a script for backing up snapshots created by the program
snapper using btrfs send and receive.")
    (home-page "")
    (license license:gpl3+)))

baksnapper
