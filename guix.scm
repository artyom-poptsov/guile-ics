;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is part of Guile-ICS.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; GNU Guix development package. To use as the basis for a development
;; environment, run:
;;
;;  guix environment --pure --container -l guix.scm
;;
;; In the new shell, run:
;;
;;  autoreconf -vif && ./configure && make check
;;
;;; Code:

(use-modules (guix gexp)
             (guix packages)
             ((guix licenses)
              #:prefix license:)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages bash)
             (gnu packages base)
             (gnu packages gettext)
             (gnu packages admin)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages tex)
             (gnu packages texinfo)
             (gnu packages man))


(define %source-dir (dirname (current-filename)))

(define-public guile-ics
  (package
    (name "guile-ics")
    (version "git")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           help2man
           texinfo
           gettext-minimal ; Gettext brings 'AC_LIB_LINKFLAGS_FROM_LIBS'.
           pkg-config
           guile-dsv
           texlive))
    (inputs (list guile-3.0 which))
    (propagated-inputs (list guile-lib guile-smc))
    (home-page "https://github.com/artyom-poptsov/guile-ics")
    (synopsis "Guile parser library for the iCalendar format")
    (description
     "Guile-ICS is an iCalendar (RFC5545) format parser library written in
pure Scheme.  The library can be used to read and write iCalendar data.

The library is shipped with documentation in Info format and usage examples.")
    (license license:gpl3+)))

;; Return the package.
guile-ics

;;; guix.scm ends here.
