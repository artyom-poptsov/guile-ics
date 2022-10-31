;;; ics.scm -- Tests for the Command-Line Interface (CLI.)

;; Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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


;;; Code:

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (tests common)
             (ics cli command-print))


(define %test-suite-name "cli")

(configure-test-logging! %test-suite-name)

(test-begin %test-suite-name)



(test-equal "command-print: pretty"
  (string-append
   "BEGIN: VCALENDAR\n"
   "    PRODID (TEXT): -//hacksw/handcal//NONSGML v1.0//EN\n"
   "    VERSION (TEXT): 2.0\n"
   "    BEGIN: VEVENT\n"
   "        UID (TEXT): 19970610T172345Z-AF23B2@example.com\n"
   "        DTSTAMP (DATE-TIME): 19970610T172345\n"
   "        DTSTART (DATE-TIME): 19970714T170000\n"
   "        DTEND (DATE-TIME): 19970715T040000\n"
   "        SUMMARY (TEXT): Bastille Day Party\n"
   "    END: VEVENT\n"
   "END: VCALENDAR\n")
  (with-output-to-string
    (lambda ()
      (with-input-from-string
          (string-append
           "BEGIN:VCALENDAR\r\n"
           "VERSION:2.0\r\n"
           "PRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\n"
           "BEGIN:VEVENT\r\n"
           "UID:19970610T172345Z-AF23B2@example.com\r\n"
           "DTSTAMP:19970610T172345Z\r\n"
           "DTSTART:19970714T170000Z\r\n"
           "DTEND:19970715T040000Z\r\n"
           "SUMMARY:Bastille Day Party\r\n"
           "END:VEVENT\r\n"
           "END:VCALENDAR\r\n")
        (lambda ()
          (command-print '("ics")))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; vcf.scm ends here.
