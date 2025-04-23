;;; ics.scm -- Tests for the Command-Line Interface (CLI.)

;; Copyright (C) 2022-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
   "    VERSION (TEXT): 2.0\n"
   "    PRODID (TEXT): -//hacksw/handcal//NONSGML v1.0//EN\n"
   "    BEGIN: VEVENT\n"
   "        UID (TEXT): 19970610T172345Z-AF23B2@example.com\n"
   "        DTSTAMP (DATE-TIME): 19970610T172345Z\n"
   "        DTSTART (DATE-TIME): 19970714T170000Z\n"
   "        DTEND (DATE-TIME): 19970715T040000Z\n"
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

;; See <https://github.com/artyom-poptsov/guile-ics/issues/3>
(test-equal "command-print: a complex object"
  "BEGIN: VCALENDAR
    VERSION (TEXT): 2.0
    PRODID (TEXT): -//PIMUTILS.ORG//NONSGML khal / icalendar //EN
    BEGIN: VEVENT
        SUMMARY (TEXT): Somesomething
        DTSTART (DATE-TIME);TZID=Europe/Berlin: 20250115T160000Z
        DTEND (DATE-TIME);TZID=Europe/Berlin: 20250115T170000Z
        DTSTAMP (DATE-TIME): 20241220T164610Z
        UID (TEXT): 79DWUFD9IOVG150LBAU4IZOMJ7TP7GUJNYYP
        SEQUENCE (INTEGER): 0
        CATEGORIES (TEXT): 
    END: VEVENT
    BEGIN: VTIMEZONE
        TZID (TEXT): Europe/Berlin
        BEGIN: DAYLIGHT
            DTSTART (DATE-TIME): 20250330T030000Z
            TZNAME (TEXT): CEST
            TZOFFSETFROM (UTC-OFFSET): +0100
            TZOFFSETTO (UTC-OFFSET): +0200
        END: DAYLIGHT
        BEGIN: STANDARD
            DTSTART (DATE-TIME): 20241027T020000Z
            TZNAME (TEXT): CET
            TZOFFSETFROM (UTC-OFFSET): +0200
            TZOFFSETTO (UTC-OFFSET): +0100
        END: STANDARD
    END: VTIMEZONE
END: VCALENDAR
"
  (with-output-to-string
    (lambda ()
      (with-input-from-string
          (string-join
           (list
            "BEGIN:VCALENDAR"
            "VERSION:2.0"
            "PRODID:-//PIMUTILS.ORG//NONSGML khal / icalendar //EN"
            "BEGIN:VTIMEZONE"
            "TZID:Europe/Berlin"
            "BEGIN:STANDARD"
            "DTSTART:20241027T020000"
            "TZNAME:CET"
            "TZOFFSETFROM:+0200"
            "TZOFFSETTO:+0100"
            "END:STANDARD"
            "BEGIN:DAYLIGHT"
            "DTSTART:20250330T030000"
            "TZNAME:CEST"
            "TZOFFSETFROM:+0100"
            "TZOFFSETTO:+0200"
            "END:DAYLIGHT"
            "END:VTIMEZONE"
            "BEGIN:VEVENT"
            "SUMMARY:Somesomething"
            "DTSTART;TZID=Europe/Berlin:20250115T160000"
            "DTEND;TZID=Europe/Berlin:20250115T170000"
            "DTSTAMP:20241220T164610Z"
            "UID:79DWUFD9IOVG150LBAU4IZOMJ7TP7GUJNYYP"
            "SEQUENCE:0"
            "CATEGORIES:"
            "END:VEVENT"
            "END:VCALENDAR\r\n")
           "\r\n")
        (lambda ()
          (command-print '("ics")))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; vcf.scm ends here.
