;;; ics.scm -- Tests for ICS parser.

;; Copyright (C) 2016, 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(use-modules (srfi srfi-64)
             (ice-9 rdelim)
             (ics)
             (ics ical-object)
             (ics parser)
             (ics fsm)
             (ics ical-object))

(test-begin "ics")


;;; Finite-State Machine tests.

(test-assert "ics-token-begin?"
  (and (ics-token-begin? "BEGIN")
       (not (ics-token-begin? "END"))))

(test-assert "ics-token-end?"
  (and (ics-token-end? "END")
       (not (ics-token-end? "BEGIN"))))

(test-assert "ics-calendar-object?"
  (and (ics-calendar-object? "VCALENDAR")
       (not (ics-calendar-object? "BEGIN"))))


(test-assert "make-ical-object"
  (equal? (make-ical-object '() '()) '((ICALPROPS ()) (COMPONENT ()))))


;;; Finite-State Machine Tests.

(test-assert "fsm-read-property"
  (let ((parser (make-string-parser (string-append "VCALENDAR"
                                                   (string #\cr)
                                                   (string #\lf)
                                                   "VERSION"))))
    (string=? (fsm-read-property parser) "VCALENDAR")))

(test-assert "fsm-read-property, property followed by EOF"
  (let ((parser (make-string-parser (string-append "VCALENDAR"
                                                   (string #\cr)
                                                   (string #\lf)))))
    (string=? (fsm-read-property parser) "VCALENDAR")))


(test-assert "fsm-skip-property"
  (let ((parser (make-string-parser (string-append "VCALENDAR"
                                                   (string #\cr)
                                                   (string #\lf)
                                                   "VERSION"))))
    (fsm-skip-property parser)
    (string=? (read-line (parser-port parser)) "VERSION")))

(test-assert "fsm-skip-property, property followed by EOF"
  (let ((parser (make-string-parser (string-append "VCALENDAR"
                                                   (string #\cr)
                                                   (string #\lf)))))
    (fsm-skip-property parser)
    (eof-object? (read-char (parser-port parser)))))


(test-assert "fsm-read-ical-object, valid object"
  (let ((parser (make-string-parser (string-append "VERSION:2.0\r\n"
                                                   "PRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\n"
                                                   "BEGIN:VEVENT\r\n"
                                                   "UID:19970610T172345Z-AF23B2@example.com\r\n"
                                                   "DTSTAMP:19970610T172345Z\r\n"
                                                   "DTSTART:19970714T170000Z\r\n"
                                                   "DTEND:19970715T040000Z\r\n"
                                                   "SUMMARY:Bastille Day Party\r\n"
                                                   "END:VEVENT"))))
    (equal? (fsm-read-ical-object parser '() '())
            '((ICALPROPS ((PRODID . "-//hacksw/handcal//NONSGML v1.0//EN")
                          (VERSION . "2.0")))
              (COMPONENT ((VEVENT (ICALPROPS ((SUMMARY . "Bastille Day Party")
                                              (DTEND . "19970715T040000Z")
                                              (DTSTART . "19970714T170000Z")
                                              (DTSTAMP . "19970610T172345Z")
                                              (UID . "19970610T172345Z-AF23B2@example.com")))
                                  (COMPONENT ()))))))))

(test-assert "fsm-read-ical-stream, valid object"
  (let ((parser (make-string-parser (string-append "BEGIN:VCALENDAR\r\n"
                                                   "VERSION:2.0\r\n"
                                                   "BEGIN:VEVENT\r\n"
                                                   "END:VEVENT\r\n"
                                                   "END:VCALENDAR"))))
    (equal? (fsm-read-ical-stream parser '())
            '(((ICALPROPS ((VERSION . "2.0")))
               (COMPONENT ((VEVENT (ICALPROPS ())
                                   (COMPONENT ())))))))))


;;; ical-value->scm

(test-assert "ical-value->scm, single value"
  (string=? (ical-value->scm "Bastille Day Party")
            "Bastille Day Party"))

(test-assert "ical-value->scm, value list"
  (equal? (ical-value->scm "Bastille,Day,Party")
          '("Bastille" "Day" "Party")))

(test-assert "ics-value->scm, value list with escaped symbols"
  (equal? (ical-value->scm "Bastille,Day\\,Party")
          '("Bastille" "Day,Party")))

;;; (ics)

(define %ics-string
  (string-append "BEGIN:VCALENDAR\r\n"
                 "VERSION:2.0\r\n"
                 "BEGIN:VEVENT\r\n"
                 "END:VEVENT\r\n"
                 "END:VCALENDAR\r\n"))

(define %ical-object
  '((ICALPROPS ((VERSION . "2.0")))
    (COMPONENT ((VEVENT (ICALPROPS ())
                        (COMPONENT ()))))))


(test-assert "ics-string->scm"
  (equal? (ics-string->scm %ics-string)
          (list %ical-object)))

(test-assert "scm->ics-string"
  (equal? (scm->ics-string %ical-object)
          %ics-string))


;;;

(test-end "ics")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; ics.scm ends here.
