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
             (oop goops)
             (ice-9 rdelim)
             (ics)
             (ics common)
             (ics parser)
             (ics fsm)
             (ics type object)
             (ics type property))

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


(test-assert "make-ics-object"
  (let ((obj (make <ics-object> #:name "VCALENDAR")))
    (string=? (ics-object-name obj) "VCALENDAR")))


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

(test-assert "fsm-read-property, list of values"
  (let ((parser (make-string-parser (string-append "a,b,c\\,d"
                                                   (string #\cr)
                                                   (string #\lf)))))
    (equal? (fsm-read-property parser) '("a" "b" "c,d"))))

(test-assert "fsm-read-property, escaped chars"
  (let ((parser (make-string-parser (string-append "a\\;,b\\\\,c\\,d\\n"
                                                   (string #\cr)
                                                   (string #\lf)))))
    (equal? (fsm-read-property parser) '("a;" "b\\" "c,d\n"))))


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


(test-assert "fsm-read-ics-object, valid object"
  (let ((parser (make-string-parser (string-append "VERSION:2.0\r\n"
                                                   "PRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\n"
                                                   "BEGIN:VEVENT\r\n"
                                                   "UID:19970610T172345Z-AF23B2@example.com\r\n"
                                                   "DTSTAMP:19970610T172345Z\r\n"
                                                   "DTSTART:19970714T170000Z\r\n"
                                                   "DTEND:19970715T040000Z\r\n"
                                                   "SUMMARY:Bastille Day Party\r\n"
                                                   "END:VEVENT"))))
    (let* ((obj (fsm-read-ics-object parser "VCALENDAR" '() '()))
           (version (ics-object-property-ref obj "VERSION"))
           (prodid  (ics-object-property-ref obj "PRODID"))
           (components (ics-object-components obj)))
      (format (current-error-port) "components: ~s~%" components)
      (and
       ;; Check object properties
       (and (and version (string=? (ics-property-value version) "2.0"))
            (and prodid (string=? (ics-property-value prodid)
                                  "-//hacksw/handcal//NONSGML v1.0//EN")))
       ;; Check object components (sub-objects)
       (let* ((vevent  (car components))
              (uid     (ics-object-property-ref vevent "UID"))
              (dtstamp (ics-object-property-ref vevent "DTSTAMP"))
              (dtstart (ics-object-property-ref vevent "DTSTART"))
              (dtend   (ics-object-property-ref vevent "DTEND"))
              (summary (ics-object-property-ref vevent "SUMMARY")))
         (format (current-error-port) "vevent: ~s (name: ~s)~%"
                 vevent (ics-object-name vevent))
         (format (current-error-port) "  uid:     ~s~%" uid)
         (format (current-error-port) "  dtstamp: ~s~%" dtstamp)
         (format (current-error-port) "  dtstart: ~s~%" dtstart)
         (format (current-error-port) "  dtend:   ~s~%" dtend)
         (format (current-error-port) "  summary: ~s~%" summary)
         (and (string=? (ics-object-name vevent)
                        "VEVENT")
              (string=? (ics-property-value uid)
                        "19970610T172345Z-AF23B2@example.com")
              (string=? (ics-property-value dtstamp)
                        "19970610T172345Z")
              (string=? (ics-property-value dtstart)
                        "19970714T170000Z")
              (string=? (ics-property-value dtend)
                        "19970715T040000Z")
              (string=? (ics-property-value summary)
                        "Bastille Day Party")))))))


(test-assert "fsm-read-ics-stream, valid object"
  (let ((parser (make-string-parser (string-append "BEGIN:VCALENDAR\r\n"
                                                   "VERSION:2.0\r\n"
                                                   "BEGIN:VEVENT\r\n"
                                                   "END:VEVENT\r\n"
                                                   "END:VCALENDAR"))))
    (let* ((vcalendar (car (fsm-read-ics-stream parser '())))
           (vevent    (car (ics-object-components vcalendar))))
      (format (current-error-port) "object: ~s~%" vcalendar)
      (and (string=? (ics-property-value (ics-object-property-ref vcalendar
                                                                    "VERSION"))
                     "2.0")
           (string=? (ics-object-name vevent) "VEVENT")))))


;;; (ics)

(define %ics-string
  (string-append "BEGIN:VCALENDAR\r\n"
                 "VERSION:2.0\r\n"
                 "BEGIN:VEVENT\r\n"
                 "END:VEVENT\r\n"
                 "END:VCALENDAR\r\n"))


(test-assert "ics-string->scm"
  (let* ((vcalendar (car (ics-string->scm %ics-string)))
         (version   (ics-object-property-ref vcalendar "VERSION")))
    (and (string=? (ics-object-name vcalendar) "VCALENDAR")
         (string=? (ics-property-name version)  "VERSION")
         (string=? (ics-property-value version) "2.0")
         (let ((vevent (car (ics-object-components vcalendar))))
           (string=? (ics-object-name vevent) "VEVENT")
           (null? (ics-object-components vevent))))))

;; RFC 5545, 3.1.
(test-assert "ics-string->scm, long content lines"
  (let* ((vcalendar (car (ics-string->scm
                          (string-append
                           "BEGIN:VCALENDAR\r\n"
                           "BEGIN:VEVENT\r\n"
                           "SUMMARY:Bastille,\r\n"
                           " Day,\r\n"
                           " Party\r\n"
                           "END:VEVENT\r\n"
                           "END:VCALENDAR\r\n"))))
         (vevent  (car (ics-object-components vcalendar)))
         (summary (ics-object-property-ref vevent "SUMMARY")))
    (and (string=? (ics-property-name summary) "SUMMARY")
         (let ((summary-value (ics-property-value summary)))
           (and (= (length summary-value) 3)
                (string=? (list-ref summary-value 0) "Bastille")
                (string=? (list-ref summary-value 1) "Day")
                (string=? (list-ref summary-value 2) "Party"))))))

(test-assert "scm->ics-string"
  (let* ((source-str (string-append
                      "BEGIN:VCALENDAR\r\n"
                      "BEGIN:VEVENT\r\n"
                      "SUMMARY:Bastille,Day,Party\r\n"
                      "END:VEVENT\r\n"
                      "END:VCALENDAR\r\n"))
         (object (car (ics-string->scm source-str)))
         (output-str (scm->ics-string object)))
    (string=? source-str output-str)))


;;;

(test-assert "ics-string->scm, property parameters"
  (let* ((source-str (string-append
                      "BEGIN:VCALENDAR\r\n"
                      "DESCRIPTION;ALTREP=\"CID:part3.msg.970415T083000@example.com\":\r\n"
                      " Project XYZ Review Meeting will include the following agenda\r\n"
                      "  items: (a) Market Overview\\, (b) Finances\\, (c) Project Man\r\n"
                      " agement\r\n"
                      "END:VCALENDAR\r\n"))
         (object (car (ics-string->scm source-str)))
         (description (ics-object-property-ref object "DESCRIPTION"))
         (description-value (ics-property-value description)))
    (and (string=? (ics-property-parameter-ref description 'ALTREP)
                   "CID:part3.msg.970415T083000@example.com")
         (string=? description-value
                   "Project XYZ Review Meeting will include the following agenda items: \
(a) Market Overview, (b) Finances, (c) Project Management"))))

(test-assert "ics-string->scm, typed"
  (let* ((source-str (string-append
                      "BEGIN:VCALENDAR\r\n"
                      "DESCRIPTION;ALTREP=\"CID:part3.msg.970415T083000@example.com\":\r\n"
                      " Project XYZ Review Meeting will include the following agenda\r\n"
                      "  items: (a) Market Overview\\, (b) Finances\\, (c) Project Man\r\n"
                      " agement\r\n"
                      "END:VCALENDAR\r\n"))
         (object (car (ics-string->scm source-str #:parse-types? #t)))
         (description (ics-object-property-ref object "DESCRIPTION"))
         (description-value (ics-property-value description))
         (description-type  (ics-property-type  description)))
    (and
     (equal?   description-type 'TEXT)
     (string=? description-value
                   "Project XYZ Review Meeting will include the following agenda items: \
(a) Market Overview, (b) Finances, (c) Project Management"))))



;;;

(test-end "ics")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; ics.scm ends here.
