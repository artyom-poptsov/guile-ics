;;; ics.scm -- Tests for ICS parser.

;; Copyright (C) 2016-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (ice-9 streams)
             (ics)
             (ics common)
             (ics type object)
             (ics type property))


(define %test-suite-name "ics")

(test-begin %test-suite-name)


(test-assert "make-ics-object"
  (let ((obj (make <ics-object> #:name "VCALENDAR")))
    (string=? (ics-object-name obj) "VCALENDAR")))


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


;;; iCalendar streams -> ICE-9 streams converter tests.

(test-assert "ics->stream"
  (with-input-from-string
      (string-append
       "BEGIN:VCALENDAR\r\n"
       "DESCRIPTION;ALTREP=\"CID:part3.msg.970415T083000@example.com\":\r\n"
       " Project XYZ Review Meeting will include the following agenda\r\n"
       "  items: (a) Market Overview\\, (b) Finances\\, (c) Project Man\r\n"
       " agement\r\n"
       "END:VCALENDAR\r\n")
    (lambda ()
      (ics->stream))))

(test-assert "ics->stream: 1st object"
  (with-input-from-string
      (string-append
       "BEGIN:VCALENDAR\r\n"
       "DESCRIPTION:Object description\r\n"
       "END:VCALENDAR\r\n")
    (lambda ()
      (stream-car (ics->stream)))))

(test-equal "ics->stream: 2nd object"
  "The second object"
  (with-input-from-string
      (string-append
       "BEGIN:VCALENDAR\r\n"
       "DESCRIPTION:The first object\r\n"
       "END:VCALENDAR\r\n"
       "BEGIN:VCALENDAR\r\n"
       "DESCRIPTION:The second object\r\n"
       "END:VCALENDAR\r\n")
    (lambda ()
      (let ((obj (stream-car (stream-cdr (ics->stream)))))
        (ics-property-value (ics-object-property-ref obj "DESCRIPTION"))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; ics.scm ends here.
