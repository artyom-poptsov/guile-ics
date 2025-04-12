;;; ics.scm -- Tests for ICS parser.

;; Copyright (C) 2016-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (oop goops)
             (ice-9 rdelim)
             (ice-9 streams)
             (tests common)
             (ics)
             (ics common)
             (ics object)
             (ics property))


(define %test-suite-name "ics")

(configure-test-logging! %test-suite-name)

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


(test-equal "ics-string->scm: object name"
  "VCALENDAR"
  (let ((vcalendar (car (ics-string->scm %ics-string))))
    (ics-object-name vcalendar)))

(test-equal "ics-string->scm: VERSION: name"
  "VERSION"
  (let ((vcalendar (car (ics-string->scm %ics-string))))
    (ics-property-name (ics-object-property-ref vcalendar "VERSION"))))

(test-equal "ics-string->scm: VERSION: value"
  "2.0"
  (let ((vcalendar (car (ics-string->scm %ics-string))))
    (ics-property-value (ics-object-property-ref vcalendar "VERSION"))))

(test-equal "ics-string->scm: VEVENT: check component name"
  "VEVENT"
  (let* ((vcalendar (car (ics-string->scm %ics-string)))
         (vevent    (car (ics-object-components vcalendar))))
    (ics-object-name vevent)))

(test-equal "ics-string->scm: VEVENT: ensure that it has none components"
  '()
  (let* ((vcalendar (car (ics-string->scm %ics-string)))
         (vevent    (car (ics-object-components vcalendar))))
    (ics-object-components vevent)))

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


(define %ics-string-2
  (string-append
   "BEGIN:VCALENDAR\r\n"
   "BEGIN:VEVENT\r\n"
   "SUMMARY:Bastille,Day,Party\r\n"
   "END:VEVENT\r\n"
   "END:VCALENDAR\r\n"))

(test-equal "scm->ics-string"
  %ics-string-2
  (let* ((source-str %ics-string-2)
         (object     (car (ics-string->scm source-str))))
    (scm->ics-string object)))


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



(test-equal "ics->stream: GEO"
  #(48.85299 2.36885)
  (with-input-from-string
      (string-append
       "BEGIN:VCALENDAR\r\n"
       "BEGIN:VEVENT\r\n"
       "GEO:48.85299;2.36885\r\n"
       "END:VEVENT\r\n"
       "END:VCALENDAR\r\n")
    (lambda ()
      (let ((obj (stream-car (ics->stream #:parse-types? #t))))
        (ics-property-value
         (ics-object-property-ref (car (ics-object-components obj)) "GEO"))))))


;; Recurrence Rule (RECUR)

(test-assert "ics->stream: RRULE"
  (with-input-from-string
      (string-append
       "BEGIN:VCALENDAR\r\n"
       "BEGIN:VEVENT\r\n"
       "RRULE:FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;BYMINUTE=30\r\n"
       "END:VEVENT\r\n"
       "END:VCALENDAR\r\n")
    (lambda ()
      (let ((obj (stream-car (ics->stream #:parse-types? #t))))
        (ics-object-property-ref (car (ics-object-components obj))
                                 "RRULE")))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; ics.scm ends here.
