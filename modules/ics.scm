;;; ics.scm -- iCalendar parser (main module)

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

;; Guile-ICS is iCalendar format (RFC5545) [1] parser for GNU Guile.
;;
;; [1] https://tools.ietf.org/html/rfc5545
;;
;; Input example (from RFC5545):

#!
BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
!#

;; Output example:

#!
(((ICALPROPS
    ((PRODID . "-//hacksw/handcal//NONSGML v1.0//EN")
     (VERSION . "2.0")))
  (COMPONENT
    ((VEVENT
       (ICALPROPS
         ((SUMMARY . "Bastille Day Party")
          (DTEND . "19970715T040000Z")
          (DTSTART . "19970714T170000Z")
          (DTSTAMP . "19970610T172345Z")
          (UID . "19970610T172345Z-AF23B2@example.com")))
       (COMPONENT ()))))))
!#


;;; Code:

(define-module (ics)
  #:use-module (ice-9 rdelim)
  ;; Guile-ICS
  #:use-module (ics common)
  #:use-module (ics fsm)
  #:use-module (ics parser)
  #:use-module (ics streams)
  #:use-module (ics ical-object)
  #:export (ics->scm ics-string->scm scm->ics ics-pretty-print)
  #:re-export (ics->stream))


;;;

(define (ics-read parser)
  (fsm-read-ical-stream parser '()))


;;;

(define* (ics->scm #:optional (port (current-input-port)))
  (ics-read (make-parser port)))

(define (ics-string->scm str)
  "Parse ICS string STR."
  (ics-read (make-string-parser str)))


;;;

(define* (ics-pretty-print vcalendar
                           #:optional (port (current-output-port))
                           #:key (indent 2))
  "Pretty-print VCALENDAR object to a PORT.  Note that the output is
intended for human to comprehent, not to a machine to parse."
  (define (print-icalprops props current-indent)
    (for-each (lambda (e)
                (let ((s (make-string current-indent #\space)))
                  (format port "~a~a: ~a\n" s (car e) (cdr e))))
              props))
  (define (print-components components current-indent)
    (let ((s (make-string current-indent #\space)))
      (for-each (lambda (component)
                  (let ((cname  (car component))
                        (object (cdr component)))
                    (format port "~aBEGIN: ~a\n" s cname)
                    (print-icalprops (ical-object-icalprops object)
                                     (+ current-indent indent))
                    (print-components (ical-object-component object)
                                      (+ current-indent indent))
                    (format port "~aEND: ~a\n" s cname)))
                components)))
  (define (print-vcalendar)
    (write-line "BEGIN: VCALENDAR" port)
    (print-icalprops (ical-object-icalprops vcalendar) indent)
    (print-components (ical-object-component vcalendar) indent)
    (write-line "END: VCALENDAR" port))

  (print-vcalendar))

(define* (scm->ics vcalendar #:optional (port (current-output-port)))
  "Convert VCALENDAR list to an vcalendar format.  Print the output to
a PORT."
  (define (print-icalprops props)
    (for-each (lambda (e) (format port "~a:~a\r\n" (car e) (cdr e)))
              props))
  (define (print-components components)
    (for-each (lambda (component)
                (let ((cname  (car component))
                      (object (cdr component)))
                  (format port "BEGIN:~a\r\n" cname)
                  (print-icalprops (ical-object-icalprops object))
                  (print-components (ical-object-component object))
                  (format port "END:~a\r\n" cname)))
              components))
  (define (print-vcalendar)
    (display "BEGIN:VCALENDAR\r\n" port)
    (print-icalprops (ical-object-icalprops vcalendar))
    (print-components (ical-object-component vcalendar))
    (display "END:VCALENDAR\r\n" port))

  (print-vcalendar))

;;; ics.scm ends here.
