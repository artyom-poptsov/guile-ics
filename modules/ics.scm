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


;;; Code:

(define-module (ics)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi  srfi-26)
  #:use-module ((string transform)
                #:select (escape-special-chars))
  #:use-module (oop goops)
  ;; Guile-ICS
  #:use-module (ics common)
  #:use-module (ics fsm)
  #:use-module (ics parser)
  #:use-module (ics type ics-stream)
  #:use-module (ics type ics-object)
  #:use-module (ics type ics-property)
  #:export (ics->scm
            ics-string->scm
            ics->stream
            scm->ics
            scm->ics-string
            ics-pretty-print)
  #:re-export (ical-object-name
               ical-object-properties
               ical-object-components))


;;; iCalendar to Scheme converters.

(define* (ics->scm #:optional (port (current-input-port)))
  "Parse input data from a PORT, return a list of iCalendar objects.
If no port is specified, read the data from the current input port."
  (ical-stream->scm (make <ical-stream> #:source port)))

(define (ics-string->scm str)
  "Parse iCalendar string STR, return a new iCalendar object."
  (ical-stream->scm (make <ical-stream> #:source str)))

(define* (ics->stream #:optional (port (current-input-port)))
  "Convert an ICS stream to an SRFI-41 stream.  Return the stream."
  (ical-stream->scm-stream (make <ical-stream> #:source port)))


;;; Scheme to iCalendar converters.

(define* (scm->ics ical-object #:optional (port (current-output-port)))
  "Convert an ICAL-OBJECT to an iCalendar format and print it to a
PORT.  If no port is specified, current output port is used."
  (ical-object->ics ical-object port))

(define (scm->ics-string ical-object)
  "Convert an ICAL-OBJECT to an iCalendar format string; return the
string."
  (with-output-to-string (lambda () (scm->ics ical-object))))


;;; iCalendar printers.

(define* (ics-pretty-print ical-object
                           #:optional (port (current-output-port))
                           #:key (indent 2))
  "Pretty-print an ICAL-OBJECT object to a PORT.  Note that the output
is intended for human to comprehent, not to a machine to parse."

  (define (print-icalprops props current-indent)
    "Print iCalendar properties from a PROPS list using a
CURRENT-INDENT for indentation."
    (for-each (lambda (e)
                (let ((s (make-string current-indent #\space)))
                  (format port "~a~a" s (ical-property-name e))
                  (for-each (lambda (property)
                              (format port ";~a=~a"
                                      (car property)
                                      (cdr property)))
                            (ical-property-parameters e))
                  (let* ((raw-value (ical-property-value e))
                         (value (if (list? raw-value)
                                    (string-join
                                     (map (cut escape-special-chars <> #\, #\\)
                                          raw-value)
                                     ",")
                                    (escape-special-chars raw-value #\, #\\))))
                    (format port ": ~a\n" value))))

              props))

  (define (print-components components current-indent)
    "Print components from a COMPONENTS list using a CURRENT-INDENT
for indentation."
    (let ((s (make-string current-indent #\space)))
      (for-each (lambda (component)
                  (let ((cname (ical-object-name component)))
                    (format port "~aBEGIN: ~a\n" s cname)
                    (print-icalprops (ical-object-properties component)
                                     (+ current-indent indent))
                    (print-components (ical-object-components component)
                                      (+ current-indent indent))
                    (format port "~aEND: ~a\n" s cname)))
                components)))

  (define (print-vcalendar)
    "Print a VCALENDAR object from an iCalendar stream."
    (write-line "BEGIN: VCALENDAR" port)
    (print-icalprops (ical-object-properties ical-object) indent)
    (print-components (ical-object-components ical-object) indent)
    (write-line "END: VCALENDAR" port))

  (print-vcalendar))


;;; ics.scm ends here.
