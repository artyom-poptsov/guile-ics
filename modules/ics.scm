;;; ics.scm -- iCalendar parser (main module)

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

;; Guile-ICS is iCalendar format (RFC5545) [1] parser for GNU Guile.
;;
;; [1] https://tools.ietf.org/html/rfc5545


;;; Code:

(define-module (ics)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (srfi  srfi-26)
  #:use-module ((string transform)
                #:select (escape-special-chars))
  #:use-module (oop goops)
  ;; Guile-ICS
  #:use-module (ics common)
  #:use-module (ics stream)
  #:use-module (ics object)
  #:use-module (ics property)
  #:export (ics->scm
            ics-string->scm
            ics->stream
            scm->ics
            scm->ics-string
            ics-pretty-print
            ics-describe)
  #:re-export (ics-object-name
               ics-object-properties
               ics-object-components
               ics-debug-set!))


;;; iCalendar to Scheme converters.

(define* (ics->scm #:optional (port (current-input-port))
                   #:key (parse-types? #f))
  "Parse input data from a PORT, return a list of iCalendar objects.
If no port is specified, read the data from the current input port."
  (ics-stream->scm (make <ics-stream>
                     #:source port
                     #:parse-types? parse-types?)))

(define* (ics-string->scm str #:key (parse-types? #f))
  "Parse iCalendar string STR, return a new iCalendar object."
  (ics-stream->scm (make <ics-stream>
                     #:source str
                     #:parse-types? parse-types?)))

(define* (ics->stream #:optional (port (current-input-port))
                      #:key (parse-types? #f))
  "Convert an ICS stream to an SRFI-41 stream.  Return the stream."
  (ics-stream->scm-stream (make <ics-stream>
                            #:source port
                            #:parse-types? parse-types?)))


;;; Scheme to iCalendar converters.

(define* (scm->ics ics-object #:optional (port (current-output-port)))
  "Convert an ICAL-OBJECT to an iCalendar format and print it to a
PORT.  If no port is specified, current output port is used."
  (ics-object->ics ics-object port))

(define (scm->ics-string ics-object)
  "Convert an ICAL-OBJECT to an iCalendar format string; return the
string."
  (with-output-to-string (lambda () (scm->ics ics-object))))


;;; iCalendar printers.

(define* (ics-pretty-print ics-object
                           #:optional (port (current-output-port))
                           #:key (indent 2) (show-types? #f))
  "Pretty-print an ICAL-OBJECT object to a PORT.  Note that the output
is intended for human to comprehent, not to a machine to parse."

  (define (print-icalprops props current-indent)
    "Print iCalendar properties from a PROPS list using a
CURRENT-INDENT for indentation."
    (define (vector->value-list raw-value)
      (string-join
       (map (lambda (element)
              (cond
               ((pair? element)
                (let ((key   (car element))
                      (value (cdr element)))
                  (format #f "~a=~a"
                          key
                          (cond
                           ((list? value)
                            (string-join
                             (map (lambda (e)
                                    (cond
                                     ((string? e)
                                      e)
                                     ((number? e)
                                      (number->string e))
                                     ((symbol? e)
                                      (symbol->string e))
                                     (else
                                      e)))
                                  value)
                             ","))
                           ((string? value)
                            (escape-special-chars value #\, #\\ ))
                           (else
                            value)))))))
            (vector->list raw-value))
       "; "))

    (for-each (lambda (e)
                (let ((s (make-string current-indent #\space))
                      (type (ics-property-type e))
                      (e (ics-typed-property->ics-property e)))
                  (if (and show-types? type)
                      (format port "~a~a (~a)" s (ics-property-name e) type)
                      (format port "~a~a" s (ics-property-name e)))
                  (for-each (lambda (property)
                              (format port ";~a=~a"
                                      (car property)
                                      (cdr property)))
                            (ics-property-parameters e))
                  (let* ((raw-value (ics-property-value e))
                         (value (cond
                                 ((list? raw-value)
                                  (string-join
                                   (map (cut escape-special-chars <> #\, #\\)
                                        raw-value)
                                   ","))
                                 ((vector? raw-value)
                                  (vector->value-list raw-value))
                                 (else
                                  (escape-special-chars raw-value #\, #\\)))))
                    (format port ": ~a\n" value))))

              props))

  (define (print-components components current-indent)
    "Print components from a COMPONENTS list using a CURRENT-INDENT
for indentation."
    (let ((s (make-string current-indent #\space)))
      (for-each (lambda (component)
                  (let ((cname (ics-object-name component)))
                    (format port "~aBEGIN: ~a\n" s cname)
                    (print-icalprops (ics-object-properties component)
                                     (+ current-indent indent))
                    (print-components (ics-object-components component)
                                      (+ current-indent indent))
                    (format port "~aEND: ~a\n" s cname)))
                components)))

  (define (print-object)
    "Print a VCALENDAR object from an iCalendar stream."
    (write-line (format #f "BEGIN: ~a" (ics-object-name ics-object)) port)
    (print-icalprops (ics-object-properties ics-object) indent)
    (print-components (ics-object-components ics-object) indent)
    (write-line (format #f "END: ~a" (ics-object-name ics-object)) port))

  (print-object))


;;;

(define-generic ics-describe)

(define-method (ics-describe (object <ics-object>))
  (ics-describe object 0))

(define-method (ics-describe (object <ics-object>) (indent <number>))
  (let ((indent-string (make-string indent #\space))
        (description (ics-calendar-component-lookup (ics-object-name object))))
    (format #t ";;; ~a ~a~a~%"
            indent-string
            (ics-object-name object)
            (if description
                (format #f " (~a)" (assoc-ref description 'title))
                ""))
    (when description
      (format #t ";;; ~a * Description: ~a~%"
              indent-string
              (string-join (assoc-ref description 'purpose)))
      (format #t ";;; ~a * Reference: ~a~%"
              indent-string
              (assoc-ref description 'uri)))
    (for-each (lambda (property)
                (ics-describe property (+ indent 4)))
              (ics-object-properties object))
    (unless (null? (ics-object-components object))
      (for-each (lambda (component)
                  (ics-describe component (+ indent 4)))
                (ics-object-components object)))))

(define-method (ics-describe (property <ics-property>))
  (ics-describe property 0))

(define-method (ics-describe (property <ics-property>) (indent <number>))
  (let ((indent-string (make-string indent #\space)))
    (format #t ";;; ~a ~a (~a)~%"
            indent-string
            (ics-property-name property)
            (ics-describe-type (ics-property-name->type (ics-property-name property))))
    (ics-describe-type (ics-property-name->type (ics-property-name property)))
    (format #t ";;; ~a    ~a~%" indent-string (ics-property-value property))
    (unless (null? (ics-property-parameters property))
      (for-each (lambda (parameter)
                  (format #t ";;; ~a    ~50a ~20a~%"
                          (make-string (+ indent 4) #\space)
                          (car parameter)
                          (cdr parameter)))
                (ics-property-parameters property)))))

(define-method (ics-describe (name <symbol>))
  (ics-describe name 0))

(define-method (ics-describe-type (name <symbol>))
  (case name
    ;; Types.
    ((BINARY)
     "BINARY: Binary type: RFC5545, 3.3.1")
    ((BOOLEAN)
     "BOOLEAN: Boolean type: RFC5545, 3.3.2")
    ((CAL-ADDRESS)
     "CAL-ADDRESS: Calendar User Address type -- RFC5545, 3.3.3)")
    ((DATE)
     "DATE: Date type: RFC5545, 3.3.4")
    ((DATE-TIME)
     "DATE-TIME: Date-Time type: RFC5545, 3.3.5")
    ((DURATION)
     "DURATION: Duration type: RFC5545, 3.3.6")
    ((FLOAT)
     "FLOAT: Float type: RFC5545, 3.3.7")
    ((INTEGER)
     "INTEGER: RFC5545, 3.3.8")
    ((PERIOD)
     "PERIOD: RFC5545, 3.3.9")
    ((RECUR)
     "RECUR: RFC5545, 3.3.10")
    ((TEXT)
     "TEXT: Text type: RFC5545, 3.3.11")
    ((TIME)
     "TIME: RFC5545, 3.3.12")
    ((URI)
     "URI: RFC5545, 3.3.13")
    ((UTC-OFFSET)
     "UTC-OFFSET: RFC5545, 3.3.14")))


;;; ics.scm ends here.
