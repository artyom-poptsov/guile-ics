;;; binary.scm -- iCalendar DATE-TIME (RFC5545, 3.3.5) type.

;; Copyright (C) 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (ics type property date-time)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (ics type property property)
  #:export     (<ics-property:date-time>
                ics-property:date-time?
                ics-property->ics-property:date-time
                ics-property:date-time->ics-property))


;;; Class definition.

(define-class <ics-property:date-time> (<ics-property>))

(define-method (initialize (property <ics-property:date-time>))
  (next-method)
  (slot-set! property 'type 'DATE-TIME))


;;; Printers.

(define-method (display (property <ics-property:date-time>) (port <port>))
  (let ((tm->iso8601 (lambda (tm) (strftime "%FT%TZ" tm)))
        (value        (ics-property-value property)))
    (format port "#<ics-property:date-time ~a: ~a ~a>"
            (ics-property-name property)
            (if (list? value)
                (string-join (map tm->iso8601 value) ",")
                (tm->iso8601 value))
            (object-address->string property))))

(define-method (write (property <ics-property:date-time>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:date-time>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:date-time>))
  (display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:date-time? x)
  "Check if X is an instance of <ics-property:date-time>, return #t if
it is, #f otherwise."
  (is-a? x <ics-property:date-time>))


;;; Converters.

(define %date-time-fmt "%Y%m%dT%H%M%S%Z")

(define-method (ics-property->ics-property:date-time
                (property <ics-property>))
  (let ((value (ics-property-value property))
        (date-time->tm (lambda (date-time)
                         (car (strptime %date-time-fmt date-time)))))
    (make <ics-property:date-time>
      #:name  (ics-property-name property)
      #:value (if (list? value)
                  (map date-time->tm value)
                  (date-time->tm value))
      #:parameters (ics-property-parameters property))))

(define-method (ics-property:date-time->ics-property
                (property <ics-property:date-time>))
  (let ((tm->date-time (cut strftime %date-time-fmt <>))
        (value         (ics-property-value property)))
    (make <ics-property>
      #:name        (ics-property-name property)
      #:value       (if (list? value)
                        (map tm->date-time value)
                        (tm->date-time value))
      #:parameters  (ics-property-parameters property))))

;;; date-time.scm ends here.
