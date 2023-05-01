;;; float.scm -- iCalendar FLOAT (RFC5545, 3.3.7) type.

;; Copyright (C) 2017-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (ics type float)
  #:use-module (oop goops)
  #:use-module (ics type property)
  #:export     (<ics-property:float>
                ics-property:float?
                ics-property->ics-property:float))


;;; Class definition.

(define-class <ics-property:float> (<ics-property>))

(define-method (initialize (property <ics-property:float>) initargs)
  (next-method)
  (slot-set! property 'type 'FLOAT))


;;; Printers.

(define (%display property port)
  (let ((value (ics-property-value property)))
    (format port "#<ics-property:float ~a: ~a ~a>"
            (ics-property-name property)
            (if (list? value)
                (string-join (map number->string value) ", ")
                value)
            (object-address->string property))))

(define-method (display (property <ics-property:float>) (port <port>))
  (%display property port))

(define-method (write (property <ics-property:float>) (port <port>))
  (%display property port))

(define-method (display (property <ics-property:float>))
  (%display property (current-output-port)))

(define-method (write (property <ics-property:float>))
  (%display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:float? x)
  "Check if X is an instance of <ics-property:float>, return #t if
it is, #f otherwise."
  (is-a? x <ics-property:float>))


;;; Converters.

(define-method (ics-property->ics-property:float
                (property <ics-property>))
  (let ((value (ics-property-value property)))
    (make <ics-property:float>
      #:name  (ics-property-name property)
      #:value (cond
               ((list? value)
                (map string->number value))
               ((vector? value)
                (list->vector (map string->number (vector->list value))))
               (else
                (string->number value)))
      #:parameters (ics-property-parameters property))))

(define-method (ics-typed-property->ics-property
                (property <ics-property:float>))
  (let ((value (ics-property-value property)))
    (make <ics-property>
      #:name       (ics-property-name property)
      #:value      (cond
                    ((list? value)
                     (map number->string value))
                    ((vector? value)
                     (list->vector (map number->string (vector->list value))))
                    (else
                     (number->string value)))
      #:parameters (ics-property-parameters property))))

;;; float.scm ends here.
