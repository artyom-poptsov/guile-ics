;;; integer.scm -- iCalendar INTEGER (RFC5545, 3.3.8) type.

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

(define-module (ics type property integer)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:integer>
                ics-property:integer?
                ics-property->ics-property:integer))


;;; Class definition.

(define-class <ics-property:integer> (<ics-property>))

(define-method (initialize (property <ics-property:integer>))
  (next-method)
  (slot-set! property 'type 'INTEGER))


;;; Printers.

(define-method (display (property <ics-property:integer>) (port <port>))
  (format port "#<ics-property:integer ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:integer>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:integer>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:integer>))
  (display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:integer? x)
  "Check if X is an instance of <ics-property:integer>, return #t if
it is, #f otherwise."
  (is-a? x <ics-property:integer>))


;;; Converters.

(define-method (ics-property->ics-property:integer
                (property <ics-property>))
  (make <ics-property:integer>
    #:name  (ics-property-name property)
    #:value (string->number (ics-property-value property))
    #:parameters (ics-property-parameters property)))

(define-method (ics-typed-property->ics-property
                (property <ics-property:integer>))
  (let ((value (ics-property-value property)))
    (make <ics-property>
      #:name  (ics-property-name property)
      #:value (if (list? value)
                  (map number->string value)
                  (number->string value))
      #:parameters (ics-property-parameters property))))

;;; integer.scm ends here.
