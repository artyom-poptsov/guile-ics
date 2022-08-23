;;; period.scm -- iCalendar PERIOD (RFC5545, 3.3.9) type.

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

(define-module (ics type property period)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:period>
                ics-property:period?
                ics-property->ics-property:period))



;;; Class definition.

(define-class <ics-property:period> (<ics-property>))

(define-method (initialize (property <ics-property:period>) initargs)
  (next-method)
  (slot-set! property 'type 'PERIOD))


;;; Printers.

(define (%display property port)
  (format port "#<ics-property:period ~a: ~a/~a ~a>"
          (ics-property-name property)
          (car (ics-property-value property))
          (cadr (ics-property-value property))
          (object-address->string property)))

(define-method (display (property <ics-property:period>) (port <port>))
  (%display property port))

(define-method (write (property <ics-property:period>) (port <port>))
  (%display property port))

(define-method (display (property <ics-property:period>))
  (%display property (current-output-port)))

(define-method (write (property <ics-property:period>))
  (%display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:period? x)
  "Check if X is an instance of <ics-property:period>, return #t if
it is, #f otherwise."
  (is-a? x <ics-property:period>))


;;; Converters.

;; TODO: Handle value lists.
(define-method (ics-property->ics-property:period
                (property <ics-property>))
  (make <ics-property:period>
    #:name  (ics-property-name property)
    #:value (string-split (ics-property-value property) #\:)
    #:parameters (ics-property-parameters property)))

(define-method (ics-typed-property->ics-property
                (property <ics-property:period>))
  (make <ics-property>
    #:name  (ics-property-name property)
    #:value (string-join (ics-property-value property) ":")
    #:parameters (ics-property-parameters property)))

;;; period.scm ends here.
