;;; cal-address.scm -- iCalendar CAL-ADDRESS (RFC5545, 3.3.3) type.

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

(define-module (ics type cal-address)
  #:use-module (oop goops)
  #:use-module (ics type property)
  #:export     (<ics-property:cal-address>
                ics-property:cal-address?
                ics-property->ics-property:cal-address))


;;; Class definition.

(define-class <ics-property:cal-address> (<ics-property>))

(define-method (initialize (ics-property <ics-property:cal-address>) initargs)
  (next-method)
  (slot-set! ics-property 'type 'CAL-ADDRESS))


;;; Printers.

(define (%display property port)
  (format port "#<ics-property:cal-address ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (display (property <ics-property:cal-address>) (port <port>))
  (%display property port))

(define-method (write (property <ics-property:cal-address>) (port <port>))
  (%display property port))

(define-method (display (property <ics-property:cal-address>))
  (%display property (current-output-port)))

(define-method (write (property <ics-property:cal-address>))
  (%display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:cal-address? x)
  "Check if X is an instance of <ics-property:cal-address>, return #t if it
is, #f otherwise."
  (is-a? x <ics-property:cal-address>))


;;; Converters.

(define-method (ics-property->ics-property:cal-address
                (property <ics-property>))
  (make <ics-property:cal-address>
    #:name  (ics-property-name property)
    #:value (ics-property-value property)
    #:parameters (ics-property-parameters property)))

(define-method (ics-typed-property->ics-property
                (property  <ics-property:cal-address>))
  (make <ics-property>
    #:name        (ics-property-name property)
    #:value       (ics-property-value property)
    #:parameters  (ics-property-parameters property)))

;;; cal-address.scm ends here.
