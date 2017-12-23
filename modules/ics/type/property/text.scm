;;; text.scm -- iCalendar TEXT (RFC5545, 3.3.10) type.

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

(define-module (ics type property text)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:text>
                ics-property:text?
                ics-property->ics-property:text))


;;; Class definition.

(define-class <ics-property:text> (<ics-property>))

(define-method (initialize (property <ics-property:text>) initargs)
  (next-method)
  (slot-set! property 'type 'TEXT))


;;; Printers.

(define (%display property port)
  (format port "#<ics-property:text ~a ~a>"
          (ics-property-name property)
          (object-address->string property)))

(define-method (display (property <ics-property:text>) (port <port>))
  (%display property port))

(define-method (write (property <ics-property:text>) (port <port>))
  (%display property port))

(define-method (display (property <ics-property:text>))
  (%display property (current-output-port)))

(define-method (write (property <ics-property:text>))
  (%display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:text? x)
  "Check if X is an instance of <ics-property:text>, return #t if
it is, #f otherwise."
  (is-a? x <ics-property:text>))


;;; Converters

(define-method (ics-property->ics-property:text
                (property <ics-property>))
  (make <ics-property:text>
    #:name  (ics-property-name property)
    #:value (ics-property-value property)
    #:parameters (ics-property-parameters property)))


;;; text.scm ends here.
