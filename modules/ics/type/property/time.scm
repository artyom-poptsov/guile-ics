;;; time.scm -- iCalendar TIME (RFC5545, 3.3.1) type.

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

(define-module (ics type property time)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:time>
                ics-property:time?
                ics-property->ics-property:time))


;;; Class definition.

(define-class <ics-property:time> (<ics-property>))

(define-method (initialize (property <ics-property:time>) initargs)
  (next-method)
  (slot-set! property 'type 'TIME))


;;; Printers.

(define-method (display (property <ics-property:time>) (port <port>))
  (format port "#<ics-property:time ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property value property)
          (object-address->string property)))

(define-method (write (property <ics-property:time>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:time>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:time>))
  (display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:time? x)
  "Check if X is an instance of <ics-property:time>, return #t if
it is, #f otherwise."
  (is-a? x <ics-property:time>))


;;; Converters

(define-method (ics-property->ics-property:time
                (property <ics-property>))
  (make <ics-property:time>
    #:name  (ics-property-name property)
    #:value (ics-property-value property)
    #:parameters (ics-property-parameters property)))

;;; time.scm ends here.
