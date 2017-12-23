;;; duration.scm -- iCalendar DURATION (RFC5545, 3.3.6) type.

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

(define-module (ics type property duration)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:duration>
                ics-property:duration?
                ics-property->ics-property:duration
                ics-property:duration->ics-property))


;;; Class definition.

(define-class <ics-property:duration> (<ics-property>))

(define-method (initialize (property <ics-property:duration>))
  (next-method)
  (slot-set! property 'type 'DURATION))


;;; Printers.

(define-method (display (property <ics-property:duration>) (port <port>))
  (format port "#<ics-property:duration ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:duration>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:duration>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:duration>))
  (display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:duration? x)
  "Check if X is an instance of <ics-property:duration>, return #t if
it is, #f otherwise."
  (is-a? x <ics-property:duration>))


;;; Converters.

(define-method (ics-property->ics-property:duration
                (property <ics-property>))
  (make <ics-property:duration>
    #:name       (ics-property-name property)
    #:value      (ics-property-value property)
    #:parameters (ics-property-parameters property)))

(define-method (ics-property:duration->ics-property
                (property <ics-property:duration>))
  (make <ics-property>
    #:name       (ics-property-name property)
    #:value      (ics-property-value property)
    #:parameters (ics-property-parameters property)))

;;; duration.scm ends here.
