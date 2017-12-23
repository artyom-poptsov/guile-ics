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

(define-module (ics type property utc-offset)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:utc-offset>
                ics-property:utc-offset?
                ics-property->ics-property:utc-offset))


;;; Class definition.

(define-class <ics-property:utc-offset> (<ics-property>))

(define-method (initialize (property <ics-property:utc-offset>) initargs)
  (next-method)
  (slot-set! property 'type 'UTC-OFFSET))


;;; Printers.

(define (%display property port)
  (format port "#<ics-property:utc-offset ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (display (property <ics-property:utc-offset>) (port <port>))
  (%display property port))

(define-method (write (property <ics-property:utc-offset>) (port <port>))
  (%display property port))

(define-method (display (property <ics-property:utc-offset>))
  (%display property (current-output-port)))

(define-method (write (property <ics-property:utc-offset>))
  (%display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:utc-offset? x)
  "Check if X is an instance of <ics-property:utc-offset>, return #t if
it is, #f otherwise."
  (is-a? x <ics-property:utc-offset>))


;;; Converters

(define-method (ics-property->ics-property:utc-offset
                (property <ics-property>))
  (make <ics-property:utc-offset>
    #:name  (ics-property-name property)
    #:value (ics-property-value property)
    #:parameters (ics-property-parameters property)))

;;; time.scm ends here.
