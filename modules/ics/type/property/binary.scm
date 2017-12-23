;;; binary.scm -- iCalendar BINARY (RFC5545, 3.3.1.) type.

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

(define-module (ics type property binary)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export (<ics-property:binary>
            ics-property:binary?
            ics-property->ics-property:binary
            ics-property:binary-encoding))


;;; Class definition.

(define-class <ics-property:binary> (<ics-property>))

(define-method (initialize (property <ics-property:binary>) initargs)
  (next-method)
  (slot-set! property 'type 'BINARY))

(define-method (ics-property:binary-encoding (property <ics-property:binary>))
  (ics-property-parameter-ref property 'ENCODING))


;;; Printers.

(define (%display property port)
  (format port "#<ics-property:binary ~a ENCODING: ~a FORMAT-TYPE: ~a ~a>"
          (ics-property-name property)
          (ics-property-parameter-ref property 'ENCODING)
          (ics-property-parameter-ref property 'FMTTYPE)
          (object-address->string property)))

(define-method (display (property <ics-property:binary>) (port <port>))
  (%display property port))

(define-method (write (property <ics-property:binary>) (port <port>))
  (%display property port))

(define-method (display (property <ics-property:binary>))
  (%display property (current-output-port)))

(define-method (write (property <ics-property:binary>))
  (%display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:binary? x)
  "Check if X is an instance of <ics-property:binary>, return #t if it
is, #f otherwise."
  (is-a? x <ics-property:binary>))


;;; Converters

(define-method (ics-property->ics-property:binary
                (property <ics-property>))
  (make <ics-property:binary>
    #:name        (ics-property-name          property)
    #:value       (ics-property-value         property)
    #:parameters  (ics-property-parameters    property)))

(define-method (ics-typed-property->ics-property
                (property <ics-property:binary>))
  (make <ics-property>
    #:name        (ics-property-name property)
    #:value       (ics-property-value property)
    #:parameters  (ics-property-parameters property)))

;;; binary.scm ends here.
