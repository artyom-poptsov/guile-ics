;;; boolean.scm -- iCalendar BOOLEAN (RFC5545, 3.3.2) type.

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

(define-module (ics type boolean)
  #:use-module (oop goops)
  #:use-module (ics type property)
  #:export     (<ics-property:boolean>
                ics-property:boolean?
                ics-property->ics-property:boolean))


;;; Class definition.

(define-class <ics-property:boolean> (<ics-property>))

(define-method (initialize (property <ics-property:boolean>) initargs)
  (next-method)
  (slot-set! property 'type 'BOOLEAN))


;;; Printers.

(define (%display property port)
  (format port "#<ics-property:boolean ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (display (property <ics-property:boolean>) (port <port>))
  (%display property port))

(define-method (write (property <ics-property:boolean>) (port <port>))
  (%display property port))

(define-method (display (property <ics-property:boolean>))
  (%display property (current-output-port)))

(define-method (write (property <ics-property:boolean>))
  (%display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:boolean? x)
  "Check if X is an instance of <ics-property:boolean>, return #t if it
is, #f otherwise."
  (is-a? x <ics-property:boolean>))


;;; Converters.

(define-method (ics-property->ics-property:boolean
                (property <ics-property>))
  (let ((value (ics-property-value property)))
    (make <ics-property:boolean>
      #:name  (ics-property-name property)
      #:value (cond
               ;; Boolean values are case-insensitive.
               ((string-ci=? value "TRUE")  #t)
               ((string-ci=? value "FALSE") #f)
               (else (error "Unknown property value (expected BOOLEAN)"
                            value)))
      #:parameters (ics-property-parameters property))))

(define-method (ics-typed-property->ics-property
                (property <ics-property:boolean>))
  (make <ics-property>
    #:name  (ics-property-name property)
    ;; Although the RFC states that boolean values are
    ;; case-insensitive, we're using uppercase spelling
    ;; for unification sake.
    #:value (if (ics-property-value property)
                "TRUE"
                "FALSE")
    #:parameters (ics-property-parameters property)))

;;; boolean.scm ends here.
