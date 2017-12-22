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
            ics-property:binary->ics-property
            ics-property:binary-encoding))


;;; Class definition.

(define-class <ics-property:binary> (<ics-property>)
  ;; string
  ;;
  ;; Either "8BIT" (RFC2045) or "BASE64" (RFC4648).
  (encoding
   #:accessor   ics-property:binary-encoding
   #:init-value #f
   #:init-keyword #:encoding))

(define-method (initialize (property <ics-property:binary>) initargs)
  (next-method)
  (slot-set! property 'type 'BINARY))


;;; Printers.

(define-method (display (property <ics-property:binary>) (port <port>))
  (format port "#<ics-property:binary ~a ENCODING: ~a FORMAT-TYPE: ~a ~a>"
          (ics-property-name property)
          (ics-property:binary-encoding property)
          (ics-property-format-type property)
          (object-address->string property)))

(define-method (write (property <ics-property:binary>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:binary>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:binary>))
  (display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:binary? x)
  "Check if X is an instance of <ics-property:binary>, return #t if it
is, #f otherwise."
  (is-a? x <ics-property:binary>))

(define-method (equal? (property1 <ics-property:binary>)
                       (property2 <ics-property:binary>))
  "Compare PROPERTY1 with PROPERTY2.  Return #t if the given
properties are identical, #f otherwise."
  (and (%ics-property=? property1 property2)
       (equal? (ics-property:binary-encoding property1)
               (ics-property:binary-encoding property2))))


;;; Converters

(define-method (ics-property->ics-property:binary
                (property <ics-property>))
  (make <ics-property:binary>
    #:name        (ics-property-name          property)
    #:parameters  (ics-property-parameters    property)
    #:format-type (ics-property-parameter-ref property 'FMTTYPE)
    #:encoding    (string->symbol (ics-property-parameter-ref property
                                                              'ENCODING))
    #:value       (ics-property-value         property)))

(define-method (ics-property:binary->ics-property
                (property <ics-property:binary>))
  (make <ics-property>
    #:name        (ics-property-name property)
    #:type        #f
    #:format-type #f
    #:value       (ics-property-value property)
    #:parameters  `((FMTTYPE  . ,(ics-property-format-type property))
                    (ENCODING . ,(ics-property:binary-encoding property)))))

;;; binary.scm ends here.
