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
            ics-property->ics-property:binary
            ics-property:binary-encoding))


;;; Class definition.

(define-class <ics-property:binary> (<ics-property>)
  ;; symbol
  ;;
  ;; Either 8BIT (RFC2045) or BASE64 (RFC4648).
  (encoding
   #:accessor   ics-property:binary-encoding
   #:init-value #f
   #:init-keyword #:encoding))

(define-method (initialize (property <ics-property:binary>) initargs)
  (next-method)
  (slot-set! property 'ics-property-type 'BINARY))


;;; Converters

(define-method (ics-property->ics-property:binary
                (property <ics-property>))
  (make <ics-property:binary>
    #:name        (ics-property-name          property)
    #:parameters  (ics-property-parameters    property)
    #:format-type (ics-property-parameter-ref property "FMTTYPE")
    #:encoding    (ics-property-parameter-ref property "ENCODING")
    #:value       (ics-property-value         property)))

;;; binary.scm ends here.
