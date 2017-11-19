;;; float.scm -- iCalendar FLOAT (RFC5545, 3.3.7) type.

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

(define-module (ics type property float)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:float>
                ics-property->ics-property:float))


;;; Class definition.

(define-class <ics-property:float> (<ics-property>))

(define-method (initialize (property <ics-property:float>))
  (next-method)
  (slot-set! property 'ics-property-type 'FLOAT))


;;; Printers.

(define-method (display (property <ics-property:float>) (port <port>))
  (format port "#<ics-property:float ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:float>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:float>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:float>))
  (display property (current-output-port)))


;;; Converters.

(define-method (ics-property->ics-property:float
                (property <ics-property>))
  (make <ics-property:float>
    #:name  (ics-property-name property)
    #:value (string->number (ics-property-value property))))

;;; float.scm ends here.
