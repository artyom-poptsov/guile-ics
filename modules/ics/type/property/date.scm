;;; date.scm -- iCalendar DATE (RFC5545, 3.3.4) type.

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

(define-module (ics type property date)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:date>
                ics-property->ics-property:date))


;;; Class definition.

(define-class <ics-property:date> (<ics-property>))

(define-method (initialize (property <ics-property:date>))
  (next-method)
  (slot-set! property 'ics-property-type 'DATE))


;;; Printers

(define-method (display (property <ics-property:date>) (port <port>))
  (format port "#<ics-property:date ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:date>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:date>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:date>))
  (display property (current-output-port)))


;;; Converters

(define-method (ics-property->ics-property:date
                (property <ics-property>))
  (let ((value (ics-property-value property)))
    (make <ics-property:date>
      #:name  (ics-property-name property)
      #:value (strptime "%Y%m%dT%H%M%S%Z" value))))

;;; date.scm ends here.
