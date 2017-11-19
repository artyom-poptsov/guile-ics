;;; cal-address.scm -- iCalendar CAL-ADDRESS (RFC5545, 3.3.3) type.

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

(define-module (ics type property cal-address)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:cal-address>
                ics-property->ics-property:cal-address))


;;; Class definition.

(define-class <ics-property:cal-address> (<ics-property>))

(define-method (initialize (ics-property <ics-property:cal-address>))
  (next-method)
  (slot-set! ics-property 'type 'CAL-ADDRESS))

(define-method (display (property <ics-property:cal-address>) (port <port>))
  (format port "#<ics-property:cal-address ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:cal-address>) (port <port>))
  (display property port))


;;; Converters.

(define-method (ics-property->ics-property:cal-address
                (property <ics-property>))
  (make <ics-property:cal-address>
    #:name  (ics-property-name property)
    #:value (ics-property-value property)))

;; (define-method (ics-data-cal-address->ics-property
;;                 (ics-property  <ics-data-cal-address>))
;;   (make <ics-property>
;;     #:value (ics-property-value ics-data-cal-address)))

;;; cal-address.scm ends here.
