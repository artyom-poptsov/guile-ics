;;; binary.scm -- iCalendar DATE-TIME (RFC5545, 3.3.5) type.

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

(define-module (ics type property date-time)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:date-time>
                ics-property:date-time-tzid
                ics-property->ics-property:date-time))


;;; Class definition.

(define-class <ics-property:date-time> (<ics-property>)
  (tzid #:accessor ics-property:date-time-tzid
        #:init-value #f
        #:init-keyword #:tzid))

(define-method (initialize (property <ics-property:date-time>))
  (next-method)
  (slot-set! property 'type 'DATE-TIME))


;;; Printers.

(define-method (display (property <ics-property:date-time>) (port <port>))
  (format port "#<ics-property:date-time ~a: ~a ~a>"
          (ics-property-name property)
          (strftime "%FT%TZ" (car (ics-property-value property)))
          (object-address->string property)))

(define-method (write (property <ics-property:date-time>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:date-time>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:date-time>))
  (display property (current-output-port)))


;;; Converters.

(define-method (ics-property->ics-property:date-time
                (property <ics-property>))
  (make <ics-property:date-time>
    #:name  (ics-property-name property)
    #:value (strptime "%Y%m%dT%H%M%S%Z" (ics-property-value property))))

;;; date-time.scm ends here.
