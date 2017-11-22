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
                ics-property:date?
                ics-property:date=?
                ics-property->ics-property:date))


;;; Class definition.

(define-class <ics-property:date> (<ics-property>))

(define-method (initialize (property <ics-property:date>))
  (next-method)
  (slot-set! property 'type 'DATE))


;;; Printers

(define-method (display (property <ics-property:date>) (port <port>))
  (format port "#<ics-property:date ~a: ~a ~a>"
          (ics-property-name property)
          (let ((value       (ics-property-value property))
                (tm->iso8601 (lambda (tm) (strftime "%F" tm))))
            (if (list? value)
                (string-join (map tm->iso8601 value) ",")
                (tm->iso8601 value)))
          (object-address->string property)))

(define-method (write (property <ics-property:date>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:date>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:date>))
  (display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:date? x)
  "Check if X is an instance of <ics-property:date>, return #t if it
is, #f otherwise."
  (is-a? x <ics-property:date>))

(define-method (ics-property:date=? (property1 <ics-property:date>)
                                    (property2 <ics-property:date>))
  "Compare PROPERTY1 with PROPERTY2.  Return #t if the given
properties are identical, #f otherwise."
  (and (string=? (ics-property-name property1)
                 (ics-property-name property2))
       (equal?   (ics-property-value property1)
                 (ics-property-value property2))
       (equal?   (ics-property-parameters property1)
                 (ics-property-parameters property2))))


;;; Converters

(define-method (ics-property->ics-property:date
                (property <ics-property>))
  (let ((date->tm (lambda (date) (car (strptime "%Y%m%d" date))))
        (value (ics-property-value property)))
    (make <ics-property:date>
      #:name       (ics-property-name property)
      #:parameters (ics-property-parameters property)
      #:value      (if (list? value)
                       (map date->tm value)
                       (date->tm value)))))

;;; date.scm ends here.
