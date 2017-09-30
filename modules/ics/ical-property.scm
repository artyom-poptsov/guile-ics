;;; ical-property.scm -- iCalendar property definition.

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

;; This module contains definition of <ical-property> class; the class
;; represents an individual attribute of an iCalendar object.
;;
;; See the Texinfo documentation for details.


;;; Code:

(define-module (ics ical-property)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ics ical-content)
  #:export (<ical-property>
            ical-property-name
            ical-property-value
            ical-property-parameters
            ical-property-parameter-ref
            ical-property->string))


;;;

(define-class <ical-property> (<ical-content>)
  (value      #:accessor     ical-property-value
              #:init-value   #f
              #:init-keyword #:value)
  ;; alist
  (parameters #:accessor     ical-property-parameters
              #:init-value   #f
              #:init-keyword #:parameters))


;;; Custom printers

(define-method (display (ical-property <ical-property>) (port <port>))
  (format port "#<ical-property ~a ~a>" (ical-property-name ical-property)
          (number->string (object-address ical-property) 16)))

(define-method (write (ical-property <ical-property>) (port <port>))
  (format port "#<ical-property ~a ~a>" (ical-property-name ical-property)
          (number->string (object-address ical-property) 16)))


;;;

(define ical-property-name ical-content-name)


;;;

(define-method (ical-property-parameter-ref (ical-property <ical-property>)
                                            (name <symbol>))
  "Get a iCalendar property parameter by a NAME, return a property
parameter value, or return #f if no parameter found."
  (assoc-ref name (ical-property-parameters ical-property)))

(define-method (ical-property->string (ical-property <ical-property>))
  "Convert an ICAL-PROPERTY to a iCalendar string, return the string."
  (define (parameters->string parameters)
    (string-join (map (lambda (parameter)
                        (format #f "~a=~a" (car parameter)
                                (cdr parameter)))
                      parameters)
                 ";"))
  (let ((parameters (ical-property-parameters ical-property))
        (name       (ical-property-name ical-property))
        (value      (ical-property-value ical-property)))
    (if parameters
        (format #f "~a;~a:~a" name (parameters->string parameters) value)
        (string-append name ":" value))))

;;; ical-property.scm ends here.
