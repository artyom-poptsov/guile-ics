;;; property.scm -- iCalendar generic property type.

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

(define-module (ics type property property)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ics type content)
  #:export (<ics-property>
            object-address->string
            %ics-property-type
            ics-property-name
            non-standard-property-name?
            ics-property-format-type
            ics-property-value
            ics-property-parameters
            ics-property-parameter-ref
            ics-property->string))


;;; Class definition.

(define-class <ics-property> (<ics-content>)
  ;; symbol || #f
  ;;
  ;; iCalendar type name as described in RFC5545, 3.2.20.
  (type
   #:accessor     %ics-property-type
   #:init-value   #f
   #:init-keyword #:type)

  ;; string
  ;;
  ;; iCalendar type format as described in RFC5545, 3.2.8.
  ;;
  ;; List of registered format types can be found in
  ;; <http://www.iana.org/assignments/media-types/>
  (format-type
   #:accessor     ics-property-format-type
   #:init-value   #f
   #:init-keyword #:format-type)

  (value      #:accessor     ics-property-value
              #:init-value   #f
              #:init-keyword #:value)
  ;; alist
  (parameters #:accessor     ics-property-parameters
              #:init-value   '()
              #:init-keyword #:parameters))

(define-method (initialize (property <ics-property>) initargs)
  (next-method)
  (unless (memq #:value initargs)
    (throw 'guile-ics-error "#:value slot is mandatory" initargs)))

(define (object-address->string object)
  (number->string (object-address object) 16))


;;; Printers.

(define-generic display)
(define-generic write)

(define-method (display (property <ics-property>) (port <port>))
  (format port "#<ics-property name: ~a ~a>"
          (ics-property-name property)
          (object-address->string property)))

(define-method (write (property <ics-property>) (port <port>))
  (display property port))

(define-method (display (property <ics-property>))
  (display property (current-output-port)))

(define-method (write (property <ics-property>))
  (display property (current-output-port)))


;;;

(define ics-property-name ics-content-name)

(define-method (ics-property-parameter-ref (ics-property <ics-property>)
                                            (name <symbol>))
  "Get a iCalendar property parameter by a NAME, return a property
parameter value, or return #f if no parameter found."
  (assoc-ref (ics-property-parameters ics-property) name))

(define-method (ics-property->string (ics-property <ics-property>))
  "Convert an ICAL-PROPERTY to a iCalendar string, return the string."
  (define (parameters->string parameters)
    (string-join (map (lambda (parameter)
                        (format #f "~a=~a" (car parameter)
                                (cdr parameter)))
                      parameters)
                 ";"))
  (let ((parameters (ics-property-parameters ics-property))
        (name       (ics-property-name ics-property))
        (value      (ics-property-value ics-property)))
    (if parameters
        (format #f "~a;~a:~a" name (parameters->string parameters) value)
        (string-append name ":" value))))

;; RFC5545, 3.8.8.2: Non-Standard Properties.
(define (non-standard-property-name? name)
  (regexp-match? (string-match "X-.*" name)))

;;; property.scm ends here.
