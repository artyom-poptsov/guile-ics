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

;; A generic property which type is not determined yet.


;;; Code:

(define-module (ics type property property)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ics type content)
  #:export (<ics-property>
            ics-property?
            %ics-property=?
            object-address->string
            %ics-property-type
            ics-property-name
            non-standard-property-name?
            ics-property-format-type
            ics-property-value
            ics-property-parameters
            ics-property-parameter-ref))


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


;;; Predicates.

(define-method (ics-property? x)
  "Check if X is an instance of <ics-property>, return #t if it is, #f
otherwise."
  (is-a? x <ics-property>))

(define-method (%ics-property=? (property1 <ics-property>)
                                (property2 <ics-property>))
  "Compare PROPERTY1 with PROPERTY2.  Return #t if the given
properties are identical, #f otherwise."
  (and (string=? (ics-property-name property1)
                 (ics-property-name property2))
       (equal?   (ics-property-format-type property1)
                 (ics-property-format-type property2))
       (equal?   (%ics-property-type property1)
                 (%ics-property-type property2))
       (equal?   (ics-property-value property1)
                 (ics-property-value property2))
       (equal?   (ics-property-parameters property1)
                 (ics-property-parameters property2))))

(define-method (equal? (property1 <ics-property>)
                       (property2 <ics-property>))
  "Compare PROPERTY1 with PROPERTY2.  Return #t if the given
properties are identical, #f otherwise."
  (%ics-property=? property1 property2))


;;;

(define ics-property-name ics-content-name)

(define-method (ics-property-parameter-ref (ics-property <ics-property>)
                                            (name <symbol>))
  "Get a iCalendar property parameter by a NAME, return a property
parameter value, or return #f if no parameter found."
  (assoc-ref (ics-property-parameters ics-property) name))

;; RFC5545, 3.8.8.2: Non-Standard Properties.
(define (non-standard-property-name? name)
  (regexp-match? (string-match "X-.*" name)))

;;; property.scm ends here.
