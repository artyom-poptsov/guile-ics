;; Copyright (C) 2016, 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains the definitin of a <ics-object> class.
;;
;; See the documentation in Texinfo format for details.


;;; Code:

(define-module (ics type object)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)            ; find
  #:use-module (ics type property)
  #:use-module (ics type content)
  #:export (<ics-object>
            ics-object->ics
            ics-object-name
            ics-object-properties
            ics-object-property-ref
            ics-object-components))


;;;

(define-class <ics-object> (<ics-content>)
  ;; list
  (properties #:accessor ics-object-properties
              #:init-value '()
              #:init-keyword #:properties)
  ;; list
  (components #:accessor ics-object-components
              #:init-value '()
              #:init-keyword #:components))


;;; Custom printers

(define-method (display (ics-object <ics-object>) (port <port>))
  (format port "#<ics-object ~a ~a>"  (ics-object-name ics-object)
          (number->string (object-address ics-object) 16)))

(define-method (write (ics-object <ics-object>) (port <port>))
  (format port "#<ics-object ~a ~a>"  (ics-object-name ics-object)
          (number->string (object-address ics-object) 16)))


;;;

(define ics-object-name ics-content-name)


;;;

(define-method (ics-object-property-ref (ics-object <ics-object>)
                                         (name <string>))
  "Get an iCalendar property by a NAME, return the property object or
#f if no property found."
  (find (lambda (property) (equal? (ics-property-name property) name))
        (ics-object-properties ics-object)))


(define-method (ics-object->ics (obj <ics-object>)
                                 (port <port>))
  "Convert an ICAL-OBJECT to the iCalendar format and print the result
to a specified PORT."
  (define (print-properties props)
    (for-each (lambda (e)
                (format port "~a" (ics-property-name e))
                (for-each (lambda (property)
                            (format port ";~a=~a"
                                    (car property)
                                    (cdr property)))
                          (ics-property-parameters e))
                (let ((value (scm->ics-value (ics-property-value e))))
                  (if (list? value)
                      (ics-format port ":~a"
                                   (string-join value ","))
                      (ics-format port ":~a" value))))
              props))
  (define (print-components components)
    (for-each (lambda (object)
                (let ((cname (ics-object-name object)))
                  (ics-format port "BEGIN:~a" cname)
                  (print-properties (ics-object-properties object))
                  (print-components (ics-object-components object))
                  (ics-format port "END:~a" cname)))
              components))
  (define (print-vcalendar)
    (ics-write-line "BEGIN:VCALENDAR" port)
    (print-properties (ics-object-properties obj))
    (print-components (ics-object-components obj))
    (ics-write-line "END:VCALENDAR" port))

  (print-vcalendar))

;;; object.scm ends here.
