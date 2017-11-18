;;; conv.scm -- Various converters for iCalendar objects.

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

;; Various converters for iCalendar objects.


;;; Code:


(define-module (ics conv)
  #:use-module (ice-9 rdelim)
  #:use-module (ics type ics-object)
  #:use-module (ics type ics-property)
  #:use-module (ics type ics-content)
  #:export (ical-object->org-mode))

(define* (ical-object->org-mode ical-object
                                #:optional (port (current-output-port)))
  "Convert an ICAL-OBJECT object to org-mode format and print the
results to a PORT."
  (define (print-properties props current-indent)
    (let ((i (make-string current-indent #\space)))
      (format port "~a:PROPERTIES:~%" i)
      (for-each (lambda (property)
                  (let ((name  (ical-property-name property))
                        (value (scm->ical-value (ical-property-value property)))
                        (parameters (ical-property-parameters property)))
                    (if (list? value)
                        (format port "~a:~a: ~a~%" i name
                                (string-join value ","))
                        (format port "~a:~a: ~a~%" i name value))
                    (for-each (lambda (parameter)
                                (format port "~a:~a:~a: ~a~%"
                                        i name
                                        (car parameter) (cdr parameter)))
                              parameters)))

                props)
      (format port "~a:END:~%" i)))
  (define (print-components components current-indent level)
    (let ((i (make-string current-indent #\space))
          (s (make-string current-indent #\*)))
      (for-each (lambda (object)
                  (let ((cname (ical-object-name object)))
                    (format port "~a ~a~%" s cname)
                    (print-properties (ical-object-properties object)
                                     (+ current-indent 1))
                    (print-components (ical-object-components object)
                                      (+ current-indent 1)
                                      (1+ level))))
                components)))
  (define (print-vcalendar)
    (write-line "* VCALENDAR" port)
    (print-properties (ical-object-properties ical-object) 2)
    (print-components (ical-object-components ical-object) 2 1))

  (print-vcalendar))

;;; conv.scm ends here.
