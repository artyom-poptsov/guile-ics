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
  #:use-module (ics ical-object)
  #:export (ical-object->org-mode))

(define* (ical-object->org-mode ical-object
                                #:optional (port (current-output-port)))
  "Convert an ICAL-OBJECT object to org-mode format and print the
results to a PORT."
  (define (print-icalprops props current-indent)
    (let ((i (make-string current-indent #\space)))
      (format port "~a:PROPERTIES:~%" i)
      (for-each (lambda (e)
                  (format port "~a:~a: ~a~%" i (car e) (cdr e)))
                props)
      (format port "~a:END:~%" i)))
  (define (print-components components current-indent level)
    (let ((i (make-string current-indent #\space))
          (s (make-string current-indent #\*)))
      (for-each (lambda (component)
                  (let ((cname  (car component))
                        (object (cdr component)))
                    (format port "~a ~a~%" s cname)
                    (print-icalprops (ical-properties object)
                                     (+ current-indent 1))
                    (print-components (ical-components object)
                                      (+ current-indent 1)
                                      (1+ level))))
                components)))
  (define (print-vcalendar)
    (write-line "* VCALENDAR" port)
    (print-icalprops (ical-properties ical-object) 2)
    (print-components (ical-components ical-object) 2 1))

  (print-vcalendar))

;;; conv.scm ends here.
