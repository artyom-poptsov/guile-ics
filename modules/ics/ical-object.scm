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


;;; Code:

(define-module (ics ical-object)
  #:export (ical-object-icalprops
            ical-object-component
            make-ical-object
            ical-value->scm
            scm->ical-value))

(define (make-ical-object icalprops component)
  "Convert an ICALPROPS list and a COMPONENT alist to an iCal object."
  (list (cons 'ICALPROPS (list icalprops))
        (cons 'COMPONENT (list component))))


(define (ical-object-icalprops ical-object)
  "Get iCal properties from an ICAL-OBJECT."
  (cadr (assoc 'ICALPROPS ical-object)))

(define (ical-object-component ical-object)
  "Get an iCal component alist from ICAL-OBJECT."
  (cadr (assoc 'COMPONENT ical-object)))

;;; ical-object.scm ends here.
