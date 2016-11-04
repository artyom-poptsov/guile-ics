;;; ics.scm -- Tests for ICS parser.

;; Copyright (C) 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(use-modules (srfi srfi-64)
             (ics)
             (ics parser)
             (ics fsm))

(test-begin "ics")


;;; Finite-State Machine tests.

(test-assert "fsm-read-property"
  (let ((parser (make-string-parser (string-append "VCALENDAR"
                                                   (string #\cr)
                                                   (string #\lf)
                                                   "VERSION"))))
    (string=? (fsm-read-property parser) "VCALENDAR")))

(test-assert "fsm-read-property, property followed by EOF"
  (let ((parser (make-string-parser (string-append "VCALENDAR"
                                                   (string #\cr)
                                                   (string #\lf)))))
    (string=? (fsm-read-property parser) "VCALENDAR")))


(test-end "ics")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; ics.scm ends here.
