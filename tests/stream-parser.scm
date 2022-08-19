;;; stream-parser.scm -- Tests for ICS stream parser.

;; Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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



(use-modules (srfi srfi-64)
             (oop goops)
             (ics fsm context)
             (ics fsm content-line-context)
             (ics fsm content-line-parser)
             (ics fsm stream-context)
             (ics fsm stream-parser))


(define %test-suite-name "stream-parser")

(test-begin %test-suite-name)


(test-equal "correct input: output test: current-object must be #f"
  #f
  (with-input-from-string
      (string-append "BEGIN:VCALENDAR\r\n"
                     "VERSION:2.0\r\n"
                     "BEGIN:VEVENT\r\n"
                     "END:VEVENT\r\n"
                     "END:VCALENDAR")
    (lambda ()
      (let* ((fsm (make <stream-parser>))
             (ctx (fsm-run! fsm (make <stream-context>))))
        (stream-context-current-object ctx)))))

(test-assert "correct input: output test: objects check"
  (with-input-from-string
      (string-append "BEGIN:VCALENDAR\r\n"
                     "VERSION:2.0\r\n"
                     "BEGIN:VEVENT\r\n"
                     "END:VEVENT\r\n"
                     "END:VCALENDAR\r\n")
    (lambda ()
      (let* ((fsm (make <stream-parser>
                    #:debug-mode? #t))
             (ctx (fsm-run! fsm (make <stream-context>))))
        (stream-context-objects ctx)))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; stream-parser.scm ends here.
