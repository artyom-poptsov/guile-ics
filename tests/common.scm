;;; cli.scm -- Guile-ICS common test code.

;; Copyright (C) 2022-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains the common code used in the tests.


;;; Code:

(add-to-load-path (getenv "abs_top_srcdir"))

(define-module (tests common)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64)
  #:use-module (ics fsm context)
  #:export (configure-test-logging! test-error/assert))

(define-method (configure-test-logging! (test-suite-name <string>))
  (smc-log-init! "file" `((file . ,(string-append test-suite-name "-smc.log")))))

(define-syntax test-error/assert
  (syntax-rules ()
    ((_ name expected-error body)
     (test-equal name
       expected-error
       (begin
         (test-result-set! (test-runner-current)
                           'expected-error
                           expected-error)
         (test-result-set! (test-runner-current)
                           'actual-value
                           #f)
         (catch #t
           (lambda ()
             body
             #f)
           (lambda (key . args)
             (test-result-set! (test-runner-current)
                               'actual-value
                               key)
             key)))))))

;;; common.scm ends here.
