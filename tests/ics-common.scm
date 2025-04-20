;;; ics-common.scm -- Tests for the common Guile-ICS code.

;; Copyright (C) 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (tests common)
             (ics common))


(define %test-suite-name "ics-common")

(configure-test-logging! %test-suite-name)

(test-begin %test-suite-name)



(test-assert "in-range?: single range: #t"
  (in-range? 10 '(0 . 15)))

(test-equal "in-range?: single range: #f"
  #f
  (in-range? 100 '(0 . 15)))

(test-assert "in-range?: two ranges: #t"
  (in-range? 10 '((-1 . -10) (0 . 15))))

(test-equal "in-range?: two ranges: #t"
  #f
  (in-range? 100 '((-1 . -10) (0 . 15))))

(test-error "in-range?: single range, wrong type"
  (in-range? 10 '(a . -10)))

(test-error "in-range?: multiple ranges, wrong type"
  (in-range? 10 '((0 . 10) (a . -10))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; vcf.scm ends here.
