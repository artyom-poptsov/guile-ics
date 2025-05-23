;;; common.scm -- Common code for Guile-ICS.

;; Copyright (C) 2015-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Common variables and procedures that are used in parsers.


;;; Code:

(define-module (ics common)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((ice-9 regex) #:select (regexp-substitute/global))
  #:use-module (ice-9 match)
  #:export (ics-debug-set!
            debug
            *debug?*

            case*

            constructor-argument
            string-join/non-null
            in-range?))


(define *debug?* #f)                    ; Is the debug mode enabled?


(define (ics-debug-set! enabled?)
  "Set debug mode to an ENABLED? value."
  (set! *debug?* enabled?))


;;;

(define-macro (case* pred key . clauses)
  `(cond
    ,@(map
       (lambda (clause)
         (let ((datum (car clause))
               (exp   (cadr clause)))
           (cond
            ((and (not (list? datum)) (not (eq? datum 'else)))
             (error "Syntax error: expected a list" datum))
            ((eq? datum 'else)
             `(else ,exp))
            ((= (length datum) 1)
             `((,pred ,key ,(car datum)) ,exp))
            (else
             `((or ,@(map (lambda (o) `(,pred ,key ,o))
                          datum)) ,exp)))))
       clauses)))



(define (constructor-argument initargs key)
  (and (memq key initargs)
       (cadr (memq key initargs))))

(define (in-range? value ranges)
  "Check if a @var{value} is in a @var{range}.  @var{range} must be a pair or a
list of pairs."
  (and (number? value)
       (match ranges
         (((? number?) . (? number?))
          (and (>= value (car ranges)) (<= value (cdr ranges))))
         ((((? number?) . (? number?)) ...)
          (fold (lambda (range prev)
                  (or prev
                      (and (>= value (car range))
                           (<= value (cdr range)))))
                #f
                ranges)))))

(define (string-join/non-null lst)
  "Infix-join a list of strings @var{lst} with spaces as separators.  Only
strings that are non-null are used; the empty strings get filtered out."
  (string-join (filter (lambda (s) (not (string-null? s))) lst)
               " "
               'infix))

;;; common.scm ends here
