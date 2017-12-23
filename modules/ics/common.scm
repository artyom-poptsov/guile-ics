;;; common.scm -- Common code for Guile-ICS.

;; Copyright (C) 2015, 2016, 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:export (set-debug! debug debug-fsm debug-fsm-transition debug-fsm-error
            ics-error

            value-or-default
            substitute unescape-chars

            case*))


(define *debug?* #f)                    ; Is the debug mode enabled?


(define (set-debug! enabled?)
  "Set debug mode to an ENABLED? value."
  (set! *debug?* enabled?))

(define (debug fmt . args)
  (and *debug?*
       (let ((fmt (string-append "DEBUG: " fmt)))
         (apply format #t fmt args))))

(define (debug-fsm state fmt . args)
  "Format and print a debug message from a finite-state machine (FSM)."
  (apply debug (format #f "[~a]: ~a~%" state fmt) args))

(define debug-fsm-transition
  (case-lambda
    "Debug a finite-state machine (FSM) transition."
    ((to)
     (debug "--->[~a]~%" to))
    ((from to)
     (debug "[~a]--->[~a]~%" from to))
    ((from to type)
     (case type
       ((final)
        (debug "[~a]---> ~a~%" from to))
       (else
        (debug "[~a]--->[~a]~%" from to))))))

(define (debug-fsm-error state)
  (debug-fsm-transition state 'ERROR 'final))


(define ics-error
  (case-lambda
    "Throw 'dsv-parser exception with the given MESSAGE and arguments ARGS.
The procedure optionally takes STATE of FSM as the first argument and prints
it as a debug message.."
    ((state message . args)
     (debug-fsm-error state)
     (throw 'guile-ics-error message args))
    ((message . args)
     (throw 'guile-ics-error message args))))


(define (value-or-default value default-value)
  "Return a VALUE if it is not 'default, else return DEFAULT-VALUE."
  (if (eq? value 'default)
      default-value
      value))

(define (substitute str regex subst-str)
  (regexp-substitute/global #f regex str 'pre subst-str 'post))

(define (unescape-chars str char escape-char)
  (substitute str (string escape-char char) (string char)))


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

;;; common.scm ends here
