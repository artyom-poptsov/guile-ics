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

(define-module (ics)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 rdelim)
  #:use-module (scheme documentation)
  #:use-module              (ice-9 pretty-print)
  ;; Guile-ICS
  #:use-module (ics common)
  #:use-module (ics parser)
  #:export (ics->scm))


;;;

(define-with-docs %ics-token-begin
  ""
  "BEGIN")

(define-with-docs %ics-token-end
  ""
  "END")


;;;

(define (fsm-read-property parser)
  (define (read-property buffer)
    (let ((ch (parser-read-char parser)))
      (if (eof-object? ch)
          buffer
          (case ch
            ((#\cr)
             (read-property buffer))
            ((#\linefeed *eof-object*)
             buffer)
            (else
             (read-property (string-append buffer (string ch))))))))
  (debug-fsm-transition "fsm-read-property")
  (read-property ""))

(define (fsm-read-ical-stream parser result)
  (define (read-ical-stream buffer)
    (let ((ch (parser-read-char parser)))
      (if (eof-object? ch)
          result
          (case ch
            ((#\:)
             (cond
              ((string=? buffer %ics-token-begin)
               (let ((key (string->symbol (fsm-read-property parser)))
                     (val (fsm-read-ical-stream parser '())))
                 (acons key val result)))
              ((string=? buffer %ics-token-end)
               result)
              (else
               (fsm-read-ical-stream parser (acons (string->symbol buffer)
                                                   (fsm-read-property parser)
                                                   result)))))
            (else
             (read-ical-stream (string-append buffer (string ch))))))))
  (debug-fsm-transition "fsm-read-ical-stream")
  (read-ical-stream ""))


(define (ics->scm parser)
  (debug-fsm-transition "fsm-read-icalstream")
  (fsm-read-ical-stream parser '()))


;;;
