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

(define-module (ics fsm)
  #:use-module (scheme documentation)
  #:use-module (ics common)
  #:use-module (ics ical-object)
  #:use-module (ics parser)
  #:export (ics-token-begin?
            ics-token-end?
            ics-calendar-object?
            ;; FSM
            fsm-read-property
            fsm-skip-property
            fsm-read-ical-object
            fsm-read-ical-stream))


;;; Constants.

(define-with-docs %ics-token-begin
  "See <https://tools.ietf.org/html/rfc5545#section-3.6>."
  "BEGIN")

(define-with-docs %ics-token-end
  "See <https://tools.ietf.org/html/rfc5545#section-3.6>."
  "END")

(define-with-docs %ics-icalendar-object
  "See <https://tools.ietf.org/html/rfc5545#section-3.4>."
  "VCALENDAR")


;;; Predicates.

(define (ics-token-begin? x)
  (string=? x %ics-token-begin))

(define (ics-token-end? x)
  (string=? x %ics-token-end))

(define (ics-calendar-object? x)
  "Check if X is a iCalendar object."
  (string=? x %ics-icalendar-object))


;;; FSM.

(define (fsm-read-property parser)
  (define (read-property buffer)
    (let ((ch (parser-read-char parser)))
      (if (eof-object? ch)
          buffer
          (case ch
            ((#\cr)
             (read-property buffer))
            ((#\linefeed)
             (let ((next-ch (parser-read-char parser)))
               ;; Lines longer than 75 octets should be split into
               ;; multiple line representations; in this case a single
               ;; space character immediately follows CRLF (see RFC
               ;; 5545, section 3.1)
               (if (equal? next-ch #\space)
                   (read-property buffer)
                   (begin
                     (unless (eof-object? next-ch)
                       (parser-unread-char parser next-ch))
                     buffer))))
            ((*eof-object*)
             buffer)
            (else
             (read-property (string-append buffer (string ch))))))))
  (debug-fsm-transition "fsm-read-property")
  (read-property ""))

(define (fsm-skip-property parser)
  (let ((ch (parser-read-char parser)))
    (unless (or (eof-object? ch) (equal? ch #\linefeed))
      (fsm-skip-property parser))))

(define (fsm-read-ical-object parser icalprops component)
  (define (read-component)
    (debug-fsm "fsm-read-ical-object" "read-component~%")
    (let ((key (string->symbol (fsm-read-property parser)))
          (val (fsm-read-ical-object parser '() '())))
      (fsm-read-ical-object parser
                            icalprops
                            (acons key val component))))
  (define (read-property name)
    (debug-fsm "fsm-read-ical-object" "read-property: NAME: ~a~%"
               name)
    (let ((key (string->symbol name))
          (val (fsm-read-property parser)))
      (fsm-read-ical-object parser
                            (acons key val icalprops)
                            component)))
  (define (read-object buffer)
    (let ((ch (parser-read-char parser)))
      (if (eof-object? ch)
          (->ical-object icalprops component)
          (case ch
            ((#\:)
             (cond
              ((ics-token-begin? buffer)
               (read-component))
              ((ics-token-end? buffer)
               (fsm-skip-property parser)
               (->ical-object icalprops component))
              (else
               (read-property buffer))))
            (else
             (read-object (string-append buffer (string ch))))))))
  (debug-fsm-transition "fsm-read-ical-object")
  (read-object ""))


(define (fsm-read-ical-stream parser result)
  (define (read-component-name)
    (let ((name (fsm-read-property parser)))
      (debug-fsm "fsm-read-component-name" "NAME: ~a~%" name)
      (if (ics-calendar-object? name)
          (begin
            (debug-fsm "fsm-read-ical-stream" "RESULT: ~a~%" result)
            (let ((result (cons (fsm-read-ical-object parser '() '())
                                result)))
              (debug-fsm "fsm-read-ical-stream" "RESULT: ~a~%" result)
              (fsm-read-ical-stream parser result)))
          (fsm-read-ical-stream parser result))))
  (define (read-ical-stream buffer)
    (let ((ch (parser-read-char parser)))
      (if (eof-object? ch)
          result
          (case ch
            ((#\:)
             (debug-fsm "fsm-read-ical-stream" "BUFFER: ~a~%" buffer)
             (cond
              ((ics-token-begin? buffer)
               (read-component-name))
              (else
               (debug-fsm-transition "fsm-read-ical-stream")
               (fsm-read-ical-stream parser result))))
            (else
             (read-ical-stream (string-append buffer (string ch))))))))
  (debug-fsm-transition "fsm-read-ical-stream")
  (read-ical-stream ""))
