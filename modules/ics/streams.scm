;;; streams.scm -- Convert iCalendar streams to SRFI-41 streams.

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

;;  Convert iCalendar streams to SRFI-41 streams.


;;; Code:

(define-module (ics streams)
  #:use-module (srfi srfi-41)
  #:use-module (ics common)
  #:use-module (ics parser)
  #:use-module (ics fsm)
  #:use-module (ics ical-object)
  #:export (ics->stream))


(define (fsm-read-ical-stream-1 parser result)
  "Read iCalendar object using PARSER.  Return iCalendar object or
'stream-null' on EOF."
  (define (read-component-name)
    (let ((name (fsm-read-property parser)))
      (debug-fsm "fsm-read-component-name" "NAME: ~a~%" name)
      (if (ics-calendar-object? name)
          (begin
            (debug-fsm "fsm-read-ical-stream" "RESULT: ~a~%" result)
            (fsm-read-ical-object parser %ics-icalendar-object '() '()))
          (fsm-read-ical-stream-1 parser result))))
  (define (read-ical-stream buffer)
    (let ((ch (parser-read-char parser)))
      (if (eof-object? ch)
          stream-null
          (case ch
            ((#\:)
             (debug-fsm "fsm-read-ical-stream" "BUFFER: ~a~%" buffer)
             (cond
              ((ics-token-begin? buffer)
               (read-component-name))
              (else
               (debug-fsm-transition "fsm-read-ical-stream")
               (fsm-read-ical-stream-1 parser result))))
            (else
             (read-ical-stream (string-append buffer (string ch))))))))
  (debug-fsm-transition "fsm-read-ical-stream-1")
  (read-ical-stream ""))


(define* (ics->stream #:optional (port (current-input-port)))
  "Convert an ICS stream to an SRFI-41 stream.  Return the stream."
  (let ((parser (make-parser port)))
    (stream-let loop ((ical-object (fsm-read-ical-stream-1 parser '())))
                (if (stream-null? ical-object)
                    stream-null
                    (stream-cons ical-object
                                 (loop (fsm-read-ical-stream-1 parser '())))))))

;;; streams.scm ends here.
