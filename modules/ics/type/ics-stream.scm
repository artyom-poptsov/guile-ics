;;; ics-stream.scm -- iCalendar streams

;; Copyright (C) 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (ics type ics-stream)
  #:use-module (srfi srfi-41)
  #:use-module (ics common)
  #:use-module (ics parser)
  #:use-module (ics fsm)
  #:use-module (ics type ics-object)
  #:use-module (oop goops)
  #:export (<ical-stream>
            ical-stream-source
            ical-stream-parser
            ical-stream->scm-stream
            ical-stream->scm))


;;;

(define-class <ical-stream> ()
  ;; <port> || <string>
  (source #:accessor   ical-stream-source
          #:init-value (current-input-port)
          #:init-keyword #:source))


;;;

(define-method (display (ical-stream <ical-stream>) (port <port>))
  (format port "#<ical-stream source: ~a ~a>"
          (if (port? (ical-stream-source ical-stream))
              'port
              'string)
          (number->string (object-address ical-content) 16)))

(define-method (write (ical-stream <ical-stream>) (port <port>))
  (display ical-stream port))


;;;

(define (ics-read parser)
  "Read iCalendar data using a PARSER, return a new iCalendar object."
  (fsm-read-ical-stream parser '()))

(define-method (ical-stream-parser (ical-stream <ical-stream>))
  (let ((source (ical-stream-source ical-stream)))
    (if (port? source)
        (make-parser source)
        (make-string-parser source))))


;;;

(define-method (ical-stream->scm (ical-stream <ical-stream>))
  "Convert an ICAL-STREAM to a list of iCalendar objects.  Return the
list."
  (ics-read (ical-stream-parser ical-stream)))


;;; SRFI streams

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

(define-method (ical-stream->scm-stream (ical-stream <ical-stream>))
  "Convert an ICS stream to an SRFI-41 stream.  Return the stream."
  (let ((parser (ical-stream-parser ical-stream)))
    (stream-let loop ((ical-object (fsm-read-ical-stream-1 parser '())))
                (if (stream-null? ical-object)
                    stream-null
                    (stream-cons ical-object
                                 (loop (fsm-read-ical-stream-1 parser '())))))))

;;; ical-stream.scm ends here.
