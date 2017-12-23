;;; stream.scm -- iCalendar streams

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

(define-module (ics type stream)
  #:use-module (srfi srfi-41)
  #:use-module (ics common)
  #:use-module (ics parser)
  #:use-module (ics fsm)
  #:use-module (ics type object)
  #:use-module (oop goops)
  #:export (<ics-stream>
            ics-stream-source
            ics-stream-parser
            ics-stream-parse-types?
            ics-stream->scm-stream
            ics-stream->scm))


;;;

(define-class <ics-stream> ()
  ;; <port> || <string>
  (source #:accessor   ics-stream-source
          #:init-value (current-input-port)
          #:init-keyword #:source)

  ;; <boolean>
  (parse-types? #:accessor ics-stream-parse-types?
                #:init-keyword #:parse-types?
                #:init-value   #f))


;;;

(define-method (display (ics-stream <ics-stream>) (port <port>))
  (format port "#<ics-stream source: ~a ~a>"
          (if (port? (ics-stream-source ics-stream))
              'port
              'string)
          (number->string (object-address ics-stream) 16)))

(define-method (write (ics-stream <ics-stream>) (port <port>))
  (display ics-stream port))


;;;

(define (ics-read parser)
  "Read iCalendar data using a PARSER, return a new iCalendar object."
  (fsm-read-ics-stream parser '()))

(define-method (ics-stream-parser (ics-stream <ics-stream>))
  (let ((source (ics-stream-source ics-stream)))
    (if (port? source)
        (make <ics-parser>
          #:port source
          #:parse-types? (ics-stream-parse-types? ics-stream))
        (call-with-input-string source
          (lambda (port)
            (make <ics-parser>
              #:port port
              #:parse-types? (ics-stream-parse-types? ics-stream)))))))


;;;

(define-method (ics-stream->scm (ics-stream <ics-stream>))
  "Convert an ICAL-STREAM to a list of iCalendar objects.  Return the
list."
  (ics-read (ics-stream-parser ics-stream)))


;;; SRFI streams

(define (fsm-read-ics-stream-1 parser result)
  "Read iCalendar object using PARSER.  Return iCalendar object or
'stream-null' on EOF."
  (define (read-component-name)
    (let ((name (fsm-read-property parser)))
      (debug-fsm "fsm-read-component-name" "NAME: ~a~%" name)
      (if (ics-calendar-object? name)
          (begin
            (debug-fsm "fsm-read-ics-stream" "RESULT: ~a~%" result)
            (fsm-read-ics-object parser %ics-icalendar-object '() '()))
          (fsm-read-ics-stream-1 parser result))))
  (define (read-ics-stream buffer)
    (let ((ch (parser-read-char parser)))
      (if (eof-object? ch)
          stream-null
          (case ch
            ((#\:)
             (debug-fsm "fsm-read-ics-stream" "BUFFER: ~a~%" buffer)
             (cond
              ((ics-token-begin? buffer)
               (read-component-name))
              (else
               (debug-fsm-transition "fsm-read-ics-stream")
               (fsm-read-ics-stream-1 parser result))))
            (else
             (read-ics-stream (string-append buffer (string ch))))))))
  (debug-fsm-transition "fsm-read-ics-stream-1")
  (read-ics-stream ""))

(define-method (ics-stream->scm-stream (ics-stream <ics-stream>))
  "Convert an ICS stream to an SRFI-41 stream.  Return the stream."
  (let ((parser (ics-stream-parser ics-stream)))
    (stream-let loop ((ics-object (fsm-read-ics-stream-1 parser '())))
                (if (stream-null? ics-object)
                    stream-null
                    (stream-cons ics-object
                                 (loop (fsm-read-ics-stream-1 parser '())))))))

;;; stream.scm ends here.
