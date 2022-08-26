;;; stream.scm -- iCalendar streams

;; Copyright (C) 2017-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (ics stream)
  #:use-module (srfi srfi-41)
  #:use-module (ics common)
  #:use-module (ics fsm stream-context)
  #:use-module (ics fsm stream-parser)
  #:use-module (ics object)
  #:use-module (oop goops)
  #:export (<ics-stream>
            ics-stream-source
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

(define-method (ics-stream->scm (ics-stream <ics-stream>))
  "Convert an ICAL-STREAM to a list of iCalendar objects.  Return the
list."
  (let* ((fsm    (make <stream-parser>
                   #:debug-mode? *debug?*))
         (source (ics-stream-source ics-stream))
         (parse-types? (ics-stream-parse-types? ics-stream))
         (port   (if (port? source)
                     source
                     (open-input-string source)))
         (ctx (fsm-run! fsm (make <stream-context>
                              #:parse-types? parse-types?
                              #:port port))))
    (stream-context-objects ctx)))


;;; SRFI streams

(define (fsm-read-ics-stream-1 port parse-types?)
  "Read iCalendar object using PARSER.  Return iCalendar object or
'stream-null' on EOF."
  (let* ((fsm (make <stream-parser>
                #:debug-mode? *debug?*))
         (ctx (fsm-run! fsm (make <stream-context>
                              #:parse-types? parse-types?
                              #:lazy? #t
                              #:port  port))))
    (if (null? (stream-context-objects ctx))
        stream-null
        (car (stream-context-objects ctx)))))

(define-method (ics-stream->scm-stream (ics-stream <ics-stream>))
  "Convert an ICS stream to an SRFI-41 stream.  Return the stream."
  (let* ((source (ics-stream-source ics-stream))
         (parse-types? (ics-stream-parse-types? ics-stream))
         (port   (if (port? source)
                     source
                     (open-input-string source))))
    (stream-let loop ((ics-object (fsm-read-ics-stream-1 port
                                                         parse-types?)))
                (if (stream-null? ics-object)
                    stream-null
                    (stream-cons ics-object
                                 (loop
                                  (fsm-read-ics-stream-1 port
                                                         parse-types?)))))))

;;; stream.scm ends here.
