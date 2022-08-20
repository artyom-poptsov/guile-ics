;;; stream-context.scm -- Context for the iCalendar stream FSM.

;; Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains the iCalendar stream parser context, as well as the
;; required guards, actions and other procedures.


;;; Code:

(define-module (ics fsm stream-context)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (ics common)
  #:use-module (ics type object)
  #:use-module (ics type property)
  #:use-module (ics parser)
  #:use-module (ics fsm context)
  #:use-module (ics fsm content-line-context)
  #:use-module (ics fsm content-line-parser)
  #:use-module (ics type object)
  #:export (<stream-context>
            stream-context?
            stream-context-append-object!
            stream-context-port
            stream-context-current-object
            stream-context-current-object-set!
            stream-context-objects
            stream-context-objects-set!
            stream-context-objects-count
            stream-context-parse-types?

            ;; FSM event sources.
            stream:read
            stream:dummy-event-source

            ;; Predicatres and FSM guards.
            stream-parser-context?
            stream:eof-object?
            stream:vcalendar-begin?
            stream:vcalendar-end?
            stream:vevent-begin?
            stream:vevent-end?

            ;; FSM actions.
            stream:create-object
            stream:append-object
            stream:append-property

            ;; FSM error handling.
            stream:error
            stream:error-unexpected-eof-in-object
            stream:error-unexpected-eof-in-vevent))


(define-class <stream-context> (<context>)
  ;; When this parameter is set the FSM will try to parse types for each ICS
  ;; property.
  ;;
  ;; <boolean>
  (parse-types?
   #:init-value   #f
   #:init-keyword #:parse-types?
   #:getter       stream-context-parse-types?)

  ;; The port from which iCalendar stream of objects is read.
  ;;
  ;; <port>
  (port
   #:init-thunk   (lambda () (current-input-port))
   #:init-keyword #:port
   #:getter       stream-context-port)

  ;; Current iCalendar object that is being read.
  ;;
  ;; <ics-object> | #f
  (current-object
   #:init-value   #f
   #:init-keyword #:current-object
   #:getter       stream-context-current-object
   #:setter       stream-context-current-object-set!)

  ;; The list of iCalendar objects from the current stream.
  ;;
  ;; <list> of <ics-object>
  (objects
   #:init-value   '()
   #:init-keyword #:objects
   #:getter       stream-context-objects
   #:setter       stream-context-objects-set!))

(define (stream-context? x)
  "Check if X is an <stream-context> instance."
  (is-a? x <stream-context>))

(define-method (stream-context-append-object! (context <stream-context>))
  "Append the current object from CONTEXT to the list of objects inside
CONTEXT.  Return the context."
  (let ((current-object (stream-context-current-object context))
        (objects        (stream-context-objects context)))
    (stream-context-objects-set! context (append objects (list current-object)))
    context))

(define-method (stream-context-objects-count (context <stream-context>))
  (length (stream-context-objects context)))


(define (stream:read ctx)
  "Event source for the ICS stream parser."
  (fsm-run! (make <content-line-parser>)
            (make <content-line-context>
              #:port (stream-context-port ctx))))

(define (stream:dummy-event-source ctx)
  #t)


;;; Guards.

(define (stream:eof-object? ctx content-line-ctx)
  "Check if CONTENT-LINE-CTX contains EOF."
  (content-line-context-eof? content-line-ctx))

(define (stream:vcalendar-begin? ctx content-line-ctx)
  "Check if CONTENT-LINE-CTX contains the beginning of an iCalendar object."
  (content-line-vcalendar-begin?
   (content-line-context-result content-line-ctx)))

(define (stream:vcalendar-end? ctx content-line-ctx)
  "Check if CONTENT-LINE-CTX contains the ending of an iCalendar object."
  (content-line-vcalendar-end?
   (content-line-context-result content-line-ctx)))

(define (stream:vevent-begin? ctx content-line-ctx)
  "Check if CONTENT-LINE-CTX contains the beginning of an iCalendar event."
  (content-line-vevent-begin?
   (content-line-context-result content-line-ctx)))

(define (stream:vevent-end? ctx content-line-ctx)
  "Check if CONTENT-LINE-CTX contains the ending of an iCalendar event."
  (content-line-vevent-end?
   (content-line-context-result content-line-ctx)))


;;; Actions

(define (stream:create-object ctx content-line-ctx)
  "Create a new <ics-object> instance from the content line that is stored inside
CONTENT-LINE-CTX. The new object is stored in the current object slot inside
<stream-context> CTX.  Return the context."
  (let* ((content-line (content-line-context-result content-line-ctx))
         (name         (content-line-name content-line)))
    (stream-context-current-object-set! ctx (make <ics-object>
                                              #:name name)))
  ctx)

(define (stream:append-object ctx content-line-ctx)
  "Append the current object of <stream-context> CTX to the list of objects. The
current object slot is set to #f.  Return the context."
  (stream-context-append-object! ctx)
  (stream-context-current-object-set! ctx #f)
  ctx)

(define (stream:append-property ctx content-line-ctx)
  "Append an iCalendar object property to the current object from <stream-context>
CTX. Return the context."
  (let* ((content-line (content-line-context-result content-line-ctx))
         (property (make <ics-property>
                     #:name       (content-line-name content-line)
                     #:value      (content-line-value content-line)
                     #:parameters (content-line-parameters content-line)))
         (property (if (stream-context-parse-types? ctx)
                       (ics-property->typed-property property)
                       property))
         (current-object (stream-context-current-object ctx))
         (current-object-properties (ics-object-properties current-object)))
    (ics-object-properties-set! current-object
                                (cons property current-object-properties))
    ctx))


;;; Error handlers.

(define (stream:error ctx content-line-parser-ctx)
  (error "Unexpected content in a stream"
         ctx
         content-line-parser-ctx))

(define (stream:error-unexpected-eof-in-object ctx content-line-parser-ctx)
  (error "Unexpected EOF in ICalendar object"
         ctx
         content-line-parser-ctx))

(define (stream:error-unexpected-eof-in-vevent ctx content-line-parser-ctx)
  (error "Unexpected EOF in VEvent component"
         ctx
         content-line-parser-ctx))

;;; stream-context.scm ends here.
