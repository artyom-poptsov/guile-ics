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
  #:use-module (ics object)
  #:use-module (ics property)
  #:use-module (ics fsm context)
  #:use-module (ics fsm content-line-context)
  #:use-module (ics fsm content-line-parser)
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
            stream-context-lazy?

            ;; FSM event sources.
            stream:read
            stream:dummy-event-source

            ;; Predicatres and FSM guards.
            stream-parser-context?
            stream:eof-object?
            stream:object-begin?
            stream:object-end?
            stream:component-begin?
            stream:component-end?
            stream:lazy?

            ;; FSM actions.
            stream:create-object
            stream:append-object
            stream:append-property
            stream:create-component
            stream:append-component-property
            stream:append-component

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

  ;; When set to #f the parser reads all iCalendar objects from the specified
  ;; port.
  ;;
  ;; When set to #t the parser reads only the first object from the port.  The
  ;; port will not be closed afterwards so the next call of the FSM on the port
  ;; will return the next object.
  ;;
  ;; <boolean>
  (lazy?
   #:init-value   #f
   #:init-keyword #:lazy?
   #:getter       stream-context-lazy?)

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

  ;; Current iCalendar object component.
  ;;
  ;; <ics-object> | #f
  (current-component
   #:init-value   #f
   #:init-keyword #:current-component
   #:getter       stream-context-current-component
   #:setter       stream-context-current-component-set!)

  ;; The list of iCalendar objects from the current stream.
  ;;
  ;; <list> of <ics-object>
  (objects
   #:init-value   '()
   #:init-keyword #:objects
   #:getter       stream-context-objects
   #:setter       stream-context-objects-set!)

  ;; Current line number of the parser.
  ;;
  ;; <number>
  (line-number
   #:init-value   0
   #:getter       stream-context-line-number
   #:setter       stream-context-line-number-set!))

(define (stream-context? x)
  "Check if X is an <stream-context> instance."
  (is-a? x <stream-context>))

(define-method (stream-context-line-number++! (context <stream-context>))
  (stream-context-line-number-set! context
                                   (+ (stream-context-line-number context) 1)))

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
  (catch 'content-line-error
         (lambda ()
           (let ((context-line
                  (fsm-run! (make <content-line-parser>)
                            (make <content-line-context>
                              #:debug-mode? (context-debug-mode? ctx)
                              #:port (stream-context-port ctx)))))
             (stream-context-line-number++! ctx)
             context-line))
         (lambda (key message content-line-context ch)
           (let* ((buffer (context-stanza content-line-context))
                  (message "Invalid content line"))
             (log-error "~a:~a:~a ~a: ~a (~a)"
                                  (stream-context-port ctx)
                                  (stream-context-line-number ctx)
                                  (context-col-number content-line-context)
                                  message
                                  buffer
                                  ch)
             (error message ctx)))))

(define (stream:dummy-event-source ctx)
  #t)


;;; Guards.

(define (stream:lazy? ctx content-line-ctx)
  (stream-context-lazy? ctx))

(define (stream:eof-object? ctx content-line-ctx)
  "Check if CONTENT-LINE-CTX contains EOF."
  (content-line-context-eof? content-line-ctx))

(define (stream:object-begin? ctx content-line-ctx)
  "Check if CONTENT-LINE-CTX contains the beginning of an iCalendar object."
  (content-line-component-begin?
   (context-result content-line-ctx)))

(define (stream:object-end? ctx content-line-ctx)
  "Check if CONTENT-LINE-CTX contains the ending of an iCalendar object."
  (content-line-component-end?
   (context-result content-line-ctx)))

(define (stream:component-begin? ctx content-line-ctx)
  "Check if CONTENT-LINE-CTX contains the beginning of an iCalendar component."
  (let ((content-line (context-result content-line-ctx)))
    (and (content-line-component-begin? content-line)
         (let* ((name      (content-line-value content-line))
                (component (ics-calendar-component-lookup name)))
           (if component
               (begin
                 (log-debug "BEGIN COMPONENT: ~a" name)
                 #t)
               #f)))))

(define (stream:component-end? ctx content-line-ctx)
  "Check if CONTENT-LINE-CTX contains the ending of an iCalendar event."
  (let ((content-line (context-result content-line-ctx)))
    (content-line-component-end? content-line)))


;;; Actions

(define (stream:create-object ctx content-line-ctx)
  "Create a new <ics-object> instance from the content line that is stored inside
CONTENT-LINE-CTX. The new object is stored in the current object slot inside
<stream-context> CTX.  Return the context."
  (let* ((content-line (context-result content-line-ctx))
         (name         (content-line-value content-line)))
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
  (let* ((content-line (context-result content-line-ctx))
         (value-type   (content-line-value-type content-line))
         (property (make <ics-property>
                     #:name       (content-line-name content-line)
                     #:value      (case value-type
                                    ((object list)
                                     (content-line-value content-line))
                                    ((structure)
                                     (list->vector
                                      (content-line-value content-line))))
                     #:parameters (content-line-parameters content-line)))
         (property (if (stream-context-parse-types? ctx)
                       (ics-property->typed-property property)
                       property))
         (current-object (stream-context-current-object ctx))
         (current-object-properties (ics-object-properties current-object)))
    (ics-object-properties-set! current-object
                                (cons property current-object-properties))
    ctx))

(define (stream:create-component ctx content-line-ctx)
  (let ((name (content-line-value (context-result content-line-ctx))))
    (stream-context-current-component-set! ctx
                                           (make <ics-object>
                                             #:name name))
    ctx))

(define (stream:append-component ctx content-line-ctx)
  (let ((current-object    (stream-context-current-object ctx))
        (current-component (stream-context-current-component ctx)))
    (ics-object-components-set! current-object
                                (append (ics-object-components current-object)
                                        (list current-component)))
    (stream-context-current-component-set! ctx #f)
    ctx))

(define (stream:append-component-property ctx content-line-ctx)
  (let* ((content-line (context-result content-line-ctx))
         (property-name (content-line-name content-line))
         (value-type   (content-line-value-type content-line))
         (property (make <ics-property>
                     #:name       property-name
                     #:value      (case value-type
                                    ((object list)
                                     (content-line-value content-line))
                                    ((structure)
                                     (list->vector
                                      (content-line-value content-line))))
                     #:parameters (content-line-parameters content-line)))
         (property (if (stream-context-parse-types? ctx)
                       (ics-property->typed-property property)
                       property))
         (current-component (stream-context-current-component ctx)))
    (ics-object-properties-set! current-component
                                (append (ics-object-properties current-component)
                                        (list property)))
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
