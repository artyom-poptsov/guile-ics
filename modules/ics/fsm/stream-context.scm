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
            stream:error-unexpected-eof-in-object))


(define-class <stream-context> (<context>)
  ;; <port>
  (port
   #:init-thunk   (lambda () (current-input-port))
   #:init-keyword #:port
   #:getter       stream-context-port)

  ;; <ics-object> | #f
  (current-object
   #:init-value   #f
   #:init-keyword #:current-object
   #:getter       stream-context-current-object
   #:setter       stream-context-current-object-set!)

  ;; <list>
  (objects
   #:init-value   '()
   #:init-keyword #:objects
   #:getter       stream-context-objects
   #:setter       stream-context-objects-set!))

(define (stream-context? x)
  (is-a? x <stream-context>))

(define-method (stream-context-append-object! (context <stream-context>))
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
  (content-line-context-eof? content-line-parser-ctx))

(define (stream:vcalendar-begin? ctx content-line-ctx)
  (content-line-vcalendar-begin?
   (content-line-parser-context-result content-line-ctx)))

(define (stream:vcalendar-end? ctx content-line-ctx)
  (content-line-vcalendar-end?
   (content-line-parser-context-result content-line-ctx)))

(define (stream:vevent-begin? ctx content-line-ctx)
  (content-line-vevent-begin?
   (content-line-parser-context-result content-line-ctx)))

(define (stream:vevent-end? ctx content-line-ctx)
  (content-line-vevent-end?
   (content-line-parser-context-result content-line-ctx)))


;;; Actions

(define (stream:create-object ctx content-line-ctx)
  (let* ((content-line (content-line-context-result content-line-ctx))
         (name         (content-line-name content-line)))
    (stream-context-current-object-set! ctx (make <ics-object>
                                              #:name name)))
  ctx)

(define (stream:append-object ctx content-line-ctx)
  (stream-context-append-object! ctx)
  (stream-context-current-object-set! ctx #f)
  ctx)

(define (stream:append-property ctx content-line-ctx)
  (let* ((content-line (content-line-context-result content-line-ctx))
         (property (make <ics-property>
                     #:name       (content-line-name content-line)
                     #:value      (content-line-value content-line)
                     #:parameters (content-line-parameters content-line)))
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

;;; stream-context.scm ends here.
