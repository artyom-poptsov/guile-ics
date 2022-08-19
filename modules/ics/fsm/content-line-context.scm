(define-module (ics fsm content-line-context)
  #:use-module (oop goops)
  #:use-module (ice-9 textual-ports)
  #:use-module (ics fsm context)
  #:export (<content-line>
            content-line?
            content-line-name
            content-line-parameters
            content-line-parameters-set!
            content-line-parameter
            content-line-parameter-set!
            content-line-value
            content-line-value-set!

            ;; Content line types.
            content-line-name=?
            content-line-value=?
            content-line-vcalendar?
            content-line-vcalendar-begin?
            content-line-vcalendar-end?
            content-line-vevent?
            content-line-vevent-begin?
            content-line-vevent-end?

            <content-line-context>
            content-line-context?
            content-line-context-eof?
            content-line-context-buffer
            content-line-context-buffer-set!
            content-line-context-result
            content-line-context-result-set!

            ;; FSM procedures.
            content-line:valid-name-character?
            content-line:safe-char?
            content-line:qsafe-char?
            content-line:value-char?
            content-line:control?
            content-line:create
            content-line:store-name
            content-line:store-value
            content-line:store-value/unget-char
            content-line:store-param-name
            content-line:store-param-value
            content-line:error-invalid-name
            content-line:error-param-eof
            content-line:error-invalid-content-line

            ;; Misc procedures.
            context-buffer->string))


;; Classes.

(define-class <content-line> ()
  ;; <string> | #f
  (name
   #:init-value   #f
   #:init-keyword #:name
   #:getter       content-line-name)

  ;; <alist>
  (parameters
   #:init-value   '()
   #:init-keyword #:parameters
   #:getter       content-line-parameters
   #:setter       content-line-parameters-set!)

  ;; <string> | #f
  (value
   #:init-value   #f
   #:init-keyword #:value
   #:getter       content-line-value
   #:setter       content-line-value-set!))

(define (content-line? x)
  (is-a? x <content-line>))

(define-class <content-line-context> (<char-context>)
  ;; <string> | #f
  (string-buffer
   #:init-value   #f
   #:getter       content-line-context-buffer
   #:setter       content-line-context-buffer-set!)

  ;; <content-line> | #f
  (result
   #:init-value   #f
   #:init-keyword #:content-line
   #:getter       content-line-context-result
   #:setter       content-line-context-result-set!))

(define (content-line-context? x)
  (is-a? x <content-line-context>))

(define-method (content-line-context-eof? (context <content-line-context>))
  (equal? (content-line-context-result context) #f))


(define-method (content-line-parameter-set! (content-line <content-line>)
                                            (name         <string>)
                                            (value        <string>))
  "Set a CONTENT-LINE parameter."
  (let ((parameters (content-line-parameters content-line)))
    (content-line-parameters-set! content-line (acons name value parameters))))

(define-method (content-line-parameter content-line name)
  "Return the value for a CONTENT-LINE parameter with the specified NAME."
  (assoc-ref (content-line-parameters content-line) name))


;; Predicates (guards).

(define (content-line:control? ctx ch)
  "All the controls except HTAB."
  (let ((codepoint (char->integer ch)))
    (or (and (>= codepoint #x00)
             (<= codepoint #x08))
        (and (>= codepoint #xA)
             (<= codepoint #x1F))
        (= codepoint #x7F))))

(define (content-line:safe-char? ctx ch)
  (and (not (content-line:control? ctx ch))
       (not (guard:double-quote? ctx ch))
       (not (guard:semicolon? ctx ch))
       (not (guard:colon? ctx ch))
       (not (guard:comma? ctx ch))))

(define (content-line:qsafe-char? ctx ch)
  (and (not (content-line:control? ctx ch))
       (not (guard:double-quote? ctx ch))))

;; TODO: Handle "NON-US-ASCII"
;; <https://datatracker.ietf.org/doc/html/rfc5545#section-3.1>
(define (content-line:value-char? ctx ch)
  (let ((codepoint (char->integer ch)))
    (or (guard:space? ctx ch)
        (and (>= codepoint #x21)
             (<= codepoint #x7E)))))

(define (content-line:valid-name-character? ctx ch)
  "Check if a character CH is a valid content line name."
  (or (guard:hyphen-minus? ctx ch)
      (char-set-contains? char-set:letter+digit ch)))


;;; Actions.

(define (context-buffer->string ctx)
  (list->string (reverse (context-buffer ctx))))

(define (content-line:create ctx ch)
  (content-line-context-result-set! ctx
                                    (make <content-line>
                                      #:name (context-buffer->string ctx)))
  (context-buffer-clear! ctx)
  ctx)

(define (content-line:store-value ctx ch)
  (let ((content-line (content-line-context-result ctx)))
    (content-line-value-set! content-line (context-buffer->string ctx))
    (context-buffer-clear! ctx)
    ctx))

(define (content-line:store-param-name ctx ch)
  (content-line-context-buffer-set! ctx (context-buffer->string ctx))
  (context-buffer-clear! ctx)
  ctx)

(define (content-line:store-param-value ctx ch)
  (let ((content-line (content-line-context-result ctx)))
    (content-line-parameter-set! content-line
                                 (content-line-context-buffer ctx)
                                 (context-buffer->string ctx))
    (context-buffer-clear! ctx)
    ctx))

(define (content-line:store-value/unget-char ctx ch)
  (content-line:store-value ctx ch)
  (unget-char (char-context-port ctx) ch)
  ctx)


;;; Errors.

(define (content-line:error-invalid-name ctx ch)
  (error "Invalid name" ctx ch))

(define (content-line:error-param-eof ctx ch)
  (error "Unexpected EOF during parameter read" ctx ch))

(define (content-line:error-invalid-content-line ctx ch)
  (error "Invalid content line" ctx ch))


;; Content line predicates.

(define-method (content-line-name=? (content-line <content-line>)
                                    (name         <string>))
  (string=? (content-line-name content-line) name))

(define-method (content-line-value=? (content-line <content-line>)
                                     (value        <string>))
  (string=? (content-line-value content-line) value))


(define-method (content-line-vcalendar? (content-line <content-line>))
  (content-line-name=? content-line "VCALENDAR"))

(define-method (content-line-vcalendar-begin? (content-line <content-line>))
  (and (content-line-vcalendar? content-line)
       (content-line-value=? content-line "BEGIN")))

(define-method (content-line-vcalendar-end? (content-line <content-line>))
  (and (content-line-vcalendar? content-line)
       (content-line-value=? content-line "END")))

(define-method (content-line-vevent? (content-line <content-line>))
  (content-line-name=? content-line "VEVENT"))

(define-method (content-line-vevent-begin? (content-line <content-line>))
  (and (content-line-vevent? content-line)
       (content-line-value=? content-line "BEGIN")))

(define-method (content-line-vevent-end? (content-line <content-line>))
  (and (content-line-vevent? content-line)
       (content-line-value=? content-line "END")))

;;; content-line-context.scm ends here.

