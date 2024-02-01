;;; content-line-context.scm -- Context for the content line reader.

;; Copyright (C) 2022-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains the iCalendar content line parser context, as well as
;; the required guards, actions and other procedures.


;;; Code:

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
            content-line-value-type
            content-line-value-type-set!

            ;; Content line types.
            content-line-name=?
            content-line-value=?
            content-line-vcalendar-begin?
            content-line-vcalendar-end?
            content-line-component-begin?
            content-line-component-end?

            <content-line-context>
            content-line-context?
            content-line-context-eof?

            ;; FSM procedures.
            content-line:valid-name-character?
            content-line:safe-char?
            content-line:qsafe-char?
            content-line:value-char?
            content-line:control?
            content-line:store-escaped
            content-line:create
            content-line:store-name
            content-line:store-value
            content-line:store-value/unget-char
            content-line:store-param-name
            content-line:store-param-value
            content-line:store-param-value/list
            content-line:error-invalid-name
            content-line:error-param-eof
            content-line:error-invalid-content-line

            ;; Misc procedures.
            context-buffer->string))


;;; Constants.

;; See <https://tools.ietf.org/html/rfc5545#section-3.6>.
(define %ics-token-begin "BEGIN")
(define %ics-token-end   "END")

;; See <https://tools.ietf.org/html/rfc5545#section-3.4>.
(define %ics-token-vcalendar "VCALENDAR")

(define %content-line-error-key 'content-line-error)


;; Classes.

(define-class <content-line> ()
  ;; The name of the content line.
  ;;
  ;; <string> | #f
  (name
   #:init-value   #f
   #:init-keyword #:name
   #:getter       content-line-name)

  ;; The associative list of parameters.
  ;;
  ;; <alist>
  (parameters
   #:init-value   '()
   #:init-keyword #:parameters
   #:getter       content-line-parameters
   #:setter       content-line-parameters-set!)

  ;; <symbol>
  (value-type
   #:init-value   'object
   #:init-keyword #:value-type
   #:getter       content-line-value-type
   #:setter       content-line-value-type-set!)

  ;; The value of the content line.
  ;;
  ;; <string> | #f
  (value
   #:init-value   #f
   #:init-keyword #:value
   #:getter       content-line-value
   #:setter       content-line-value-set!))

(define (content-line? x)
  "Check if X is a <content-line> instance."
  (is-a? x <content-line>))

(define-class <content-line-context> (<char-context>))

(define (content-line-context? x)
  "Check if X is a <content-line-context> instance."
  (is-a? x <content-line-context>))

(define-method (content-line-context-eof? (context <content-line-context>))
  "Check if a CONTEXT contains no result (that is, the iCalendar stream ended with
EOF.)"
  (equal? (context-result context) '()))


(define-method (content-line-parameter-set! (content-line <content-line>)
                                            (name         <symbol>)
                                            (value        <top>))
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
       (not (char:double-quote? ctx ch))
       (not (char:semicolon? ctx ch))
       (not (char:colon? ctx ch))
       (not (char:comma? ctx ch))))

(define (content-line:qsafe-char? ctx ch)
  (and (not (content-line:control? ctx ch))
       (not (char:double-quote? ctx ch))))

(define (content-line:value-char? ctx ch)
  "Check if a CH is a valid character for a value."
  (let ((codepoint (char->integer ch)))
    (>= codepoint #x20)))

(define (content-line:valid-name-character? ctx ch)
  "Check if a character CH is a valid content line name."
  (or (char:hyphen-minus? ctx ch)
      (char-set-contains? char-set:letter+digit ch)))


;;; Actions.

(define (context-buffer->string ctx)
  (list->string (context-buffer/reversed ctx)))

(define (content-line:store-escaped ctx ch)
  (case ch
    ((#\n #\N)
     (push-event-to-buffer ctx #\newline))
    (else
     (push-event-to-buffer ctx ch))))

(define (content-line:create ctx ch)
  (context-result-set! ctx (make <content-line>
                             #:name (context-buffer->string ctx)))
  (clear-buffer ctx))

(define (content-line:store-value ctx ch)
  (let* ((content-line  (context-result ctx))
         (current-value (content-line-value content-line))
         (new-value     (context-buffer->string ctx)))
    (if current-value
        (if (list? current-value)
            (content-line-value-set! content-line
                                     (append current-value
                                             (list new-value)))
            (content-line-value-set! content-line
                                     (append (list current-value)
                                             (list new-value))))
        (content-line-value-set! content-line new-value))
    (case ch
      ((#\,)
       (content-line-value-type-set! content-line 'list))
      ((#\;)
       (content-line-value-type-set! content-line 'structure)))
    (clear-stanza (clear-buffer ctx))))

(define (content-line:store-param-name ctx ch)
  (clear-buffer (push-event-to-stanza ctx (context-buffer->string ctx))))

(define (content-line:store-param-value ctx ch)
  (let* ((content-line  (context-result ctx))
         (param-name    (string->symbol (car (context-stanza ctx))))
         (param-value   (context-buffer->string ctx))
         (param-current (content-line-parameter content-line param-name)))
    (if param-current
        (begin
          (log-warning (string-append "~a:~a:~a:"
                                      " duplicated parameter '~a':"
                                      " old value: '~a'; new value '~a'"
                                      " -- assuming it is a value list")
                       (context-port ctx)
                       (context-row-number ctx)
                       (context-col-number ctx)
                       param-name
                       param-current
                       param-value)
          (content-line-parameter-set! content-line
                                       param-name
                                       (list
                                        (content-line-parameter content-line
                                                                param-name)))
          (content-line:store-param-value/list ctx ch))
        (content-line-parameter-set! content-line param-name param-value))
    (clear-stanza (clear-buffer ctx))))

(define (content-line:store-param-value/list ctx ch)
  "Append a value to the list of parameter values for the parameter that is being
read."
  (let* ((content-line  (context-result ctx))
         (param-name    (string->symbol (car (context-stanza ctx))))
         (param-value   (context-buffer->string ctx))
         (param-current (content-line-parameter content-line param-name)))
    (if param-current
        (content-line-parameter-set! content-line
                                     param-name
                                     (append param-current (list param-value)))
        (content-line-parameter-set! content-line
                                     param-name
                                     (list param-value)))
    (clear-buffer ctx)))

(define (content-line:store-value/unget-char ctx ch)
  "Return a character CH to the iCalendar stream port from the context CTX.  Return
the context."
  (content-line:store-value ctx ch)
  (unget-char (context-port ctx) ch)
  ctx)


;;; Errors.

(define (content-line:error-invalid-name ctx ch)
  (let ((msg "Invalid name"))
    (log-error "~a:~a:~a: ~a"
               (context-port ctx)
               (context-row-number ctx)
               (context-col-number ctx)
               msg)
    (throw %content-line-error-key msg ctx ch)))

(define (content-line:error-param-eof ctx ch)
  (let ((msg "Unexpected EOF during parameter read"))
    (log-error "~a:~a:~a: ~a"
               (context-port ctx)
               (context-row-number ctx)
               (context-col-number ctx)
               msg)
    (throw %content-line-error-key msg ctx ch)))

(define (content-line:error-invalid-content-line ctx ch)
  (let ((msg "Invalid content line"))
    (log-error "~a:~a:~a: ~a"
               (context-port ctx)
               (context-row-number ctx)
               (context-col-number ctx)
               msg)
    (throw %content-line-error-key msg ctx ch)))


;; Content line predicates.

(define-method (content-line-name=? (content-line <content-line>)
                                    (name         <string>))
  (string=? (content-line-name content-line) name))

(define-method (content-line-value=? (content-line <content-line>)
                                     (value        <string>))
  (string=? (content-line-value content-line) value))

(define-method (content-line-component-begin? (content-line <content-line>))
  (content-line-name=? content-line %ics-token-begin))

(define-method (content-line-component-end? (content-line <content-line>))
  (content-line-name=? content-line %ics-token-end))

(define-method (content-line-vcalendar-begin? (content-line <content-line>))
  (and (content-line-name=? content-line %ics-token-begin)
       (content-line-value=? content-line %ics-token-vcalendar)))

(define-method (content-line-vcalendar-end? (content-line <content-line>))
  (and (content-line-name=? content-line %ics-token-end)
       (content-line-value=? content-line %ics-token-vcalendar)))

;;; content-line-context.scm ends here.
