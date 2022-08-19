;;; ics.scm -- Tests for ICS parser.

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


;;; Code:

(use-modules (srfi srfi-64)
             (oop goops)
             (ics fsm context)
             (ics fsm content-line-context))


(define %test-suite-name "content-line-context")

(test-begin %test-suite-name)


(test-equal "content-line-parameter: non-existing parameter"
    #f
  (let ((content-line (make <content-line>)))
    (content-line-parameter content-line "parameter")))

(test-equal "content-line-parameter-set!"
  "value"
  (let ((content-line (make <content-line>)))
    (content-line-parameter-set! content-line "parameter" "value")
    (content-line-parameter content-line "parameter")))


;;; Guards.

(test-equal "content-line:control?: #t"
  #t
  (content-line:control? #f #\vtab))

(test-equal "content-line:control?: #f"
  #f
  (content-line:control? #f #\a))

(test-equal "content-line:safe-char?: #t"
  #t
  (content-line:safe-char? #f #\a))

(test-equal "content-line:safe-char?: #f"
  #f
  (content-line:safe-char? #f #\:))

(test-equal "content-line:value-char?: #t"
  #t
  (and (content-line:value-char? #f #\space)
       (content-line:value-char? #f #\a)))

(test-equal "content-line:value-char?: #f"
  #f
  (content-line:value-char? #f #\newline))

(test-equal "content-line:valid-name-character?: #t"
  #t
  (and (content-line:valid-name-character? #f #\-)
       (content-line:valid-name-character? #f #\a)
       (content-line:valid-name-character? #f #\1)))

(test-equal "content-line:valid-name-character?: #f"
  #f
  (content-line:valid-name-character? #f #\newline))


(test-equal "context-buffer->string"
  "hello"
  (let ((ctx  (make <content-line-context>))
        (data (string->list "hello")))
    (for-each (lambda (ch)
                (action:store ctx ch))
              data)
    (context-buffer->string ctx)))

(test-assert "content-line:create: context result check"
  (let ((ctx  (make <content-line-context>))
        (data (string->list "hello")))
    (for-each (lambda (ch)
                (action:store ctx ch))
              data)
    (content-line:create ctx #f)
    (and (content-line? (content-line-context-result ctx))
         (string=? (content-line-name (content-line-context-result ctx))
                   "hello"))))

(test-equal "content-line:create: content line name check"
  "hello"
  (let ((ctx  (make <content-line-context>))
        (data (string->list "hello")))
    (for-each (lambda (ch)
                (action:store ctx ch))
              data)
    (content-line:create ctx #f)
    (content-line-name (content-line-context-result ctx))))

(test-equal "content-line:store-value"
  "world"
  (let ((ctx   (make <content-line-context>))
        (name  (string->list "hello"))
        (value (string->list "world")))
    (for-each (lambda (ch)
                (action:store ctx ch))
              name)
    (content-line:create ctx #f)
    (for-each (lambda (ch)
                (action:store ctx ch))
              value)
    (content-line:store-value ctx #f)
    (content-line-value (content-line-context-result ctx))))

(test-equal "content-line:store-param-name"
  "param"
  (let ((ctx   (make <content-line-context>))
        (content-line-name (string->list "test"))
        (param-name        (string->list "param")))
    (for-each (lambda (ch)
                (action:store ctx ch))
              content-line-name)
    (content-line:create ctx #f)
    (for-each (lambda (ch)
                (action:store ctx ch))
              param-name)
    (content-line:store-param-name ctx #f)
    (content-line-context-buffer ctx)))

(test-equal "content-line:store-param-value"
  "param-value"
  (let ((ctx   (make <content-line-context>))
        (content-line-name (string->list "test"))
        (param-name        (string->list "param"))
        (param-value       (string->list "param-value")))
    (for-each (lambda (ch)
                (action:store ctx ch))
              content-line-name)
    (content-line:create ctx #f)
    (for-each (lambda (ch)
                (action:store ctx ch))
              param-name)
    (content-line:store-param-name ctx #f)
    (for-each (lambda (ch)
                (action:store ctx ch))
              param-value)
    (content-line:store-param-value ctx #f)
    (content-line-parameter (content-line-context-result ctx)
                            "param")))


;; Test content line predicates.

(test-assert "content-line-name=?"
  (content-line-name=? (make <content-line>
                         #:name "VCALENDAR"
                         #:value "BEGIN")
                       "VCALENDAR"))

(test-assert "content-line-value=?"
  (content-line-value=? (make <content-line>
                          #:name "VCALENDAR"
                         #:value "BEGIN")
                       "BEGIN"))

(test-assert "content-line-vcalendar?"
  (content-line-vcalendar? (make <content-line>
                             #:name "VCALENDAR"
                             #:value "BEGIN")))

(test-assert "content-line-vcalendar-begin?"
  (content-line-vcalendar-begin? (make <content-line>
                                   #:name "VCALENDAR"
                                   #:value "BEGIN")))

(test-assert "content-line-vcalendar-end?"
  (content-line-vcalendar-end? (make <content-line>
                                   #:name "VCALENDAR"
                                   #:value "END")))

(test-assert "content-line-vevent?"
  (content-line-vevent? (make <content-line>
                          #:name "VEVENT"
                          #:value "BEGIN")))

(test-assert "content-line-vevent-begin?"
  (content-line-vevent-begin? (make <content-line>
                                #:name "VEVENT"
                                #:value "BEGIN")))

(test-assert "content-line-vevent-end?"
  (content-line-vevent-end? (make <content-line>
                              #:name "VEVENT"
                              #:value "END")))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; content-line-parser-context.scm ends here.
