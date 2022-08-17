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
             (ics fsm content-line-parser-context)
             (ics fsm content-line-parser))


(define %test-suite-name "content-line-parser")

(test-begin %test-suite-name)

(test-equal "correct input: name check"
  "VERSION"
  (with-input-from-string
      "VERSION:2.0\r\n"
    (lambda ()
      (let* ((fsm (make <content-line-parser>))
             (ctx (fsm-run! fsm (make <content-line-context>
                                  #:port (current-input-port)))))
        (content-line-name (content-line-context-result ctx))))))

(test-equal "correct input: value check"
  "2.0"
  (with-input-from-string
      "VERSION:2.0\r\n"
    (lambda ()
      (let* ((fsm (make <content-line-parser>))
             (ctx (fsm-run! fsm (make <content-line-context>
                                  #:port (current-input-port)))))
        (content-line-value (content-line-context-result ctx))))))


(test-equal "correct input with parameter: name check"
  "NAME"
  (with-input-from-string
      "NAME;PARAM_NAME=PARAM_VALUE:VALUE\r\n"
    (lambda ()
      (let* ((fsm (make <content-line-parser>))
             (ctx (fsm-run! fsm (make <content-line-context>
                                  #:port (current-input-port)))))
        (content-line-name (content-line-context-result ctx))))))

(test-equal "correct input with parameter: value check"
  "VALUE"
  (with-input-from-string
      "NAME;PARAM_NAME=PARAM_VALUE:VALUE\r\n"
    (lambda ()
      (let* ((fsm (make <content-line-parser>))
             (ctx (fsm-run! fsm (make <content-line-context>
                                  #:port (current-input-port)))))
        (content-line-value (content-line-context-result ctx))))))

(test-equal "correct input with parameter: parameter check"
  "PARAM_VALUE"
  (with-input-from-string
      "NAME;PARAM_NAME=PARAM_VALUE:VALUE\r\n"
    (lambda ()
      (let* ((fsm (make <content-line-parser>))
             (ctx (fsm-run! fsm (make <content-line-context>
                                  #:port (current-input-port)))))
        (content-line-parameter (content-line-context-result ctx)
                                "PARAM_NAME")))))


(test-equal "correct multi-line input: name check"
  "NAME"
  (with-input-from-string
      (string-append
       "NAME:\r\n"
       " This is\r\n"
       "  a long stri\r\n"
       " ng of text\r\n")
    (lambda ()
      (let* ((fsm (make <content-line-parser>))
             (ctx (fsm-run! fsm (make <content-line-context>
                                  #:port (current-input-port)))))
        (content-line-name (content-line-context-result ctx))))))

(test-equal "correct multi-line input: value check"
  "This is a long string of text"
  (with-input-from-string
      (string-append
       "NAME:\r\n"
       " This is\r\n"
       "  a long stri\r\n"
       " ng of text\r\n")
    (lambda ()
      (let* ((fsm (make <content-line-parser>))
             (ctx (fsm-run! fsm (make <content-line-context>
                                  #:port (current-input-port)))))
        (content-line-value (content-line-context-result ctx))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; content-line-parser-context.scm ends here.
