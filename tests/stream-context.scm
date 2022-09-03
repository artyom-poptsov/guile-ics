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

(add-to-load-path (getenv "abs_top_builddir"))

(use-modules (srfi srfi-64)
             (oop goops)
             (tests common)
             (ics object)
             (ics fsm context)
             (ics fsm content-line-context)
             (ics fsm stream-context))


(define %test-suite-name "stream-context")

(configure-test-logging! %test-suite-name)

(test-begin %test-suite-name)



(test-assert "stream-context?"
  (stream-context? (make <stream-context>)))

(test-assert "stream:create-object"
  (let* ((content-line     (make <content-line>
                             #:name "BEGIN"
                             #:value "VCALENDAR"))
         (content-line-ctx (make <content-line-context>
                             #:content-line content-line))
         (ctx              (stream:create-object (make <stream-context>)
                                                 content-line-ctx)))
    (ics-object? (stream-context-current-object ctx))))

(test-equal "stream-context-objects-count"
  1
  (let ((ctx (make <stream-context>
               #:objects (list (make <ics-object>
                                 #:name "BEGIN"
                                 #:value "VCALENDAR")))))
    (stream-context-objects-count ctx)))

(test-assert "stream:append-object"
  (let* ((content-line     (make <content-line>
                             #:name "BEGIN"
                             #:value "VCALENDAR"))
         (content-line-ctx (make <content-line-context>
                             #:content-line content-line))
         (ctx              (stream:create-object (make <stream-context>)
                                                 content-line-ctx)))
    (stream:append-object ctx #f)
    (and (not (stream-context-current-object ctx))
         (= (stream-context-objects-count ctx) 1)
         (ics-object? (car (stream-context-objects ctx))))))

(test-assert "stream:append-property"
  (let ((ctx (make <stream-context>
               #:current-object (make <ics-object>
                                  #:name  "BEGIN"
                                  #:value "VCALENDAR")))
        (content-line-ctx (make <content-line-context>
                            #:content-line (make <content-line>
                                             #:name "VERSION"
                                             #:value "2.0"))))
    (stream:append-property ctx content-line-ctx)
    (let ((current-object (stream-context-current-object ctx)))
      (and (ics-object? current-object)
           (= (length (ics-object-properties current-object)) 1)))))


(test-assert "stream:read"
  (with-input-from-string
      (string-append
       "BEGIN:VCALENDAR\r\n"
       "VERSION:2.0\r\n"
       "END:VCALENDAR\r\n")
    (lambda ()
      (let ((ctx (stream:read (make <stream-context>
                                #:port (current-input-port)))))
        (and (content-line-context? ctx)
             (content-line? (content-line-context-result ctx)))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; stream-context.scm ends here.


