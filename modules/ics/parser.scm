;; Copyright (C) 2016, 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (ics parser)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:export (<ics-parser> make-parser make-string-parser
                         ics-parser? parser-port
                         parse-types?
                         parser-read-char parser-unread-char))


;;;

(define-class <ics-parser> ()
  (port #:accessor     parser-port
        #:init-keyword #:port)

  ;; <boolean>
  (parse-types? #:accessor     parse-types?
                #:init-keyword parse-types?
                #:init-value   #f))

(define-method (ics-parser? x)
  (is-a? x <ics-parser>))


;;;

(define (make-parser port)
  (make <ics-parser> #:port port))

(define (make-string-parser str)
  (call-with-input-string str
    (lambda (port)
      (make <ics-parser> #:port port))))


;;;

(define-method (parser-read-char parser)
  (read-char (parser-port parser)))

(define-method (parser-unread-char parser ch)
  (unread-char ch (parser-port parser)))


;;;

