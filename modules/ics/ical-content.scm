;;; ical-content.scm -- Common classes and procedures.

;; Copyright (C) 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (ics ical-content)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:export (<ical-content>
            ical-content-name
            ical-write-line
            ical-format
            scm->ical-value))


;;;

(define-class <ical-content> ()
  ;; string
  (name       #:accessor ical-content-name
              #:init-value #f
              #:init-keyword #:name))


;;;

(define-method (display (ical-content <ical-content>) (port <port>))
  (format port "#<ical-content ~a ~a>"  (ical-content-name ical-content)
          (number->string (object-address ical-content) 16)))

(define-method (write (ical-content <ical-content>) (port <port>))
  (format port "#<ical-content ~a ~a>"  (ical-content-name ical-content)
          (number->string (object-address ical-content) 16)))


;;;

(define* (ical-write-line obj #:optional (port (current-output-port)))
  "Display an OBJ to a PORT with CRLF line ending."
  (format port "~a\r\n" obj))

(define (ical-format dest fmt . arg)
  "Write output specified by the FMT string to DEST with CRLF line
ending."
  (ical-write-line (apply format #f fmt arg) dest))


;;;

;; RFC 5545, 3.3.11.
(define (escape-chars text)
  (regexp-substitute/global #f "[\n]"
                            (regexp-substitute/global #f "([\\;,])"
                                                      text
                                                      'pre "\\" 0 'post)
                            'pre "\\n" 'post))

(define (scm->ical-value value)
  (if (list? value)
      (string-join (map escape-chars value) ",")
      (escape-chars value)))

;;; ical-content.scm ends here.
