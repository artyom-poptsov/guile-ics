;;; ics-content.scm -- Common classes and procedures.

;; Copyright (C) 2017-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains definition of <ics-content> class -- a common
;; class that represents generic iCalendar contents.
;;
;; See the Texinfo documentation for details.


;;; Code:

(define-module (ics type content)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:export (<ics-content>
            ics-content-name
            ics-write-line
            ics-format
            scm->ics-value))


;;;

(define-class <ics-content> ()
  ;; string
  (name       #:accessor ics-content-name
              #:init-keyword #:name))

(define-method (initialize (property <ics-content>) initargs)
  (next-method)
  (unless (memq #:name initargs)
    (throw 'guile-ics-error "#:name slot is mandatory" initargs)))


;;; Class printers.

(define-method (%display (ics-content <ics-content>) (port <port>))
  (next-method)
  (format port "#<ics-content ~a ~a>"  (ics-content-name ics-content)
          (number->string (object-address ics-content) 16)))

(define-method (write (ics-content <ics-content>) (port <port>))
  (%display ics-content port))

(define-method (display (ics-content <ics-content>) (port <port>))
  (%display ics-content port))


;;;

(define* (ics-write-line obj #:optional (port (current-output-port)))
  "Display an OBJ to a PORT with CRLF line ending."
  (format port "~a\r\n" obj))

(define (ics-format dest fmt . arg)
  "Write output specified by the FMT string to DEST with CRLF line
ending."
  (ics-write-line (apply format #f fmt arg) dest))


;;;

;; RFC 5545, 3.3.11.
(define (escape-chars text)
  (regexp-substitute/global #f "[\n]"
                            (regexp-substitute/global #f "([\\;,])"
                                                      text
                                                      'pre "\\" 0 'post)
                            'pre "\\n" 'post))

(define (scm->ics-value value)
  "Convert a VALUE to iCalendar format.  VALUE can be either a list of
strings or a string."
  (if (list? value)
      (string-join (map escape-chars value) ",")
      (escape-chars value)))

;;; ics-content.scm ends here.
