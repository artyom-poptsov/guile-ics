;; Copyright (C) 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 rdelim)
  #:use-module (scheme documentation)
  #:export (<ics-parser> make-parser parser? parser-port
                         parser-read-char))


;;;

(define-immutable-record-type <ics-parser>
  (make-parser port)
  parser?
  (port parser-port))


;;;

(define (parser-read-char parser)
  (read-char (parser-port parser)))


;;;

