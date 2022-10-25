;;; command-convert.scm -- CLI interface for iCalendar conversion.

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

(define-module (ics cli command-convert)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-41)
  #:use-module (ics)
  #:use-module (ics conv)
  #:export (command-convert))


(define (print-help)
  (display "\
Usage: ics convert [options] [input-file]

Options:
  --help, -h                 Print this message and exit.
  --to, -t <target-format>   Set the target format.
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))))


(define (command-convert args)
  (let* ((options          (getopt-long args %option-spec))
         (help-needed?     (option-ref options 'help  #f))
         (to               (option-ref options 'to    "org-mode"))
         (args             (option-ref options '()    #f)))

    (when help-needed?
      (print-help)
      (exit 0))

    (let* ((port   (if (null? args)
                       (current-input-port)
                       (let ((p (open-input-file (car args))))
                         (unless p
                           (error "Could not open a file" (car args)))
                         p)))
           (stream (ics->stream port)))
      (stream-for-each ics-object->org-mode stream))))

;;; command-convert.scm ends here.
