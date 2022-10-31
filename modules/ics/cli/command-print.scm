;;; command-print.scm -- CLI for iCalendar/vCard formatting.

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

;; This module contains the command line interface for printing iCalendar/vCard
;; data in various formats.


;;; Code:

(define-module (ics cli command-print)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-41)
  #:use-module (ics)
  #:use-module (ics conv)
  #:export (command-print))


(define (print-help)
  (display "\
Usage: ics print [options] [input-file]

Options:
  --help, -h                 Print this message and exit.
  --format, -f <output-format>
                             Set the target format.
                             Supported targets:
                             - \"pretty\" (default)
                             - \"org-mode\"
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (format                   (single-char #\f) (value #t))))


(define (command-print args)
  (let* ((options          (getopt-long args %option-spec))
         (help-needed?     (option-ref options 'help  #f))
         (fmt              (option-ref options 'format "pretty"))
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
      (case (string->symbol fmt)
        ((pretty)
         (stream-for-each (lambda (e)
                            (ics-pretty-print e
                                              #:indent 4
                                              #:show-types? #t))
                          stream))
        ((org-mode)
         (stream-for-each ics-object->org-mode stream))
        (else
         (error "Unknown format" fmt))))))

;;; command-convert.scm ends here.
