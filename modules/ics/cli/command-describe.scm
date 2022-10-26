;;; command-describe.scm -- CLI for iCalendar/vCard describing..

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

;; This module contains the command line interface for getting various
;; human-readable information about iCalendar/vCard data.


;;; Code:


(define-module (ics cli command-describe)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-41)
  #:use-module (ics)
  #:export (command-describe))

(define (print-help)
  (display "\
Usage: ics describe [options] [input-file]

Options:
  --help, -h                 Print this message and exit.
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))))

(define (command-describe args)
  (let* ((options          (getopt-long args %option-spec))
         (help-needed?     (option-ref options 'help  #f))
         (args             (option-ref options '()        #f)))

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
      (stream-for-each ics-describe stream))))

;;; command-describe.scm ends here.
