;;; command-convert.scm -- CLI interface for the format converter.

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

;; This module contains the CLI interface for converting formats to
;; icalendar/vCard.


;;; Code:

(define-module (ics cli command-convert)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-41)
  #:use-module (oop goops)
  #:use-module (ics)
  #:use-module (ics property)
  #:use-module (ics object)
  #:export (command-convert))


(define (print-help)
  (display "\
Usage: ics describe [options] [input-file]

Options:
  --help, -h                 Print this message and exit.
  --format, -f <input-format>
                             Set the input data format.
                             Supported formats:
                             - \"dsv\" (default, requires Guile-DSV)
  --to, -t <output-format>   Set the output format.
                             Supported output formats:
                             - \"vcf\" (\"Virtual Contact File\", default)
                             - \"ics\" (\"Internet Calendaring and Scheduling\")
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (format                   (single-char #\f) (value #t))
    (to                       (single-char #\t) (value #t))))

(define (record->object name record header)
  "Convert a RECORD to a vCard object using HEADER.  Return a new vCard object."
  (let loop ((r          record)
             (h          header)
             (properties '()))
    (if (null? r)
        (make <ics-object>
          #:name name
          #:properties properties)
        (let* ((name  (string->symbol (car h)))
               (value (car r))
               (prop  (make <ics-property>
                        #:name name
                        #:value value)))
          (loop (cdr r)
                (cdr h)
                (cons prop properties))))))

(define (command-convert args)
  (let* ((options          (getopt-long args %option-spec))
         (fmt              (option-ref options 'format "dsv"))
         (to               (option-ref options 'to     "vcf"))
         (help-needed?     (option-ref options 'help   #f))
         (args             (option-ref options '()     #f)))

    (when help-needed?
      (print-help)
      (exit 0))

    (let* ((port (if (null? args)
                     (current-input-port)
                     (let ((p (open-input-file (car args))))
                       (unless p
                         (error "Could not open a file" (car args)))
                       p))))
      (if (string=? fmt "dsv")
          (begin
            (let* ((delimiter (let* ((line
                                      (read-line port))
                                     (delimiter
                                      ((@ (dsv) guess-delimiter) line)))
                                (unget-char port #\newline)
                                (unget-string port line)
                                delimiter))
                   (data      ((@ (dsv) dsv->scm) port delimiter))
                   (header    (car data))
                   (rest      (cdr data))
                   (name      (case (string->symbol to)
                                ((vcf)
                                 "VCARD")
                                ((ics)
                                 "VCALENDAR")
                                (else
                                 (error "Unknown output format" to)))))
              (let ((vcards (map (lambda (record)
                                   (record->object name record header))
                                 rest)))
                (for-each (lambda (vcard)
                            (format #t "BEGIN:~a\r\n" (ics-object-name vcard))
                            (for-each (lambda (prop)
                                        (format #t "~a:~a\r\n"
                                                (ics-property-name prop)
                                                (ics-property-value prop)))
                                      (ics-object-properties vcard))
                            (format #t "END:~a\r\n\r\n" (ics-object-name vcard)))
                          vcards))))
          (error "Unknown format" fmt)))))

;;; command-convert.scm ends here.
