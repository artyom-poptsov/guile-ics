;;; ics.scm -- Tests for VCF parser.

;; Copyright (C) 2022-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (oop goops)
             (ice-9 rdelim)
             (ice-9 streams)
             (tests common)
             (ics)
             (ics common)
             (ics object)
             (ics property))


(define %test-suite-name "vcf")

(configure-test-logging! %test-suite-name)

(test-begin %test-suite-name)



(test-equal "ics->stream: FN"
  "Random J. Hacker"
  (with-input-from-string
      (string-append
       "BEGIN:VCARD\r\n"
       "FN:Random J. Hacker\r\n"
       "END:VCARD\r\n")
    (lambda ()
      (let ((obj (stream-car (ics->stream))))
        (ics-property-value (ics-object-property-ref obj "FN"))))))

;; See <https://github.com/artyom-poptsov/guile-ics/issues/1>
(test-equal "property with multiple instances of a parameter"
  "bystander@vcard.broken"
  (with-input-from-string
      (string-append "BEGIN:VCARD\r\n"
                     "VERSION:3.0\r\n"
                     "FN:Bystander\r\n"
                     "EMAIL;TYPE=INTERNET;TYPE=HOME:bystander@vcard.broken\r\n"
                     "END:VCARD\r\n")
    (lambda ()
      (let ((obj (stream-car (ics->stream))))
        (ics-property-value (ics-object-property-ref obj "EMAIL"))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; vcf.scm ends here.
