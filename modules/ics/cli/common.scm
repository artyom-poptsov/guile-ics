;;; common.scm -- Common CLI procedures for Guile-ICS.

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

;; This module contains common CLI procedures for Guile-ICS.


;;; Code:

(define-module (ics cli common)
  #:export (string-any=?
            command-match))

(define (string-any=? str string-list)
  (cond
   ((null? string-list)
    #f)
   ((string=? str (car string-list))
    #t)
   (else
    (string-any=? str (cdr string-list)))))

(define (command-match command command-list)
  (if (null? command-list)
      #f
      (if (string-any=? command (caar command-list))
          (cadar command-list)
          (command-match command (cdr command-list)))))

;;; common.scm ends here.
