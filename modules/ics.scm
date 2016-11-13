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


;;; Commentary:

;; Input example (from RFC5545):

#!
BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:19970610T172345Z-AF23B2@example.com
DTSTAMP:19970610T172345Z
DTSTART:19970714T170000Z
DTEND:19970715T040000Z
SUMMARY:Bastille Day Party
END:VEVENT
END:VCALENDAR
!#

;; Output example:

#!
(((ICALPROPS
    ((PRODID . "-//hacksw/handcal//NONSGML v1.0//EN")
     (VERSION . "2.0")))
  (COMPONENT
    ((VEVENT
       (ICALPROPS
         ((SUMMARY . "Bastille Day Party")
          (DTEND . "19970715T040000Z")
          (DTSTART . "19970714T170000Z")
          (DTSTAMP . "19970610T172345Z")
          (UID . "19970610T172345Z-AF23B2@example.com")))
       (COMPONENT ()))))))
!#


;;; Code:

(define-module (ics)
  ;; Guile-ICS
  #:use-module (ics common)
  #:use-module (ics fsm)
  #:use-module (ics parser)
  #:export (ics->scm ics-string->scm))


(define (ics->scm port)
  (debug-fsm-transition "fsm-read-ical-stream")
  (fsm-read-ical-stream (make-parser port) '()))

(define (ics-string->scm str)
  "Parse ICS string STR."
  (let ((parser (make-string-parser str)))
    (ics->scm parser)))


;;;
