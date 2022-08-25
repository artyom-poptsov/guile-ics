;;; stream-parser.scm -- Tests for ICS stream parser.

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



(use-modules (srfi srfi-64)
             (oop goops)
             (ics type object)
             (ics type property)
             (ics fsm context)
             (ics fsm content-line-context)
             (ics fsm content-line-parser)
             (ics fsm stream-context)
             (ics fsm stream-parser))


(define %test-suite-name "stream-parser")

(test-begin %test-suite-name)


(test-equal "correct input: output test: current-object must be #f"
  #f
  (with-input-from-string
      (string-append "BEGIN:VCALENDAR\r\n"
                     "VERSION:2.0\r\n"
                     "BEGIN:VEVENT\r\n"
                     "END:VEVENT\r\n"
                     "END:VCALENDAR")
    (lambda ()
      (let* ((fsm (make <stream-parser>))
             (ctx (fsm-run! fsm (make <stream-context>))))
        (stream-context-current-object ctx)))))

(test-assert "correct input: output test: objects basic check"
  (with-input-from-string
      (string-append "BEGIN:VCALENDAR\r\n"
                     "VERSION:2.0\r\n"
                     "BEGIN:VEVENT\r\n"
                     "END:VEVENT\r\n"
                     "END:VCALENDAR\r\n")
    (lambda ()
      (let* ((fsm (make <stream-parser>
                    #:debug-mode? #t))
             (ctx (fsm-run! fsm (make <stream-context>)))
             (objects (stream-context-objects ctx)))
        objects))))

(test-equal "correct input: output test: objects count check"
  1
  (with-input-from-string
      (string-append "BEGIN:VCALENDAR\r\n"
                     "VERSION:2.0\r\n"
                     "BEGIN:VEVENT\r\n"
                     "END:VEVENT\r\n"
                     "END:VCALENDAR\r\n")
    (lambda ()
      (let* ((fsm (make <stream-parser>
                    #:debug-mode? #t))
             (ctx (fsm-run! fsm (make <stream-context>)))
             (objects (stream-context-objects ctx)))
        (length objects)))))

(test-equal "correct input: output test: object name check"
  "VCALENDAR"
  (with-input-from-string
      (string-append "BEGIN:VCALENDAR\r\n"
                     "VERSION:2.0\r\n"
                     "BEGIN:VEVENT\r\n"
                     "END:VEVENT\r\n"
                     "END:VCALENDAR\r\n")
    (lambda ()
      (let* ((fsm (make <stream-parser>
                    #:debug-mode? #t))
             (ctx (fsm-run! fsm (make <stream-context>)))
             (objects (stream-context-objects ctx)))
        (ics-object-name (car objects))))))

(define %rfc5545-complex-object-1
  (string-append
   "BEGIN:VCALENDAR\r\n"
   "PRODID:-//xyz Corp//NONSGML PDA Calendar Version 1.0//EN\r\n"
   "VERSION:2.0\r\n"
   "BEGIN:VEVENT\r\n"
   "DTSTAMP:19960704T120000Z\r\n"
   "UID:uid1@example.com\r\n"
   "ORGANIZER:mailto:jsmith@example.com\r\n"
   "DTSTART:19960918T143000Z\r\n"
   "DTEND:19960920T220000Z\r\n"
   "STATUS:CONFIRMED\r\n"
   "CATEGORIES:CONFERENCE\r\n"
   "SUMMARY:Networld+Interop Conference\r\n"
   "DESCRIPTION:Networld+Interop Conference\r\n"
   "  and Exhibit\\nAtlanta World Congress Center\\n\r\n"
   " Atlanta\\, Georgia\r\n"
   "END:VEVENT\r\n"
   "END:VCALENDAR\r\n"))

(define %rfc5545-vtodo-example-1
  (string-append
   "BEGIN:VCALENDAR\r\n"
   "BEGIN:VTODO\r\n"
   "UID:20070313T123432Z-456553@example.com\r\n"
   "DTSTAMP:20070313T123432Z\r\n"
   "DUE;VALUE=DATE:20070501\r\n"
   "SUMMARY:Submit Quebec Income Tax Return for 2006\r\n"
   "CLASS:CONFIDENTIAL\r\n"
   "CATEGORIES:FAMILY,FINANCE\r\n"
   "STATUS:NEEDS-ACTION\r\n"
   "END:VTODO\r\n"
   "END:VCALENDAR\r\n"))

(define %rfc5545-vjournal-example-1
  (string-append
   "BEGIN:VCALENDAR\r\n"
   "BEGIN:VJOURNAL\r\n"
   "UID:19970901T130000Z-123405@example.com\r\n"
   "DTSTAMP:19970901T130000Z\r\n"
   "DTSTART;VALUE=DATE:19970317\r\n"
   "SUMMARY:Staff meeting minutes\r\n"
   "DESCRIPTION:1. Staff meeting: Participants include Joe\\,\r\n"
   "  Lisa\\, and Bob. Aurora project plans were reviewed.\r\n"
   "  There is currently no budget reserves for this project.\r\n"
   "  Lisa will escalate to management. Next meeting on Tuesday.\\n\r\n"
   " 2. Telephone Conference: ABC Corp. sales representative\r\n"
   "  called to discuss new printer. Promised to get us a demo by\r\n"
   "  Friday.\\n3. Henry Miller (Handsoff Insurance): Car was\r\n"
   "  totaled by tree. Is looking into a loaner car. 555-2323\r\n"
   "  (tel).\r\n"
   "END:VJOURNAL\r\n"
   "END:VCALENDAR\r\n"))

(test-equal "RFC5545 complex object 1: Check objects count"
  1
  (with-input-from-string
      %rfc5545-complex-object-1
    (lambda ()
      (let* ((fsm (make <stream-parser>
                    #:debug-mode? #t))
             (ctx (fsm-run! fsm (make <stream-context>)))
             (objects (stream-context-objects ctx)))
        (length objects)))))

(test-assert "RFC5545 complex object 1: Validate object"
  (with-input-from-string
      %rfc5545-complex-object-1
    (lambda ()
      (let* ((fsm (make <stream-parser>
                    #:debug-mode? #t))
             (ctx (fsm-run! fsm (make <stream-context>)))
             (objects (stream-context-objects ctx)))
        (ics-object? (car objects))))))

(test-equal "RFC5545 complex object 1: Check PRODID"
  "-//xyz Corp//NONSGML PDA Calendar Version 1.0//EN"
  (with-input-from-string
      %rfc5545-complex-object-1
    (lambda ()
      (let* ((fsm (make <stream-parser>
                    #:debug-mode? #t))
             (ctx (fsm-run! fsm (make <stream-context>)))
             (objects (stream-context-objects ctx)))
        (ics-property-value (ics-object-property-ref (car objects)
                                                     "PRODID"))))))

(test-equal "RFC5545 complex object 1: Check VERSION"
  "2.0"
  (with-input-from-string
      %rfc5545-complex-object-1
    (lambda ()
      (let* ((fsm (make <stream-parser>
                    #:debug-mode? #t))
             (ctx (fsm-run! fsm (make <stream-context>)))
             (objects (stream-context-objects ctx)))
        (ics-property-value (ics-object-property-ref (car objects)
                                                     "VERSION"))))))

(test-equal "RFC5545 complex object 1: Check VEVENT properties count"
  9
  (with-input-from-string
      %rfc5545-complex-object-1
    (lambda ()
      (let* ((fsm (make <stream-parser>
                    #:debug-mode? #t))
             (ctx (fsm-run! fsm (make <stream-context>)))
             (object    (car (stream-context-objects ctx)))
             (component (car (ics-object-components object))))
        (length (ics-object-properties component))))))

(test-assert "RFC5545 complex object 1: Check VEVENT: DTSTART"
  (with-input-from-string
      %rfc5545-complex-object-1
    (lambda ()
      (let* ((fsm       (make <stream-parser>
                          #:debug-mode? #t))
             (ctx       (fsm-run! fsm (make <stream-context>)))
             (object    (car (stream-context-objects ctx)))
             (component (car (ics-object-components object))))
        (ics-object-property-ref component "DTSTART")))))

(test-equal "RFC5545 complex object 1: Check VEVENT: DTSTART value"
  "19960918T143000Z"
  (with-input-from-string
      %rfc5545-complex-object-1
    (lambda ()
      (let* ((fsm       (make <stream-parser>
                          #:debug-mode? #t))
             (ctx       (fsm-run! fsm (make <stream-context>)))
             (object    (car (stream-context-objects ctx)))
             (component (car (ics-object-components object))))
        (ics-property-value (ics-object-property-ref component "DTSTART"))))))

(test-equal "RFC5545 complex object 1: Check VEVENT: DTSTART type"
  'DATE-TIME
  (with-input-from-string
      %rfc5545-complex-object-1
    (lambda ()
      (let* ((fsm       (make <stream-parser>
                          #:debug-mode? #t))
             (ctx       (fsm-run! fsm (make <stream-context>
                                        #:parse-types? #t)))
             (object    (car (stream-context-objects ctx)))
             (component (car (ics-object-components object))))
        (ics-property-type (ics-object-property-ref component "DTSTART"))))))

(test-equal "RFC5545 VTODO: CATEGORIES: Check value"
  '("FAMILY" "FINANCE")
  (with-input-from-string
      %rfc5545-vtodo-example-1
    (lambda ()
      (let* ((fsm       (make <stream-parser>
                          #:debug-mode? #t))
             (ctx       (fsm-run! fsm (make <stream-context>
                                        #:parse-types? #t)))
             (object    (car (stream-context-objects ctx)))
             (component (car (ics-object-components object))))
        (ics-property-value (ics-object-property-ref component "CATEGORIES"))))))

(test-equal "RFC5545 VJOURNAL: DESCRIPTION: Check value"
  (string-append
   "1. Staff meeting: Participants include Joe,"
   " Lisa, and Bob. Aurora project plans were reviewed."
   " There is currently no budget reserves for this project."
   " Lisa will escalate to management. Next meeting on Tuesday.\n"
   "2. Telephone Conference: ABC Corp. sales representative"
   " called to discuss new printer. Promised to get us a demo by"
   " Friday.\n3. Henry Miller (Handsoff Insurance): Car was"
   " totaled by tree. Is looking into a loaner car. 555-2323"
   " (tel).")
  (with-input-from-string
      %rfc5545-vjournal-example-1
    (lambda ()
      (let* ((fsm       (make <stream-parser>
                          #:debug-mode? #t))
             (ctx       (fsm-run! fsm (make <stream-context>
                                        #:parse-types? #t)))
             (object    (car (stream-context-objects ctx)))
             (component (car (ics-object-components object))))
        (ics-property-value (ics-object-property-ref component "DESCRIPTION"))))))



(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; stream-parser.scm ends here.
