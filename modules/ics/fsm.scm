;;; fsm.scm -- Finite state machine for the iCalendar parser.

;; Copyright (C) 2016, 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Finite state machine (FSM) for the iCalendar parser.


;;; Code:

(define-module (ics fsm)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (ics common)
  #:use-module (ics type object)
  #:use-module (ics type property)
  #:use-module (ics parser)
  #:export (ics-token-begin?
            ics-token-end?
            ics-calendar-object?
            ;; Tokens
            %ics-icalendar-object
            %ics-token-begin
            %ics-token-end
            ;; FSM
            fsm-read-property
            fsm-skip-property
            fsm-read-ical-object
            fsm-read-ical-stream))


;;; Constants.

;; See <https://tools.ietf.org/html/rfc5545#section-3.6>.
(define %ics-token-begin "BEGIN")
(define %ics-token-end "END")

;; See <https://tools.ietf.org/html/rfc5545#section-3.4>.
(define %ics-icalendar-object "VCALENDAR")


;;; Predicates.

(define (ics-token-begin? x)
  "Check if an X is the beginning of an IANA token."
  (string=? x %ics-token-begin))

(define (ics-token-end? x)
  "Check if an X is the ending of an IANA token."
  (string=? x %ics-token-end))

(define (ics-calendar-object? x)
  "Check if X is a iCalendar object."
  (string=? x %ics-icalendar-object))

;;; Helper procedures.

(define (string-append-char str ch)
  "Return a newly allocated string whose characters form the
concatenation of the given string STR and char CH."
  (string-append str (string ch)))


;;; Finite State Machine.
;;
;;        .------------. .------------------------------------.
;;        :            : :             .-----------.          :
;;        :            : :             V           :          :
;;        :            : :          [fsm-read-property]<-.    :
;;        :            : :             :                 :    :
;; START  :            : :             :  .--------------+    :
;; :      V            : V             V  V              :    :
;; '->[fsm-read-ical-stream]---+----->[fsm-read-ical-object]->:
;;        A            :       :                         A    :
;;        :            :       :       .-----------.     :    :
;;        :            :       :       V           :     :    :
;;        :            V       '--->[fsm-read-property]  :    :
;;        :           END                    :           :    :
;;        '----------------------------------'           :    :
;;                                     .-----------.     :    :
;;                                     V           :     :    :
;;                             .--->[fsm-skip-property]--'    :
;;                             :                              :
;;                             '------------------------------'

(define (fsm-read-property parser)
  ;;
  ;;       .---------------------------.
  ;;       :                           :
  ;;       :     .-----------.         :
  ;;       V     :           V         :
  ;; START----->[read-property]----.   :
  ;;                               :   :
  ;;       .-----------------------'   :
  ;;       :                           :
  ;;       +--->[read-escaped-char]----'
  ;;       :
  ;;       '--->[handle-result]----->END
  ;;
  (define (read-escaped-char buffer result)
    (let ((ch (parser-read-char parser)))
      (case ch
        ((*eof-object*)
         (debug-fsm-error "fsm-read-property")
         (error "Could not read escaped char"))
        ;; RFC 5545, 3.3.11:
        ;;   ESCAPED-CHAR = ("\\" / "\;" / "\," / "\N" / "\n")
        ((#\\ #\; #\,)
         (read-property (string-append-char buffer ch) result))
        ((#\N #\n)
         (read-property (string-append buffer "\n") result))
        (else
         (debug-fsm-error "fsm-read-property")
         (error "Unknown escaped character.")))))

  (define (handle-result buffer result)
    (if (null? result)
        buffer
        (reverse (if (string-null? buffer)
                     result
                     (cons buffer result)))))

  (define (read-property buffer result)
    (debug-fsm "fsm-read-property" "read-property")
    (let ((ch (parser-read-char parser)))
      (if (eof-object? ch)
          buffer
          (case ch
            ((#\cr)
             (read-property buffer result))
            ((#\linefeed)
             (let ((next-ch (parser-read-char parser)))
               ;; Lines longer than 75 octets should be split into
               ;; multiple line representations; in this case a single
               ;; space character immediately follows CRLF (see RFC
               ;; 5545, section 3.1)
               (if (equal? next-ch #\space)
                   (read-property buffer result)
                   (begin
                     (unless (eof-object? next-ch)
                       (parser-unread-char parser next-ch))
                     (debug-fsm "fsm-read-property" "handle-result")
                     (handle-result buffer result)))))
            ((#\\)
             (debug-fsm "fsm-read-property" "read-escaped-char")
             (read-escaped-char buffer result))
            ((#\,)
             (read-property "" (cons buffer result)))
            ((*eof-object*)
             (debug-fsm "fsm-read-property" "handle-result")
             (handle-result buffer result))
            (else
             (read-property (string-append-char buffer ch)
                            result))))))
  (debug-fsm-transition "fsm-read-property")
  (read-property "" '()))

(define (fsm-skip-property parser)
  (debug-fsm-transition "fsm-skip-property")
  (let ((ch (parser-read-char parser)))
    (unless (or (eof-object? ch) (equal? ch #\linefeed))
      (fsm-skip-property parser))))

(define (fsm-read-ical-object parser object-name icalprops component)
  (define (read-component)
    (debug-fsm "fsm-read-ical-object" "read-component~%")
    (let* ((key (fsm-read-property parser))
           (val (fsm-read-ical-object parser key '() '())))
      (debug-fsm "fsm-read-ical-object" "read-component: key: ~a; val: ~a~%"
                 key val)
      (fsm-read-ical-object parser
                            object-name
                            icalprops
                            (cons val component))))
  (define (parse-name name)
    (map (lambda (e)
           (debug-fsm "fsm-read-ical-object" "read-property: e: ~a~%"
                      e)
           (let ((key&value (string-split e #\=)))
             (debug-fsm "fsm-read-ical-object" "read-property: key&value: ~a~%"
                        key&value)
             (if (> (length key&value) 1)
                 (cons (string->symbol (car key&value))
                       (cadr key&value))
                 (list (car key&value)))))
         (string-split name #\;)))

  (define (read-property name)
    (debug-fsm "fsm-read-ical-object" "read-property: NAME: ~a~%"
               name)
    (let* ((parsed (parse-name name))
           (d      (debug-fsm "fsm-read-ical-object" "read-property: parsed: ~a~%"
                              parsed))
           (key (caar parsed))
           (val (fsm-read-property parser)))
      (debug-fsm "fsm-read-ical-object" "read-property: key: ~a; val: ~a~%"
                 key val)
      (let ((ical-property (make <ical-property>
                             #:name  key
                             #:value val
                             #:parameters (cdr parsed))))
        (fsm-read-ical-object parser
                              object-name
                              (cons ical-property icalprops)
                              component))))

  (define (read-object-in-quotes buffer)
    (let ((ch (parser-read-char parser)))
      (when (eof-object? ch)
        (error "Unexpected EOF inside quotes."))
      (case ch
        ((#\")
         (read-object buffer))
        (else
         (read-object-in-quotes (string-append-char buffer ch))))))

  (define (read-object buffer)
    (let ((ch (parser-read-char parser)))
      (if (eof-object? ch)
          (make <ical-object>
            #:name       object-name
            #:properties icalprops
            #:components component)
          (case ch
            ((#\")
             (read-object-in-quotes buffer))
            ((#\:)
             (cond
              ((ics-token-begin? buffer)
               (read-component))
              ((ics-token-end? buffer)
               (fsm-skip-property parser)
               (make <ical-object>
                 #:name       object-name
                 #:components component
                 #:properties icalprops))
              (else
               (read-property buffer))))
            ((#\linefeed)
             (fsm-read-ical-object parser object-name icalprops component))
            (else
             (read-object (string-append-char buffer ch)))))))
  (debug-fsm-transition "fsm-read-ical-object")
  (read-object ""))


(define (fsm-read-ical-stream parser result)
  (define (read-component-name)
    (let ((name (fsm-read-property parser)))
      (debug-fsm "fsm-read-component-name" "NAME: ~a~%" name)
      (if (ics-calendar-object? name)
          (begin
            (debug-fsm "fsm-read-ical-stream" "RESULT: ~a~%" result)
            (let ((result (cons (fsm-read-ical-object parser
                                                      %ics-icalendar-object
                                                      '() '())
                                result)))
              (debug-fsm "fsm-read-ical-stream" "RESULT: ~a~%" result)
              (fsm-read-ical-stream parser result)))
          (fsm-read-ical-stream parser result))))
  (define (read-ical-stream buffer)
    (let ((ch (parser-read-char parser)))
      (if (eof-object? ch)
          result
          (case ch
            ((#\:)
             (debug-fsm "fsm-read-ical-stream" "BUFFER: ~a~%" buffer)
             (cond
              ((ics-token-begin? buffer)
               (read-component-name))
              (else
               (debug-fsm-transition "fsm-read-ical-stream")
               (fsm-read-ical-stream parser result))))
            (else
             (read-ical-stream (string-append-char buffer ch)))))))
  (debug-fsm-transition "fsm-read-ical-stream")
  (read-ical-stream ""))

;;; fsm.scm ends here.
