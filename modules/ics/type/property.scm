;;; property.scm -- iCalendar property definition.

;; Copyright (C) 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains definition of <ics-property> class; the class
;; represents an individual attribute of an iCalendar object.
;;
;; See the Texinfo documentation for details.


;;; Code:

(define-module (ics type property)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ics type property property)
  #:use-module (ics type property binary)
  #:use-module (ics type property boolean)
  #:use-module (ics type property cal-address)
  #:use-module (ics type property date)
  #:use-module (ics type property date-time)
  #:use-module (ics type property duration)
  #:use-module (ics type property float)
  #:use-module (ics type property integer)
  #:use-module (ics type property period)
  #:use-module (ics type property recur)
  #:export (ics-property->typed-property))

;; This macro is taken from Guile-JSON.
(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (ics type property property)
                   (ics type property binary)
                   (ics type property boolean)
                   (ics type property cal-address)
                   (ics type property date)
                   (ics type property date-time)
                   (ics type property duration)
                   (ics type property float)
                   (ics type property integer)
                   (ics type property period)
                   (ics type property recur))



;;; TEXT (RFC5545, 3.3.10)
;; TODO:

;; (define-class <ics-property:text> (<ics-property>))

;; (define-method (initialize (property <ics-property:text>))
;;   (next-method)
;;   (slot-set! property 'ics-property-type 'TEXT))

;; (define-method (ics-property->ics-property:text
;;                 (property <ics-property>))
;;   (make <ics-property:text>
;;     #:value (ics-property-value property)))

;;; TIME (RFC5545, 3.3.1)
;; TODO:

;; (define-class <ics-property:time> (<ics-property>))

;; (define-method (initialize (property <ics-property:time>))
;;   (next-method)
;;   (slot-set! property 'ics-property-type 'TIME))

;; (define-method (ics-property->ics-property:time
;;                 (property <ics-property>))
;;   (make <ics-property:time>
;;     #:value (ics-property-value property)))

;;; TIME (RFC5545, 3.3.1)

;; (define-class <ics-property:time> (<ics-property>))

;; (define-method (initialize (property <ics-property:time>))
;;   (next-method)
;;   (slot-set! property 'ics-property-type 'TIME))

;; (define-method (ics-property->ics-property:time
;;                 (property <ics-property>))
;;   (make <ics-property:time>
;;     #:value (ics-property-value property)))

;;; Converters

(define-macro (case* pred key . clauses)
  `(cond
    ,@(map
       (lambda (clause)
         (let ((datum (car clause))
               (exp   (cadr clause)))
           (cond
            ((and (not (list? datum)) (not (eq? datum 'else)))
             (error "Syntax error: expected a list" datum))
            ((eq? datum 'else)
             `(else ,exp))
            ((= (length datum) 1)
             `((,pred ,key ,(car datum)) ,exp))
            (else
             `((or ,@(map (lambda (o) `(,pred ,key ,o))
                          datum)) ,exp)))))
       clauses)))

(define-method (ics-property-type (property <ics-property>))
  (or (%ics-property-type property)
      (let ((name  (ics-property-name property))
            (value (ics-property-parameter-ref property 'VALUE)))
        (if value

            ;; If the property value type is set explicitly, return the
            ;; type.
            (string->symbol value)

            (case* string=? name
              (("DSTART" "COMPLETED" "DTEND" "DATE" "DUE" "RECURRENCE-ID"
                "EXDATE" "RDATE" "CREATED" "DTSTAMP" "LAST-MODIFIED")
               'DATE-TIME)
              (("GEO")
               'FLOAT)
              (("DURATION" "TRIGGER")
               'DURATION)
              (("FREEBUSY")
               'PERIOD)
              (("RRULE")
               'RECUR)
              (("CLASS" "COMMENT" "DESCRIPTION" "CATEGORIES" "LOCATION"
                "RESOURCES" "STATUS" "SUMMARY" "TRANSP" "TZID" "TZNAME"
                "CONTACT" "RELATED-TO" "UID" "ACTION" "REQUEST-STATUS")
               'TEXT)
              (("TZOFFSETFROM" "TZOFFSETTO")
               'UTC-OFFSET)
              (("TZURL" "URL")
               'URI)
              (("ATTENDEE" "ORGANIZER")
               'CAL-ADDRESS)
              (("PERCENT-COMPLETE" "PRIORITY" "REPEAT" "SEQUENCE")
               'INTEGER)
              (else
               ;; Non-standard properties and IANA properties are
               ;; of type TEXT by default (see RFC5545, 3.8.8.)
               'TEXT))))))

(define %converters
  `((BINARY      . ,ics-property->ics-property:binary)
    (BOOLEAN     . ,ics-property->ics-property:boolean)
    (CAL-ADDRESS . ,ics-property->ics-property:cal-address)
    (DATE        . ,ics-property->ics-property:date)
    (DATE-TIME   . ,ics-property->ics-property:date-time)
    (DURATION    . ,ics-property->ics-property:duration)
    (FLOAT       . ,ics-property->ics-property:float)
    (INTEGER     . ,ics-property->ics-property:integer)
    (PERIOD      . ,ics-property->ics-property:period)
    (RECUR       . ,ics-property->ics-property:recur)))

    ;; TODO:

    ;; (TEXT        . ,ics-property->ics-property:text)
    ;; (UTC-OFFSET  . ,ics-property->ics-property:utc-offset)
    ;; (URI         . ,ics-property->ics-property:uri)))

(define-method (ics-property->typed-property (property <ics-property>))
  (let* ((type (ics-property-type property))
         (converter (assoc-ref %converters type)))
    (unless converter
      (error 'guile-ics-error
             "Converter for the property type is not defined yet"
             type))
    (converter property)))

;;; property.scm ends here.
