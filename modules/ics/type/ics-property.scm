;;; ical-property.scm -- iCalendar property definition.

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

;; This module contains definition of <ical-property> class; the class
;; represents an individual attribute of an iCalendar object.
;;
;; See the Texinfo documentation for details.


;;; Code:

(define-module (ics type ics-property)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ics type ics-content)
  #:export (<ical-property>
            ical-property-name
            ical-property-value
            ical-property-type
            ical-property-parameters
            ical-property-parameter-ref
            ical-property->typed-property
            ical-property->ical-property:boolean
            ical-property->ical-property:cal-address
            ical-property->ical-property:date
            ical-property->ical-property:date-time
            ical-property->ical-property:duration
            ical-property->ical-property:float
            ical-property->ical-property:integer
            ical-property->ical-property:period
            non-standard-property-name?
            ical-property->string))


;;; Generic property (RFC5545, 3.3)

(define-class <ical-property> (<ical-content>)
  ;; symbol || #f
  ;;
  ;; iCalendar type name as described in RFC5545, 3.2.20.
  (type
   #:accessor     %ical-property-type
   #:init-value   #f
   #:init-keyword #:type)

  ;; string
  ;;
  ;; iCalendar type format as described in RFC5545, 3.2.8.
  ;;
  ;; List of registered format types can be found in
  ;; <http://www.iana.org/assignments/media-types/>
  (format-type
   #:accessor     ical-property-format-type
   #:init-value   #f
   #:init-keyword #:format-type)

  (value      #:accessor     ical-property-value
              #:init-value   #f
              #:init-keyword #:value)
  ;; alist
  (parameters #:accessor     ical-property-parameters
              #:init-value   #f
              #:init-keyword #:parameters))

(define (object-address->string object)
  (number->string (object-address object) 16))

(define-generic display)
(define-generic write)

(define-method (display (property <ical-property>) (port <port>))
  (format port "#<ical-property name: ~a type: ~a ~a>"
          (ical-property-name property)
          (ical-property-type property)
          (object-address->string property)))

(define-method (write (property <ical-property>) (port <port>))
  (next-method)
  (display property port))

(define-method (display (property <ical-property>))
  (next-method)
  (display property (current-output-port)))

(define-method (write (property <ical-property>))
  (next-method)
  (display property (current-output-port)))


(define ical-property-name ical-content-name)


;;; BINARY (RFC5545, 3.3.1)

(define-class <ical-property:binary> (<ical-property>)
  ;; symbol
  ;;
  ;; Either 8BIT (RFC2045) or BASE64 (RFC4648).
  (encoding
   #:accessor   ical-property-binary-encoding
   #:init-value #f
   #:init-keyword #:encoding))

(define-method (initialize (property <ical-property:binary>) initargs)
  (next-method)
  (slot-set! property 'ical-property-type 'BINARY))

(define-method (ical-property->ical-property:binary
                (property <ical-property>))
  (make <ical-property:binary>
    #:name        (ical-property-name          property)
    #:parameters  (ical-property-parameters    property)
    #:format-type (ical-property-parameter-ref property "FMTTYPE")
    #:encoding    (ical-property-parameter-ref property "ENCODING")
    #:value       (ical-property-value         property)))

;;; BOOLEAN (RFC5545, 3.3.2)

(define-class <ical-property:boolean> (<ical-property>))

(define-method (initialize (property <ical-property:boolean>))
  (next-method)
  (slot-set! property 'ical-property-type 'BOOLEAN))

(define-method (display (property <ical-property:boolean>) (port <port>))
  (format port "#<ical-property:boolean ~a: ~a ~a>"
          (ical-property-name property)
          (ical-property-value property)
          (object-address->string property)))

(define-method (write (property <ical-property:boolean>) (port <port>))
  (display property port))

(define-method (ical-property->ical-property:boolean
                (property <ical-property>))
  (let ((value (ical-property-value property)))
    (make <ical-property:boolean>
      #:name  (ical-property-name property)
      #:value (cond
               ;; Boolean values are case-insensitive.
               ((string-ci=? value "TRUE")  #t)
               ((string-ci=? value "FALSE") #f)
               (else (error "Unknown property value (expected BOOLEAN)"
                            value))))))

;; (define-method (ical-data-boolean->ical-property
;;                 (property <ical-data-boolean>))
;;   (make <ical-property>
;;     ;; Although the RFC states that boolean values are
;;     ;; case-insensitive, we're using uppercase spelling
;;     ;; for unification sake.
;;     #:value (if (ical-data-value ical-data-boolean)
;;                 "TRUE"
;;                 "FALSE")
;;     #:parameters `(("VALUE" . "BOOLEAN"))))


;;; CAL-ADDRESS (RFC5545, 3.3.3)

(define-class <ical-property:cal-address> (<ical-property>))

(define-method (initialize (ical-property <ical-property:cal-address>))
  (next-method)
  (slot-set! ical-property 'ical-prperty-type 'CAL-ADDRESS))

(define-method (display (property <ical-property:cal-address>) (port <port>))
  (format port "#<ical-property:cal-address ~a: ~a ~a>"
          (ical-property-name property)
          (ical-property-value property)
          (object-address->string property)))

(define-method (write (property <ical-property:boolean>) (port <port>))
  (display property port))

(define-method (ical-property->ical-property:cal-address
                (property <ical-property>))
  (make <ical-property:cal-address>
    #:name  (ical-property-name property)
    #:value (ical-property-value property)))

;; (define-method (ical-data-cal-address->ical-property
;;                 (ical-property  <ical-data-cal-address>))
;;   (make <ical-property>
;;     #:value (ical-property-value ical-data-cal-address)))


;;; DATE (RFC5545, 3.3.4)

(define-class <ical-property:date> (<ical-property>))

(define-method (initialize (property <ical-property:date>))
  (next-method)
  (slot-set! property 'ical-property-type 'DATE))

;; Printers

(define-method (display (property <ical-property:date>) (port <port>))
  (format port "#<ical-property:date ~a: ~a ~a>"
          (ical-property-name property)
          (ical-property-value property)
          (object-address->string property)))

(define-method (write (property <ical-property:date>) (port <port>))
  (display property port))

(define-method (display (property <ical-property:date>))
  (display property (current-output-port)))

(define-method (write (property <ical-property:date>))
  (display property (current-output-port)))

;; Converters

(define-method (ical-property->ical-property:date
                (property <ical-property>))
  (let ((value (ical-property-value property)))
    (make <ical-property:date>
      #:name  (ical-property-name property)
      #:value (strptime "%Y%m%dT%H%M%S%Z" value))))


;;; DATE-TIME (RFC5545, 3.3.5)

(define-class <ical-property:date-time> (<ical-property>)
  (tzid #:accessor ical-property-date-time-tzid
        #:init-value #f
        #:init-keyword #:tzid))

(define-method (initialize (property <ical-property:date-time>))
  (next-method)
  (slot-set! property 'ical-property-type 'DATE-TIME))

;; Printers

(define-method (display (property <ical-property:date-time>) (port <port>))
  (format port "#<ical-property:date-time ~a: ~a ~a>"
          (ical-property-name property)
          (strftime "%FT%TZ" (car (ical-property-value property)))
          (object-address->string property)))

(define-method (write (property <ical-property:date-time>) (port <port>))
  (display property port))

(define-method (display (property <ical-property:date-time>))
  (display property (current-output-port)))

(define-method (write (property <ical-property:date-time>))
  (display property (current-output-port)))

;; Converters

(define-method (ical-property->ical-property:date-time
                (property <ical-property>))
  (make <ical-property:date-time>
    #:name  (ical-property-name property)
    #:value (strptime "%Y%m%dT%H%M%S%Z" (ical-property-value property))))


;;; DURATION (RFC5545, 3.3.6)

(define-class <ical-property:duration> (<ical-property>))

(define-method (initialize (property <ical-property:duration>))
  (next-method)
  (slot-set! property 'ical-property-type 'DURATION))

;; Printers

(define-method (display (property <ical-property:duration>) (port <port>))
  (format port "#<ical-property:duration ~a: ~a ~a>"
          (ical-property-name property)
          (ical-property-value property)
          (object-address->string property)))

(define-method (write (property <ical-property:duration>) (port <port>))
  (display property port))

(define-method (display (property <ical-property:duration>))
  (display property (current-output-port)))

(define-method (write (property <ical-property:duration>))
  (display property (current-output-port)))

;; Converters

(define-method (ical-property->ical-property:duration
                (property <ical-property>))
  (make <ical-property:duration>
    #:name  (ical-property-name property)
    #:value (ical-property-value property)))


;;; FLOAT (RFC5545, 3.3.7)

(define-class <ical-property:float> (<ical-property>))

(define-method (initialize (property <ical-property:float>))
  (next-method)
  (slot-set! property 'ical-property-type 'FLOAT))

;; Printers

(define-method (display (property <ical-property:float>) (port <port>))
  (format port "#<ical-property:float ~a: ~a ~a>"
          (ical-property-name property)
          (ical-property-value property)
          (object-address->string property)))

(define-method (write (property <ical-property:float>) (port <port>))
  (display property port))

(define-method (display (property <ical-property:float>))
  (display property (current-output-port)))

(define-method (write (property <ical-property:float>))
  (display property (current-output-port)))

;; Converters

(define-method (ical-property->ical-property:float
                (property <ical-property>))
  (make <ical-property:float>
    #:name  (ical-property-name property)
    #:value (string->number (ical-property-value property))))


;;; INTEGER (RFC5545, 3.3.8)

(define-class <ical-property:integer> (<ical-property>))

(define-method (initialize (property <ical-property:integer>))
  (next-method)
  (slot-set! property 'ical-property-type 'INTEGER))

;; Printers

(define-method (display (property <ical-property:integer>) (port <port>))
  (format port "#<ical-property:integer ~a: ~a ~a>"
          (ical-property-name property)
          (ical-property-value property)
          (object-address->string property)))

(define-method (write (property <ical-property:integer>) (port <port>))
  (display property port))

(define-method (display (property <ical-property:integer>))
  (display property (current-output-port)))

(define-method (write (property <ical-property:integer>))
  (display property (current-output-port)))

;; Converters

(define-method (ical-property->ical-property:integer
                (property <ical-property>))
  (make <ical-property:integer>
    #:name  (ical-property-name property)
    #:value (string->number (ical-property-value property))))


;;; PERIOD (RFC5545, 3.3.9)

(define-class <ical-property:period> (<ical-property>))

(define-method (initialize (property <ical-property:period>))
  (next-method)
  (slot-set! property 'ical-property-type 'PERIOD))

;; Printers

(define-method (display (property <ical-property:period>) (port <port>))
  (format port "#<ical-property:period ~a: ~a/~a ~a>"
          (ical-property-name property)
          (car (ical-property-value property))
          (cadr (ical-property-value property))
          (object-address->string property)))

(define-method (write (property <ical-property:period>) (port <port>))
  (display property port))

(define-method (display (property <ical-property:period>))
  (display property (current-output-port)))

(define-method (write (property <ical-property:period>))
  (display property (current-output-port)))

;; Converters

(define-method (ical-property->ical-property:period
                (property <ical-property>))
  (make <ical-property:period>
    #:name  (ical-property-name property)
    #:value (string-split (ical-property-value property) #\:)))


;;; RECUR (RFC5545, 3.3.10)
;; TODO:

;; (define-class <ical-property:recur> (<ical-property>))

;; (define-method (initialize (property <ical-property:recur>))
;;   (next-method)
;;   (slot-set! property 'ical-property-type 'RECUR))

;; (define-method (ical-property->ical-property:recur
;;                 (property <ical-property>))
;;   (make <ical-property:recur>
;;     #:value (ical-property-value property)))


;;; TEXT (RFC5545, 3.3.10)
;; TODO:

;; (define-class <ical-property:text> (<ical-property>))

;; (define-method (initialize (property <ical-property:text>))
;;   (next-method)
;;   (slot-set! property 'ical-property-type 'TEXT))

;; (define-method (ical-property->ical-property:text
;;                 (property <ical-property>))
;;   (make <ical-property:text>
;;     #:value (ical-property-value property)))

;;; TIME (RFC5545, 3.3.1)
;; TODO:

;; (define-class <ical-property:time> (<ical-property>))

;; (define-method (initialize (property <ical-property:time>))
;;   (next-method)
;;   (slot-set! property 'ical-property-type 'TIME))

;; (define-method (ical-property->ical-property:time
;;                 (property <ical-property>))
;;   (make <ical-property:time>
;;     #:value (ical-property-value property)))

;;; TIME (RFC5545, 3.3.1)

;; (define-class <ical-property:time> (<ical-property>))

;; (define-method (initialize (property <ical-property:time>))
;;   (next-method)
;;   (slot-set! property 'ical-property-type 'TIME))

;; (define-method (ical-property->ical-property:time
;;                 (property <ical-property>))
;;   (make <ical-property:time>
;;     #:value (ical-property-value property)))


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

;; RFC5545, 3.8.8.2: Non-Standard Properties.
(define (non-standard-property-name? name)
  (regexp-match? (string-match "X-.*" name)))

(define-method (ical-property-type (property <ical-property>))
  (or (%ical-property-type property)
      (let ((name  (ical-property-name property))
            (value (ical-property-parameter-ref property 'VALUE)))
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
  `((BINARY      . ,ical-property->ical-property:binary)
    (BOOLEAN     . ,ical-property->ical-property:boolean)
    (CAL-ADDRESS . ,ical-property->ical-property:cal-address)
    (DATE        . ,ical-property->ical-property:date)
    (DATE-TIME   . ,ical-property->ical-property:date-time)
    (DURATION    . ,ical-property->ical-property:duration)
    (FLOAT       . ,ical-property->ical-property:float)
    (INTEGER     . ,ical-property->ical-property:integer)
    (PERIOD      . ,ical-property->ical-property:period)))

    ;; TODO:
    ;; (RECUR       . ,ical-property->ical-property:recur)
    ;; (TEXT        . ,ical-property->ical-property:text)
    ;; (UTC-OFFSET  . ,ical-property->ical-property:utc-offset)
    ;; (URI         . ,ical-property->ical-property:uri)))

(define-method (ical-property->typed-property (property <ical-property>))
  (let* ((type (ical-property-type property))
         (converter (assoc-ref %converters type)))
    (unless converter
      (error 'guile-ics-error
             "Converter for the property type is not defined yet"
             type))
    (converter property)))


;;;

(define-method (ical-property-parameter-ref (ical-property <ical-property>)
                                            (name <symbol>))
  "Get a iCalendar property parameter by a NAME, return a property
parameter value, or return #f if no parameter found."
  (assoc-ref name (ical-property-parameters ical-property)))

(define-method (ical-property->string (ical-property <ical-property>))
  "Convert an ICAL-PROPERTY to a iCalendar string, return the string."
  (define (parameters->string parameters)
    (string-join (map (lambda (parameter)
                        (format #f "~a=~a" (car parameter)
                                (cdr parameter)))
                      parameters)
                 ";"))
  (let ((parameters (ical-property-parameters ical-property))
        (name       (ical-property-name ical-property))
        (value      (ical-property-value ical-property)))
    (if parameters
        (format #f "~a;~a:~a" name (parameters->string parameters) value)
        (string-append name ":" value))))

;;; ical-property.scm ends here.
