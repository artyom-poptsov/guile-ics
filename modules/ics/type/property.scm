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
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ics type content)
  #:export (<ics-property>
            ics-property-name
            ics-property-value
            ics-property-type
            ics-property-parameters
            ics-property-parameter-ref
            ics-property->typed-property
            ics-property->ics-property:boolean
            ics-property->ics-property:cal-address
            ics-property->ics-property:date
            ics-property->ics-property:date-time
            ics-property->ics-property:duration
            ics-property->ics-property:float
            ics-property->ics-property:integer
            ics-property->ics-property:period
            non-standard-property-name?
            ics-property->string))


;;; Generic property (RFC5545, 3.3)

(define-class <ics-property> (<ics-content>)
  ;; symbol || #f
  ;;
  ;; iCalendar type name as described in RFC5545, 3.2.20.
  (type
   #:accessor     %ics-property-type
   #:init-value   #f
   #:init-keyword #:type)

  ;; string
  ;;
  ;; iCalendar type format as described in RFC5545, 3.2.8.
  ;;
  ;; List of registered format types can be found in
  ;; <http://www.iana.org/assignments/media-types/>
  (format-type
   #:accessor     ics-property-format-type
   #:init-value   #f
   #:init-keyword #:format-type)

  (value      #:accessor     ics-property-value
              #:init-value   #f
              #:init-keyword #:value)
  ;; alist
  (parameters #:accessor     ics-property-parameters
              #:init-value   #f
              #:init-keyword #:parameters))

(define (object-address->string object)
  (number->string (object-address object) 16))

(define-generic display)
(define-generic write)

(define-method (display (property <ics-property>) (port <port>))
  (format port "#<ics-property name: ~a type: ~a ~a>"
          (ics-property-name property)
          (ics-property-type property)
          (object-address->string property)))

(define-method (write (property <ics-property>) (port <port>))
  (next-method)
  (display property port))

(define-method (display (property <ics-property>))
  (next-method)
  (display property (current-output-port)))

(define-method (write (property <ics-property>))
  (next-method)
  (display property (current-output-port)))


(define ics-property-name ics-content-name)


;;; BINARY (RFC5545, 3.3.1)

(define-class <ics-property:binary> (<ics-property>)
  ;; symbol
  ;;
  ;; Either 8BIT (RFC2045) or BASE64 (RFC4648).
  (encoding
   #:accessor   ics-property-binary-encoding
   #:init-value #f
   #:init-keyword #:encoding))

(define-method (initialize (property <ics-property:binary>) initargs)
  (next-method)
  (slot-set! property 'ics-property-type 'BINARY))

(define-method (ics-property->ics-property:binary
                (property <ics-property>))
  (make <ics-property:binary>
    #:name        (ics-property-name          property)
    #:parameters  (ics-property-parameters    property)
    #:format-type (ics-property-parameter-ref property "FMTTYPE")
    #:encoding    (ics-property-parameter-ref property "ENCODING")
    #:value       (ics-property-value         property)))

;;; BOOLEAN (RFC5545, 3.3.2)

(define-class <ics-property:boolean> (<ics-property>))

(define-method (initialize (property <ics-property:boolean>))
  (next-method)
  (slot-set! property 'ics-property-type 'BOOLEAN))

(define-method (display (property <ics-property:boolean>) (port <port>))
  (format port "#<ics-property:boolean ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:boolean>) (port <port>))
  (display property port))

(define-method (ics-property->ics-property:boolean
                (property <ics-property>))
  (let ((value (ics-property-value property)))
    (make <ics-property:boolean>
      #:name  (ics-property-name property)
      #:value (cond
               ;; Boolean values are case-insensitive.
               ((string-ci=? value "TRUE")  #t)
               ((string-ci=? value "FALSE") #f)
               (else (error "Unknown property value (expected BOOLEAN)"
                            value))))))

;; (define-method (ics-data-boolean->ics-property
;;                 (property <ics-data-boolean>))
;;   (make <ics-property>
;;     ;; Although the RFC states that boolean values are
;;     ;; case-insensitive, we're using uppercase spelling
;;     ;; for unification sake.
;;     #:value (if (ics-data-value ics-data-boolean)
;;                 "TRUE"
;;                 "FALSE")
;;     #:parameters `(("VALUE" . "BOOLEAN"))))


;;; CAL-ADDRESS (RFC5545, 3.3.3)

(define-class <ics-property:cal-address> (<ics-property>))

(define-method (initialize (ics-property <ics-property:cal-address>))
  (next-method)
  (slot-set! ics-property 'ics-prperty-type 'CAL-ADDRESS))

(define-method (display (property <ics-property:cal-address>) (port <port>))
  (format port "#<ics-property:cal-address ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:boolean>) (port <port>))
  (display property port))

(define-method (ics-property->ics-property:cal-address
                (property <ics-property>))
  (make <ics-property:cal-address>
    #:name  (ics-property-name property)
    #:value (ics-property-value property)))

;; (define-method (ics-data-cal-address->ics-property
;;                 (ics-property  <ics-data-cal-address>))
;;   (make <ics-property>
;;     #:value (ics-property-value ics-data-cal-address)))


;;; DATE (RFC5545, 3.3.4)

(define-class <ics-property:date> (<ics-property>))

(define-method (initialize (property <ics-property:date>))
  (next-method)
  (slot-set! property 'ics-property-type 'DATE))

;; Printers

(define-method (display (property <ics-property:date>) (port <port>))
  (format port "#<ics-property:date ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:date>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:date>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:date>))
  (display property (current-output-port)))

;; Converters

(define-method (ics-property->ics-property:date
                (property <ics-property>))
  (let ((value (ics-property-value property)))
    (make <ics-property:date>
      #:name  (ics-property-name property)
      #:value (strptime "%Y%m%dT%H%M%S%Z" value))))


;;; DATE-TIME (RFC5545, 3.3.5)

(define-class <ics-property:date-time> (<ics-property>)
  (tzid #:accessor ics-property-date-time-tzid
        #:init-value #f
        #:init-keyword #:tzid))

(define-method (initialize (property <ics-property:date-time>))
  (next-method)
  (slot-set! property 'ics-property-type 'DATE-TIME))

;; Printers

(define-method (display (property <ics-property:date-time>) (port <port>))
  (format port "#<ics-property:date-time ~a: ~a ~a>"
          (ics-property-name property)
          (strftime "%FT%TZ" (car (ics-property-value property)))
          (object-address->string property)))

(define-method (write (property <ics-property:date-time>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:date-time>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:date-time>))
  (display property (current-output-port)))

;; Converters

(define-method (ics-property->ics-property:date-time
                (property <ics-property>))
  (make <ics-property:date-time>
    #:name  (ics-property-name property)
    #:value (strptime "%Y%m%dT%H%M%S%Z" (ics-property-value property))))


;;; DURATION (RFC5545, 3.3.6)

(define-class <ics-property:duration> (<ics-property>))

(define-method (initialize (property <ics-property:duration>))
  (next-method)
  (slot-set! property 'ics-property-type 'DURATION))

;; Printers

(define-method (display (property <ics-property:duration>) (port <port>))
  (format port "#<ics-property:duration ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:duration>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:duration>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:duration>))
  (display property (current-output-port)))

;; Converters

(define-method (ics-property->ics-property:duration
                (property <ics-property>))
  (make <ics-property:duration>
    #:name  (ics-property-name property)
    #:value (ics-property-value property)))


;;; FLOAT (RFC5545, 3.3.7)

(define-class <ics-property:float> (<ics-property>))

(define-method (initialize (property <ics-property:float>))
  (next-method)
  (slot-set! property 'ics-property-type 'FLOAT))

;; Printers

(define-method (display (property <ics-property:float>) (port <port>))
  (format port "#<ics-property:float ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:float>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:float>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:float>))
  (display property (current-output-port)))

;; Converters

(define-method (ics-property->ics-property:float
                (property <ics-property>))
  (make <ics-property:float>
    #:name  (ics-property-name property)
    #:value (string->number (ics-property-value property))))


;;; INTEGER (RFC5545, 3.3.8)

(define-class <ics-property:integer> (<ics-property>))

(define-method (initialize (property <ics-property:integer>))
  (next-method)
  (slot-set! property 'ics-property-type 'INTEGER))

;; Printers

(define-method (display (property <ics-property:integer>) (port <port>))
  (format port "#<ics-property:integer ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:integer>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:integer>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:integer>))
  (display property (current-output-port)))

;; Converters

(define-method (ics-property->ics-property:integer
                (property <ics-property>))
  (make <ics-property:integer>
    #:name  (ics-property-name property)
    #:value (string->number (ics-property-value property))))


;;; PERIOD (RFC5545, 3.3.9)

(define-class <ics-property:period> (<ics-property>))

(define-method (initialize (property <ics-property:period>))
  (next-method)
  (slot-set! property 'ics-property-type 'PERIOD))

;; Printers

(define-method (display (property <ics-property:period>) (port <port>))
  (format port "#<ics-property:period ~a: ~a/~a ~a>"
          (ics-property-name property)
          (car (ics-property-value property))
          (cadr (ics-property-value property))
          (object-address->string property)))

(define-method (write (property <ics-property:period>) (port <port>))
  (display property port))

(define-method (display (property <ics-property:period>))
  (display property (current-output-port)))

(define-method (write (property <ics-property:period>))
  (display property (current-output-port)))

;; Converters

(define-method (ics-property->ics-property:period
                (property <ics-property>))
  (make <ics-property:period>
    #:name  (ics-property-name property)
    #:value (string-split (ics-property-value property) #\:)))


;;; RECUR (RFC5545, 3.3.10)
;; TODO:

;; (define-class <ics-property:recur> (<ics-property>))

;; (define-method (initialize (property <ics-property:recur>))
;;   (next-method)
;;   (slot-set! property 'ics-property-type 'RECUR))

;; (define-method (ics-property->ics-property:recur
;;                 (property <ics-property>))
;;   (make <ics-property:recur>
;;     #:value (ics-property-value property)))


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

;; RFC5545, 3.8.8.2: Non-Standard Properties.
(define (non-standard-property-name? name)
  (regexp-match? (string-match "X-.*" name)))

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
    (PERIOD      . ,ics-property->ics-property:period)))

    ;; TODO:
    ;; (RECUR       . ,ics-property->ics-property:recur)
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


;;;

(define-method (ics-property-parameter-ref (ics-property <ics-property>)
                                            (name <symbol>))
  "Get a iCalendar property parameter by a NAME, return a property
parameter value, or return #f if no parameter found."
  (assoc-ref name (ics-property-parameters ics-property)))

(define-method (ics-property->string (ics-property <ics-property>))
  "Convert an ICAL-PROPERTY to a iCalendar string, return the string."
  (define (parameters->string parameters)
    (string-join (map (lambda (parameter)
                        (format #f "~a=~a" (car parameter)
                                (cdr parameter)))
                      parameters)
                 ";"))
  (let ((parameters (ics-property-parameters ics-property))
        (name       (ics-property-name ics-property))
        (value      (ics-property-value ics-property)))
    (if parameters
        (format #f "~a;~a:~a" name (parameters->string parameters) value)
        (string-append name ":" value))))

;;; property.scm ends here.
