;;; period.scm -- iCalendar RECUR (RFC5545, 3.3.10) type.

;; Copyright (C) 2017-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (ics type recur)
  #:use-module (oop goops)
  #:use-module (ics common)
  #:use-module (ics type property)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:export     (<ics-property:recur>
                ics-property:recur?
                ics-property->ics-property:recur

                ics-property:recur-part-describe
                ics-property:recur-part-validate

                ;; Getters.
                ics-property:recur-freq
                ics-property:recur-until
                ics-property:recur-count
                ics-property:recur-interval
                ics-property:recur-by-second
                ics-property:recur-by-minute
                ics-property:recur-by-hour
                ics-property:recur-by-day
                ics-property:recur-by-month-day
                ics-property:recur-by-year-day
                ics-property:recur-by-week-no
                ics-property:recur-by-month
                ics-property:recur-by-set-pos
                ics-property:recur-wkst

                ;; RRULE validation procedures.
                rrule-part-freq-validate
                rrule-part-until-validate
                rrule-part-count-validate
                rrule-part-interval-validate
                rrule-part-bysecond-validate
                rrule-part-byminute-validate
                rrule-part-byhour-validate
                rrule-part-byday-validate
                rrule-part-bymonthday-validate
                rrule-part-bymonth-validate
                rrule-part-bysetpos-validate
                rrule-part-wkst-validate))



(define (rrule-error message . args)
  (apply throw 'guile-ics-rrule-error message args))



(define %rrule-freq-values
  (alist->hash-table
   '((SECONDLY
      . "Repeating event based on an interval of a second or more")
     (MINUTELY
      . "Repeating event based on an interval of a minute or more")
     (HOURLY
      . "Repeating event based on an interval of an hour or more")
     (DAILY
      . "Repeating event based on an interval of a day or more")
     (WEEKLY
      . "Repeating event based on an interval of a week or more")
     (MONTHLY
      . "Repeating event based on an interval of a month or more")
     (YEARLY
      . "Repeating event based on an interval of a year or more"))))

(define %rrule-freq-values/list
  (reverse (hash-fold (lambda (key value prior)
                        (cons key prior))
                      '()
                      %rrule-freq-values)))

(define (rrule-print-description title description)
  (format (current-error-port) ";;; ~a:~%" title)
  (format (current-error-port) ";;;~%")
  (for-each (lambda (line)
              (format (current-error-port)
                      ";;; ~a~%"
                      line))
            description))


;; Rule parts.

(define %rrule-part-freq-description
  (list
   "The FREQ rule part identifies the type of recurrence rule.  This"
   "rule part MUST be specified in the recurrence rule.  Valid values"
   "include SECONDLY, to specify repeating events based on an interval"
   "of a second or more; MINUTELY, to specify repeating events based"
   "on an interval of a minute or more; HOURLY, to specify repeating"
   "events based on an interval of an hour or more; DAILY, to specify"
   "repeating events based on an interval of a day or more; WEEKLY, to"
   "specify repeating events based on an interval of a week or more;"
   "MONTHLY, to specify repeating events based on an interval of a"
   "month or more; and YEARLY, to specify repeating events based on an"
   "interval of a year or more."))

(define* (rrule-part-freq-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "FREQ" %rrule-part-freq-description)))
  (cond
   ((symbol? value)
    (let ((description (hash-ref %rrule-freq-values value)))
      (unless description
        (describe)
        (rrule-error "FREQ value is not in the list of possible values"
                     value
                     %rrule-freq-values/list))))
   (else
    (describe)
    (rrule-error "FREQ must be a symbol" value))))

(define %rrule-part-until-description
  (list
   "The UNTIL rule part defines a DATE or DATE-TIME value that bounds"
   "the recurrence rule in an inclusive manner.  If the value"
   "specified by UNTIL is synchronized with the specified recurrence,"
   "this DATE or DATE-TIME becomes the last instance of the"
   "recurrence.  The value of the UNTIL rule part MUST have the same"
   "value type as the \"DTSTART\" property.  Furthermore, if the"
   "\"DTSTART\" property is specified as a date with local time, then"
   "the UNTIL rule part MUST also be specified as a date with local"
   "time.  If the \"DTSTART\" property is specified as a date with UTC"
   "time or a date with local time and time zone reference, then the"
   "UNTIL rule part MUST be specified as a date with UTC time.  In the"
   "case of the \"STANDARD\" and \"DAYLIGHT\" sub-components the UNTIL"
   "rule part MUST always be specified as a date with UTC time.  If"
   "specified as a DATE-TIME value, then it MUST be specified in a UTC"
   "time format.  If not present, and the COUNT rule part is also not"
   "present, the \"RRULE\" is considered to repeat forever."))

(define %date-fmt "%Y%m%d")
(define %date-time-fmt "%Y%m%dT%H%M%S%Z")

(define* (rrule-part-until-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "UNTIL" %rrule-part-until-description)))
  (cond
   ((string? value)
    (unless (or (catch #t
                  (lambda () (strptime %date-fmt value))
                  (const #f))
                (catch #t
                  (lambda () (strptime %date-time-fmt value))
                  (const #f)))
      (describe)
      (rrule-error
       "UNTIL value must be a string in the date or date-time format"
       %date-fmt
       %date-time-fmt
       value)))
   (else
    (describe)
    (rrule-error "UNTIL value must be a string" value))))

(define %rrule-part-count-description
  (list
   "The COUNT rule part defines the number of occurrences at which to"
   "range-bound the recurrence.  The \"DTSTART\" property value always"
   "counts as the first occurrence."))

(define* (rrule-part-count-validate value #:key (verbose? #t))
  (unless (number? value)
    (when verbose?
      (rrule-print-description "COUNT" %rrule-part-count-description))
    (rrule-error "COUNT value must be a number" value)))

(define %rrule-part-interval-description
  (list
   "The INTERVAL rule part contains a positive integer representing at"
   "which intervals the recurrence rule repeats.  The default value is"
   "\"1\", meaning every second for a SECONDLY rule, every minute for a"
   "MINUTELY rule, every hour for an HOURLY rule, every day for a"
   "DAILY rule, every week for a WEEKLY rule, every month for a"
   "MONTHLY rule, and every year for a YEARLY rule.  For example,"
   "within a DAILY rule, a value of \"8\" means every eight days."))

(define* (rrule-part-interval-validate value #:key (verbose? #t))
  (unless (and (number? value) (> value 0))
    (when verbose?
      (rrule-print-description "INTERVAL" %rrule-part-interval-description))
    (rrule-error "INTERVAL must be a positive integer" value)))

;; Also describes BYMINUTE and BYHOUR.
(define %rrule-part-bysecond-description
  (list
   "The BYSECOND rule part specifies a COMMA-separated list of seconds"
   "within a minute.  Valid values are 0 to 60.  The BYMINUTE rule"
   "part specifies a COMMA-separated list of minutes within an hour."
   "Valid values are 0 to 59.  The BYHOUR rule part specifies a COMMA-"
   "separated list of hours of the day.  Valid values are 0 to 23."
   "The BYSECOND, BYMINUTE and BYHOUR rule parts MUST NOT be specified"
   "when the associated \"DTSTART\" property has a DATE value type."
   "These rule parts MUST be ignored in RECUR value that violate the"
   "above requirement (e.g., generated by applications that pre-date"
   "this revision of iCalendar)."))

(define* (rrule-part-bysecond-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "BYSECOND"
                               %rrule-part-interval-description)))
  (define (validate-value v)
    (unless (in-range? v '(0 . 60))
      (describe)
      (rrule-error "BYSECOND values must be numbers in range of 0 to 60"
                   value
                   v)))
  (cond
   ((number? value)
    (validate-value value))
   ((list? value)
    (for-each validate-value value))
   (else
    (describe)
    (rrule-error "BYSECOND must be a number or a list of numbers" value))))

(define* (rrule-part-byminute-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "BYMINUTE"
                               %rrule-part-interval-description)))
  (define (validate-value v)
    (unless (in-range? v '(0 . 59))
      (describe)
      (rrule-error "BYMINUTE values must be numbers in range from 0 to 59"
                   value
                   v)))
  (cond
   ((number? value)
    (validate-value value))
   ((list? value)
    (for-each validate-value value))
   (else
    (describe)
    (rrule-error "BYMINUTE must be a number or a list of numbers" value))))

(define* (rrule-part-byhour-validate value #:key (verbose? #t))
  (define (validate-value v)
    (unless (in-range? v '(0 . 23))
      (when verbose?
        (rrule-print-description "BYHOUR"
                                 %rrule-part-interval-description))
      (rrule-error "BYHOUR values must be numbers in range from 0 to 23"
                   value
                   v)))
  (cond
   ((number? value)
    (validate-value value))
   ((list? value)
    (for-each validate-value value))
   (else
    (when verbose?
      (rrule-print-description "BYHOUR" %rrule-part-interval-description))
    (rrule-error "BYHOUR must be a number or a list of numbers" value))))

(define %rrule-part-byday-description
  (list
   "The BYDAY rule part specifies a COMMA-separated list of days of"
   "the week; SU indicates Sunday; MO indicates Monday; TU indicates"
   "Tuesday; WE indicates Wednesday; TH indicates Thursday; FR"
   "indicates Friday; and SA indicates Saturday."
   ""
   "Each BYDAY value can also be preceded by a positive (+n) or"
   "negative (-n) integer.  If present, this indicates the nth"
   "occurrence of a specific day within the MONTHLY or YEARLY \"RRULE\"."
   ""
   "For example, within a MONTHLY rule, +1MO (or simply 1MO)"
   "represents the first Monday within the month, whereas -1MO"
   "represents the last Monday of the month."))

(define %rrule-part-byday-values
  '(SU MO TU WE TH FR SA))

(define %rrule-part-byday-regex
  (make-regexp "^([+-\\ ][0-9]+)(SU|MO|TU|WE|TH|FR|SA)"))

(define* (rrule-part-byday-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "BYDAY"
                               %rrule-part-byday-description)))
  (define (validate-value v)
    (unless (symbol? v)
      (describe)
      (rrule-error "BYDAY values must be represented as symbols"
                   value
                   v))
    (or (member v %rrule-part-byday-values)
        (let* ((str (symbol->string v))
               (m   (regexp-exec %rrule-part-byday-regex str)))
          (unless m
            (describe)
            (rrule-error "BYDAY value has invalid format"
                         value
                         v)))))
  (cond
   ((symbol? value)
    (validate-value value))
   ((list? value)
    (for-each validate-value value))
   (else
    (describe)
    (rrule-error "BYDAY value has wrong type" value))))

(define %rrule-part-bymonthday-description
  (list
   "The BYMONTHDAY rule part specifies a COMMA-separated list of days"
   "of the month.  Valid values are 1 to 31 or -31 to -1.  For"
   "example, -10 represents the tenth to the last day of the month."
   "The BYMONTHDAY rule part MUST NOT be specified when the FREQ rule"
   "part is set to WEEKLY."))

(define* (rrule-part-bymonthday-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "BYMONTHDAY"
                               %rrule-part-bymonthday-description)))
  (define (validate-value v)
    (unless (in-range? v '((-31 . -1) (1 . 31)))
      (rrule-error
       "BYMONTHDAY values must be numbers in range -31..-1 or 1..31"
       value
       v)))
  (cond
   ((number? value)
    (validate-value value))
   ((list? value)
    (for-each validate-value value))
   (else
    (describe)
    (rrule-error "BYMONTHDAY must be a number or a list of numbers" value))))

(define %rrule-part-byyearday-description
  (list
   "The BYYEARDAY rule part specifies a COMMA-separated list of days"
   "of the year.  Valid values are 1 to 366 or -366 to -1.  For"
   "example, -1 represents the last day of the year (December 31st)"
   "and -306 represents the 306th to the last day of the year (March"
   "1st).  The BYYEARDAY rule part MUST NOT be specified when the FREQ"
   "rule part is set to DAILY, WEEKLY, or MONTHLY."))

(define* (rrule-part-byyearday-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "BYYEARDAY"
                               %rrule-part-byyearday-description)))
  (define (validate-value v)
    (unless (in-range? v '((-366 . -1) (1 . 366)))
      (describe)
      (rrule-error
       "BYMONTHDAY values must be numbers in range -31..-1 or 1..31"
       value
       v)))
  (cond
   ((number? value)
    (validate-value value))
   ((list? value)
    (for-each validate-value value))
   (else
    (describe)
    (rrule-error "BYYEARDAY must be a number or a list of numbers" value))))

(define %rrule-part-byweekno-description
  (list
   "The BYWEEKNO rule part specifies a COMMA-separated list of"
   "ordinals specifying weeks of the year.  Valid values are 1 to 53"
   "or -53 to -1.  This corresponds to weeks according to week"
   "numbering as defined in [ISO.8601.2004].  A week is defined as a"
   "seven day period, starting on the day of the week defined to be"
   "the week start (see WKST).  Week number one of the calendar year"
   "is the first week that contains at least four (4) days in that"
   "calendar year.  This rule part MUST NOT be used when the FREQ rule"
   "part is set to anything other than YEARLY.  For example, 3"
   "represents the third week of the year."))

(define* (rrule-part-byweekno-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "BYWEEKNO"
                               %rrule-part-byweekno-description)))
  (define (validate-value v)
    (unless (in-range? v '((-53 . -1) (1 . 53)))
      (describe)
      (rrule-error
       "BYWEEKNO values must be numbers in range -53..-1 or 1..53"
       value
       v)))
  (cond
   ((number? value)
    (validate-value value))
   ((list? value)
    (for-each validate-value value))
   (else
    (describe)
    (rrule-error "BYWEEKNO must be a number or a list of numbers" value))))

(define %rrule-part-bymonth-description
  (list
   "The BYMONTH rule part specifies a COMMA-separated list of months"
   "of the year.  Valid values are 1 to 12."))

(define* (rrule-part-bymonth-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "BYMONTH"
                               %rrule-part-bymonth-description)))
  (define (validate-value v)
    (unless (in-range? v '(1 . 12))
      (describe)
      (rrule-error
       "BYMONTH values must be numbers in range 1..12"
       value
       v)))
  (cond
   ((number? value)
    (validate-value value))
   ((list? value)
    (for-each validate-value value))
   (else
    (describe)
    (rrule-error "BYMONTH must be a number or a list of numbers" value))))

(define %rrule-part-bysetpos-description
  (list
   "The BYSETPOS rule part specifies a COMMA-separated list of values"
   "that corresponds to the nth occurrence within the set of"
   "recurrence instances specified by the rule.  BYSETPOS operates on"
   "a set of recurrence instances in one interval of the recurrence"
   "rule.  For example, in a WEEKLY rule, the interval would be one"
   "week A set of recurrence instances starts at the beginning of the"
   "interval defined by the FREQ rule part.  Valid values are 1 to 366"
   "or -366 to -1.  It MUST only be used in conjunction with another"
   "BYxxx rule part.  For example \"the last work day of the month\""
   "could be represented as:"
   ""
   "  FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1"
   ""
   "Each BYSETPOS value can include a positive (+n) or negative (-n)"
   "integer.  If present, this indicates the nth occurrence of the"
   "specific occurrence within the set of occurrences specified by the"
   "rule."))

(define* (rrule-part-bysetpos-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "BYSETPOS"
                               %rrule-part-bysetpos-description)))
  (define (validate-value v)
    (unless (in-range? v '((-366 . -1) (1 . 366)))
      (describe)
      (rrule-error
       "BYSETPOS values must be numbers in range -366..-1 or 1..366"
       value
       v)))
  (cond
   ((number? value)
    (validate-value value))
   ((list? value)
    (for-each validate-value value))
   (else
    (describe)
    (rrule-error "BYSETPOS must be a number or a list of numbers" value))))

(define %rrule-part-wkst-description
  (list
   "The WKST rule part specifies the day on which the workweek starts."
   "Valid values are MO, TU, WE, TH, FR, SA, and SU.  This is"
   "significant when a WEEKLY \"RRULE\" has an interval greater than 1,"
   "and a BYDAY rule part is specified.  This is also significant when"
   "in a YEARLY \"RRULE\" when a BYWEEKNO rule part is specified.  The"
   "default value is MO."))

(define %rrule-part-wkst-values
  '(SU MO TU WE TH FR SA))

(define* (rrule-part-wkst-validate value #:key (verbose? #t))
  (define (describe)
    (when verbose?
      (rrule-print-description "WKST"
                               %rrule-part-wkst-description)))
  (define (validate-value v)
    (unless (symbol? v)
      (describe)
      (rrule-error "WKST values must be represented as symbols"
                   value
                   v))
    (unless (member v %rrule-part-byday-values)
      (describe)
      (rrule-error "WKST valid values are MO, TU, WE, TH, FR, SA, and SU"
                   value
                   v)))
  (cond
   ((symbol? value)
    (validate-value value))
   ((list? value)
    (for-each validate-value value))
   (else
    (describe)
    (rrule-error "WKST value has wrong type" value))))



(define %rrule-parts
  (alist->hash-table
   `((FREQ       . ,(cons %rrule-part-freq-description
                          rrule-part-freq-validate))
     (UNTIL      . ,(cons %rrule-part-until-description
                          rrule-part-until-validate))
     (COUNT      . ,(cons %rrule-part-count-description
                          rrule-part-count-validate))
     (INTERVAL   . ,(cons %rrule-part-interval-description
                          rrule-part-interval-validate))
     (BYSECOND   . ,(cons %rrule-part-bysecond-description
                          rrule-part-bysecond-validate))
     (BYMINUTE   . ,(cons %rrule-part-bysecond-description
                          rrule-part-byminute-validate))
     (BYHOUR     . ,(cons %rrule-part-bysecond-description
                          rrule-part-byhour-validate))
     (BYDAY      . ,(cons %rrule-part-byday-description
                          rrule-part-byday-validate))
     (BYMONTHDAY . ,(cons %rrule-part-bymonthday-description
                          rrule-part-bymonthday-validate))
     (BYYEARDAY  . ,(cons %rrule-part-byyearday-description
                          rrule-part-byyearday-validate))
     (BYWEEKNO   . ,(cons %rrule-part-byweekno-description
                          rrule-part-byweekno-validate))
     (BYMONTH    . ,(cons %rrule-part-bymonth-description
                          rrule-part-bymonth-validate))
     (BYSETPOS   . ,(cons %rrule-part-bysetpos-description
                          rrule-part-bysetpos-validate))
     (WKST       . ,(cons %rrule-part-wkst-description
                          rrule-part-wkst-validate)))))

(define-method (ics-property:recur-part-describe (part <symbol>))
  "Get the description of RRULE @var{part} as a list of strings."
  (let ((part (hash-ref part %rrule-parts)))
    (unless part
      (rrule-error "No RRULE found" part))
    (car part)))

(define-method (ics-property:recur-part-validate (part <symbol>) value)
  "Validate a @var{value} for a RRULE @var{part}.
Throw @code{guile-ics-rrule-error} on failed validation.  Return value is
undefined."
  (let ((part (hash-ref %rrule-parts part)))
    (unless part
      (rrule-error "No RRULE found" part))
    ((cdr part) value)))


;;; Class definition.

(define-class <ics-property:recur> (<ics-property>)
  ;; <symbol>
  (freq
   #:init-value   #f
   #:init-keyword #:freq
   #:getter       ics-property:recur-freq)

  ;; <ics-property:date> | <ics-property:date-time>
  (until
   #:init-value   #f
   #:init-keyword #:until
   #:getter       ics-property:recur-until)

  ;; <number>
  (count
   #:init-value   #f
   #:init-keyword #:count
   #:getter       ics-property:recur-count)

  (interval
   #:init-value   #f
   #:init-keyword #:interval
   #:getter       ics-property:recur-interval)

  (by-second
   #:init-value   #f
   #:init-keyword #:by-second
   #:getter       ics-property:recur-by-second)

  (by-minute
   #:init-value   #f
   #:init-keyword #:by-minute
   #:getter       ics-property:recur-by-minute)

  (by-hour
   #:init-value   #f
   #:init-keyword #:by-hour
   #:getter       ics-property:recur-by-hour)

  (by-day
   #:init-value   #f
   #:init-keyword #:by-day
   #:getter       ics-property:recur-by-day)

  (by-month-day
   #:init-value   #f
   #:init-keyword #:by-month-day
   #:getter       ics-property:recur-by-month-day)

  (by-year-day
   #:init-value   #f
   #:init-keyword #:by-year-day
   #:getter       ics-property:recur-by-year-day)

  (by-week-no
   #:init-value   #f
   #:init-keyword #:by-week-no
   #:getter       ics-property:recur-by-week-no)

  (by-month
   #:init-value   #f
   #:init-keyword #:by-month
   #:getter       ics-property:recur-by-month)

  (by-set-pos
   #:init-value   #f
   #:init-keyword #:by-set-pos
   #:getter       ics-property:recur-by-set-pos)

  (wkst
   #:init-value   #f
   #:init-keyword #:wkst
   #:getter       ics-property:recur-wkst))

(define-method (initialize (property <ics-property:recur>) initargs)
  (define (validate key part)
    (and=> (constructor-argument initargs key)
           (lambda (v) (ics-property:recur-part-validate part v))))
  (next-method)
  (slot-set! property 'type 'RECUR)
  (let ((freq (constructor-argument initargs #:freq)))
    (unless freq
      (error "#:freq is required" initargs))
    (rrule-part-freq-validate freq))
  (validate #:until        'UNTIL)
  (validate #:count        'COUNT)
  (validate #:interval     'INTERVAL)
  (validate #:by-second    'BYSECOND)
  (validate #:by-minute    'BYMINUTE)
  (validate #:by-hour      'BYHOUR)
  (validate #:by-day       'BYDAY)
  (validate #:by-month-day 'BYMONTHDAY)
  (validate #:by-year-day  'BYYEARDAY)
  (validate #:by-week-no   'BYWEEKNO)
  (validate #:by-month     'BYMONTH)
  (validate #:by-set-pos   'BYSETPOS)
  (validate #:wkst         'WKST))


;;; Printers.

(define (%display property port)
  (let* ((fmt (lambda (k v) (format #f "~a: ~a" k v)))
         (value->string (lambda (k proc)
                          (let ((v (proc property)))
                            (if v (fmt k v) "")))))
    (format port "#<ics-property:recur ~a ~a>"
            (string-join/non-null
             (list
              (value->string 'freq         ics-property:recur-freq)
              (value->string 'until        ics-property:recur-until)
              (value->string 'count        ics-property:recur-count)
              (value->string 'interval     ics-property:recur-interval)
              (value->string 'by-second    ics-property:recur-by-second)
              (value->string 'by-minute    ics-property:recur-by-minute)
              (value->string 'by-hour      ics-property:recur-by-hour)
              (value->string 'by-day       ics-property:recur-by-day)
              (value->string 'by-month     ics-property:recur-by-month)
              (value->string 'by-month-day ics-property:recur-by-month-day)
              (value->string 'by-year-day  ics-property:recur-by-year-day)
              (value->string 'by-week-no   ics-property:recur-by-week-no)
              (value->string 'by-set-pos   ics-property:recur-by-set-pos)
              (value->string 'wkst         ics-property:recur-wkst)))
            (object-address->string property))))

(define-method (display (property <ics-property:recur>) (port <port>))
  (%display property port))

(define-method (write (property <ics-property:recur>) (port <port>))
  (%display property port))

(define-method (display (property <ics-property:recur>))
  (%display property (current-output-port)))

(define-method (write (property <ics-property:recur>))
  (%display property (current-output-port)))


;;; Predicates.

(define-method (ics-property:recur? x)
  "Check if X is an instance of <ics-property:recur>, return #t if
it is, #f otherwise."
  (is-a? x <ics-property:recur>))


;;; Converters

(define-method (ics-property->ics-property:recur
                (property <ics-property>))
  (let* ((rrule (vector->list (ics-property-value property)))
         (ref   (lambda (k) (assoc-ref rrule k)))
         (strings->number (lambda (v)
                            (if (list? v)
                                (map string->number v)
                                (string->number v)))))
    (make <ics-property:recur>
      #:name  (ics-property-name property)
      #:value (ics-property-value property)
      #:parameters (ics-property-parameters property)
      #:freq         (and=> (ref "FREQ") string->symbol)
      #:until        (ref "UNTIL")
      #:count        (and=> (ref "COUNT") string->number)
      #:interval     (and=> (ref "INTERVAL") string->number)
      #:by-second    (and=> (ref "BYSECOND") strings->number)
      #:by-minute    (and=> (ref "BYMINUTE") strings->number)
      #:by-hour      (and=> (ref "BYHOUR") strings->number)
      #:by-day       (and=> (ref "BYDAY") string->symbol)
      #:by-month-day (and=> (ref "BYMONTHDAY") strings->number)
      #:by-year-day  (and=> (ref "BYYEARDAY") strings->number)
      #:by-week-no   (and=> (ref "BYWEEKNO") strings->number)
      #:by-month     (and=> (ref "BYMONTH") strings->number)
      #:by-set-pos   (and=> (ref "BYSETPOS") strings->number)
      #:wkst         (and=> (ref "WKST") string->symbol))))

(define-method (serialize-values (property <ics-property:recur>))
  "Serialize @var{property} values into a vector of pairs/lists.  Return the
vector."
  (define (p k proc)
    (cons k (proc property)))
  (list->vector
   (filter (lambda (v) v)
           (map (lambda (kv)
                  (let ((k (car kv))
                        (v (cdr kv)))
                    (and v
                         (cons k
                               (cond
                                ((list? v)
                                 (map object->string v))
                                (else
                                 (object->string v)))))))
                (list (p "FREQ"       ics-property:recur-freq)
                      (p "UNTIL"      ics-property:recur-until)
                      (p "COUNT"      ics-property:recur-count)
                      (p "INTERVAL"   ics-property:recur-interval)
                      (p "BYSECOND"   ics-property:recur-by-second)
                      (p "BYMINUTE"   ics-property:recur-by-minute)
                      (p "BYHOUR"     ics-property:recur-by-hour)
                      (p "BYDAY"      ics-property:recur-by-day)
                      (p "BYMONTHDAY" ics-property:recur-by-month-day)
                      (p "BYYEARDAY"  ics-property:recur-by-year-day)
                      (p "BYWEEKNO"   ics-property:recur-by-week-no)
                      (p "BYMONTH"    ics-property:recur-by-month)
                      (p "BYSETPOS"   ics-property:recur-by-set-pos)
                      (p "WKST"       ics-property:recur-wkst))))))

(define-method (ics-typed-property->ics-property
                (property <ics-property:recur>))
  (make <ics-property>
    #:name  (ics-property-name property)
    #:value (serialize-values property)
    #:parameters (ics-property-parameters property)))

;;; recur.scm ends here.
