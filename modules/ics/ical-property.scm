(define-module (ics ical-property)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<ical-property>
            ical-property-name
            ical-property-value
            ical-property-parameters
            ical-property-parameter
            ical-property->string))


;;;

(define-class <ical-property> ()
  (name       #:accessor     ical-property-name
              #:init-value   #f
              #:init-keyword #:name)
  (value      #:accessor     ical-property-value
              #:init-value   #f
              #:init-keyword #:value)
  ;; alist
  (parameters #:accessor     ical-property-parameters
              #:init-value   #f
              #:init-keyword #:parameters))

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

;;;
