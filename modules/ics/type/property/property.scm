(define-module (ics type property property)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ics type content)
  #:export (<ics-property>
            object-address->string
            ics-property-name
            non-standard-property-name?
            ics-property-type
            ics-property-value
            ics-property-parameters
            ics-property-parameter-ref
            ics-property->string))

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

;; RFC5545, 3.8.8.2: Non-Standard Properties.
(define (non-standard-property-name? name)
  (regexp-match? (string-match "X-.*" name)))

;;; property.scm ends here.
