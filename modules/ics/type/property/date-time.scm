(define-module (ics type property date-time)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:date-time>
                ics-property:date-time-tzid
                ics-property->ics-property:date-time))


;;; DATE-TIME (RFC5545, 3.3.5)

(define-class <ics-property:date-time> (<ics-property>)
  (tzid #:accessor ics-property:date-time-tzid
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
