(define-module (ics type property duration)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:duration>
                ics-property->ics-property:duration))


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
