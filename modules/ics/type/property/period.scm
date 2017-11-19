(define-module (ics type property period)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:period>
                ics-property->ics-property:period))



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
