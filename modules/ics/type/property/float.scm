(define-module (ics type property float)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:float>
                ics-property->ics-property:float))


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
