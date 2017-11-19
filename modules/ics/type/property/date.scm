(define-module (ics type property date)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:date>
                ics-property->ics-property:date))


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
