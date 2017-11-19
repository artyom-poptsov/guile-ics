(define-module (ics type property cal-address)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:cal-address>
                ics-property->ics-property:cal-address))


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

(define-method (write (property <ics-property:cal-address>) (port <port>))
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
