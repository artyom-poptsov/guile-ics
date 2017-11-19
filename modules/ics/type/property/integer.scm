(define-module (ics type property integer)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:integer>
                ics-property->ics-property:integer))


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
