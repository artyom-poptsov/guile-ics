(define-module (ics type property boolean)
  #:use-module (oop goops)
  #:use-module (ics type property property)
  #:export     (<ics-property:boolean>
                ics-property->ics-property:boolean))

;;; BOOLEAN (RFC5545, 3.3.2)

(define-class <ics-property:boolean> (<ics-property>))

(define-method (initialize (property <ics-property:boolean>))
  (next-method)
  (slot-set! property 'ics-property-type 'BOOLEAN))

(define-method (display (property <ics-property:boolean>) (port <port>))
  (format port "#<ics-property:boolean ~a: ~a ~a>"
          (ics-property-name property)
          (ics-property-value property)
          (object-address->string property)))

(define-method (write (property <ics-property:boolean>) (port <port>))
  (display property port))

(define-method (ics-property->ics-property:boolean
                (property <ics-property>))
  (let ((value (ics-property-value property)))
    (make <ics-property:boolean>
      #:name  (ics-property-name property)
      #:value (cond
               ;; Boolean values are case-insensitive.
               ((string-ci=? value "TRUE")  #t)
               ((string-ci=? value "FALSE") #f)
               (else (error "Unknown property value (expected BOOLEAN)"
                            value))))))

;; (define-method (ics-data-boolean->ics-property
;;                 (property <ics-data-boolean>))
;;   (make <ics-property>
;;     ;; Although the RFC states that boolean values are
;;     ;; case-insensitive, we're using uppercase spelling
;;     ;; for unification sake.
;;     #:value (if (ics-data-value ics-data-boolean)
;;                 "TRUE"
;;                 "FALSE")
;;     #:parameters `(("VALUE" . "BOOLEAN"))))
