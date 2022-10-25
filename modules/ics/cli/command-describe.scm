(define-module (ics cli command-describe)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-41)
  #:use-module (ics)
  #:export (command-describe))

(define (print-help)
  (display "\
Usage: ics describe [options] [input-file]
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))))

(define (command-describe args)
  (let* ((options          (getopt-long args %option-spec))
         (help-needed?     (option-ref options 'help  #f))
         (args             (option-ref options '()        #f)))

    (when help-needed?
      (print-help)
      (exit 0))

    (let* ((port   (if (null? args)
                       (current-input-port)
                       (let ((p (open-input-file (car args))))
                         (unless p
                           (error "Could not open a file" (car args)))
                         p)))
           (stream (ics->stream port)))
      (stream-for-each ics-describe stream))))

;;; describe.scm ends here.
