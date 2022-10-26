(define-module (ics cli command-convert)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-41)
  #:use-module (ics)
  #:export (command-convert))

(define (print-help)
  (display "\
Usage: ics describe [options] [input-file]

Options:
  --help, -h                 Print this message and exit.
  --format, -f <input-format>
                             Set the input data format.
                             Supported formats:
                             - \"dsv\" (default, requires Guile-DSV)
  --to, -t <output-format>   Set the output format.
                             Supported output formats:
                             - \"vcard\" (default)
                             - \"ical\"
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (format                   (single-char #\f) (value #t))
    (to                       (single-char #\t) (value #t))))

(define (command-describe args)
  (let* ((options          (getopt-long args %option-spec))
         (fmt              (option-ref options 'format "dsv"))
         (to               (option-ref options 'to     "vcard"))
         (help-needed?     (option-ref options 'help   #f))
         (args             (option-ref options '()     #f)))

    (when help-needed?
      (print-help)
      (exit 0))

    (let* ((port (if (null? args)
                     (current-input-port)
                     (let ((p (open-input-file (car args))))
                       (unless p
                         (error "Could not open a file" (car args)))
                       p))))
      (if (string=? fmt "dsv")
          (begin
            (use-modules (dsv))
            (let* ((data   (dsv->scm port))
                   (header (car data))
                   (rest   (cdr data)))
              (format (current-output-port) "BEGIN:VCARD\r\n")
              (for-each (lambda (record)
                          #t))
              (format (current-output-port) "END:VCARD\r\n")))
          (error "Unknown format" fmt)))))

;;; describe.scm ends here.
