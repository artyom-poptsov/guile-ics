#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (ics)
             (srfi srfi-9 gnu)
             (ice-9 pretty-print)
             (ics parser)
             (ics common))

(define parser (make-parser (open-input-file "./rfc-simplified.ics")))

(define (main args)
  (set-debug! #t)
  (let ((res (ics->scm parser)))
    (display "RESULT: \n")
    (pretty-print res)))


