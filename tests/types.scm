;;; ics.scm -- Type tests for ICS parser.

;; Copyright (C) 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:

(use-modules (srfi srfi-64)
             (oop goops)
             (ice-9 rdelim)
             (ics)
             (ics common)
             (ics parser)
             (ics type object)
             (ics type property))

(test-begin "types")


;;; BINARY

(test-assert "binary: make"
  (let ((p (make <ics-property:binary>
             #:name "ATTACH"
             #:format-type "image/vnd.microsoft.icon"
             #:encoding    'BASE64
             #:value       "AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAAACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAEREAAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")))
    p))

(test-assert "binary: ics-property->ics-property:binary"
  (let* ((p (make <ics-property>
              #:name        "ATTACH"
              #:value       "R05VIEd1aWxlCg=="
              #:parameters '((ENCODING . "BASE64")
                             (FMTTYPE  . "image/vnd.microsoft.icon"))))
         (b (ics-property->ics-property:binary p)))
    (and (equal? (ics-property:binary-encoding b) 'BASE64)
         (equal? (ics-property-format-type b) "image/vnd.microsoft.icon")
         (string=? (ics-property-value b) "R05VIEd1aWxlCg=="))))

(test-assert "binary: ics-property:binary?"
  (let* ((p (make <ics-property>
              #:name        "ATTACH"
              #:value       "R05VIEd1aWxlCg=="
              #:parameters '((ENCODING . "BASE64")
                             (FMTTYPE  . "image/vnd.microsoft.icon"))))
         (b (ics-property->ics-property:binary p)))
    (ics-property:binary? b)))

(test-assert "binary: ics-property:binary=?"
  (let* ((p (make <ics-property>
              #:name        "ATTACH"
              #:value       "R05VIEd1aWxlCg=="
              #:parameters '((ENCODING . "BASE64")
                             (FMTTYPE  . "image/vnd.microsoft.icon"))))
         (b1 (ics-property->ics-property:binary p))
         (b2 (ics-property->ics-property:binary p)))
    (ics-property:binary=? b1 b2)))


;;; BOOLEAN

(test-assert "boolean: make"
  (let ((p (make <ics-property:boolean>
             #:name  "NON-SMOKING"
             #:value #t)))
    p))

(test-assert "boolean: ics-property:boolean=?"
  (let ((p1 (make <ics-property:boolean>
              #:name "NON-SMOKING"
              #:value #t))
        (p2 (make <ics-property:boolean>
              #:name "NON-SMOKING"
              #:value #t)))
    (ics-property:boolean=? p1 p2)))


;;; CAL-ADDRESS

(test-assert "cal-address: make"
  (let ((p (make <ics-property:cal-address>
             #:name       "ORGANIZER"
             #:value      "mailto:jsmith@example.com"
             #:parameters '((CN . "John Smith")))))
    p))


;;; DATE

(test-assert "date: make"
  (let ((p (make <ics-property:date>
             #:name "RDATE"
             #:value (strptime "%Y%m%d" "19970714"))))
    p))

(test-assert "date: ics-property:date=?"
  (let ((p1 (make <ics-property:date>
              #:name "RDATE"
              #:value (strptime "%Y%m%d" "19970714")))
        (p2 (make <ics-property:date>
              #:name "RDATE"
              #:value (strptime "%Y%m%d" "19970714"))))
    (ics-property:date=? p1 p2)))


;;;

(test-end "types")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; types.scm ends here.
