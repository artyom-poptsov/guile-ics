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

(test-assert "boolean: ics-property:boolean?"
  (let ((p (make <ics-property:boolean>
             #:name  "NON-SMOKING"
             #:value #t)))
    (ics-property:boolean? p)))

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

(test-assert "cal-address: ics-property:cal-address?"
  (let ((p (make <ics-property:cal-address>
             #:name       "ORGANIZER"
             #:value      "mailto:jsmith@example.com"
             #:parameters '((CN . "John Smith")))))
    (ics-property:cal-address? p)))


;;; DATE

(test-assert "date: make"
  (let ((p (make <ics-property:date>
             #:name "RDATE"
             #:value (strptime "%Y%m%d" "19970714"))))
    p))

(test-assert "date: ics-property:date?"
  (let ((p (make <ics-property:date>
             #:name "RDATE"
             #:value (strptime "%Y%m%d" "19970714"))))
    (ics-property:date? p)))

(test-assert "date: ics-property:date=?"
  (let ((p1 (make <ics-property:date>
              #:name "RDATE"
              #:value (strptime "%Y%m%d" "19970714")))
        (p2 (make <ics-property:date>
              #:name "RDATE"
              #:value (strptime "%Y%m%d" "19970714"))))
    (ics-property:date=? p1 p2)))

(test-assert "date: list of values"
  (let* ((p (make <ics-property>
             #:name  "X-GNU-DATES"
             #:value '("19970715" "19970716" "19970717")))
         (d (ics-property->ics-property:date p))
         (v (ics-property-value d)))
    (and (list? v)
         (= (length v) 3)
         v)))


;;; DATE-TIME

(test-assert "date-time: make"
  (let ((p (make <ics-property:date-time>
             #:name "DTSTART"
             #:tzid "America/New_York"
             #:value (localtime (current-time)))))
    p))

(test-assert "date-time: ics-property:date-time?"
  (let ((p (make <ics-property:date-time>
             #:name "DTSTART"
             #:tzid "America/New_York"
             #:value (localtime (current-time)))))
    (ics-property:date-time? p)))

(test-assert "date-time: list of values"
  (let* ((p (make <ics-property>
             #:name  "X-GNU-DTIME"
             #:value '("19970610T172345Z" "19970610T172346Z")))
         (d (ics-property->ics-property:date-time p))
         (v (ics-property-value d)))
    (and (list? v)
         (= (length v) 2)
         v)))


;;; DURATION

(test-assert "duration: make"
  (let ((p (make <ics-property:duration>
             #:name  "DURATION"
             #:value "P15DT5H0M20S")))
    p))

(test-assert "duration: ics-property:duration?"
  (let ((p (make <ics-property:duration>
             #:name  "DURATION"
             #:value "P15DT5H0M20S")))
    (ics-property:duration? p)))


;;; FLOAT

(test-assert "float: make"
  (let ((p (make <ics-property:float>
             #:name  "X-GNU-PI"
             #:value 3.14)))
    p))

(test-assert "float: ics-property:float?"
  (let ((p (make <ics-property:float>
             #:name  "X-GNU-PI"
             #:value 3.14)))
    (ics-property:float? p)))

(test-assert "float: ics-property->ics-property:float"
  (let* ((p (make <ics-property>
             #:name  "X-GNU-PI"
             #:value "3.14"))
         (f (ics-property->ics-property:float p)))
    (= (ics-property-value f) 3.14)))

(test-assert "float: equal?"
  (let ((p1 (make <ics-property:float>
              #:name  "X-GNU-PI"
              #:value 3.14))
        (p2 (make <ics-property:float>
              #:name  "X-GNU-PI"
              #:value 3.14))
        (p3 (make <ics-property:float>
              #:name  "X-GNU-PI"
              #:value 3.14)))
    (equal? p1 p2 p3)))

(test-assert "float: ics-property->ics-property:float, value list"
  (let* ((p (make <ics-property>
             #:name  "X-GNU-FLOATS"
             #:value '("3.14" "3.15")))
         (f (ics-property->ics-property:float p)))
    (equal? (ics-property-value f) '(3.14 3.15))))

(test-assert "float: ics-property:float->ics-property"
  (let* ((p      (make <ics-property>
                   #:name  "X-GNU-FLOATS"
                   #:value '("3.14" "3.15")))
         (f      (ics-property->ics-property:float p))
         (result (ics-property:float->ics-property f)))
    (equal? p result)))


;;;

(test-end "types")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; types.scm ends here.
