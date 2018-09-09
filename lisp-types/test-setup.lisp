;; Copyright (c) 2016 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(defpackage :lisp-types-test
  (:shadowing-import-from :lisp-types "TEST" "A")
  ;;(:shadowing-import-from :closer-mop "STANDARD-GENERIC-FUNCTION" "DEFMETHOD" "DEFGENERIC")
  (:use :cl :lisp-types :jimka-test ;;:closer-mop
	:jimka-addons
        :lisp-types-analysis
        :cl-robdd
   #+sbcl :sb-pcl
   #+allegro :aclmop
        ))


(in-package :lisp-types-test)

(shadow-all-symbols :package-from :lisp-types :package-into :lisp-types-test)

;; these types are multiply defined.   I don't know why, but they get un-defined
;;    during testing and TYPE/3-TYPES fails.   I'm just redefining them because
;;    I can't figure out the error
(deftype test-float-radix () '(integer 0 (64)))
(deftype test-float-digits () '(integer 0 64))
(deftype test-array-rank () '(integer 0 (65529)))
(deftype test-array-total-size () `(integer 0 (,(- most-positive-fixnum 2))))
(deftype test-char-code () '(integer 0 (#x110000)))
(deftype test-char-int () 'test-char-code)

