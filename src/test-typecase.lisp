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



(in-package :lisp-types-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :lisp-types :package-into :lisp-types-test))

(define-test lisp-types/disjoint-typecase
  (assert-true (equal (macroexpand-1 '(disjoint-typecase fred
				       (integer 42)
				       (bignum 43)
				       (number 44)
				       (string 45)
				       ((or string float) 46)
				       ((or string number) 47)))
		      '(TYPECASE FRED
			(INTEGER 42)
			(NIL 43)
			((AND NUMBER (NOT INTEGER)) 44)
			(STRING 45)
			(NIL 46)
			(NIL 47))))
  (assert-true (equal (macroexpand-1 '(disjoint-typecase fred
				       (integer 42)
				       ((and number (not integer)) 43)
				       ((and number (not bignum)) 44)))
		      '(TYPECASE FRED
			(INTEGER 42)
			((AND NUMBER (NOT INTEGER)) 43)
			(NIL 44)))))

(define-test lisp-types/reduced-typecase
  (assert-true (equal (macroexpand-1 '(reduced-typecase fred
				       (integer 42)
				       (bignum 43)
				       (number 44)
				       (string 45)
				       ((or string float) 46)
				       ((or string number) 47)))
		      '(TYPECASE FRED
			(INTEGER 42)
			(BIGNUM 43)
			(NUMBER 44)
			(STRING 45)
			(FLOAT 46)
			(NIL 47))))
  (assert-true (equal (macroexpand-1 '(reduced-typecase fred
				       (integer 42)
				       ((and number (not integer)) 43)
				       ((and number (not bignum)) 44)))
		      '(TYPECASE FRED
			(INTEGER 42)
			(NUMBER 43)
			(NIL 44)))))

(defclass A-174 () ())
(defclass B-174 () ())
(defclass C-174 () ())
(defclass abc-174 (A-174 B-174 C-174) ())

(define-test lisp-types/reduced-typecase2
  (assert-true (equal (macroexpand-1 '(reduced-typecase fred
				       ((or A-174 B-174)
					41)
				       (A-174
					42)
				       (B-174
					43)))
		      '(TYPECASE FRED ((OR A-174 B-174) 41) (NIL 42) (NIL 43))))
  (assert-true (equal (macroexpand-1 '(reduced-typecase fred
				       ((and A-174 B-174)
					41)
				       (A-174
					42)
				       (B-174
					43)))
		      '(TYPECASE FRED ((AND A-174 B-174) 41) (A-174 42) (B-174 43))))
  (assert-true (equal (macroexpand-1 '(reduced-typecase fred
				       ((or (not A-174) B-174)
					41)
				       (A-174
					42)
				       (B-174
					43)))
		      '(TYPECASE FRED ((OR B-174 (NOT A-174)) 41) (T 42) (NIL 43))))
  (assert-true (equal (macroexpand-1 '(reduced-typecase fred
				       ((or (not (and A-174 B-174)) C-174)
					41)
				       (A-174
					42)
				       (B-174
					43)
				       (C-174
					44)))
		      '(TYPECASE FRED ((OR C-174 (NOT A-174) (NOT B-174)) 41) (T 42) (T 43) (NIL 44))))
  )





(define-test lisp-types/auto-permute-typecase
  (dolist (obj '(1 "hello" 3.4 nil :x))
    (assert-true (equal (auto-permute-typecase obj
			  (fixnum 1)
			  (string 2)
			  (float 5)
			  (list 6)
			  (keyword 8)
			  (t 9))
			(typecase obj
			  (fixnum 1)
			  (string 2)
			  (float 5)
			  (list 6)
			  (keyword 8)
			  (t 9))))

    (assert-true (equal (auto-permute-typecase obj
			  (fixnum 1)
			  (string 2)
			  (number 5)
			  (list 6)
			  (keyword 8)
			  (t 9))
			(typecase obj
			  (fixnum 1)
			  (string 2)
			  (number 5)
			  (list 6)
			  (keyword 8)
			  (t 9))))

    (assert-true (equal (auto-permute-typecase obj
			  (fixnum 1)
			  (string 2)
			  ((and number (not real)) 5)
			  (list 6)
			  (keyword 8)
			  (t 9))
			(typecase obj
			  (fixnum 1)
			  (string 2)
			  ((and number (not real)) 5)
			  (list 6)
			  (keyword 8)
			  (t 9))))

    (assert-true (equal (auto-permute-typecase obj
			  (fixnum 1)
			  (string 2)
			  ((and number (not real)) 5)
			  (list 6)
			  (symbol 7)
			  (keyword 8)
			  (t 9))
			(typecase obj
			  (fixnum 1)
			  (string 2)
			  ((and number (not real)) 5)
			  (list 6)
			  (symbol 7)
			  (keyword 8)
			  (t 9))))

    (assert-true (equal (auto-permute-typecase obj
			  (fixnum 1)
			  (string 2)
			  ((and number (not real)) 5)
			  (list 6)
			  (keyword 7)
			  (symbol 8)
			  (t 9))
			(typecase obj
			  (fixnum 1)
			  (string 2)
			  ((and number (not real)) 5)
			  (list 6)
			  (keyword 7)
			  (symbol 8)
			  (t 9))))

    (assert-true (equal (auto-permute-typecase obj
			  (string 2)
			  ((and number (not real)) 5)
			  (list 6)
			  (keyword 7)
			  (symbol 8)
			  (t 9))
			(typecase obj
			  (string 2)
			  ((and number (not real)) 5)
			  (list 6)
			  (keyword 7)
			  (symbol 8)
			  (t 9))))
    ))
			
