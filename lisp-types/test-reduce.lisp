;; Copyright (c) 2018 EPITA Research and Development Laboratory
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

(shadow-all-symbols :package-from :lisp-types :package-into :lisp-types-test)

(assert (not (equal (type-expand 'test-array-rank)
		    'test-array-rank)))


(define-test test/deftypes
  (let ((defined-types '(test-float-radix
			 test-float-digits
			 test-array-rank
			 test-array-total-size
			 test-char-code
			 test-char-int)))
    (dolist (type defined-types)
      (assert-false (equal type (type-expand type))))))


(define-test test/type-to-dnf
  (assert-true (type-to-dnf '(AND (NOT (AND INTEGER (NOT BIT)))
			          (AND ATOM (NOT BIT))))))

(define-test test/type-to-dnf-bottom-up-1
  (assert-true (equal nil
		      (type-to-dnf-bottom-up nil)))
  (assert-true (equal t
		      (type-to-dnf-bottom-up t)))
  (assert-true (equal 'number
		      (type-to-dnf-bottom-up 'number)))
  (assert-true (equal '(not number)
		      (type-to-dnf-bottom-up '(not number))))
  (assert-true (equal '(and number string)
		      (type-to-dnf-bottom-up '(and number string))))
  (assert-true (equal '(or number string)
		      (type-to-dnf-bottom-up '(or number string))))
  (assert-true (equal '(and number string list)
		      (type-to-dnf-bottom-up '(and (and number string) list))))
  (assert-true (equal '(and number string list)
		      (type-to-dnf-bottom-up '(and number (and string list)))))
  (assert-true (equal '(or (not number) (not string))
		      (type-to-dnf-bottom-up '(not (and number string)))))
  (assert-true (equal '(and (not number) (not string))
		      (type-to-dnf-bottom-up '(and (not number) (not string)))))
  (assert-true (equal (type-to-dnf-bottom-up '(and (or a b) (or x y)))
		      '(or (and a x) (and a y) (and b x) (and b y))))
  (assert-true (equal (type-to-dnf-bottom-up '(or (and a b) (and x y)))
		      '(or (and a b) (and x y))))
  (assert-true (equal (type-to-dnf-bottom-up '(and a (and b (and c d))))
		      '(and a b c d)))
  (assert-true (equal (type-to-dnf-bottom-up '(or a (or b (or c d))))
		      '(or a b c d)))
  (assert-true (equal (type-to-dnf-bottom-up '(and (not (or a b))
					           (not (or c d))))
		      '(and (not a) (not b) (not c) (not d))))
  (assert-true (equal (type-to-dnf-bottom-up '(not (and (not a) (not b) (not c) (not d))))
		      '(or a b c d)))
  (assert-true (equal (type-to-dnf-bottom-up '(not (and (not (or a b))
						        (not (or c d)))))
		      '(or a b c d)))
  (assert-true (equal (type-to-dnf-bottom-up '(not (or a b c d)))
		      '(and (not a) (not b) (not c) (not d))))

  (assert-true (equal (type-to-dnf-bottom-up '(and (and a b) (or x y)))
		      '(or (and a b x) (and a b y))))
  (assert-true (equal (type-to-dnf-bottom-up '(and (and (not a) b) (or x y)))
		      '(or (and (not a) b x) (and (not a) b y))))

  (assert-true (equal (type-to-dnf-bottom-up '(and (and a b) (or (not x) y)))
		      '(or (and a b (not x)) (and a b y))))

  (assert-true (equal (type-to-dnf-bottom-up '(and (and a b) (or (not x) (not y))))
		      '(or (and a b (not x)) (and a b (not y)))))

  (assert-true (equal (type-to-dnf-bottom-up '(and a (or (not x) (not (and y z)))))
		      '(or (and a (not x)) (and a (not y)) (and a (not z)))))
  )
  
  
  
  

