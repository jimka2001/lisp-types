;; Copyright (c) 2017 EPITA Research and Development Laboratory
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

(in-package :lisp-types.test)


(define-test lisp-types/reduced-typecase
  (bdd '(:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 300))))
  (bdd '(OR
         (:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 300)))))
  (bdd-or *bdd-true*
          (bdd '(:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 300)))))
  (bdd-or *bdd-false*
          (bdd '(:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 300)))))
  (bdd-or (bdd '(:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 300))))
          *bdd-true*)
  (bdd-or (bdd '(:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 300))))
          *bdd-false*)
  (bdd-or (bdd 'float)
          (bdd '(:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 300)))))
  (bdd '(OR
         float
         (:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 300)))))
  (bdd '(OR
         (AND (AND FLOAT (NOT (EQL 3.14)))
          (:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 300))))))
  (bdd '(OR
         (AND (AND FLOAT NUMBER)
          (:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 100))))
         (AND (OR STRING BIGNUM)
          (:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 200))))
         (AND (AND FLOAT (NOT (EQL 3.14)))
          (:TYPECASE-FORM (RETURN-FROM #:|return654| (PROGN 300))))))
  (bdd '(OR
         (AND (AND FLOAT (NOT (EQL 3.14)))
          (:TYPECASE-FORM (RETURN-FROM #:|return652| (PROGN 300))))))


  (dolist (obj '(t 1 1.0 "hello"))
    (bdd-typecase obj
                  ((and float number) 100))
    (bdd-typecase obj
                  ((and float (not (eql 3.14)))  300))
    (bdd-typecase obj
                  ((and float number) 100)
                  ((and float (not (eql 3.14)))  300))
    (bdd-typecase obj
                  ((and float number) 100)
                  ((or string bignum) 200)
                  ((and float (not (eql 3.14)))  300))))
