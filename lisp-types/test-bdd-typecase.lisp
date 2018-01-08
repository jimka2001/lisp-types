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

(let ((lisp-types-test (find-package  :lisp-types.test))
      (lisp-types (find-package  :lisp-types)))
  (do-symbols (name :lisp-types)
    (when (and (eq lisp-types (symbol-package name))
               (not (find-symbol (symbol-name name) lisp-types-test)))
      (format t "3 importing name=~A into  :lisp-types.test~%" name)
      (shadowing-import name :lisp-types.test))))

(define-test lisp-types/typecase-to-type
  (assert-test (equal nil
                      (typecase-to-type 
                       '( ;; typecase obj
                         ))))
  
  (assert-test (equal
                '(OR NIL (AND (NOT (OR)) FLOAT (:TYPECASE-FORM 100)))
                (typecase-to-type 
                 '( ;; typecase obj
                   (float 100)
                   ))))

  (assert-test (equal
                '(OR (OR NIL (AND (NOT (OR)) FLOAT (:TYPECASE-FORM 100)))
                  (AND (NOT (OR FLOAT)) NUMBER (:TYPECASE-FORM 200)))
                (typecase-to-type 
                 '( ;; typecase obj          ;
                   (float 100)
                   (number 200)))))
  (assert-test (equal
                '(OR
                  (OR
                   (OR (OR NIL (AND (NOT (OR)) FLOAT (:TYPECASE-FORM 100)))
                    (AND (NOT (OR FLOAT)) NUMBER (:TYPECASE-FORM 200)))
                   (AND (NOT (OR NUMBER FLOAT)) STRING (:TYPECASE-FORM 300)))
                  (AND (NOT (OR STRING NUMBER FLOAT)) ARRAY (:TYPECASE-FORM 400)))
                (typecase-to-type 
                 '( ;; typecase obj          ;
                   (float 100)
                   (number 200)
                   (string 300)
                   (array 400))))))

(define-test lisp-types/reduced-typecase
  (bdd nil)
  (bdd '(OR NIL (AND (NOT (OR)) FLOAT (:TYPECASE-FORM 100))))
  (bdd '(OR (OR NIL (AND (NOT (OR)) FLOAT (:TYPECASE-FORM 100)))
         (AND (NOT (OR FLOAT)) NUMBER (:TYPECASE-FORM 200))))
  (bdd '(OR
         (OR
          (OR (OR NIL (AND (NOT (OR)) FLOAT (:TYPECASE-FORM 100)))
           (AND (NOT (OR FLOAT)) NUMBER (:TYPECASE-FORM 200)))
          (AND (NOT (OR NUMBER FLOAT)) STRING (:TYPECASE-FORM 300)))
         (AND (NOT (OR STRING NUMBER FLOAT)) ARRAY (:TYPECASE-FORM 400)))))
