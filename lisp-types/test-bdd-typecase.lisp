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

(let ((package-into (find-package  :lisp-types.test))
      (package-from (find-package  :lisp-types))
      (*package* (find-package :keyword)))
  (do-symbols (name package-from)
    (when (and (eq package-from (symbol-package name))
               (not (find-symbol (symbol-name name) package-into)))
      (format t "importing name=~A into ~S ~%" name package-into)
      (shadowing-import name package-into))))

(defun cmp-equal (obj1 obj2)
  (typecase obj1
    (atom
     (equal obj1 obj2))
    ((cons (eql SATISFIES))
     (typep obj2 '(cons (eql SATISFIES))))
    (t
     (and (= (length obj1) (length obj2))
          (cmp-equal (car obj1) (car obj2))
          (cmp-equal (cdr obj1) (cdr obj2))))))

(define-test lisp-types/typecase-to-type
  (let ((*satisfies-symbols* nil))
    (assert-test (cmp-equal nil
                            (typecase-to-type 
                             '( ;; typecase obj
                               ))))
  
    (assert-test (cmp-equal
                  '(OR NIL (AND (NOT (OR)) FLOAT (SATISFIES 100)))
                  (typecase-to-type 
                   '( ;; typecase obj
                     (float 100)
                     ))))

    (assert-test (cmp-equal
                  '(OR (OR NIL (AND (NOT (OR)) FLOAT (SATISFIES 100)))
                    (AND (NOT (OR FLOAT)) NUMBER (SATISFIES 200)))
                  (typecase-to-type 
                   '( ;; typecase obj          ;
                     (float 100)
                     (number 200)))))

    (assert-test (cmp-equal
                  '(OR
                    (OR
                     (OR (OR NIL (AND (NOT (OR)) FLOAT (SATISFIES 100)))
                      (AND (NOT (OR FLOAT)) NUMBER (SATISFIES 200)))
                     (AND (NOT (OR NUMBER FLOAT)) STRING (SATISFIES 300)))
                    (AND (NOT (OR STRING NUMBER FLOAT)) ARRAY (SATISFIES 400)))
                  (typecase-to-type 
                   '( ;; typecase obj          ;
                     (float 100)
                     (number 200)
                     (string 300)
                     (array 400)))))))

