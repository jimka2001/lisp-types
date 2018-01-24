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

(in-package :lisp-types.test)

#|

|#

(let ((lisp-types-test (find-package  :lisp-types.test))
      (lisp-types (find-package  :lisp-types)))
  (do-symbols (name :lisp-types)
    (when (and (eq lisp-types (symbol-package name))
               (not (find-symbol (symbol-name name) lisp-types-test)))
      (format t "1 importing name=~A into  :lisp-types.test~%" name)
      (shadowing-import name :lisp-types.test))))

(define-test profile/test1
  (flet ((test-function ()
           (let (nums)
             (dotimes (a 100)
               (dotimes (b 100)
                 (pushnew (random 1.0) nums)))
             (sort nums #'<))))
    (let (profiling )
      (call-with-profiling (lambda () (test-function))
                           (lambda (plists)
                             (setf profiling plists)))
      (forall item profiling
        (assert-true (numberp (getf item :nr))))
      (forall item profiling
        (assert-true (consp (getf item :self))))
      (forall item profiling
        (assert-true (consp (getf item :total))))
      (forall item profiling
        (assert-true (consp (getf item :cumul))))
      (forall item profiling
        (assert-true (getf item :calls)))
      (forall item profiling
        (assert-true (getf item :function))))))
      

