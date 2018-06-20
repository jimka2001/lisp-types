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

(defpackage :lisp-types
  (:use :cl :cl-robdd)
  (:export
   "*DECOMPOSE-FUN-PARAMETERIZED-NAMES*"
   "AMBIGUOUS-SUBTYPE"
   "AUTO-PERMUTE-TYPECASE"
   "BDD-DECOMPOSE-TYPES"
   "BDD-DECOMPOSE-TYPES-STRONG"
   "BDD-DECOMPOSE-TYPES-WEAK"
   "BDD-DECOMPOSE-TYPES-WEAK-DYNAMIC"
   "BDD-DISJOINT-TYPES-P"
   "BDD-EMPTY-TYPE"
   "BDD-SUBTYPEP"
   "BDD-TYPE-EQUAL"
   "CACHING-TYPES"
   "CHOOSE-RANDOMLY"
   "DECOMPOSE-TYPES"
   "DECOMPOSE-TYPES"
   "DECOMPOSE-TYPES-BDD-GRAPH"
   "DECOMPOSE-TYPES-BDD-GRAPH-STRONG"
   "DECOMPOSE-TYPES-BDD-GRAPH-WEAK"
   "DECOMPOSE-TYPES-BDD-GRAPH-WEAK-DYNAMIC"
   "DECOMPOSE-TYPES-GRAPH"
   "DECOMPOSE-TYPES-RTEV2"
   "DECOMPOSE-TYPES-SAT"
   "DISJOINT-TYPES-P"
   "EQUIVALENT-TYPES-P"
   "LTBDD"
   "LTBDD-NODE"
   "REDUCE-LISP-TYPE"
   "REDUCED-TYPECASE"
   "RUN-PROGRAM"
   "SHUFFLE-LIST"
   "SMARTER-SUBTYPEP"
   "VALID-TYPE-P"
   ))

(in-package   :lisp-types)

(defmacro exists (obj data &body body)
  (typecase obj
    (list
     (let ((var (gensym "exists")))
       `(member-if (lambda (,var)
                     (destructuring-bind ,obj ,var
                       ,@body)) ,data)))
    (t
     `(member-if (lambda (,obj) ,@body) ,data))))


(defmacro while (test &body body)
  `(loop :while ,test
	 :do (progn ,@body)))

(defmacro forall (var data &body body)
  `(every #'(lambda (,var) ,@body) ,data))

(defmacro setof (var data &body body)
  `(remove-if-not (lambda (,var) ,@body) ,data))

(defmacro prog1-let ((var expr) &body body)
  `(let ((,var ,expr))
     ,@body
     ,var))

(defmacro exists-tail (var list &body body)
  (let ((name (gensym)))
    `(block ,name
       (mapl #'(lambda (,var)
		 (when (progn ,@body)
		   (return-from ,name ,var)))
	     ,list)
       nil)))

