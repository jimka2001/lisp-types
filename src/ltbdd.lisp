;; Copyright (c) 2016,2017 EPITA Research and Development Laboratory
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

(in-package   :lisp-types)

(defclass lisp-type-bdd (bdd)
  ()
  (:documentation "Subclass of bdd for represeting an ROBDD with lisp-type semantics"))

(defclass lisp-type-bdd-node (lisp-type-bdd bdd-node)
  ()
  (:documentation "Subclass of bdd-node for represeting an ROBDD with lisp-type semantics"))

;; TODO bdd-to-dnf or %bdd-to-dnf should remove superclasses from conjunctions, and remove subclasses from disjunctions

(defmethod initialize-instance :after ((bdd lisp-type-bdd) &rest initargs)
  (declare (ignore initargs))
  (unless (valid-type-p (bdd-label bdd))
    (error "invalid type specifier: ~A" (bdd-label bdd))))

(defun ltbdd (obj)
  (bdd obj :bdd-node-class 'lisp-type-bdd-node))

(defmethod bdd-factory ((bdd-class (eql (find-class 'lisp-type-bdd-node))))
  #'ltbdd)

(defmethod bdd-factory ((bdd-class (eql 'lisp-type-bdd-node)))
  #'ltbdd)

(defun ltbdd-node (label positive negative)
  (bdd-node label positive negative :bdd-node-class 'lisp-type-bdd-node))

(defmacro ltbdd-with-new-hash (vars &body body)
  `(bdd-call-with-new-hash (lambda ,vars ,@body)
                           :bdd-node-type '(or lisp-type-bdd-node bdd-leaf)))

