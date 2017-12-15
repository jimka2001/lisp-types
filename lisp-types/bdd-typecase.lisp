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

(in-package :lisp-types)

(defclass bdd-typecase-node (bdd-node)
  ())

(defmethod print-object ((bdd bdd-typecase-node) stream)
  (print-unreadable-object (bdd stream :type t :identity nil)
    (when (slot-boundp bdd 'ident)
      (format stream "[~D]" (slot-value bdd 'ident)))
    (format stream "~S" (bdd-label bdd))))

(defmethod bdd-cmp-bdd ((bdd1 bdd-typecase-node) (bdd2 bdd-node))
  '>)

(defmethod bdd-cmp-bdd ((bdd1 bdd-node) (bdd2 bdd-typecase-node))
  '<)

(defmethod bdd-cmp-bdd ((bdd1 bdd-typecase-node) (bdd2 bdd-typecase-node))
  (bdd-cmp (bdd-label bdd1) (bdd-label bdd2)))

(defmethod bdd-find-reduction (label (bdd bdd-typecase-node) reduction-rules)
  nil)

(defmethod bdd-reduce-allocated ((bdd bdd-typecase-node) new-left new-right)
  bdd)

(defmethod bdd-list-to-bdd ((head (eql :typecase-form)) tail)
  (%bdd-node tail *bdd-true* *bdd-false* :bdd-node-class 'bdd-typecase-node))

(defmacro bdd-typecase (obj &rest clauses)
  (let ((return (gensym "return"))
        (var (gensym "obj")))
    (labels ((build-bdd-from-typecase-clauses (clauses)
               `(or ,@(mapcar #'build-one-clause clauses)))
             (build-one-clause (clause &aux (type-spec (car clause)) (clause-body (cdr clause)))
               `(and ,type-spec (:typecase-form (return-from ,return (progn ,@clause-body))))))
;;      (cl-user::print-vals (build-bdd-from-typecase-clauses clauses))
      (let* ((bdd-arg (build-bdd-from-typecase-clauses clauses))
            (bdd (bdd bdd-arg)))
        (cl-user::print-vals bdd-arg)
        `(,(bdd-to-if-then-else-4 bdd var)
          ,obj)))))

