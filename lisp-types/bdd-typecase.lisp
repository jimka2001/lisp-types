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

(defclass bdd-typecase-node (bdd-leaf)
  ((satisfies :initarg :satisfies)
   (proxy :accessor bdd-proxy)))

(defmethod slot-unbound (class (obj bdd-typecase-node) (slot-name (eql 'proxy)))
  (setf (slot-value obj 'proxy) (%bdd-node (bdd-serialize obj) *bdd-true* *bdd-false*)))

(defmethod slot-unbound (class (obj bdd-typecase-node) (slot-name (eql 'dnf)))
  (setf (slot-value obj 'dnf) (bdd-serialize obj)))

(defmethod slot-unbound (class (obj bdd-typecase-node) (slot-name (eql 'expr)))
  (setf (slot-value obj 'expr) (bdd-serialize obj)))

(defmethod bdd-serialize ((leaf bdd-typecase-node))
  `(satisfies ,(slot-value leaf 'satisfies)))

(defmethod print-object ((bdd bdd-typecase-node) stream)
  (print-unreadable-object (bdd stream :type t :identity nil)
    (when (slot-boundp bdd 'ident)
      (format stream "[~D]" (slot-value bdd 'ident)))
    (format stream "~S" (bdd-label bdd))))

(defmethod bdd-and ((bdd1 bdd-node) (leaf bdd-typecase-node))
  (bdd-and bdd1 (bdd-proxy leaf)))

(defmethod bdd-and ((leaf bdd-typecase-node) (bdd1 bdd-node))
  (bdd-and (bdd-proxy leaf) bdd1))

(defmethod bdd-and ((leaf1 bdd-typecase-node) (leaf2 bdd-typecase-node))
  (bdd-and (bdd-proxy leaf1) (bdd-proxy leaf2)))


(defmethod bdd-or ((bdd1 bdd-node) (leaf bdd-typecase-node))
  (bdd-or bdd1 (bdd-proxy leaf)))

(defmethod bdd-or ((leaf bdd-typecase-node) (bdd1 bdd-node))
  (bdd-or (bdd-proxy leaf) bdd1))

(defmethod bdd-or ((leaf1 bdd-typecase-node) (leaf2 bdd-typecase-node))
  (bdd-or (bdd-proxy leaf1) (bdd-proxy leaf2)))


(defmethod bdd-and-not ((bdd1 bdd-node) (leaf bdd-typecase-node))
  (bdd-and-not bdd1 (bdd-proxy leaf)))

(defmethod bdd-and-not ((leaf bdd-typecase-node) (bdd1 bdd-node))
  (bdd-and-not (bdd-proxy leaf) bdd1))

(defmethod bdd-and-not ((leaf1 bdd-typecase-node) (leaf2 bdd-typecase-node))
  (bdd-and-not (bdd-proxy leaf1) (bdd-proxy leaf2)))

(defmethod bdd-cmp-bdd ((bdd1 bdd-typecase-node) (bdd2 bdd-node))
  '>)

(defmethod bdd-cmp-bdd ((bdd1 bdd-node) (bdd2 bdd-typecase-node))
  '<)

(defmethod bdd-cmp-bdd ((bdd1 bdd-typecase-node) (bdd2 bdd-typecase-node))
  (bdd-cmp (bdd-label bdd1) (bdd-label bdd2)))

(defmethod bdd-find-reduction (label (bdd bdd-typecase-node) reduction-rules)
  bdd)

(defmethod bdd-reduce-allocated ((bdd bdd-typecase-node) new-left new-right)
  bdd)

(defmethod bdd-reduce-allocated ((bdd bdd-node) (new-left bdd-typecase-node) (new-right bdd-typecase-node))
  bdd)

(defmethod bdd-reduce-allocated ((bdd bdd-node) (new-left bdd-node) (new-right bdd-typecase-node))
  (cond
    ((bdd-type-equal bdd new-left)
     new-left)
    ((smarter-subtypep (bdd-to-expr bdd) nil)
     *bdd-false*)
    ((smarter-subtypep t (bdd-to-expr bdd))
     *bdd-true*)
    (t
     bdd)))

(defmethod bdd-reduce-allocated ((bdd bdd-node) (new-left bdd-typecase-node) (new-right bdd-node))
  (cond
    ((bdd-type-equal bdd new-right)
     new-right)
    ((smarter-subtypep (bdd-to-expr bdd) nil)
     *bdd-false*)
    ((smarter-subtypep t (bdd-to-expr bdd))
     *bdd-true*)
    (t
     bdd)))


(defmethod bdd-list-to-bdd ((head (eql :typecase-form)) tail)
  (destructuring-bind (&key typecase-form satisfies) (cons head tail)
    (make-instance 'bdd-typecase-node
                   :satisfies satisfies 
                   :label typecase-form)))

(defun build-bdd-arg-from-typecase-body (type-case-body)
  (apply #'values
         (reduce (lambda (acc clause)
                   (destructuring-bind (type &rest clause-body) clause
                     (destructuring-bind (acc-type leading-types) acc
                       (let ((f (gensym "f")))
                         (setf (symbol-function f) #'(lambda (obj)
                                                       (typep obj type)))
                         (list `(or ,acc-type
                                    (and (not (or ,@leading-types)) ,type
                                         (:typecase-form ,clause-body
                                          :satisfies ,f )))
                               (cons type leading-types))))))
                 type-case-body :initial-value '(nil nil))))


(defmacro bdd-typecase (obj &rest clauses)
  (let ((return (gensym "return"))
        (var (gensym "obj")))
    (labels ((build-bdd-from-typecase-clauses (clauses)
               `(or ,@(mapcar #'build-one-clause clauses)))
             (build-one-clause (clause &aux (type-spec (car clause)) (clause-body (cdr clause)))
               `(and ,type-spec (:typecase-form (return-from ,return (progn ,@clause-body))))))
      
  
      (let* ((bdd-arg (build-bdd-from-typecase-clauses clauses))
             (bdd (bdd bdd-arg)))
        `(,(bdd-to-if-then-else-4 bdd var)
          ,obj)))))

