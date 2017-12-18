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

(defvar *satisfies-symbols* t
  "The initial value is t so there will be an error if a function tries to push onto it without rebinding with dynamic extent.")
(defun build-bdd-arg-from-typecase-body (type-case-body)
  (apply #'values
         (reduce (lambda (acc clause)
                   (destructuring-bind (type &rest clause-body) clause
                     (destructuring-bind (acc-type leading-types) acc
                       (let ((f (gensym "f")))
                         (push f *satisfies-symbols*)
                         (setf (get f :clause-body) clause-body)
                         (setf (symbol-function f) #'(lambda (obj)
                                                       (typep obj type)))
                         (list `(or ,acc-type
                                    (and (not (or ,@leading-types)) ,type
                                         (:typecase-form ,clause-body
                                          :satisfies ,f )))
                               (cons type leading-types))))))
                 type-case-body :initial-value '(nil nil))))

(defun bdd-to-if-then-else-labels (bdd obj)
  "expand into linear size code as LABELS, whose runtime is logrithmic and code size is linear"
  (let (bdd->name-mapping
        (label 0))
    (labels ((walk-bdd (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (assoc bdd bdd->name-mapping)
                    (incf label)
                    (push (list bdd label) bdd->name-mapping)
                    (walk-bdd (bdd-left bdd))
                    (walk-bdd (bdd-right bdd))))))
             (branch (bdd)
               (typecase bdd
                 (bdd-false nil)
                 (bdd-true t)
                 (bdd-node
                  (list (cadr (assoc bdd bdd->name-mapping))))))
             (label-function (bdd)
               (cl-user::print-vals (type-of bdd) (bdd-label bdd) *satisfies-symbols*

                                    (typep (bdd-label bdd) '(cons (eql satisfies)))
                                    (when (typep (bdd-label bdd) '(cons (eql satisfies)))
                                      (member (cadr (bdd-label bdd)) *satisfies-symbols*))
                                    (when (and (typep (bdd-label bdd) '(cons (eql satisfies)))
                                               (member (cadr (bdd-label bdd)) *satisfies-symbols*))
                                      (get (cadr (bdd-label bdd)) :clause-body)
                                    ))
               (typecase bdd
                 (bdd-node
                  (if (and (typep (bdd-label bdd) '(cons (eql satisfies)))
                           (member (cadr (bdd-label bdd)) *satisfies-symbols*))
                      `(,(cadr (assoc bdd bdd->name-mapping)) ()
                        ,@(get (cadr (bdd-label bdd)) :clause-body))
                      `(,(cadr (assoc bdd bdd->name-mapping)) ()
                        (if (typep ,obj ',(bdd-label bdd))
                            ,(branch (bdd-left bdd))
                            ,(branch (bdd-right
                                      bdd)))))))))
      (walk-bdd bdd)
      `(lambda (,obj)
         (labels ,(mapcar #'label-function (mapcar #'car bdd->name-mapping))
             ,(branch bdd))))))

;; (bdd-to-if-then-else-3 (bdd '(or (and sequence (not array))
;;                                      number
;;                               (and (not sequence) array))) 'X)
      
      

(defun bdd-to-if-then-else-tagbody/go (bdd obj)
  "expand into linear size code as TAGBODY/GO, whose runtime is logrithmic and code size is linear"
  (let (bdd->name-mapping (num 0))
    (labels ((walk-bdd (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (assoc bdd bdd->name-mapping)
                    (push (list bdd (incf num)) bdd->name-mapping)
                    (walk-bdd (bdd-left bdd))
                    (walk-bdd (bdd-right bdd))))))
             (branch (bdd)
               (typecase bdd
                 (bdd-false `(return nil))
                 (bdd-true `(return t))
                 (bdd-node
                  `(go ,(cadr (assoc bdd bdd->name-mapping))))))
             (label-function (bdd)
               (typecase bdd
                 (bdd-node
                  (if (and (typep (bdd-label bdd) '(cons (eql satisfies)))
                           (member (cadr (bdd-label bdd)) *satisfies-symbols*))
                      `(,(cadr (assoc bdd bdd->name-mapping))
                        (return (progn ,@(get (cadr (bdd-label bdd)) :clause-body))))
                      `(,(cadr (assoc bdd bdd->name-mapping))
                        (if (typep ,obj ',(bdd-label bdd))
                            ,(branch (bdd-left bdd))
                            ,(branch (bdd-right bdd)))))))))
      (walk-bdd bdd)
      `(lambda (,obj)
         (block nil
           (tagbody 
              ,@(mapcan #'label-function
                        (mapcar #'car (reverse bdd->name-mapping)))))))))

(defun bdd-typecase-cmp (t1 t2)
  (if (and (typep t1 '(cons (eql satisfies)))
           (typep t2 '(cons (eql satisfies))))
      (let ((f1 (cadr t1))
            (f2 (cadr t2)))
        (cond
          ((eql f1 f2)
           '=)
          ((and (member f1 *satisfies-symbols*)
                (member f2 *satisfies-symbols*))
           (if (member f1 (cdr (member f2 *satisfies-symbols*)))
               '<
               '>))
          ((member f1 *satisfies-symbols*)
           '>)
          ((member f2 *satisfies-symbols*)
           '<)
          (t
           (%bdd-cmp t1 t2))))
      (%bdd-cmp t1 t2)))

(defmacro bdd-typecase (obj &rest clauses)
  (let ((*satisfies-symbols* nil)
        (var (gensym "obj")))
    (let* ((*bdd-cmp-function* #'bdd-typecase-cmp)
           (bdd-arg (build-bdd-arg-from-typecase-body clauses))
           (bdd (bdd bdd-arg)))
      ;; to make the output more human readable, using LABELS, rather than GO
      ;;  use bdd-to-if-then-else-5 rather than bdd-to-if-then-else-tagbody/go
      `(,(funcall if-then-else bdd var)
        ,obj))))


(defmacro bdd-typecase (obj &rest clauses)
  ;; to make the output more human readable, using LABELS, rather than GO
  ;;  use bdd-to-if-then-else-labels rather than bdd-to-if-then-else-tagbody/go
  (bdd-typecase-expander obj clauses #'bdd-to-if-then-else-labels))
