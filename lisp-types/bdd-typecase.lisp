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


(defvar *satisfies-symbols* t
  "The initial value is t so there will be an error if a function tries to push onto it without rebinding with dynamic extent.")

(defun define-type-predicate (type-specifier)
  (let ((function-name (gensym "f")))
    (setf (symbol-function function-name)
          #'(lambda (obj)
              (typep obj type-specifier)))
    function-name))

(defun build-bdd-arg-from-typecase-body (type-case-body)
  (apply #'values
         (reduce (lambda (acc clause)
                   (destructuring-bind (type &rest clause-body) clause
                     (destructuring-bind (acc-type leading-types) acc
                       (let ((f (define-type-predicate type)))
                         (push f *satisfies-symbols*)
                         (setf (get f :clause-body) clause-body)
                         (list `(or ,acc-type
                                    (and (not (or ,@leading-types)) ,type
                                         (satisfies ,f)))
                               (cons type leading-types))))))
                 type-case-body :initial-value '(nil nil))))

(defun bdd-to-if-then-else-labels (bdd obj)
  "expand into linear size code as LABELS, whose runtime is logrithmic and code size is linear"
  (let (bdd->name-mapping)
    (labels ((walk-bdd (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (assoc bdd bdd->name-mapping)
                    (push (list bdd (gensym "L")) bdd->name-mapping)
                    (walk-bdd (bdd-left bdd))
                    (walk-bdd (bdd-right bdd))))))
             (branch (bdd)
               (typecase bdd
                 (bdd-false nil)
                 (bdd-true t)
                 (bdd-node
                  (list (cadr (assoc bdd bdd->name-mapping))))))
             (label-function (bdd)
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

(defun bdd-to-if-then-else-tagbody/go (bdd obj)
  "expand into linear size code as TAGBODY/GO, whose runtime is logrithmic and code size is linear"
  (let (bdd->name-mapping
        (block-name (gensym "block")))
    (labels ((walk-bdd (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (assoc bdd bdd->name-mapping)
                    (push (list bdd (gensym "L")) bdd->name-mapping)
                    (walk-bdd (bdd-left bdd))
                    (walk-bdd (bdd-right bdd))))))
             (branch (bdd)
               (typecase bdd
                 (bdd-false `(return-from ,block-name nil))
                 (bdd-true `(return-from ,block-name t))
                 (bdd-node
                  `(go ,(cadr (assoc bdd bdd->name-mapping))))))
             (label-function (bdd)
               (typecase bdd
                 (bdd-node
                  (if (and (typep (bdd-label bdd) '(cons (eql satisfies)))
                           (member (cadr (bdd-label bdd)) *satisfies-symbols*))
                      `(,(cadr (assoc bdd bdd->name-mapping))
                        (return-from ,block-name (progn ,@(get (cadr (bdd-label bdd)) :clause-body))))
                      `(,(cadr (assoc bdd bdd->name-mapping))
                        (if (typep ,obj ',(bdd-label bdd))
                            ,(branch (bdd-left bdd))
                            ,(branch (bdd-right bdd)))))))))
      (walk-bdd bdd)
      `(lambda (,obj)
         (block ,block-name
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

(defun bdd-typecase-expander (obj clauses if-then-else)
  (let ((*satisfies-symbols* nil)
        (*reduce-member-type* nil)
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
  (bdd-typecase-expander obj clauses #'bdd-to-if-then-else-tagbody/go))
