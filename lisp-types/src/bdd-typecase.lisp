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

(defun define-type-predicate (type-specifier &key suggestion clause-body)
  (let ((function-name (gensym (typecase suggestion
                                 ((cons symbol)
                                  (concatenate 'string
                                               (symbol-name (car suggestion)) "-"))
                                 (t "f")))))

    (setf (symbol-function function-name)
          #'(lambda (obj)
              (typep obj type-specifier)))
    (assert (listp *satisfies-symbols*) ()
            "define-type-predicate only works in the dynamic extent of *satisfies-symbols* rebinding to list")
    (push function-name *satisfies-symbols*)
    (setf (get function-name :clause-body) clause-body)
    function-name))

(defun typecase-to-type (type-case-body)
  (car
   (reduce (lambda (acc clause)
             (destructuring-bind (type &rest clause-body) clause
               (destructuring-bind (acc-type leading-types) acc
                 (let ((f (define-type-predicate type :clause-body clause-body :suggestion (car (last clause-body)))))
                   (list `(or ,acc-type
                              (and (not (or ,@leading-types)) ,type
                                   (satisfies ,f)))
                         (cons type leading-types))))))
           type-case-body :initial-value '(nil nil))))

(defun bdd-to-if-then-else-labels (bdd obj)
  "expand into linear size code as LABELS, whose runtime is logrithmic and code size is linear"
  (let (used mapping)
    (labels
        ((walk-bdd (bdd)
           (typecase bdd
             (bdd-false)
             (bdd-true)
             (bdd-node
              (unless (assoc bdd mapping)
                (push (list bdd (gensym "L")) mapping)
                (walk-bdd (bdd-positive bdd))
                (walk-bdd (bdd-negative bdd))))))
         (branch (bdd)
           (typecase bdd
             (bdd-false nil)
             (bdd-true t)
             (bdd-node
              (list (cadr (assoc bdd mapping))))))
         (label-function (bdd)
           (typecase bdd
             (bdd-node
              (cond
                ((and (typep (bdd-label bdd) '(cons (eql satisfies)))
                      (member (cadr (bdd-label bdd)) *satisfies-symbols*))
                 (pushnew (cadr (bdd-label bdd)) used)
                 `(,(cadr (assoc bdd mapping)) ()
                   ,@(get (cadr (bdd-label bdd)) :clause-body)))
                (t
                 `(,(cadr (assoc bdd mapping)) ()
                   (if (typep ,obj ',(bdd-label bdd))
                       ,(branch (bdd-positive bdd))
                       ,(branch (bdd-negative
                                 bdd))))))))))
      (walk-bdd bdd)
      (values
       `(lambda (,obj)
          (labels ,(mapcar #'label-function (mapcar #'car mapping))
            ,(branch bdd)))
       (loop :for f :in *satisfies-symbols*
             :unless (member f used)
               :collect (get f :clause-body))))))

(defun bdd-to-if-then-else-tagbody/go (bdd obj)
  "expand into linear size code as TAGBODY/GO, whose runtime is logrithmic and code size is linear"
  (let (bdd->name-mapping
        (block-name (gensym "BLOCK")))
    (labels ((walk-bdd (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (assoc bdd bdd->name-mapping)
                    (push (list bdd (gensym "L")) bdd->name-mapping)
                    (walk-bdd (bdd-positive bdd))
                    (walk-bdd (bdd-negative bdd))))))
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
                            ,(branch (bdd-positive bdd))
                            ,(branch (bdd-negative bdd)))))))))
      (walk-bdd bdd)
      `(lambda (,obj)
         (block ,block-name
           (tagbody 
              ,@(mapcan #'label-function
                        (mapcar #'car (reverse bdd->name-mapping)))))))))

(defun bdd-typecase-cmp (t1 t2)
  (cond
    ((eql t1 t2)
     '=)
    ((and (typep t1 '(cons (eql satisfies)))
          (typep t2 '(cons (eql satisfies))))
     (let ((f1 (cadr t1))
           (f2 (cadr t2)))
       (cond
         ((and (member f1 *satisfies-symbols*)
               (member f2 *satisfies-symbols*))
          (compare-objects t1 t2))
         ((member f1 *satisfies-symbols*)
          '>)
         ((member f2 *satisfies-symbols*)
          '<)
         (t
          (compare-objects t1 t2)))))
    (t
     (compare-objects t1 t2))))

(defun bdd-typecase-expander (obj clauses if-then-else)
  (ltbdd-with-new-hash ()
    (let ((*satisfies-symbols* nil)
          (*reduce-member-type* nil)
          (var (gensym "OBJ")))
      (let* ((*bdd-cmp-function* #'bdd-typecase-cmp)
             (bdd-arg (typecase-to-type clauses))
             (bdd (ltbdd bdd-arg)))
	;; (bdd-to-dot bdd "/Users/jimka/research/dot/launch-missiles.dot")
        `(,(funcall if-then-else bdd var)
          ,obj)))))

(defmacro bdd-typecase (obj &rest clauses)
  "Syntactically and symantically similar to CL:TYPECASE, but uses BDDs
 to expand into code avoiding redundant type checks."
  ;; to make the output more human readable, using LABELS, rather than GO
  ;;  use bdd-to-if-then-else-labels rather than bdd-to-if-then-else-tagbody/go
  (bdd-typecase-expander obj clauses #'bdd-to-if-then-else-tagbody/go)
  ;;(bdd-typecase-expander obj clauses #'bdd-to-if-then-else-labels)
  )
