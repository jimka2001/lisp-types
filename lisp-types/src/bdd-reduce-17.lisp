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

(in-package :lisp-types)

(defgeneric label (tir-node))
(defgeneric (setf label) (new-type tir-node))

(defvar *tir-node-num* 0)

(defclass tir-node ()
  ((id :type unsigned-byte :reader id :initform (incf *tir-node-num*))
   (label :initarg :label :accessor label)
   (touches :initform (make-hash-table :test #'eq) :accessor touches)
   (subsets :initform nil :type list :accessor subsets)
   (supersets :initform nil :type list :accessor supersets)))

(defmethod print-object ((n tir-node) stream)
  (print-unreadable-object (n stream :type t :identity nil)
    (format stream "~D: ~A" (id n) (label n))))

(defgeneric add-tir-node (tir-graph tir-node))
(defgeneric tir-node-and (tir-node1 tir-node2))
(defgeneric tir-node-and-not (tir-node1 tir-node2))
(defgeneric tir-node-subtypep (tir-node1 tir-node2))
(defgeneric tir-node-empty-type (tir-node))
(defgeneric tir-node-disjoint-types-p (tir-node1 tir-node2))

(defclass tir-graph ()
  ((tir-nodes :type list :accessor tir-nodes :initarg :tir-nodes
          :initform nil)
   (blue :type list :accessor blue
         :initform nil
         :documentation "List of blue arrows in order (origin destination)")
   (green :type list :accessor green
          :initform nil
          :documentation "List of green lines connecting tir-nodes, order of pair (x y) is semantically unimportant, but for ease of access (id x) < (id y)")
   (disjoint :type list :accessor disjoint
             :initform nil)))

(defgeneric extract-disjoint (tir-graph))
(defgeneric decompose-tir-graph-1 (g u))
(defgeneric decompose-tir-graph-2 (g u))
(defgeneric construct-tir-graph (g u))

(defmethod decompose-tir-graph-1 ((g tir-graph) u)
  (declare (ignore u))
  (loop :while (or (blue g) (green g))
        :do (dolist (x->y (blue g))
              (destructuring-bind (x y) x->y
                (break-relaxed-subset g x y)))
        :do (dolist (x--y (green g))
              (destructuring-bind (x y) x--y
                (break-touching g x y))))
  (remove-duplicates (remove nil (extract-disjoint g))
		     :test #'equal))

(defmethod decompose-tir-graph-1 :around ((g tir-graph) u)
  (construct-tir-graph g u)
  (call-next-method))

(defmethod decompose-tir-graph-2 :around ((g tir-graph) u)
  (construct-tir-graph g u)
  (call-next-method))

(defun decompose-by-graph-1 (u &key (tir-graph-class 'sexp-tir-graph))
  (declare (type list u))
  (decompose-tir-graph-1 (make-instance tir-graph-class) u))

(defmethod decompose-tir-graph-2 ((g tir-graph) u)
  (loop :while (or (blue g) (green g))
        :do (dolist (x->y (blue g))
              (destructuring-bind (x y) x->y
                (break-strict-subset g x y)))
        :do (dolist (x--y (green g))
              (destructuring-bind (x y) x--y
                (break-touching g x y)))
        :do (dolist (x->y (blue g))
              (destructuring-bind (x y) x->y
                (break-loop g x y))))
  (remove-duplicates (remove nil (extract-disjoint g))
		     :test #'equal))

(defun decompose-by-graph-2 (u &key (tir-graph-class 'sexp-tir-graph))
  (declare (type list u))
  (decompose-tir-graph-2 (make-instance tir-graph-class) u))

(defmethod construct-tir-graph ((g tir-graph) u)
  (declare (type list u))
  (dolist (label u)
    (add-tir-node g label))
  (mapl (lambda (tail)
          (let ((x (car tail)))
            (mapc (lambda (y)
                    (cond
                      ((tir-node-subtypep x y)
                       (add-blue-arrow g x y))
                      ((tir-node-subtypep y x)
                       (add-blue-arrow g y x))
                      (t
                       (multiple-value-bind (disjoint trust) (tir-node-disjoint-types-p x y)
                         (cond
                           ((null trust) ;; maybe intersection types, not sure
                            (add-green-line g x y))
                           (disjoint
                            nil)
                           (t ;; intersecting types
                            (add-green-line g x y)))))))
                  (cdr tail)))) (tir-nodes g))
  
  (dolist (tir-node (tir-nodes g))
    (maybe-disjoint-tir-node g tir-node))

  g)

(defun empty-hash (hash)
  (= 0 (hash-table-count hash)))

(defun inhabited-hash (hash)
  (/= 0 (hash-table-count hash)))

(defun maybe-disjoint-tir-node (g tir-node)
  (declare (type tir-graph g) (type tir-node tir-node))
  (cond
    ((tir-node-empty-type tir-node)
     (setf (tir-nodes g) (remove tir-node (tir-nodes g) :test #'eq)))
    ((null (or (inhabited-hash (touches tir-node))
               (supersets tir-node)
               (subsets tir-node)))
     (setf (tir-nodes g) (remove tir-node (tir-nodes g) :test #'eq))
     (pushnew tir-node (disjoint g) :test #'eq))))

(defun sort-tir-nodes (n1 n2)
  (declare (type tir-node n1 n2))
  (if (< (id n1) (id n2))
      (list n1 n2)
      (list n2 n1)))

(defun add-green-line (g x y)
  (declare (type tir-graph g) (type tir-node x y))
  (pushnew (sort-tir-nodes x y) (green g) :test #'equal)
  (setf (gethash x (touches y)) t
        (gethash y (touches x)) t))

(defun delete-green-line (g x y)
  (declare (type tir-graph g) (type tir-node x y))
  (setf (green g)   (remove (sort-tir-nodes x y) (green g) :test #'equal))
  (remhash x (touches y))
  (remhash y (touches x))
  (maybe-disjoint-tir-node g x)
  (maybe-disjoint-tir-node g y))

(defun add-blue-arrow (g x y)
  (pushnew (list x y) (blue g) :test #'equal)
  (pushnew x (subsets y) :test #'eq)
  (pushnew y (supersets x) :test #'eq))

(defun delete-blue-arrow (g x y)
  (declare (type tir-graph g) (type tir-node x y))
  (setf (blue g)      (remove (list x y) (blue g) :test #'equal)
        (subsets y)   (remove x (subsets y) :test #'eq)
        (supersets x) (remove y (supersets x) :test #'eq))
  (maybe-disjoint-tir-node g x)
  (maybe-disjoint-tir-node g y))

(defun break-strict-subset (g sub super)
  (declare (type tir-graph g) (type tir-node sub super))
  (cond 
    ((null (member super (supersets sub) :test #'eq))
     nil)
    ((subsets sub)
     nil)
    ((inhabited-hash (touches sub))
     nil)
    (t
     (setf (label super) (tir-node-and-not super sub))
     (delete-blue-arrow g sub super)))
  g)

(defun break-relaxed-subset (g sub super)
  (declare (type tir-graph g) (type tir-node sub super))
  (cond ((null (member super (supersets sub) :test #'eq))
         nil)
        ((subsets sub)
         nil)
        (t
         (setf (label super) (tir-node-and-not super sub))
         (dolist (alpha (subsets super))
           ;; intersection (touches sub) (subsets super)
           (when (gethash alpha (touches sub))
             (add-green-line g alpha super)
             (delete-blue-arrow g alpha super)))
         (delete-blue-arrow g sub super)))
  g)



(defun break-touching (g x y)
  (declare (type tir-graph g) (type tir-node x y))
  (cond
    ((null (gethash y (touches x)))
     nil)
    ((subsets x)
     nil)
    ((subsets y)
     nil)
    (t
     (flet ((maphash-intersection (function-designator hash1 hash2)
              (declare (type hash-table hash1 hash2)
                       (type (function (t) t) function-designator))
              (when (< (hash-table-count hash1)
                       (hash-table-count hash2))
                (rotatef hash1 hash2))
              (maphash (lambda (key _)
                         (declare (ignore _))
                         (when (gethash key hash2)
                           (funcall function-designator key)))
                       hash1)))
       (let ((z (add-tir-node g (tir-node-and x y))))
         (psetf (label x) (tir-node-and-not x y)
                (label y) (tir-node-and-not y x))
         (dolist (alpha (union (supersets x) (supersets y) :test #'eq))
           (add-blue-arrow g z alpha))
         (maphash-intersection (lambda (alpha)
                                 (add-green-line g z alpha))
                               (touches x)
                               (touches y))
         (maybe-disjoint-tir-node g z)))
     (delete-green-line g x y)))
  g)
       
(defun break-loop  (g x y)
  (declare (type tir-graph g) (type tir-node x y))
  (cond
    ((null (gethash y (touches x)))
     nil)
    ((subsets x)
     nil)
    ((subsets y)
     nil)
    (t
     (let ((z (add-tir-node g (tir-node-and x y))))
       (setf (label x) (tir-node-and-not x y))
       (maphash (lambda (alpha _)
                  (declare (ignore _))
                  (add-green-line g z alpha))
                (touches x))
       (dolist (alpha (union (supersets x) (supersets y) :test #'eq))
         (add-blue-arrow g z alpha))
       (add-blue-arrow g z y)
       (add-blue-arrow g z x)
       (delete-blue-arrow g x y))))
  g)

;; implemention of sexp based types

(defclass sexp-tir-node (tir-node)
  ((label :type (or list symbol))))

(defmethod tir-node-and-not ((x sexp-tir-node) (y sexp-tir-node))
  (reduce-lisp-type `(and ,(label x) (not ,(label y)))))
  
(defmethod tir-node-and  ((x sexp-tir-node) (y sexp-tir-node))
  (reduce-lisp-type `(and ,(label x) ,(label y))))

(defmethod tir-node-empty-type ((tir-node sexp-tir-node))
  (null (label tir-node)))

(defmethod tir-node-subtypep ((x sexp-tir-node) (y sexp-tir-node))
  (cached-subtypep (label x) (label y)))

(defmethod tir-node-disjoint-types-p ((x sexp-tir-node) (y sexp-tir-node))
  (disjoint-types-p (label x) (label y)))

(defclass sexp-tir-graph (tir-graph)
  ())

(defmethod add-tir-node ((g sexp-tir-graph) type-specifier)
  (let ((z (make-instance 'sexp-tir-node :label type-specifier)))
    (push z
          (tir-nodes g))
    z))

(defmethod extract-disjoint ((g sexp-tir-graph))
  (mapcar #'label (disjoint g)))

(defmethod decompose-tir-graph-1 ((g sexp-tir-graph) u)
  (caching-types
    (call-next-method)))

(defmethod decompose-tir-graph-2 ((g sexp-tir-graph) u)
  (caching-types
    (call-next-method)))


;; implemention of bdd based types

(defclass bdd-tir-node (tir-node)
  ((label :type bdd)))

(defmethod tir-node-and-not ((x bdd-tir-node) (y bdd-tir-node))
  (bdd-and-not (label x) (label y)))

(defmethod tir-node-and ((x bdd-tir-node) (y bdd-tir-node))
  (bdd-and (label x) (label y)))

(defmethod tir-node-empty-type ((tir-node bdd-tir-node))
  (eq *bdd-false* (label tir-node)))

(defmethod tir-node-subtypep ((x bdd-tir-node) (y bdd-tir-node))
  (bdd-subtypep (label x) (label y)))

(defmethod tir-node-disjoint-types-p ((x bdd-tir-node) (y bdd-tir-node))
  (values (bdd-disjoint-types-p (label x) (label y))
          t))

(defclass bdd-tir-graph (tir-graph)
  ())

(defmethod add-tir-node ((g bdd-tir-graph) type-specifier)
  (let ((z (make-instance 'bdd-tir-node :label (ltbdd type-specifier))))
    (push z
          (tir-nodes g))
    z))

(defmethod extract-disjoint ((g bdd-tir-graph))
  (mapcar #'bdd-to-dnf (mapcar #'label (disjoint g))))


(defmethod decompose-tir-graph-1 :around ((g bdd-tir-graph) u)
  (ltbdd-with-new-hash ()
   (call-next-method)))

(defmethod decompose-tir-graph-2 :around ((g bdd-tir-graph) u)
  (ltbdd-with-new-hash ()
    (construct-tir-graph g u)
    (call-next-method)))
