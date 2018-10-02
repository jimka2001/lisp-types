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

(in-package   :lisp-types)



(labels ((relation (r x-parity y-parity)
           #'(lambda (x y)
               (funcall r
                        (if x-parity
                            x
                            `(not ,x))
                        (if y-parity
                            y
                            `(not ,y)))))
         (super (x-parity y-parity)
           (relation #'(lambda (a b)
                         (smarter-subtypep b a)) x-parity y-parity))
         (sub (x-parity y-parity)
           (relation #'smarter-subtypep x-parity y-parity))
         (disjoint (x-parity y-parity)
           (relation #'disjoint-types-p x-parity y-parity)))
  (let* ((reductions `((:case  1 :child :positive :relation ,(disjoint t t)     :reduction ,#'bdd-negative)
                       (:case  2 :child :positive :relation ,(disjoint t nil)   :reduction ,#'bdd-positive)
                       (:case  3 :child :negative :relation ,(disjoint nil t)   :reduction ,#'bdd-negative)
                       (:case  4 :child :negative :relation ,(disjoint nil nil) :reduction ,#'bdd-positive)

                       (:case  5 :child :negative :relation ,(super t t)        :reduction ,#'bdd-negative)
                       (:case  6 :child :negative :relation ,(super t nil)      :reduction ,#'bdd-positive)
                       (:case  7 :child :positive :relation ,(super nil t)      :reduction ,#'bdd-negative)
                       (:case  8 :child :positive :relation ,(super nil nil)    :reduction ,#'bdd-positive)

                       (:case  9 :child :positive :relation ,(sub t t)          :reduction ,#'bdd-positive)
                       (:case 10 :child :positive :relation ,(sub t nil)        :reduction ,#'bdd-negative)
                       (:case 11 :child :negative :relation ,(sub nil t)        :reduction ,#'bdd-positive)
                       (:case 12 :child :negative :relation ,(sub nil nil)      :reduction ,#'bdd-negative)))
         ;; :relation ... :reduction 
         (positive-reductions  (mapcar #'cddddr (setof r reductions
                                              (eq :positive (getf r :child)))))
         (negative-reductions (mapcar #'cddddr (setof r reductions
                                              (eq :negative (getf r :child))))))

    (defun lisp-type-bdd-allocate (label positive-bdd negative-bdd &key (bdd-node-class 'lisp-type-bdd-node))
      (let ((new-positive (bdd-reduce label positive-bdd positive-reductions))
            (new-negative (bdd-reduce label negative-bdd negative-reductions)))
        (cond
          ((eq new-positive new-negative) ;; 2.5%
           new-positive)
          ((bdd-find (bdd-hash) label new-positive new-negative)) ;;7%
          (t
           (let* ((bdd (make-instance bdd-node-class
                                      :label label
                                      :positive  new-positive
                                      :negative new-negative))
                  (key (bdd-make-key label (bdd-ident new-positive) (bdd-ident new-negative))))
             (setf (gethash key (bdd-hash)) bdd)
             (setf (gethash key (bdd-hash))
                   (bdd-reduce-allocated bdd new-positive new-negative)))))))))

(defmethod bdd-allocate (label (positive-bdd lisp-type-bdd-node) (negative-bdd bdd) &key (bdd-node-class (class-of positive-bdd)))
  (declare (type (not (eql bdd-node)) bdd-node-class))
  (lisp-type-bdd-allocate label positive-bdd negative-bdd :bdd-node-class bdd-node-class))

(defmethod bdd-allocate (label (positive-bdd bdd) (negative-bdd lisp-type-bdd-node) &key (bdd-node-class (class-of negative-bdd)))
  (declare (type (not (eql bdd-node)) bdd-node-class))
  (lisp-type-bdd-allocate label positive-bdd negative-bdd :bdd-node-class bdd-node-class))

(defmethod bdd-reduce-allocated ((bdd lisp-type-bdd-node) new-positive new-negative)
  (cond
    ;; check (bdd-and-not bdd new-positive)
    ;;   vs  (bdd-and-not new-positive bdd)
    ((bdd-type-equal bdd new-positive) ;; 0.006%   ;; TODO perhaps it is more interesting to check equivalance first to the 'smaller' of new-positive and new-negative, not sure because checking for smaller might be slow, and making a new slot to store the size might expand memory enough to also make the program slower?
     new-positive)
    ;; check (bdd-and-not bdd new-negative)
    ;;   vs  (bdd-and-not new-negative bdd)
    ((bdd-type-equal bdd new-negative) ;; 0.5%
     new-negative)
    ;; the next two clauses, which use CL:SUBTYPEP are necessary
    ;; because the CL type system contains lots of identities
    ;; which are difficult to encode.  such as
    ;;   (nil = (and (not integer) (not ration) rational))
    ;;  and several more.  Even if we could encode all these relationships,
    ;;  there are more potential relationships every time a user's deftype
    ;;  is evaluated.  For example, it might be that
    ;;  (nil = (and user-type (not (cons string))))
    ;;  but we can't find that out as there's no way to iterate
    ;;  through all the user's type definitions and their expansions.
    ((smarter-subtypep (bdd-to-expr bdd) nil) ;; 0.03%
     *bdd-false*)
    ((smarter-subtypep t (bdd-to-expr bdd))
     *bdd-true*)
    (t ;; 1.7%
     bdd)))

(defmethod bdd-find-reduction (label (bdd lisp-type-bdd) reduction-rules)
  (declare (type list reduction-rules)
           (optimize (speed 3)))
  "Apply each of the REDUCTION-RULES to BDD.  Some of the reduction rules may
result in reducing the BDD to a simpler form.   If no reduction rule applies
then NIL is returned, otherwise the reduced BDD is returned.
Each element of REDUCTION-RULES is a plist having at least the keys
  :RELATION - a relation between two type specifiers, eg., #'SMARTER-SUBTYPEP
  :REDUCTION - a function from BDD->BDD, which normally returns either 
             the positive or negative child E.g., #'BDD-POSITIVE or #'BDD-NEGATIVE"
  (let ((reduced (reduce (lambda (bdd reduction-rule-plist)
                           (cond
                             ((typep bdd 'bdd-leaf)
                              bdd)
                             ((funcall (the function (getf reduction-rule-plist :relation)) label (bdd-label bdd))
                              (funcall (the function (getf reduction-rule-plist :reduction)) bdd))
                             (t
                              bdd)))
                         reduction-rules
                         :initial-value bdd)))
    (if (eq bdd reduced)
        nil
        reduced)))


(defun bdd-reduce (label bdd search)
  (declare (type (or lisp-type-bdd bdd-leaf) bdd)
           (type list search))
  "This function starts at a BDD and walks the bdd applying BDD-FIND-REDUNCTION 
to each node using the given LABEL and SEARCH."
  (bdd-walk bdd (lambda (bdd)
		  (bdd-find-reduction label bdd search)) :bdd-node-class 'lisp-type-bdd-node))

;; when converting a lisp-type-bdd to dnf, we need to remove subtypes
;; from every (OR ...) clause
(defmethod bdd-dnf-wrap ((bdd lisp-type-bdd) (op (eql 'or)) zero forms)
  (call-next-method bdd 'or zero (remove-subs forms)))

;; when converting a lisp-type-bdd to dnf, we need to remove supertypes
;; from every (AND ...) clause
(defmethod bdd-dnf-wrap ((bdd lisp-type-bdd) (op (eql 'and)) zero forms)
  (call-next-method bdd 'and zero (remove-supers forms)))

(defun check-table ()
  nil
  )

(defun remove-super-types (type-specs)
  (cond
    ((null type-specs)
     nil)
    ((null (cdr type-specs))
     type-specs)
    (t
     (let ((new (reduce (lambda (t1 specs)
                          (cond
                            ((exists t2 specs
                               (cached-subtypep t2 t1))
                             specs)
                            (t
                             (cons t1 (setof t2 specs
                                        (not (cached-subtypep t1 t2)))))))
                        type-specs :initial-value nil :from-end t)))
       ;; if there were no supers to remove, then return the original list
       ;; allowing the newly allocated one to be GC-ed.
       (if (equal new type-specs)
           type-specs
           new)))))

(defun bdd-subtypep (t-sub t-super)
  (declare (type bdd t-super t-sub))
  ;; TODO--isn't there a quicker way to find out whether (bdd-and-not A B)
  ;;   is false other than calculating the whole thing?
  (eq *bdd-false* (bdd-and-not t-sub t-super)))

(defun bdd-empty-type (bdd)
  (bdd-subtypep bdd *bdd-false*))

(defun bdd-disjoint-types-p (bdd1 bdd2)
  (bdd-empty-type (bdd-and bdd1 bdd2)))

(defun bdd-type-equal (t1 t2)
  (declare (type (or lisp-type-bdd-node bdd-leaf) t1 t2))
  (and (bdd-subtypep t1 t2)
       (bdd-subtypep t2 t1)))

(defun bdd-to-if-then-else-1 (bdd obj)
  "expand into worse-case exponentially large code as IF-THEN-ELSE, whose run-time is logrithmic."
  (labels ((expand (bdd)
             (typecase bdd
               (bdd-false nil)
               (bdd-true t)
               (lisp-type-bdd-node `(if (typep ,obj ',(bdd-label bdd))
                                        ,(expand (bdd-positive bdd))
                                        ,(expand (bdd-negative bdd)))))))
    (typecase bdd
      (bdd-false
       `(lambda (,obj)
          (declare (ignore ,obj))
          nil))
      (bdd-true
       `(lambda (,obj)
          (declare (ignore ,obj))
          t))
      (lisp-type-bdd-node
       `(lambda (,obj)
          ,(expand bdd))))))

(defun topological-sort (graph &key (test 'eql))
  ;; this function was taking verbatim from rosettacode.org
  ;; https://rosettacode.org/wiki/Topological_sort#Common_Lisp
  "Graph is an association list whose keys are objects and whose
values are lists of objects on which the corresponding key depends.
Test is used to compare elements, and should be a suitable test for
hash-tables.  Topological-sort returns two values.  The first is a
list of objects sorted toplogically.  The second is a boolean
indicating whether all of the objects in the input graph are present
in the topological ordering (i.e., the first value)."
  (let ((entries (make-hash-table :test test)))
    (flet ((entry (vertex)
             "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
             (multiple-value-bind (entry presentp) (gethash vertex entries)
               (if presentp entry
                 (setf (gethash vertex entries) (cons 0 '()))))))
      ;; populate entries initially
      (dolist (vertex graph)
        (destructuring-bind (vertex &rest dependencies) vertex
          (let ((ventry (entry vertex)))
            (dolist (dependency dependencies)
              (let ((dentry (entry dependency)))
                (unless (funcall test dependency vertex)
                  (incf (car ventry))
                  (push vertex (cdr dentry))))))))
      ;; L is the list of sorted elements, and S the set of vertices
      ;; with no outstanding dependencies.
      (let ((L '())
            (S (loop for entry being each hash-value of entries
                     using (hash-key vertex)
                     when (zerop (car entry)) collect vertex)))
        ;; Until there are no vertices with no outstanding dependencies,
        ;; process vertices from S, adding them to L.
        (do* () ((endp S))
          (let* ((v (pop S)) (ventry (entry v)))
            (remhash v entries)
            (dolist (dependant (cdr ventry) (push v L))
              (when (zerop (decf (car (entry dependant))))
                (push dependant S)))))
        ;; return (1) the list of sorted items, (2) whether all items
        ;; were sorted, and (3) if there were unsorted vertices, the
        ;; hash table mapping these vertices to their dependants
        (let ((all-sorted-p (zerop (hash-table-count entries))))
          (values (nreverse L)
                  all-sorted-p
                  (unless all-sorted-p
                    entries)))))))

;; (bdd-to-if-then-else-2 (bdd '(or (and sequence (not array))
;;                                      number
;;                               (and (not sequence) array))) 'X)

(defun bdd-to-if-then-else-2 (bdd obj)
  "expand into linear size code as LET*, whose runtime is linear"
  (let ((constraints (make-hash-table :test #'eq)))
    (labels ((calc-constraints (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (nth-value 1 (gethash bdd constraints))
                    (setf (gethash bdd constraints) nil))
                  (pushnew bdd (gethash (bdd-positive bdd) constraints nil) :test #'eq)
                  (pushnew bdd (gethash (bdd-negative bdd) constraints nil) :test #'eq)
                  (calc-constraints (bdd-positive bdd))
                  (calc-constraints (bdd-negative bdd))))))
      (calc-constraints bdd)
      ;; constraints is a hash table mapping BEFORE to a list of BDDS which which BEFORE must preceed in the sorted list.

      (let* ((nodes (topological-sort (let (collected)
                                        (maphash (lambda (key value)
                                                   (push (cons key value) collected)) constraints)
                                        collected) :test #'eq))
             (name-map (mapcar (lambda (node)
                                 (list node (gensym "N")))
                               nodes))
             (vars (mapcar (lambda (node)
                             (typecase node
                               (bdd-false
                                (list (cadr (assoc node name-map)) nil))
                               (bdd-true
                                (list (cadr (assoc node name-map)) t))
                               (bdd-node
                                (list (cadr (assoc node name-map))
                                      `(if (typep ,obj ',(bdd-label node))
                                           ,(cadr (assoc (bdd-positive node) name-map))
                                           ,(cadr (assoc (bdd-negative node) name-map)))))))
                           (reverse nodes))))
        `(lambda (,obj)
           (let* ,vars
             ,(caar (last vars))))))))



(defun bdd-to-if-then-else-3 (bdd obj)
  "expand into linear size code as LABELS, whose runtime is logrithmic and code size is linear"
  (let (bdd->name-mapping)
    (labels ((walk-bdd (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (assoc bdd bdd->name-mapping)
                    (push (list bdd (gensym)) bdd->name-mapping)
                    (walk-bdd (bdd-positive bdd))
                    (walk-bdd (bdd-negative bdd))))))
             (branch (bdd)
               (typecase bdd
                 (bdd-false nil)
                 (bdd-true t)
                 (lisp-type-bdd-node
                  (list (cadr (assoc bdd bdd->name-mapping))))))
             (label-function (bdd)
               (typecase bdd
                 (bdd-node
                  `(,(cadr (assoc bdd bdd->name-mapping)) ()
                    (if (typep ,obj ',(bdd-label bdd))
                        ,(branch (bdd-positive bdd))
                        ,(branch (bdd-negative bdd))))))))
      (walk-bdd bdd)
      `(lambda (,obj)
         (labels ,(mapcar #'label-function (mapcar #'car bdd->name-mapping))
           ,(branch bdd))))))

;; (bdd-to-if-then-else-3 (bdd '(or (and sequence (not array))
;;                                      number
;;                               (and (not sequence) array))) 'X)
      
      

(defun bdd-to-if-then-else-4 (bdd obj)
  "expand into linear size code as TAGBODY/GO, whose runtime is logrithmic and code size is linear"
  (let (bdd->name-mapping (num 0))
    (labels ((walk-bdd (bdd)
               (typecase bdd
                 (bdd-false)
                 (bdd-true)
                 (bdd-node
                  (unless (assoc bdd bdd->name-mapping)
                    (push (list bdd (incf num)) bdd->name-mapping)
                    (walk-bdd (bdd-positive bdd))
                    (walk-bdd (bdd-negative bdd))))))
             (branch (bdd)
               (typecase bdd
                 (bdd-false `(return nil))
                 (bdd-true `(return t))
                 (bdd-node
                  `(go ,(cadr (assoc bdd bdd->name-mapping))))))
             (label-function (bdd)
               (typecase bdd
                 (bdd-node
                  `(,(cadr (assoc bdd bdd->name-mapping))
                    (if (typep ,obj ',(bdd-label bdd))
                        ,(branch (bdd-positive bdd))
                        ,(branch (bdd-negative bdd))))))))
      (walk-bdd bdd)
      `(lambda (,obj)
         (block nil
           (tagbody 
              ,@(mapcan #'label-function
                        (mapcar #'car (reverse bdd->name-mapping)))))))))

(defun bdd-typep (obj type-specifier)
  "This function has the same syntax as CL:TYPEP, but using a BDD based algorithm " 
  (bdd-type-p obj (ltbdd type-specifier)))

(define-compiler-macro bdd-typep (obj type-specifier)
  (typecase type-specifier
    ((cons (eql quote))
     (ltbdd-with-new-hash (&aux (bdd (ltbdd (cadr type-specifier))))
       `(funcall ,(bdd-to-if-then-else-3 bdd (gensym)) ,obj)))
    (t
     `(typep ,obj ,type-specifier))))


;; (funcall (compiler-macro-function 'bdd-typep) '(bdd-typep X '(or (and sequence (not array))
;;                                       number
;;                                (and (not sequence) array))) nil)


(defun bdd-type-p (obj bdd)
  "Similar semantics to TYPEP but takes a bdd rather than a type-specifier.
If a CL type specifier is given as 2nd argument, it is interpreted as
the corresponding BDD object, via a call to the function BDD.
Returns T if the OBJ of an element of the specified type,
Returns NIL otherwise."
  (etypecase bdd
    (bdd-false
     nil)
    (bdd-true
     t)
    (lisp-type-bdd-node
     (bdd-type-p obj 
                 (if (typep obj (bdd-label bdd))
                     (bdd-positive bdd)
                     (bdd-negative bdd))))
    (t
     (bdd-type-p obj (the lisp-type-bdd (ltbdd bdd))))))

(defun bdd-reduce-lisp-type (type)
    "Given a common lisp type designator such as (AND A (or (not B) C)), 
convert it to DNF (disjunctive-normal-form)"
  (bdd-to-dnf (ltbdd type)))

(defun %mdtd-bdd (type-specifiers)
  ;;(declare (optimize (debug 0) (speed 3))) ;; optimize tail call 
  (ltbdd-with-new-hash (&aux (bdds (remove-if #'bdd-empty-type (mapcar #'ltbdd type-specifiers))))
    (declare (type list bdds))
    (labels ((try (bdds disjoint-bdds &aux (bdd-a (car bdds)))
               (declare (type (or null bdd) bdd-a))
               (cond
                 ((null bdds)
                  disjoint-bdds)
                 (t
                  (flet ((reduction (acc bdd-b &aux (bdd-ab (bdd-and bdd-a bdd-b)))
                           (declare (type bdd bdd-b bdd-ab)
                                    (type (cons (member t nil) (cons list (eql nil))) acc))
                           (destructuring-bind (all-disjoint? bdd-set) acc
                             (declare (type (member t nil) all-disjoint?)
                                      (type list bdd-set)
                                      #+sbcl (notinline union))
                             (cond
                               ((bdd-empty-type bdd-ab)
                                ;; If the intersection of A and B is the empty type,
                                ;; then we don't need to calculate A\B and B\A because
                                ;; we know that A\B = A and B\A = B.
                                ;; Thus we simply add B to the bdd-set being accumulated.
                                (list all-disjoint? (adjoin bdd-b bdd-set)))
                               (t
                                ;; If the interesction of A and B is non empty,
                                ;; then we augment bdd-set with at most 3 types.  Looking at
                                ;; {AB, A\B, B\A} \ {{}}, some of which might be equal, so we
                                ;; remove duplicates, and accumulate also all-disjoint?=nil because
                                ;; we've found something A is not disjoint with.
                                (list nil
                                      (union (remove-duplicates
                                              (remove-if #'bdd-empty-type
                                                         (list bdd-ab
                                                               (bdd-and-not bdd-a bdd-ab)
                                                               (bdd-and-not bdd-b bdd-ab))))
                                             bdd-set)))))))
                    (destructuring-bind (all-disjoint? bdd-set)
                        (reduce #'reduction (cdr bdds) :initial-value '(t nil))
                      (try bdd-set
                           (if all-disjoint?
                               (pushnew bdd-a disjoint-bdds)
                               disjoint-bdds))))))))
      (try bdds nil))))

(defun bdd-collect-terms (bdd)
  (declare (type bdd bdd))
  "Return a list of bdds each of which has one path from the top to a t leaf.
The union, bdd-or, of the element of the return list represents the equivalent
type as the given bdd.  Otherwise stated, if the given bdd is assumed to be a sum
of min-terms, this function returns a list of the min-terms."
  (labels ((recure (term)
             (etypecase term
               (bdd-true
                (list term))
               (bdd-false
                nil)
               (bdd-node
                (nconc (mapcar (lambda (positive)
                                 (bdd-ensure-node (bdd-label term) positive *bdd-false* :bdd-node-class 'lisp-type-bdd-node))
                               (recure (bdd-positive term)))
                       (mapcar (lambda (negative)
                                 (bdd-ensure-node (bdd-label term) *bdd-false* negative :bdd-node-class 'lisp-type-bdd-node))
                               (recure (bdd-negative term))))))))
    (recure bdd)))

(defun mdtd-bdd-strong (type-specifiers)
  (mdtd-bdd type-specifiers :bdd-hash-strength :strong))

(defun mdtd-bdd-weak (type-specifiers)
  (mdtd-bdd type-specifiers :bdd-hash-strength :weak))

(defun mdtd-bdd-weak-dynamic (type-specifiers)
  (mdtd-bdd type-specifiers :bdd-hash-strength :weak-dynamic))

(defun mdtd-bdd (type-specifiers &key ((:bdd-hash-strength *bdd-hash-strength*) :weak-dynamic))
  (when type-specifiers
    (caching-types
      (mapcar #'bdd-to-dnf
              (%mdtd-bdd type-specifiers)))))

(defun bdd-find-dup-bdds (bdds)
  "A debugging function.  It can be used to find whether two (or more) bdds
in the given list have the same dnf form."
  (let ((hash (make-hash-table :test #'equal))
        dups)
    (dolist (bdd bdds)
      (push bdd (gethash (bdd-to-dnf bdd) hash nil)))
    (maphash (lambda (dnf bdds)
               (declare (ignore dnf))
               (when (cdr bdds)
                 (push (cons (bdd-to-dnf (car bdds)) bdds) dups)))
             hash)
    dups))


