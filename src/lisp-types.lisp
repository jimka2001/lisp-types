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

(in-package   :lisp-types)

(defvar *ambiguous-subtypes* nil "Special variable containing ambiguous subtype information which has already been warned about.")

(define-condition ambiguous-subtype (style-warning)
  ((sub   :type (or symbol nil cons) :initarg :sub :initform :UNINITIALIZED)
   (super :type (or symbol nil cons) :initarg :super :initform :UNINITIALIZED)
   (consequence :type (or string nil) :initarg :consequence :initform nil))
  (:documentation "Warning raised when unable to determine the subtype relationship.")
  (:report (lambda (condition stream)
	     (format stream "Cannot determine whether ~S is a subtype of ~S"
		     (slot-value condition 'sub)
		     (slot-value condition 'super))
	     (when (slot-value condition 'consequence)
	       (format stream ", ~A" (slot-value condition 'consequence))))))

(defun warn-ambiguous-subtype (&rest plist &key sub super consequence)
  "Issue a warning, via WARN, if the given pair of types has a subtype
relation which cannot be determined by SUBTYPEP.  However, the warning
is supressed if this situation has happened already and memoized into
*AMBIGUOUS-SUBTYPES*."
  (unless (member plist *ambiguous-subtypes* :test #'equal)
    (push plist *ambiguous-subtypes*)
    (warn 'ambiguous-subtype :sub sub :super super
			     :consequence consequence)))
		     
(defun hash-to-list (hash &aux list)
  "HASH is a hashtable with test=EQUAL which has been used with ENTER-CONSES.
  HASH-TABLE-returns the list of lists which ENTER-CONSES has accumulated."
  (maphash (lambda (key value)
	     (declare (ignore value))
	     (push key list)) hash)
  list)

(defun enter-conses (hash object)
  "HASH is a hash-table which uses EQUAL as test.
   OBJECT is any lisp object.
   ENTER-CONSES returns an object which is EQUAL to the given OBJECT,
   but subsequent times ENTER-CONSES is called with another such EQUAL
   object, the value returned is EQ.
   E.g., If (EQUAL A B), then (ENTER-CONSES hash A) and (ENTER-CONSES hash B)
   returns values which are EQ to each other.
   In addition, The elements of the given list (if OBJECT is indeed a list)
   share the same EQ property.  I.e., if A is a list, and B is a list and A and B
   contain elements which are EQUAL to each other, they the corresponding values
   in the return value will be EQ."
  (cond
    ((atom object)
     object)
    ((gethash object hash))
    (t
     (setf object (cons (enter-conses hash (car object))
			(enter-conses hash (cdr object))))
     (setf (gethash object hash) object))))


(defmacro multiple-value-destructuring-bind (destructuring-lambda-list form &body body)
  `(destructuring-bind ,destructuring-lambda-list (multiple-value-list ,form)
     ,@body))


(defvar *reduce-member-type* t)




(defun slow-smarter-subtypep (t1 t2)
  (declare ;;(optimize (speed 3) (compilation-speed 0))
   )
  (cond
    ((and *reduce-member-type*
          (typep t1 '(cons (member eql member)))) ; (eql obj) or (member obj1 ...)
     (list (every #'(lambda (obj)
                      (declare #+sbcl (notinline typep))
                      (ignore-errors (typep obj t2)))
                  (cdr t1))
           t))
    ;; T1 <: T2 <==> not(T2) <: not(T1)
    ((and (typep t1 '(cons (eql not)))
          (typep t2 '(cons (eql not))))
     (multiple-value-list (smarter-subtypep (cadr t2) (cadr t1))))
    ;; T1 <: T2 <==> not( T1 <= not(T2))
    ((and (typep t2 '(cons (eql not)))
          (smarter-subtypep t1 (cadr t2)))
     '(nil t))
    ;; T1 <: T2 <==> not( not(T1) <= T2)
    ((and (typep t1 '(cons (eql not)))
          (smarter-subtypep (cadr t1) t2))
     '(nil t))
    ;; (subtypep '(and cell-error type-error) 'cell-error)
    ((and (typep t1 '(cons (eql and)))
          (exists t3 (cdr t1)
                  (smarter-subtypep t3 t2)))
     '(t t))
    ;;(subtypep '(AND ARITHMETIC-ERROR CELL-ERROR) nil)
    ((and (typep t1 '(cons (eql and)))
          (smarter-subtypep t2 nil)
          (exists t3 (cdr t1)
            (exists t4 (cdr t1)
              (and (not (eq t3 t4))
                   (disjoint-types-p t3 t4)))))
     '(t t))
    ;; (subtypep 'arithmetic-error  '(not cell-error))
    ((and (typep t2 '(cons (eql not)))
          (disjoint-types-p t1 (cadr t2))) ;; (disjoint? 'arithmetic-error 'cell-error)
     '(t t))
    ;; this is the dual of the previous clause, but it appears sbcl gets this one right
    ;;   so we comment it out
    ;; ((and (typep t2 '(cons (eql or)))
    ;;       (exists t3 (cdr t2)
    ;;         (smarter-subtypep t1 t3)))
    ;;  (values t t))
    (t
     '(nil nil))))

;; A performance analysis of mdtd-bdd-graph shows that a HUGE portion of the time
;; is being spent in subtypep, and a major source of calls to subtypep is smarter-subtypep.
;; It seems that runs containing lots of type specifiers for which subtypep returns nil,nil
;; is a source of bad performance.  So *subtype-hash* is introduced to cache some of these
;; cases.  If smarter-subtypep returns nil,nil, then we remember this case in *subtype-hash*
;; in order to avoid recursive calls to smarter-subtypep and subtypep in future calls
;; with the same arguments.
;;
;; TODO need to update some calls to subtypep to use smarter-subtypep instead.
(def-cache-fun (smarter-subtypep call-with-subtype-hash) (t1 t2)
  "The sbcl subtypep function does not know that (eql :x) is a subtype of keyword,
this function SMARTER-SUBTYPEP understands this."
  (declare ;;(optimize (speed 3) (compilation-speed 0))
   )
  (let ((t1 (type-to-dnf t1))
        (t2 (type-to-dnf t2)))
    (multiple-value-bind (T1<=T2 OK) (cached-subtypep t1 t2)
      (cond
        (OK
         (values T1<=T2 t))
        (t
         (apply #'values (slow-smarter-subtypep t1 t2)))))))

(defun xor (a b)
  (or (and a (not b))
      (and (not a) b)))

(defun void-type-p (type)
  (cached-subtypep type nil))

(defun universal-type-p (type)
  (cached-subtypep t type))

(def-cache-fun (disjoint-types-p call-with-disjoint-hash) (T1 T2)
  "Two types are considered disjoint, if their interseciton is empty,
i.e., is a subtype of nil."
  (apply #'values (slow-disjoint-types-p T1 T2)))

(defun slow-disjoint-types-p (t1 t2
                              &aux
                                (T1 (type-to-dnf t1))
                                (T2 (type-to-dnf t2))
                                X Y
                                (t12 (type-to-dnf (list 'and T1 T2))))
  "SLOW-DISJOINT-TYPES-P returns a list of two booleans, whereas DISJOINT-TYPES-P returns
 the two corresponding VALUES."
  (declare #+sbcl (notinline subsetp))
  (multiple-value-bind (disjointp OK) (cached-subtypep t12 nil)
    (cond
      (OK
       (cons disjointp '(t)))
      ((and (symbolp T1)
            (symbolp T2)
            (find-class T1 nil)
            (find-class T2 nil))
       ;; e.g., ARITHMETIC-ERROR vs CELL-ERROR
       (list (not (dispatch:specializer-intersections (find-class T1) (find-class T2)))
             t))
      ((subsetp '((t t) (nil t))
                (list (setf X (multiple-value-list (smarter-subtypep T1 T2)))
                      (multiple-value-list (smarter-subtypep T2 T1)))
                :test #'equal)
       ;; Is either  T1<:T2 and not T2<:T1
       ;;    or      T2<:T1 and not T1<:T2 ?
       ;; if so, then one is a propert subtype of the other.
       ;; thus they are not disjoin.t
       '(nil t))
               
      ((and (typep T1 '(cons (eql not)))
            (typep T2 '(cons (eql not)))
            (smarter-subtypep t (list 'and (cadr T1) (cadr T2)))
            ;; (not (void-type-p (cadr T1)))
            ;; (not (void-type-p (cadr T2)))
            (disjoint-types-p (cadr T1) (cadr T2)))
       '(nil t))

      ;;  T1 ^ T2 = 0 ==> !T1 ^ T2 != 0 if T1!=1 and T2 !=0
      ;; !T1 ^ T2 = 0 ==>  T1 ^ T2 != 0 if T1!=0 and T2 !=0
      ((and (typep T1 '(cons (eql not)))
            (not (void-type-p (cadr T1)))
            (not (void-type-p T2))
            (disjoint-types-p (cadr T1) T2))
       '(nil t))
      ;; T1 ^  T2 = 0 ==> T1 ^ !T2 != 0  if T1!=0 and T2!=1
      ;; T1 ^ !T2 = 0 ==> T1 ^  T2 != 0  if T1!=0 and T2!=0
      ((and (typep T2 '(cons (eql not))) 
            (not (void-type-p T1))
            (not (void-type-p (cadr T2)))
            (disjoint-types-p T1 (cadr T2)))
       '(nil t))
      ;; e.g., (disjoint-types-p (not float) number) ==> (nil t)
      ;;       (disjoint-types-p (not number) float) ==> (t t)
      ((and (typep T1 '(cons (eql not)))
            (setf Y (multiple-value-list (smarter-subtypep (cadr T1) T2)))
            (setf X (multiple-value-list (smarter-subtypep T2 (cadr T1))))
            (subsetp '((t t) (nil t)) (list X Y) :test #'equal))
       (list (car X) t))
      ;; e.g., (disjoint-types-p float (not number)) ==> (t t)
      ;;       (disjoint-types-p number (not float)) ==> (nil t)
      ((and (typep T2 '(cons (eql not)))
            (setf Y (multiple-value-list (smarter-subtypep T1 (cadr T2))))
            (setf X (multiple-value-list (smarter-subtypep (cadr T2) T1)))
            (subsetp '((t t) (nil t)) (list X Y) :test #'equal))
       (list (car Y) t))
      ((or (smarter-subtypep T1 T2)
           (smarter-subtypep T2 T1))
       '(nil t))
      (t
       '(nil nil)))))

(def-cache-fun (equivalent-types-p call-with-equiv-hash) (T1 T2)
               "Two types are considered equivalent if each is a subtype of the other."
  (let ((T1 (type-to-dnf T1))
        (T2 (type-to-dnf T2)))
    (if (equal T1 T2)
	'(t t)
	(multiple-value-bind (T1<=T2 okT1T2) (smarter-subtypep T1 T2)
	  (if (and okT1T2 (not T1<=T2))
	      (values nil t)  ; no need to call subtypep a second time
	      (multiple-value-bind (T2<=T1 okT2T1) (smarter-subtypep T2 T1)
		(if (and okT2T1 (not T2<=T1))
		    (values nil t)
		    (values (and T1<=T2 T2<=T1) (and okT1T2 okT2T1)))))))))

(defmacro caching-types (&body body)
  "Evaluate the given body with several subtype related caches enabled."
  `(call-with-disjoint-hash
    (lambda ()
      (call-with-subtype-hash
       (lambda ()
         (call-with-equiv-hash
          (lambda ()
            (call-with-subtypep-cache
             (lambda ()
               ,@body)))))))))

(defun set-equalp (set-a set-b &key (test #'equal))
  (declare #+sbcl (notinline set-exclusive-or))
  (not (set-exclusive-or set-a set-b :test test)))

(defun set-subsetp (set-sub set-super &key (test #'equal))
  (every (lambda (s)
	   (member s set-super :test test))
	 set-sub))

(defmacro pattern-bind (destructuring-lambda-list object &rest body)
  "Like destructuring-bind, but if you use a variable named _ (one or more times)
an automatic (declare (ignore _)) will be generated at the head of the body. 
WARNING: this function is not robust enough to destinguish _ used a variable declaration
vs in an evaluation position; it simply checks whether the lambda-list contains the
symbol _ somewhere (recursively)."
  (labels ((find_ (obj)
	     (cond ((eq '_ obj)
		    t)
		   ((listp obj)
		    (some #'find_ obj)))))
    (if (find_ destructuring-lambda-list)
	`(destructuring-bind ,destructuring-lambda-list ,object
	   (declare (ignore _))
	   ,@body)
	`(destructuring-bind ,destructuring-lambda-list ,object
	   ,@body))))

(defun partition-by-predicate (predicate data)
  (let (true-elements false-elements)
    (dolist (element data)
      (if (funcall predicate element)
	  (push element true-elements)
	  (push element false-elements)))
    (values true-elements false-elements)))

(defun alphabetize (patterns)
  "descructively sort a list of patterns into a canonical order."
  (declare (type list patterns))
  (sort patterns #'cmp-objects))

(defun alphabetize-type (type)
  (declare ;;(optimize (speed 3) (compilation-speed 0))
   )
  (cond
    ((atom type)
     type)
    ((member (car type) '(and or not))
     (cons (car type) (alphabetize
		       (mapcar #'alphabetize-type (remove-duplicates (remove-duplicates (cdr type) :test #'eq)
								     :test #'equal)))))
    ((eq 'cons (car type))
     (cons 'cons (mapcar #'alphabetize-type (cdr type))))
    ((eq 'member (car type))
     (cons 'member (alphabetize (copy-list (cdr type)))))
    (t
     type)))

(defmacro rule-case (object &body clauses)
"return the value of the first clause-value not EQUAL to OBJECT of the first clause
whose test is true, otherwise return OBJECT.
I.e., the test of each clause is evaluated until one test is true, 
once a true test has been found the body of the clause is evaluated.  
If the body evaluates to somethong other than OBJECT
that value is returned.  However, if the body evaluates EQUAL to OBJECT, then
the search continues for the next clause whose test is true.
E.g.  (rule-case 12 ;; OBJECT
         ((= 1 2)  ;; not true
          ...)
         ((= 42 42) ;; true
          12) ;; not returned because 12 == OBJECT
         ((= 42 42) ;; true
          13) ;; returned because it is different than OBJECT
         (... ;; remaining tests ignored
          ))"
  (let ((new (gensym "NEW"))
        (old (gensym "OLD")))
    (labels ((expand-clause (clause)
               (destructuring-bind (test &body body) clause
                 (assert body () "invalid test/body used in RULE-CASE: ~A" clause)
                 `(cond ((not (equal ,new ,old)) ;; when old is still EQUAL to true, then try the next test
                         ;; skip the test
                         nil)
                        (,test
                         (setf ,new (progn ,@body)))
                        (t
                         nil))
                 ;; `(when (and (equal ,new ,old) ;; when old is still EQUAL to true, then try the next test
                 ;;             ,test)
                 ;;   (setf ,new (progn ,@body)))
                 ))
             (expand-clauses ()
               (mapcar #'expand-clause clauses)))
      `(let* ((,new ,object)
              (,old ,new))
         ,@(expand-clauses)
         ,new))))

(proclaim '(inline sub-super))

(defun sub-super (types)
  (declare ;;(optimize (speed 3) (compilation-speed 0))
   )
  (loop :for tail :on types
	:do (when (cdr types)
	      (loop :for t1 :in (cdr tail)
		    :with t2 = (car tail)
		    :do
		       (cond ((cached-subtypep t1 t2)
			      (return-from sub-super (values t t1 t2)))
			     ((cached-subtypep t2 t1)
			      (return-from sub-super (values t t2 t1)))))))
  (values nil))

(defun remove-supers (types)
  "Given a list of types, return a new list with with all elements removed which specifies a supertype of something else in the list.  If the list contains two elements which specify the same type, only one of them is removed, unless it is a supertype of something else in the list in which case both are removed."
  (labels ((recure (types acc)
	     (cond
	       ((null types)
		(nreverse acc))
	       ((exists type acc
		  (cached-subtypep type (car types)))
		(recure (cdr types) acc))
	       ((exists type (cdr types)
		  (cached-subtypep type (car types)))
		(recure (cdr types) acc))
	       (t
		(recure (cdr types) (cons (car types) acc))))))
    (recure types nil)))

(defun remove-subs (types)
  "Given a list of types, return a new list with with all elements removed which specifies a subtype of something else in the list.  If the list contains two elements which specify the same type, only one of them is removed, unless it is a subtype of something else in the list in which case both are removed."
  (labels ((recure (types acc)
	     (cond
	       ((null types)
		(nreverse acc))
	       ((exists type acc
		  (cached-subtypep (car types) type))
		(recure (cdr types) acc))
	       ((exists type (cdr types)
		  (cached-subtypep (car types) type))
		(recure (cdr types) acc))
	       (t
		(recure (cdr types) (cons (car types) acc))))))
    (recure types nil)))


