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




(in-package :lisp-types-test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :lisp-types :package-into :lisp-types-test))



(defun test ()
  (run-package-tests :lisp-types-test))

(define-test type/reduce-b
  (assert-true (equal (reduce-lisp-type '(AND ARITHMETIC-ERROR (NOT CELL-ERROR)))

                      'arithmetic-error))
    (assert-true (equal (reduce-lisp-type '(OR ARITHMETIC-ERROR (NOT CELL-ERROR)))

                        '(NOT CELL-ERROR))))


(define-test type/reduce-a
  (assert-true (equal (reduce-lisp-type '(and (and (integer 0) (not (integer 0 3)))
                                          (and fixnum (not (integer 0 3)))))
                      '(and fixnum (integer 0) (not (integer 0 3)))))
  (assert-true (equal (reduce-lisp-type '(and ARITHMETIC-ERROR CELL-ERROR))
                      nil))
  (assert-true (equal (reduce-lisp-type '(AND
                          ATOM
                          CONDITION
                          CELL-ERROR
                                          ARITHMETIC-ERROR))
                      nil))
  (assert-true (equal (reduce-lisp-type '(AND
                                          (NOT (AND CONDITION (NOT CELL-ERROR))) ATOM
                                           CONDITION CELL-ERROR
                                          ARITHMETIC-ERROR))
                      nil))
  (assert-true (equal (reduce-lisp-type '(AND
                                          (NOT (AND CONDITION (NOT CELL-ERROR))) ATOM
                                           (AND CONDITION CELL-ERROR)
                                          ARITHMETIC-ERROR))
                      nil))
  (assert-true (equal (reduce-lisp-type '(AND
                                          (AND (NOT (AND CONDITION (NOT CELL-ERROR))) ATOM)
                                           (AND CONDITION CELL-ERROR)
                                          ARITHMETIC-ERROR))
                      nil))
  (assert-true (equal (reduce-lisp-type '(AND
                                          (AND (AND (NOT (AND CONDITION (NOT CELL-ERROR))) ATOM)
                                           (AND CONDITION CELL-ERROR))
                                          ARITHMETIC-ERROR))
                      nil)))

(define-test type/reduce-compound
  ;; array
  (assert-true (equal (reduce-lisp-type '(array (and integer number) (3)))
		      '(array integer (3))))
  (assert-true (equal (reduce-lisp-type '(array * (3)))
		      '(array * (3))))

  ;; base-string
  (assert-true (equal (reduce-lisp-type '(base-string *))
		      'base-string))

  ;; bit-vector
  (assert-true (equal (reduce-lisp-type '(bit-vector *))
		      'bit-vector))

  (assert-true (equal (reduce-lisp-type '(bit-vector 3))
		      '(bit-vector 3)))

  ;; complex
  (assert-true (equal (reduce-lisp-type '(complex (and number real)))
		      '(complex real)))
  (assert-true (equal (reduce-lisp-type '(complex *))
		      'complex ))

  ;; simple-array
  (assert-true (equal (reduce-lisp-type '(simple-array (and number real) (3)))
		      '(simple-array real (3))))

  ;; vector
  (assert-true (equal (reduce-lisp-type '(vector (and number real)))
		      '(vector real)))

  )


(define-test type/reduce-cons
  (assert-true (equal (reduce-lisp-type '(cons (and float number) (or string (not string))))
		      '(cons float t)))
  (assert-true (equal (reduce-lisp-type '(cons * *))
		      'cons))
  (assert-true (equal (reduce-lisp-type '(cons (and float number) *))
		      '(cons float)))
  (assert-true (equal (reduce-lisp-type '(cons * (and float number)))
		      '(cons * float))))

(define-test type/reduce-function
  (assert-true (equal (reduce-lisp-type '(function (integer integer) integer))
		      '(function (integer integer) integer)))
  (assert-true (equal (reduce-lisp-type '(function ((and integer integer) integer) integer))
		      '(function (integer integer) integer)))

  (assert-true (equal (reduce-lisp-type '(function ((and integer integer) (and integer integer)) (and integer integer)))
		      '(function (integer integer) integer)))
  ;; test some optional arguments &optional &key &rest etc

  ;; &optional
  (assert-true (equal (reduce-lisp-type '(function (&optional) (and list cons)))
		      '(function (&optional) cons)))

  (assert-true (equal (reduce-lisp-type '(function (&optional (and integer number)) (and list cons)))
		      '(function (&optional integer) cons)))
  
  ;; &rest
  (assert-true (equal (reduce-lisp-type '(function (&rest (and integer number)) (and list cons)))
		      '(function (&rest integer) cons)))

  (assert-error error (reduce-lisp-type '(function (&rest t t))))


  ;; &key
  (assert-true (equal (reduce-lisp-type '(function (&key) t))
		      '(function (&key) t)))

  (assert-true (equal (reduce-lisp-type '(function (&key (x (and integer number))) (and list cons)))
		      '(function (&key (x integer)) cons)))

  ;; combining &optional &key &rest
  (assert-true (equal (reduce-lisp-type
		       '(function ((and integer number)
				   &optional (and integer number) (and integer number)
				   &rest (and integer number)
				   &key (x (and integer number)) (y (and integer number)))
			 (and list cons)))
		      '(function (integer
				  &optional integer integer
				  &rest integer
				  &key (x integer) (y integer))
			cons)))

  )

;; test function subtypes
;; (define-test type/function-subtypes
;;   ;; If   T1 <: S1  and S2 <: T2
;;   ;; then S1->S2 <: T1->T2
;;
;;   ;; If  in-sub <: in-super   and out-sub <: out-super
;;   ;; then in-super -> out-sub <: in-sub -> out-super
;;
;;   (let ((types '(number real integer))) 
;;     (dolist (T1 types)
;;       (dolist (T2 types)
;; 	(dolist (S1 types)
;; 	  (dolist (S2 types)
;; 	    (when (and (subtypep T1 S1)
;; 		       (subtypep S2 T2))
;; 	      (assert-true (subtypep `(function (,S1) ,S2)
;; 				     `(function (,T1) ,T2))))))))))


(define-test type/reduce-lisp-type
  (flet ((reduce-lisp-type (type)
	   (reduce-lisp-type type)))
    (assert-true (equal (reduce-lisp-type '(and))
			t))
    (assert-true (equal (reduce-lisp-type '(or))
			nil))
    (assert-true (equal (reduce-lisp-type '(and float))
			'float))
    (assert-true (equal (reduce-lisp-type '(or float))
			'float))
    (assert-true (equal (reduce-lisp-type '(and float t))
			'float))
    (assert-true (equal (reduce-lisp-type '(and t float))
			'float))
    (assert-true (equal (reduce-lisp-type '(or float nil))
			'float))
    (assert-true (equal (reduce-lisp-type '(or nil float))
			'float))
    (assert-true (equal (reduce-lisp-type '(not nil))
			t))
    (assert-true (equal (reduce-lisp-type '(not t))
			nil))
    (assert-true (equal (reduce-lisp-type '(not (not float)))
			'float))
    (assert-true (equal (reduce-lisp-type '(not (or float string)))
			'(and (not float) (not string))))
    (assert-true (equal (reduce-lisp-type '(not (and float string)))
			t)) ;; because (and float string) is nil
    ))

(define-test type/reduce-type-nil
  (assert-true (eql nil
		    (reduce-lisp-type 
		     '(and (not t)
		           (and (not keyword) (eql :a)))))))

(defclass AB-247 () ())
(defclass A1-247 (AB-247) ())
(defclass B1-247 (AB-247) ())
(defclass AB1-247 (A1-247 B1-247) ())
(defclass C-247 () ())
(defclass D-247 () ())
(defclass E-247 () ())
(defclass :F-247 () ())
(define-test type/reduce-lisp-type2

  (assert-false (sb-mop:class-direct-subclasses (find-class 'C-247)))
  (assert-false (sb-mop:class-direct-subclasses (find-class 'D-247)))
  (assert-false (sb-mop:class-direct-subclasses (find-class 'E-247)))
  (assert-false (sb-mop:class-direct-subclasses (find-class ':F-247)))
  (assert-true (intersection (sb-mop:class-direct-subclasses (find-class 'A1-247))
                             (sb-mop:class-direct-subclasses (find-class 'B1-247))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or A1-247 (and A1-247 B1-247 C-247 D-247) E-247))
                                   '(or E-247 A1-247)))
  (let ((un-interned (gensym)))
    (assert-true (equivalent-types-p (reduce-lisp-type `(or (and A1-247 B1-247)
                                                            (and A1-247 B1-247 C-247 D-247)
                                                            E-247 ,un-interned))
                                     `(or ,un-interned E-247 (and B1-247 A1-247)))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A1-247 B1-247) (and A1-247 B1-247 C-247 D-247) E-247 :F-247))
                                   '(or :f-247 E-247 (and B1-247 A1-247))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or A1-247 (and (not A1-247) B1-247)))
                                   '(or B1-247 A1-247)))
  (assert-true (equivalent-types-p (reduce-lisp-type'(or (and (not A1-247) B1-247) A1-247))
                                   '(or B1-247 A1-247)))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (not A1-247) (and A1-247 B1-247)))
                                   '(or B1-247 (not A1-247))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A1-247 B1-247) (not A1-247)))
                                   '(or B1-247 (not A1-247)))))

(defclass W-282 () ())
(defclass A-282 () ())
(defclass B-282 () ())
(defclass C-282 () ())
(defclass U-282 () ())
(defclass V-282 () ())
(defclass X-282 () ())
(defclass Y-282 () ())
(defclass Z-282 () ())
(defclass join-282 (W-282 A-282 B-282 C-282 U-282 V-282 X-282 Y-282 Z-282) ())
(define-test type/consensus-theorem
  (assert-true (reduce (lambda (classes class-name)
                         (intersection classes
                                       (sb-mop:class-direct-subclasses (find-class class-name))))
                       '(W-282 A-282 B-282 C-282 U-282 V-282 X-282 Y-282 Z-282)
                       :initial-value (sb-mop:class-direct-subclasses (find-class 'W-282))))
          
  (assert-true (equivalent-types-p (reduce-lisp-type '(or W-282 (and A-282 B-282) X-282
                                                       Y-282 (and (not A-282) C-282)
                                                       Z-282 (and B-282 C-282)))
                                   '(OR X-282 Y-282 W-282 Z-282 (AND A-282 B-282) (AND C-282 (NOT A-282)))))
					
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A-282 B-282)
                                                       (and (not A-282) C-282)
                                                       (and B-282 C-282)))
                                   '(OR (AND A-282 B-282) (AND C-282 (NOT A-282)))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A-282 B-282)
                                                       (and B-282 C-282)
                                                       (and (not A-282) C-282)))
                                   '(OR (AND A-282 B-282) (AND C-282 (NOT A-282)))))
  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A-282 B-282)
                                                       (and B-282 C-282)
                                                       (and C-282 (not A-282))))
                                   '(OR (AND A-282 B-282) (AND C-282 (NOT A-282)))))

  (assert-true (equivalent-types-p (reduce-lisp-type '(or (and A-282 U-282 V-282)
                                                       (and V-282 W-282 (not A-282))
                                                       (and V-282 W-282 U-282)))
                                   '(OR (AND A-282 U-282 V-282) (AND V-282 W-282 (NOT A-282)))))
  (assert-true (member (reduce-lisp-type '(or (and A-282 U-282 V-282)
                                           (and (not A-282) V-282 W-282)
                                           (and U-282 V-282 W-282)))
                       
                       '((OR (AND A-282 U-282 V-282) (AND V-282 W-282 (NOT A-282))))
                       :test #'equivalent-types-p)))

(define-test test/side-effect-2
  (let ((n 0))
    (flet ((my-equal (a b)
	     (= a b)))
      (assert-true (my-equal 42
			     (progn
			       (format t "incrementing n~%")
			       (incf n)
			       42)))
      (assert-true (equal n 1)))))

(define-test test/side-effect
  (let ((n 0))
    (assert-true (equal 42
			(progn
			  (format t "incrementing n~%")
			  (incf n)
			  42)))
    (assert-true (equal n 1))))

(define-test type/rule-case
  (let ((n 0))
    (assert-true (equal (lisp-types::rule-case (+ 4 5)
			  (t
			   (incf n)
			   (format t "1 n=~A~%" n)
			   9)
			  ((= 1 n)
			   (incf n)
			   (format t "2 n=~A~%" n)
			   9)
			  ((= 2 n)
			   (incf n)
			   (format t "3 n=~A~%" n)
			   0))
			0))
    (assert-true (equal 3 n))))
				  
(define-test type/reduce-member
  (assert-true (equal (reduce-lisp-type '(member))
		      nil))
  (assert-true (equal (reduce-lisp-type '(and (member 1 2 3)
					  (member 2 3 4)))
		      '(member 2 3)))
  (assert-true (equal (reduce-lisp-type '(and (not (member 1 2 3))
					  (not (member 2 3 4))))
		      '(not (member 1 2 3 4 ))))
  (assert-true (equal (reduce-lisp-type '(and (not (member 20))
					  (not (member 2 3 4))
					  (member 10 20 30)
					  (member 20 30)))
		      '(eql 30)))
  (assert-true (equal (reduce-lisp-type '(and keyword (member :y :z) (not (eql :y))))
		      '(eql :z)))
  (assert-true (equal (reduce-lisp-type '(and (member a b 2 3) symbol))
		      '(member a b)))
  (assert-true (equal (reduce-lisp-type '(and (member a 2) symbol))
		      '(eql a)))
  (assert-true (equal (reduce-lisp-type '(and (member a b) fixnum))
		      nil))

  (assert-true (equal (reduce-lisp-type '(and (not (member 1 2 a)) (not (member 2 3 4 b))))
		      '(not (member 1 2 3 4 a b))))
  (assert-true (equal (reduce-lisp-type '(and fixnum (not (member 1 2 a b))))
		      '(and fixnum (not (member 1 2)))))
  (assert-true (equal (reduce-lisp-type '(and fixnum (not (member a b))))
		      'fixnum))
  (assert-true (equal (reduce-lisp-type '(or fixnum string (member 1 2 "hello" a b)))
		      '(or fixnum string (member a b))))
  (assert-true (equal (lisp-types::reduce-lisp-type '(and keyword (not (member :x))))
		      '(and keyword (not (eql :x)))))
  (assert-true (equal (lisp-types::reduce-lisp-type '(or number (not (member 1 2 a b))))
		      '(not (member a b))))
  )

(define-test type/mdtd-t
  (assert-false (member t (mdtd-baseline '(KEYWORD (MEMBER :A :B) T          (NOT (EQL :A)) (EQL :A) (NOT (EQL :B)) (EQL :B)))))
  (assert-false (member t (mdtd-baseline '(        (MEMBER :A :B) T          (NOT (EQL :A)) (EQL :A) (NOT (EQL :B)) (EQL :B)))))
  (assert-false (member t (mdtd-baseline '(KEYWORD (MEMBER :A :B) T (EQL :A)                         (NOT (EQL :B)) (EQL :B)))))
  (assert-false (member t (mdtd-baseline '(KEYWORD (MEMBER :A :B) T                                  (NOT (EQL :B)) (EQL :B)))))
  (assert-false (member t (mdtd-baseline '(KEYWORD (MEMBER :A :B) T                                                 (EQL :B)))))
  (assert-false (member t (mdtd-baseline '(KEYWORD (MEMBER :A :B) T))))
  )

(define-test type/mdtd-member
  (assert-true (equal '(t) (mdtd-baseline '(nil t))))
  (assert-true (equal '(t) (mdtd-baseline '((member) t)))))

(define-test type/mdtd-baseline
  (let ((A '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	(B '(2 3 5 6))
	(C '(3 4 5 7))
	(D '(5 6 7 8 9 10 11 12 13))
	(E '(12 13 14))
	(F '(9 10))
	(G '(10 11 12))
	(H '(15))
	(I '(16)))
    (let ((disjoint (mdtd-baseline `((member ,@A)
					 (member ,@B)
					 (member ,@C)
					 (member ,@D)
					 (member ,@E)
					 (member ,@F)
					 (member ,@G)
					 (member ,@H)
					 (member ,@I)))))
      (assert-true (equal 16 (length disjoint)))
      (assert-true (notany (lambda (s)
			     (subtypep-wrapper s nil)) disjoint))
      (dotimes (e 16)
	(let ((e (+ e 1)))
	  (assert-true (equal 1 (count-if (lambda (s)
					    (typep e s))
					  disjoint))))))))

(define-test type/mdtd-baseline2 ()
  (let ((A '(1 2 3 4 5 6 7 8 9 10 11 12 13))
	(B '(2 3 5 6))
	(C '(3 4 5 7))
	(D '(5 6 7 8 9 13))
	(E '(9))
	(F '(10))
	(G '(11))
	(H '(9 12 13)))
    (let ((disjoint (mdtd-baseline `((member ,@A)
					 (member ,@B)
					 (member ,@C)
					 (member ,@D)
					 (member ,@E)
					 (member ,@F)
					 (member ,@G)
					 (member ,@H)))))
      (assert-true (equal 13 (length disjoint)))
      (assert-true (notany (lambda (s)
			     (subtypep-wrapper s nil)) disjoint))
      (dotimes (e 13)
	(let ((e (+ e 1)))
	  (assert-true (equal 1 (count-if (lambda (s)
					    (typep e s))
					  disjoint))))))))

(define-test type/enter-conses ()
  (let ((hash (make-hash-table :test #'equal)))
    (assert-true (eq (lisp-types::enter-conses hash '(a (1 2 3) b))
		     (lisp-types::enter-conses hash '(a (1 2 3) b))))
    (assert-true (eq (cadr (lisp-types::enter-conses hash '(a (1 2 3) b)))
		     (cadr (lisp-types::enter-conses hash '(c (1 2 3) d)))))
    (assert-true (eq (cdr (lisp-types::enter-conses hash '(a 1 2 3 4)))
		     (cdr (lisp-types::enter-conses hash '(b 1 2 3 4)))))))

(define-test type/subtype
  (assert-true (equal '(t t) (multiple-value-list (smarter-subtypep '(eql :x) 'keyword))))
  (assert-true (equal '(t t) (multiple-value-list (smarter-subtypep '(not keyword) '(not (eql :x))))))
  (assert-true (equal '(nil t) (multiple-value-list (smarter-subtypep 'keyword '(eql :x)))))
  (assert-true (equal '(nil t) (multiple-value-list (smarter-subtypep '(not keyword) '(eql :x)))))
  (assert-true (equal '(nil t) (multiple-value-list (smarter-subtypep '(not (eql :x)) 'keyword)))))

;; disjoint-types-p
(define-test type/disjoint-types-p
  (assert-false (disjoint-types-p 'fixnum '(member 1 2)))
  (assert-false (disjoint-types-p 'fixnum '(not (member 1 2))))
  (assert-false (disjoint-types-p 'keyword '(member :a :b)))
  (assert-false (disjoint-types-p 'keyword '(eql :a)))
  (assert-false (disjoint-types-p '(AND KEYWORD (NOT (MEMBER :A :B))) T))
  (assert-false (disjoint-types-p '(and symbol (not (eql a))) t))
  (assert-true (equal '(nil t) (multiple-value-list (disjoint-types-p 'number '(not float)))))
  (assert-true (equal '(nil t) (multiple-value-list (disjoint-types-p '(not float) 'number))))
  (assert-true (equal '(t t) (multiple-value-list (disjoint-types-p '(not number) 'float))))
  (assert-true (equal '(t t) (multiple-value-list (disjoint-types-p 'float '(not number)))))
  (assert-false (disjoint-types-p '(not float) '(not integer))))

(define-test type/alphbetize-type
  (assert-true (equal (ALPHABETIZE-TYPE
                       '(OR (AND (NOT FIXNUM) BIT FIXNUM (NOT BIT))
                         (AND BIT FIXNUM (NOT BIT))
                         (AND BIT FIXNUM (NOT BIT) (NOT FIXNUM))))
                      '(OR (AND BIT FIXNUM (NOT BIT))
                        (AND BIT FIXNUM (NOT BIT) (NOT FIXNUM))
                        (AND BIT FIXNUM (NOT BIT) (NOT FIXNUM))))))

(deftype A1 () '(member 1 2 3 4 5 6 7 8 9 10    12 13))
(deftype A2 () '(member   2 3   5 6))
(deftype A3 () '(member     3 4 5   7))
(deftype A4 () '(member         5 6 7 8 9          13))
(deftype A5 () '(member                 9))
(deftype A6 () '(member                   10))
(deftype A7 () '(member                      11))
(deftype A8 () '(member                 9       12 13))

(defun demo-baseline (U &key (interactive nil))
  "version of mdtd-baseline algorithm used for demo purpose, which
contains verbose information about the progress of the algorithm."
  (labels ((union-types (new-types old-types)
	     ;; prepend any elements of new-types to the beginning
	     ;; of old-types unless they are already present.
	     ;; equality measured with #'equivalent-types-p,
	     ;; preserving the order of old-types	       
	     (if (null new-types)
		 old-types
		 (union-types 
		  (cdr new-types)
		  (let ((new (type-to-dnf-bottom-up (car new-types))))
		    (cond
		      ((subtypep new nil)
		       old-types)
		      (t
		       (adjoin (car new-types)
			;; new
			old-types
			:test #'equivalent-types-p)))))))
	   (find-intersecting-pair (U)
	     (map-pairs (lambda (x y)
			  (unless (subtypep `(and ,x ,y) nil)
			    (return-from find-intersecting-pair (values x y))))
			U)
	     nil)
	   (set-minus (yes no &key acc (test #'eq))
	     (cond
	       ((null yes)
		(nreverse acc))
	       ((member (car yes) no :test test)
		(set-minus (cdr yes) no :acc acc :test test))
	       (t
		(set-minus (cdr yes) no :acc (cons (car yes) acc) :test test))))
	   (type-minus (yes no)
	     (set-minus yes no :test #'equivalent-types-p))
	   (print-types (prefix S &aux (indent 2) (n 0))
	     (format t "~A (~%" prefix)
	     (dolist (v S)
	       (incf n)
	       (dotimes (_ indent)
		 (format t " "))
	       (format t "~D: ~A~%" n v))
	     (format t ")~%")))

    (let ((step 0)
	  (D ()))
      (while t
	(format t "------------ step=~D -------------~%" (incf step))
	(let ((new-disjoint (setof v U
			      (forall w (remove v U)
				(disjoint-types-p v w)))))
	  (format t "found ~A disjoint:~%" (length new-disjoint))
	  (print-types "new-disjoint" new-disjoint)
	  (setf D (union-types new-disjoint D)
		U (type-minus U new-disjoint)))

	(format t "D=~A U=~A~%" (length D) (length U))
	(print-types "D" D)
	(print-types "U" U)

	(cond
	  ((null U)
	   (return-from demo-baseline D))
	  (t
	   (multiple-value-bind (X Y) (find-intersecting-pair U)
	     (when (null (or X Y))
	       (error "didn't find intersection"))
	     (format t "intersecting:~%  ~A~%  ~A~%" X Y)

	     (cond ((null interactive))
		   ((null (yes-or-no-p "continue?"))
		    (return-from demo-baseline nil)))			  
	     
	     (let ((X<Y (subtypep X Y))
		   (Y<X (subtypep Y X)))
	       (setf U (cond
			 ((and X<Y Y<X)
			  (remove Y U))
			 (X<Y
			  ;; add Y!X to U and remove Y from U,
			  ;; unless Y!X is already in D in which case
			  ;;    just remove Y from U.
			  (union-types (type-minus (list `(and ,Y (not ,X)))
						   D)
				       (remove Y U)))
			 (Y<X
			  (union-types (type-minus (list `(and ,X (not ,Y)))
						   D)
				       (remove X U)))
			 (t
			  (union-types (type-minus (list `(and ,X (not ,Y))
							 `(and ,Y (not ,X))
							 `(and ,X ,Y))
						   D)
				       (set-minus U (list X Y))))))))))))))


(define-test type/demo-baseline
  (let ((U `(A1 A2 A3 A4 A5 A6 A7 A8)))
    (demo-baseline U :interactive nil)))
			      
