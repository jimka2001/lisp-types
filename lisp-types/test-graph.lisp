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

(shadow-all-symbols :package-from :lisp-types :package-into :lisp-types-test)




(defun perf-mdtd-graph (&key (max 18))
  (declare (notinline string< sort))
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (setf all-types (set-difference all-types '(compiled-function ; see https://groups.google.com/forum/#!topic/comp.lang.lisp/S-O94JzjlFw
						test-char-code ; same as char-int
						)))
    (setf all-types (sort all-types #'string<))
    (let ( data)
      (flet ((test1 (types &aux sorted)
	       (format t "~A~%" (car types))
	       ;;(format t "  types=~A~%" types)
	       (dolist (algo (list (list :algorithm 'mdtd-graph
					 :function (lambda (types)
						     (mdtd-graph types :reduce t)))
				   (list :algorithm 'mdtd-sat
					 :function #'mdtd-sat)
				   (list :algorithm 'mdtd-baseline
					 :function #'mdtd-baseline)))
		 (let ((t1 (get-internal-run-time))
		       (t2 (progn (setf sorted (funcall (getf algo :function) types))
				  (get-internal-run-time))))
		   (unless (= t1 t2)
		     (push (list
			    :algorithm (getf algo :algorithm)
			    :time (/ (- t2 t1) internal-time-units-per-second)
			    :input-length (length types)
			    :output-length (length sorted))
			   data)
		     (format t "  ~A ~D ~D ~F~%"
			     (getf algo :algorithm)
			     (length types)
			     (length sorted)
			     (/ (- t2 t1) internal-time-units-per-second)))))))
	(dotimes (r 10)
	  (let ((rnd-all-types (shuffle-list (copy-list all-types)))
		(testing-types nil))
	    (while (and rnd-all-types
			(>= max (length testing-types)))
	      (push (pop rnd-all-types) testing-types)
	      (test1 testing-types)))))
      (values data
	      (length data)))))

(define-test types/find-duplicates
  (assert-true (equal '(a b) (lisp-types::find-duplicates '(a b a b)))))



(define-test types/graph2
  (declare (notinline set-difference))
  (let ((all-numbers (remove nil (valid-subtypes 'number))))
    ;; (mdtd-rtev2 (valid-subtypes 'number))
    
    (let ((types1 (mdtd-rtev2 all-numbers))
          (types2 (mdtd-graph all-numbers)))
      (caching-types
        (assert-false (set-exclusive-or types1
                                        types2
                                        :test #'equivalent-types-p))))))

(define-test type/graph-7
  (let ((types '( UNSIGNED-BYTE BIGNUM TEST-ARRAY-RANK RATIONAL)))
    (assert-false (set-exclusive-or (MDTD-GRAPH types)
                                    (mdtd-rtev2 types)
                                    :test #'equivalent-types-p))))

(define-test type/graph-6
  (let ((types '( UNSIGNED-BYTE FLOAT BIGNUM REAL TEST-ARRAY-RANK RATIONAL)))
    (assert-false (set-exclusive-or (MDTD-GRAPH types)
                                    (mdtd-rtev2 types)
                                    :test #'equivalent-types-p))))

(define-test type/graph-5
  (let ((types '(COMPLEX TEST-FLOAT-DIGITS UNSIGNED-BYTE BIGNUM TEST-ARRAY-RANK RATIONAL)))
    (assert-false (set-exclusive-or (MDTD-GRAPH types)
                                    (mdtd-rtev2 types)
                                    :test #'equivalent-types-p))))

(define-test type/graph-4
  (let ((types '((OR TEST-ARRAY-RANK (AND REAL (NOT BIGNUM) (NOT FLOAT) (NOT UNSIGNED-BYTE)))
                 (AND REAL (NOT BIGNUM) (NOT FLOAT) (NOT UNSIGNED-BYTE)))))
    (assert-false (set-exclusive-or (MDTD-GRAPH types)
                                    (mdtd-rtev2 types)
                                    :test #'equivalent-types-p))))

(define-test type/graph-3
  (let ((types '(COMPLEX
                 FLOAT
                 TEST-FLOAT-DIGITS
                 (AND BIGNUM (NOT UNSIGNED-BYTE))
                 (AND BIGNUM UNSIGNED-BYTE)
                 (AND TEST-ARRAY-RANK (NOT TEST-FLOAT-DIGITS))
                 (AND UNSIGNED-BYTE (NOT TEST-ARRAY-RANK) (NOT BIGNUM))
                 (AND REAL (NOT BIGNUM) (NOT FLOAT) (NOT UNSIGNED-BYTE))
                 )))
    (assert-false (set-exclusive-or (MDTD-GRAPH types)
                                    (mdtd-rtev2 types)
                                    :test #'equivalent-types-p))))

(define-test type/graph-2
  (let ((types '(COMPLEX
                 FLOAT
                 TEST-FLOAT-DIGITS
                 (AND BIGNUM (NOT UNSIGNED-BYTE))
                 (AND BIGNUM UNSIGNED-BYTE)
                 (AND TEST-ARRAY-RANK (NOT TEST-FLOAT-DIGITS))
                 (AND UNSIGNED-BYTE (NOT TEST-ARRAY-RANK) (NOT BIGNUM))
                 (OR TEST-ARRAY-RANK (AND REAL (NOT BIGNUM) (NOT FLOAT) (NOT UNSIGNED-BYTE))))))
    (assert-false (set-exclusive-or (MDTD-GRAPH types)
                                    (mdtd-rtev2 types)
                                    :test #'equivalent-types-p))))


(define-test type/graph-8
  (SET-EXCLUSIVE-OR (MDTD-GRAPH '(BIT ATOM TEST-ARRAY-RANK))
		    (MDTD-RTEV2 '(BIT ATOM TEST-ARRAY-RANK)) :TEST #'EQUIVALENT-TYPES-P))

(define-test type/graph
  (assert-false (set-exclusive-or (MDTD-GRAPH '( COMPLEX
							   TEST-FLOAT-DIGITS
							   UNSIGNED-BYTE
							   FLOAT
							   BIGNUM
							   REAL
							   TEST-ARRAY-RANK
							   RATIONAL
							   ))
				  (mdtd-rtev2   '( COMPLEX
						       TEST-FLOAT-DIGITS
						       UNSIGNED-BYTE
						       FLOAT
						       BIGNUM
						       REAL
						       TEST-ARRAY-RANK
						       RATIONAL
						       ))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (MDTD-GRAPH '( COMPLEX
							   FIXNUM
							   TEST-FLOAT-DIGITS
							   NUMBER
							   TEST-CHAR-CODE
							   UNSIGNED-BYTE
							   FLOAT
							   BIGNUM
							   REAL

							   TEST-ARRAY-RANK
					      
							   RATIONAL
							   RATIO

							   SHORT-FLOAT))
				  (mdtd-rtev2 '( COMPLEX
						     FIXNUM
						     TEST-FLOAT-DIGITS
						     NUMBER
						     TEST-CHAR-CODE
						     UNSIGNED-BYTE
						     FLOAT
						     BIGNUM
						     REAL

						     TEST-ARRAY-RANK
					      
						     RATIONAL
						     RATIO

						     SHORT-FLOAT))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (MDTD-GRAPH '( COMPLEX
							   FIXNUM
							   TEST-FLOAT-DIGITS
							   NUMBER
							   TEST-CHAR-CODE
							   UNSIGNED-BYTE
							   FLOAT
							   BIGNUM
							   REAL
							   LONG-FLOAT
							   INTEGER
							   TEST-ARRAY-RANK
							   BIT
							   RATIONAL
							   SHORT-FLOAT))
				  (mdtd-rtev2      '( COMPLEX
							  FIXNUM
							  TEST-FLOAT-DIGITS
							  NUMBER
							  TEST-CHAR-CODE
							  UNSIGNED-BYTE
							  FLOAT
							  BIGNUM
							  REAL
							  LONG-FLOAT
							  INTEGER
							  TEST-ARRAY-RANK
							  BIT
							  RATIONAL
							  SHORT-FLOAT))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (MDTD-GRAPH '( COMPLEX
							   FIXNUM
							   TEST-FLOAT-DIGITS
							   NUMBER
							   TEST-CHAR-CODE
							   UNSIGNED-BYTE
							   FLOAT
							   BIGNUM
							   REAL
							   LONG-FLOAT
							   INTEGER
							   TEST-ARRAY-RANK
							   BIT
							   RATIONAL
							   RATIO
							   SHORT-FLOAT))
				  (mdtd-rtev2      '( COMPLEX
							  FIXNUM
							  TEST-FLOAT-DIGITS
							  NUMBER
							  TEST-CHAR-CODE
							  UNSIGNED-BYTE
							  FLOAT
							  BIGNUM
							  REAL
							  LONG-FLOAT
							  INTEGER
							  TEST-ARRAY-RANK
							  BIT
							  RATIONAL
							  RATIO
							  SHORT-FLOAT))
				  :test #'equivalent-types-p))
  
  (assert-false (set-exclusive-or (mdtd-graph '((or (eql 10)
							    (member 1 2)
							    (member 3 4))
							   (or (eql 11)
							    (member 1 3)
							    (member 2 4))
							   (member 10 11)))
				  (mdtd-rtev2       '((or (eql 10)
							    (member 1 2)
							    (member 3 4))
							   (or (eql 11)
							    (member 1 3)
							    (member 2 4))
							   (member 10 11)))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '((eql 1) (eql 2) (member 1 2)))
				  (mdtd-rtev2       '((eql 1) (eql 2) (member 1 2)))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '(CONDITION CLASS CELL-ERROR BUILT-IN-CLASS))
				  (mdtd-rtev2       '(CONDITION CLASS CELL-ERROR BUILT-IN-CLASS))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '(CONDITION CONCATENATED-STREAM COMPLEX CLASS CHARACTER TEST-CHAR-INT
							   CELL-ERROR BUILT-IN-CLASS BROADCAST-STREAM BOOLEAN BIT-VECTOR BIT
							   BIGNUM BASE-STRING BASE-CHAR ATOM TEST-ARRAY-TOTAL-SIZE TEST-ARRAY-RANK ARRAY
							   ARITHMETIC-ERROR))
				  (mdtd-rtev2       '(CONDITION CONCATENATED-STREAM COMPLEX CLASS CHARACTER TEST-CHAR-INT
							   CELL-ERROR BUILT-IN-CLASS BROADCAST-STREAM BOOLEAN BIT-VECTOR BIT
							   BIGNUM BASE-STRING BASE-CHAR ATOM TEST-ARRAY-TOTAL-SIZE TEST-ARRAY-RANK ARRAY
							   ARITHMETIC-ERROR))
				  :test #'equivalent-types-p))

  (assert-false (set-exclusive-or (mdtd-graph '( BIT  ATOM         TEST-ARRAY-RANK))
				  (mdtd-rtev2       '( BIT  ATOM         TEST-ARRAY-RANK))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '( BIT  ATOM         TEST-ARRAY-RANK ARRAY))
				  (mdtd-rtev2       '( BIT  ATOM         TEST-ARRAY-RANK ARRAY))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '(TEST-CHAR-CODE CELL-ERROR BUILT-IN-CLASS BROADCAST-STREAM BOOLEAN
							   BIT-VECTOR BIT BIGNUM BASE-STRING BASE-CHAR ATOM
							   TEST-ARRAY-TOTAL-SIZE TEST-ARRAY-RANK ARRAY ARITHMETIC-ERROR))
				  (mdtd-rtev2       '(TEST-CHAR-CODE CELL-ERROR BUILT-IN-CLASS BROADCAST-STREAM BOOLEAN
							   BIT-VECTOR BIT BIGNUM BASE-STRING BASE-CHAR ATOM
							   TEST-ARRAY-TOTAL-SIZE TEST-ARRAY-RANK ARRAY ARITHMETIC-ERROR))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (mdtd-graph '(cell-error arithmetic-error ))
				  (mdtd-rtev2 '(cell-error arithmetic-error ))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (mdtd-graph '(cell-error BUILT-IN-CLASS ))
				  (mdtd-rtev2 '(cell-error BUILT-IN-CLASS ))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (mdtd-graph '( arithmetic-error BUILT-IN-CLASS ))
				  (mdtd-rtev2 '( arithmetic-error BUILT-IN-CLASS ))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (mdtd-graph '(cell-error arithmetic-error BUILT-IN-CLASS ))
				  (mdtd-rtev2 '(cell-error arithmetic-error BUILT-IN-CLASS ))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (mdtd-graph '(integer))
				  (mdtd-rtev2 '(integer))
				  :test #'equivalent-types-p ))
  (assert-false (set-exclusive-or (mdtd-graph '(integer fixnum))
				  (mdtd-rtev2 '(integer fixnum))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '(fixnum number))
				  (mdtd-rtev2 '(fixnum number))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '(integer number))
				  (mdtd-rtev2 '(integer number))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '(integer fixnum number))
				  (mdtd-rtev2 '(integer fixnum number))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '(integer fixnum number bit unsigned-byte bignum))
				  (mdtd-rtev2 '(integer fixnum number bit unsigned-byte bignum))
				  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '(FIXNUM TEST-CHAR-CODE UNSIGNED-BYTE))
                                  (mdtd-rtev2       '(FIXNUM TEST-CHAR-CODE UNSIGNED-BYTE))
                                  :test #'equivalent-types-p))
  (assert-false (set-exclusive-or (mdtd-graph '(FIXNUM (integer 0 (1114112)) UNSIGNED-BYTE))
                        (mdtd-rtev2       '(FIXNUM (integer 0 (1114112)) UNSIGNED-BYTE))
                        :test #'equivalent-types-p))


)


