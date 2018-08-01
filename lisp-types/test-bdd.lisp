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
(shadow-all-symbols :package-from :lisp-types-analysis :package-into :lisp-types-test)

(define-test test/lisp-type-bdd
  (ltbdd-with-new-hash ()
    (let ((bdd (ltbdd 'z1)))
      (assert-true (typep bdd 'lisp-type-bdd))
      (assert-true (typep bdd 'lisp-type-bdd-node))
      (assert-true (typep (bdd-positive bdd) 'bdd-leaf))
      (assert-true (typep (bdd-negative bdd) 'bdd-leaf)))))
  

(define-test test/bdd-to-dnf
  (ltbdd-with-new-hash ()
    (assert-true (equal 'integer
                        (bdd-to-dnf (ltbdd 'integer))))
    (assert-true (bdd-to-dnf (ltbdd '(or string integer))))
    (assert-true (bdd-to-dnf (ltbdd '(or (and integer (not string)) (and string (not integer))))))))

(define-test test/bdd-create
  (ltbdd-with-new-hash ()
    (assert-true (ltbdd 'integer))
    (assert-true (ltbdd '(or integer float)))
    (assert-true (ltbdd '(or (and integer (not string)) (and string (not integer)))))
    (assert-true (eq (ltbdd '(or integer string))
                     (ltbdd '(or string integer))))
    (assert-true (eq (ltbdd '(or (and integer (not string)) (and string (not integer))))
                     (ltbdd '(or (and (not integer) string) (and integer (not string))))))

    ))

(define-test types/bdd-collect-atomic-types
  (ltbdd-with-new-hash ()
    (assert-false (set-exclusive-or (bdd-collect-atomic-types (ltbdd '(or (and integer (not string)) (and string (not integer)))))
                                  
                                  '(integer string)))))

  
(define-test test/certain-reductions
  (ltbdd-with-new-hash ()
    (assert-true (ltbdd '(or (and integer (not string)) (and string (not integer)))))
    (assert-false (bdd-to-dnf (bdd-and-not (ltbdd 'integer) (ltbdd 'number))))))


(define-test type/bdd-sample-a
  (ltbdd-with-new-hash ()
    (let ((types '((member 1 2) (member 2 3) (member 1 2 3 4))))
      (assert-false (set-exclusive-or (bdd-decompose-types types)
                                      (decompose-types types)
                                      :test #'equivalent-types-p)))
    (assert-false (set-exclusive-or (bdd-decompose-types '(UNSIGNED-BYTE FIXNUM RATIONAL))
                                    (decompose-types     '(UNSIGNED-BYTE FIXNUM RATIONAL))
                                    :test #'equivalent-types-p))

    (assert-false (set-exclusive-or (bdd-decompose-types '(unsigned-byte bit fixnum rational number float))
                                    (decompose-types-graph  '(unsigned-byte bit fixnum rational number float))
                                    :test #'equivalent-types-p))))

(define-test type/3-types
  (let ((decomp (bdd-decompose-types '(TEST-CHAR-CODE DOUBLE-FLOAT UNSIGNED-BYTE))))
    (dolist (t1 decomp)
      (dolist (t2 (remove t1 decomp))
        (assert-false (subtypep t1 t2))
        (assert-false (subtypep t2 t1))
        (assert-false (smarter-subtypep t1 t2))
        (assert-false (smarter-subtypep t2 t1))))))

(define-test type/bdd-subtypep
  (ltbdd-with-new-hash ()
    (assert-true (bdd-subtypep (ltbdd 'float) (ltbdd 'number)))
    (assert-true (bdd-subtypep (ltbdd '(eql :x)) (ltbdd 'keyword)))
    (assert-true (bdd-subtypep (ltbdd '(not keyword)) (ltbdd '(not (eql :x)))))
    (assert-false (bdd-subtypep (ltbdd 'keyword) (ltbdd '(eql :x))))
    (assert-false (bdd-subtypep (ltbdd '(not keyword)) (ltbdd '(eql :x))))
    (assert-false (bdd-subtypep (ltbdd '(not (eql :x))) (ltbdd 'keyword)))

    (assert-true (bdd-type-equal (ltbdd '(and (member :a :b) keyword))
                                 (ltbdd '(member :a :b))))

    (assert-true (equal (bdd-to-dnf (ltbdd '(and (member :a :b) keyword)))
                        '(member :a :b)))
    ))


(define-test type/test1
  (ltbdd-with-new-hash ()
    (equal 'number
           (bdd-to-dnf (ltbdd '(AND number (not array)))))
    (equal '(and (not array) sequence) 
           (bdd-to-dnf (ltbdd '(AND (NOT ARRAY) (NOT NUMBER) SEQUENCE))))
    (member 'number
            (bdd-to-dnf (ltbdd '(OR (AND ARRAY (NOT SEQUENCE))
                                 NUMBER
                                 (AND (NOT ARRAY) (NOT NUMBER) SEQUENCE)))))))

(define-test type/bdd-performance-test
  (ltbdd-with-new-hash ()
    (let* ((decomp '((AND (NOT BIT) TEST-ARRAY-RANK) BIT (AND (NOT TEST-CHAR-INT) TEST-ARRAY-TOTAL-SIZE)
                     (AND TEST-CHAR-INT (NOT TEST-ARRAY-RANK)) BASE-CHAR (AND CHARACTER (NOT BASE-CHAR))
                     (AND (NOT CELL-ERROR) BUILT-IN-CLASS ARITHMETIC-ERROR)
                     (AND (NOT CLASS) (NOT CELL-ERROR) ARITHMETIC-ERROR)
                     (AND CELL-ERROR BUILT-IN-CLASS ARITHMETIC-ERROR)
                     (AND CLASS CELL-ERROR (NOT BUILT-IN-CLASS) (NOT ARITHMETIC-ERROR))
                     (AND (NOT CELL-ERROR) BUILT-IN-CLASS (NOT ARITHMETIC-ERROR))
                     (AND CLASS (NOT CELL-ERROR) (NOT BUILT-IN-CLASS) (NOT ARITHMETIC-ERROR))
                     (AND (NOT COMPLEX) (NOT CLASS) (NOT CHARACTER) (NOT CELL-ERROR)
                      (NOT BROADCAST-STREAM) (NOT BOOLEAN) (NOT BIGNUM) ATOM
                      (NOT TEST-ARRAY-TOTAL-SIZE) (NOT ARRAY) (NOT ARITHMETIC-ERROR))
                     COMPLEX (AND (NOT CLASS) CELL-ERROR (NOT ARITHMETIC-ERROR))
                     (AND CELL-ERROR BUILT-IN-CLASS (NOT ARITHMETIC-ERROR))
                     (AND CLASS CELL-ERROR (NOT BUILT-IN-CLASS) ARITHMETIC-ERROR)
                     (AND CLASS (NOT CELL-ERROR) (NOT BUILT-IN-CLASS) ARITHMETIC-ERROR)
                     (AND (NOT CLASS) CELL-ERROR ARITHMETIC-ERROR) BROADCAST-STREAM BOOLEAN BIGNUM
                     (AND (NOT BIT-VECTOR) (NOT BASE-STRING) ARRAY) BIT-VECTOR BASE-STRING))
           (t3 (ltbdd 'CONCATENATED-STREAM))
           (t2 (ltbdd `(or ,@decomp)))
           (t4 (bdd-and-not t3 t2)))

      (dolist (t1 decomp)
        (let ((bdd1 (ltbdd t1)))
          (dolist (f (list #'bdd-and #'bdd-and-not #'(lambda (a b) (bdd-and-not b a))))
            (let ((t5 (funcall f bdd1 t4)))
              (if (bdd-empty-type t5) nil 'not-nil))))))))

(defun types/perf-bdd ()
  (declare (notinline sort))
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (setf all-types (set-difference all-types '(compiled-function control-error division-by-zero error
                                                test-char-code base-char)))
    (setf all-types (sort all-types #'string<))
    (ltbdd-with-new-hash ()
     
      (let ((n 1)
            (testing-types (list (pop all-types))))
        (flet ((test1 (types &aux sorted)
                 (format t "~A~%" (car types))
                 (let ((t1 (get-internal-run-time))
                       (t2 (progn (setf sorted (bdd-decompose-types types))
                                  (get-internal-run-time))))
                   (format t "   ~D ~D ~F~%"
                           n
                           (length sorted)
                           (/ (- t2 t1) internal-time-units-per-second))
                   (incf n))))
          (loop :while testing-types
                :do (progn (test1 testing-types)
                           (push (pop all-types) testing-types))))))))

(defclass A-150 () ())
(defclass B-151 () ())

(define-test type/reduce-c
  (assert-false (class-direct-subclasses (find-class 'a-150)))
  (assert-false (class-direct-subclasses (find-class 'b-151)))
  (assert-true (equal (reduce-lisp-type '(OR (NOT A-150) B-151))
                      '(not a-150))))

(deftype non-number () `(not number))
(deftype non-integer () `(not integer))
(define-test type/bdd-reduce
  (ltbdd-with-new-hash ()

    ;; there are six cases to test

    ;; 1) disjoint on left
    ;;  (number (string nil t) nil)
    ;;  --> (number t nil)
    (assert-true (equal (bdd-serialize
                         (ltbdd-node 'number
                                   (ltbdd-node 'string nil t)
                                   nil))
                        '(number t nil)))
        
    ;; 2) disjoint on right of negative type
    (assert-true (equal (bdd-serialize
                         (ltbdd-node 'non-number
                                   nil
                                   (ltbdd-node 'string nil t)))
                        '(non-number nil t)))
                      
    ;; 3) subtype on right
    (assert-true (equal (bdd-serialize
                         (ltbdd-node 'integer
                                   (ltbdd-node 'number t nil)
                                   nil))
                        '(integer t nil)))

    ;; 4) subtype on left of negative type
    (assert-true (equal (bdd-serialize
                         (ltbdd-node 'non-number
                                   (ltbdd-node 'integer nil t)
                                   nil))
                        '(non-number t nil)))
                      

    ;; 5) supertype on left
    (assert-true (equal (bdd-serialize
                         (ltbdd-node 'integer
                                   (ltbdd-node 'number t nil)
                                   nil))
                        '(integer t nil)))

    ;; 6) supertype on right of negative type
    (assert-true (equal (bdd-serialize
                         (ltbdd-node 'non-integer
                                   nil
                                   (ltbdd-node 'number t nil)))
                        '(non-integer nil t)))))

                   
(define-test test/bdd-type-p
  (ltbdd-with-new-hash ()
    (assert-false (bdd-type-p  t (ltbdd '(or (and sequence (not array))
                                        number
                                        (and (not sequence) array)))))
    (assert-true (bdd-type-p  3 (ltbdd '(or (and sequence (not array))
                                       number
                                       (and (not sequence) array)))))))

(define-test test/bdd-dnf
  (ltbdd-with-new-hash ()
     (assert-true (member 'number (bdd-to-dnf (ltbdd '(or (and sequence (not array))
                                                     number
                                                     (and (not sequence) array))))))
     (assert-false (member '(and number) (bdd-to-dnf (ltbdd '(or (and sequence (not array))
                                                            number
                                                            (and (not sequence) array)))) :test #'equal))))

