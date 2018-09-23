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
  (shadow-all-symbols :package-from :lisp-types          :package-into :lisp-types-test))

#+sbcl
(define-test disjoint-cmp-j
  (setf *perf-results* nil)
  (ltbdd-with-new-hash ()
    (types/cmp-perf :types '((MEMBER 0 1 2 4 5 6 8 9 10) (MEMBER 1 2 4 6 8)
                             (MEMBER 1 2 3 5 6 7 8 10) (MEMBER 1 5 6 7 9 10) (MEMBER 0 1 6 7 8 10)
                             (MEMBER 0 1 2 3 5 6 10) (MEMBER 0 1 3 4 5 6 8 9 10)
                             (MEMBER 0 3 5 6 8) (MEMBER 0 1 2 3 6 7 9) (MEMBER 0 2 4 8 10)
                             (MEMBER 0 1 5 9) (MEMBER 0 1 2 4 8) (MEMBER 1 3 5 6 8 9 10)
                             (MEMBER 3 5 7 9) (MEMBER 5 6 7 8 9 10) (MEMBER 0 4 6 7 8 9)
                             (MEMBER 1 4 7 9) (MEMBER 0 3 4 7 8 10) (MEMBER 0 1 4 5 7 8)
                             (MEMBER 0 2 4 5 7 9 10) (MEMBER 0 9 10)))))


#+sbcl
(define-test disjoint-cmp-1
  (setf *perf-results* nil)
  (ltbdd-with-new-hash ()
    (types/cmp-perfs :file-name "disjoint-cmp-1"
                     :destination-dir "/tmp"
                     :types '(sb-pcl::SYSTEM-CLASS
                              sb-pcl::SLOT-DEFINITION
                              sb-pcl::EQL-SPECIALIZER) :time-out nil)))

#+sbcl
(define-test test/bdd-numbers-3
  (ltbdd-with-new-hash ()
    (assert-true (types/cmp-perfs :limit 3
                                  :file-name "bdd-numbers"
                                  :destination-dir "/tmp"
                                  :decompose '(lisp-types::mdtd-bdd)
                                  :types (valid-subtypes 'number)))))

#+sbcl
(define-test test/bdd-numbers-6
  (ltbdd-with-new-hash ()
    (assert-true (types/cmp-perfs :limit 6
                                  :file-name "bdd-numbers"
                                  :destination-dir "/tmp"
                                  :decompose '(lisp-types::mdtd-bdd)
                                  :types (valid-subtypes 'number)))))

#+sbcl
(define-test test/bdd-numbers
  (ltbdd-with-new-hash ()
    (assert-true (types/cmp-perfs :limit 15
                                  :file-name "bdd-numbers"
                                  :destination-dir "/tmp"
                                  :decompose '(lisp-types::mdtd-bdd)
                                  :types (valid-subtypes 'number)))))




#+sbcl
(define-test disjoint-cmp-2
  (setf *perf-results* nil)
  (ltbdd-with-new-hash ()
    (types/cmp-perfs :file-name "disjoint-cmp-2"
                     :destination-dir "/tmp"
                     :types '(sb-pcl::SYSTEM-CLASS
                              sb-pcl::STANDARD-SLOT-DEFINITION
                              sb-pcl::EFFECTIVE-SLOT-DEFINITION
                              sb-pcl::FUNCALLABLE-STANDARD-OBJECT
                              sb-pcl::SPECIALIZER
                              sb-pcl::EQL-SPECIALIZER
                              sb-pcl::DIRECT-SLOT-DEFINITION
                              sb-pcl::SLOT-DEFINITION)
                     :time-out 5)))

#+sbcl
(define-test disjoint-cmp-3
  (setf *perf-results* nil)
  (ltbdd-with-new-hash ()
    (types/cmp-perfs :file-name "disjoint-cmp-3"
                   :destination-dir "/tmp"
                   :types '(SB-PCL:SYSTEM-CLASS
                            SB-MOP:STANDARD-WRITER-METHOD
                            SB-MOP:DIRECT-SLOT-DEFINITION))))


#+sbcl
(define-test disjoint-cmp-4
  (setf *perf-results* nil)
  (ltbdd-with-new-hash ()
    (types/cmp-perfs :file-name "disjoint-cmp-4"
                     :destination-dir "/tmp"
                     :types '(SB-PCL:SYSTEM-CLASS
                              SB-MOP:DIRECT-SLOT-DEFINITION
                              SB-MOP:FORWARD-REFERENCED-CLASS
                              SB-MOP:EFFECTIVE-SLOT-DEFINITION
                              SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION
                              SB-MOP:STANDARD-ACCESSOR-METHOD
                              SB-MOP:STANDARD-READER-METHOD
                              SB-MOP:FUNCALLABLE-STANDARD-CLASS
                              SB-MOP:FUNCALLABLE-STANDARD-OBJECT)
                     :time-out 8)))

#+sbcl
(define-test disjoint-cmp-5
  (setf *perf-results* nil)
  (ltbdd-with-new-hash ()
    ;; mdtd-bdd-graph
    (types/cmp-perfs :file-name "disjoint-cmp-5"
                     :destination-dir "/tmp"
                     :types '(SB-PCL:SYSTEM-CLASS
                              SB-MOP:STANDARD-ACCESSOR-METHOD
                              SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION))))

(define-test disjoint-cmp-6
  (setf *perf-results* nil)
  (ltbdd-with-new-hash ()
  (types/cmp-perfs :file-name "disjoint-cmp-6"
                   :destination-dir "/tmp"
                   :types '((MEMBER 2 4 5 6 8)
                            (MEMBER 0 5 6 9 10)
                            (MEMBER 0 1 3 4 10)
                            (MEMBER 0 3 5 8 9 10)))))

(define-test disjoint-cmp-7
  (setf *perf-results* nil)
    (ltbdd-with-new-hash ()
  (types/cmp-perfs :file-name "disjoint-cmp-7"
                   :destination-dir "/tmp"
                   :types '((MEMBER 1 3 4 5 6 9)
                            (MEMBER 1 5 7 8)
                            (MEMBER 4 7 8 9 10)
                            (MEMBER 3 4 5 7 9)
                            (MEMBER 0 2 3 7 8 10)) )))

(define-test disjoint-cmp-8
  (setf *perf-results* nil)
    (ltbdd-with-new-hash ()
  (types/cmp-perfs :file-name "disjoint-cmp-8"
                   :destination-dir "/tmp"
                   :types '((MEMBER 0 2)
                            (MEMBER 0 1 2)
                            (MEMBER 0 2 4)))))

(define-test disjoint-cmp-9
  (ltbdd-with-new-hash ()
   (assert-true (= 3 (length (mdtd-bdd '((MEMBER 0 2)
                                                      (MEMBER 0 1 2)
                                                      (MEMBER 0 2 4))))))))

(define-test disjoint-cmp-a
  (ltbdd-with-new-hash ()
    (ltbdd-with-new-hash ()
      (let* ((t1 (ltbdd '(member 0 2)))
             (t2 (ltbdd '(member 0 1 2)))
             (t3 (ltbdd '(member 0 2 4)))
             (bdds (list t1 t2 t3))
             (U (reduce #'bdd-or bdds :initial-value *bdd-false*)))
        (assert-false (eq '= (bdd-cmp '(member  0 2 4) '(member 0 2))))
        (forall x '(0 1 2 4)
          (assert-true (bdd-type-p x U)))
        (assert-false (bdd-type-p 3 U))
        (assert-true (bdd-type-p 0 (bdd-and U t1)))
        (assert-true (bdd-type-p 2 (bdd-and U t1)))
        (assert-true (bdd-type-p 4 (bdd-and-not U t1)))))))

(define-test disjoint-cmp-b
  (assert-true (= 9 (length (mdtd-graph
                             '((MEMBER 1 3 4 5 6 9)
                               (MEMBER 1 5 7 8)
                               (MEMBER 4 7 8 9 10)
                               (MEMBER 3 4 5 7 9)
                               (MEMBER 0 2 3 7 8 10)))))))
    
(define-test disjoint-cmp-c
  (assert-true (mdtd-graph '((MEMBER 0 3 4)
                                        (EQL 2)
                                        (MEMBER 2 3)
                                        (MEMBER 2 3 4)
                                        (MEMBER 1 2 4)
                                        (EQL 4)
                                        (MEMBER 0 1 3 4)
                                        (MEMBER 1 2 3)
                                        (MEMBER 1 4)
                                        (EQL 3)))))


(define-test disjoint-cmp-d
  (assert-true (mdtd-graph '((EQL 2)
                                        (MEMBER 2 3 4)
                                        (MEMBER 1 2 4)
                                        (MEMBER 0 1 3)
                                        (EQL 0)
                                        (MEMBER 1 3 4)))
               ))


(define-test disjoing-cmp-e
  (assert-true (mdtd-graph '((MEMBER 1 4)
                                        (EQL 2)
                                        (MEMBER 0 3)
                                        (MEMBER 2 3 4)
                                        (MEMBER 1 3 4)
                                        (MEMBER 0 1)
                                        (MEMBER 0 1 4)
                                        (MEMBER 0 1 3 4)))))

(define-test disjoing-cmp-f
  (assert-true (mdtd-graph '((MEMBER 1 4)
                                        (MEMBER 1 2 4)
                                        (MEMBER 0 1 3 4)
                                        (MEMBER 0 3)
                                        (MEMBER 0 1 4)
                                        (MEMBER 1 3)
                                        (EQL 4)
                                        (MEMBER 0 3 4)
                                        (MEMBER 1 3 4)
                                        (EQL 2)
                                        (MEMBER 2 3)
                                        (MEMBER 0 1 3)
                                        (MEMBER 0 1 2 4)
                                        (MEMBER 2 3 4)
                                        (MEMBER 0 2 4)
                                        ))))

(define-test disjoint-cmp-g
  (setf *perf-results* nil)
  (ltbdd-with-new-hash ()
    (types/cmp-perfs :file-name "disjoint-cmp-g"
                     :destination-dir "/tmp"
                     :types '((MEMBER 0 3 4)
                              (EQL 3)
                              (MEMBER 1 4)
                              (MEMBER 1 2 3)
                              (MEMBER 0 2)
                              NULL
                              (MEMBER 0 2 3)
                              (MEMBER 0 1 2 3)))))

(define-test disjoint-cmp-h
  (setf *perf-results* nil)
  (ltbdd-with-new-hash ()
    (types/cmp-perfs :file-name "disjoint-cmp-h"
                     :destination-dir "/tmp"
                     :limit 5
                     :types '((COMMON-LISP:EQL 3)
                              (COMMON-LISP:MEMBER 1 2 3)
                              (COMMON-LISP:MEMBER 0 2) COMMON-LISP:NULL
                              (COMMON-LISP:MEMBER 0 2 3)
                              (COMMON-LISP:MEMBER 0 1 2 3)))))

(define-test disjoint-cmp-i
  (setf *perf-results* nil)
  (ltbdd-with-new-hash ()
    (types/cmp-perfs :limit 5
                     :file-name "disjoint-cmp-i"
                     :destination-dir "/tmp"
                     :types '(STRING STANDARD-GENERIC-FUNCTION ATOM METHOD SIMPLE-BASE-STRING
                              SEQUENCE COMPLEX STANDARD-OBJECT STANDARD-METHOD))))


(define-test disjoint-cmp-k
  (let ((type-specifiers
          '((and arithmetic-error reader-error structure-class (not style-warning))
            (and arithmetic-error reader-error (not structure-class) (not style-warning))
            (and arithmetic-error (not reader-error) (not structure-class) style-warning)
            (and arithmetic-error (not reader-error) structure-class style-warning)
            (and (not arithmetic-error) condition (not reader-error) structure-class (not style-warning))
            (and (not arithmetic-error) reader-error structure-class (not style-warning))
            (and arithmetic-error (not reader-error) structure-class (not style-warning))
            (and (not arithmetic-error) (not reader-error) structure-class style-warning)
            (or (and condition (not reader-error)) (and reader-error (not style-warning)))
            (and (not condition) structure-class (not style-warning))
            (and arithmetic-error (not reader-error) (not structure-class) (not style-warning))
            (or (and (not reader-error) warning) (and reader-error (not style-warning) warning))
            (or (and (not reader-error) stream-error) (and reader-error (not style-warning)))
            (and (not arithmetic-error) reader-error (not structure-class) (not style-warning))
            (and (not arithmetic-error) (not reader-error) (not structure-class) style-warning))))
    (ltbdd-with-new-hash ()
      (parameterized-mdtd-bdd-graph type-specifiers 
                                      :sort-nodes #'(lambda (graph)
                                                      (declare (notinline sort))
                                                      (sort graph #'< :key #'lisp-types::count-parents-per-node))
                                      :sort-strategy "TOP-TO-BOTTOM"
                                      :inner-loop :operation
                                      :do-break-sub :strict
                                      :do-break-loop t))))

(define-test disjoint-cmp-l
  (let ((type-specifiers
          '(CONDITION RESTART RATIONAL CONS RATIO READER-ERROR STRUCTURE-CLASS
            SYNONYM-STREAM ARITHMETIC-ERROR TEST-CHAR-CODE WARNING FLOAT-RADIX
            SIMPLE-BIT-VECTOR STREAM-ERROR ARRAY STYLE-WARNING)))
    (ltbdd-with-new-hash ()
      (parameterized-mdtd-bdd-graph type-specifiers 
                                               :sort-nodes #'(lambda (graph)
                                                               (declare (notinline sort))
                                                               (sort graph #'< :key #'lisp-types::count-parents-per-node))
                                               :sort-strategy "TOP-TO-BOTTOM"
                                               :inner-loop :operation
                                               :do-break-sub :strict
                                               :do-break-loop t))))

;; (lisp-types-test::sort-results "/Users/jnewton/newton.16.edtchs/src/member.sexp" nil)

(defun perf-test-1 (&key (size 11))
  (ltbdd-with-new-hash (&aux (type-specifiers (lisp-types::choose-randomly (loop :for name being the external-symbols in "SB-PCL"
                                                                                 :when (find-class name nil)
                                                                                   :collect name) size)))
    (parameterized-mdtd-bdd-graph type-specifiers
                                             :sort-nodes (lambda (graph)
                                                           (declare (notinline sort))
                                                           (sort graph #'< :key
                                                                 #'lisp-types::count-connections-per-node))
                                             :sort-strategy  "INCREASING-CONNECTIONS"
                                             :inner-loop :node
                                             :do-break-sub :relaxed
                                             :do-break-loop nil)))

(defun read-trace (stream)
  (let (pending)
    (labels ((next-token ()
               (if pending
                   (pop pending)
                   (let (chars)
                     (do ((c (read-char stream) (read-char stream nil 'the-end)))
                         ((char= #\: c) chars)
                       (when (digit-char-p c)
                         (push c chars))))))
             (un-read (token)
               (push token pending))
             (next-expr ()
               (read stream nil nil))
             (read-suffix (prefix)
               (list :prefix prefix
                     :fname (next-expr)
                     :returned (next-expr)
                     :output (next-expr)))
             (read-one-node ()
               (let* ((prefix (next-token))
                      (call (next-expr))
                      (token (next-token))
                      sub-nodes)
                 (while (not (equal token prefix))
                   (un-read token)
                   (push (read-one-node) sub-nodes)
                   (setf token (next-token)))
                 
                 `(:sub-nodes ,(reverse sub-nodes)
                   :input ,(cadr call)
                   ,@(read-suffix prefix)))))
      (read-one-node))))

(defun filter-trace (filename)
  "read the printed output traced functions, throw away function calls and returns
if the function returned the same as it was passed as input (according to EQUAL)"
  (labels ((print-trace (node)
             (when node
               (destructuring-bind (&key prefix input fname returned output sub-nodes) node
                 (cond
                   ((equal output input)
                    (mapc #'print-trace sub-nodes))
                   (t
                    (format t "~A ~A~%" prefix (list fname input))
                    (mapc #'print-trace sub-nodes)
                    (format t "~A ~A ~A ~A~%"
                            prefix fname returned output)))))))
    (print-trace (with-open-file (str filename :direction :input)
                   (read-trace str)))))
    
(define-test test/parameterization-report-1
  (let* ((bucket-index 1)
         (bucket (nth bucket-index *bucket-reporters* )))
    (parameterization-report :create-png-p nil
                             :multiplier 0.3
                             :bucket-reporters (list bucket)
                             :destination-dir "/tmp/jnewton/analysis/.")))


(define-test test/parameterization-report-2
  (let* ((bucket-index 2)
         (bucket (nth bucket-index *bucket-reporters* )))
    (parameterization-report :create-png-p nil
                             :multiplier 0.3
                             :bucket-reporters (list bucket)
                             :destination-dir "/tmp/jnewton/analysis/.")))

(define-test test/parameterization-report-3
  (let* ((bucket-index 3)
         (bucket (nth bucket-index *bucket-reporters* )))
    (parameterization-report :create-png-p nil
                             :multiplier 0.3
                             :bucket-reporters (list bucket)
                             :destination-dir "/tmp/jnewton/analysis/.")))

(define-test test/parameterized-mdtd-bdd-graph
  (ltbdd-with-new-hash ()
    (parameterized-mdtd-bdd-graph '((and function program-error) concatenated-stream
                                               (or package floating-point-overflow)
                                               (and simple-string program-error) (or method vector)
                                               (or concatenated-stream synonym-stream) simple-warning
                                               control-error (or package-error parse-error)
                                               (and string control-error) (and function unbound-variable)
                                               (or integer undefined-function) (and simple-error base-char)
                                               (or logical-pathname built-in-class)
                                               (or built-in-class division-by-zero)
                                               (and unsigned-byte signed-byte) floating-point-inexact
                                               (and integer undefined-function) (or complex two-way-stream)
                                               (and package-error fixnum))
                                             :sort-strategy "BOTTOM-TO-TOP"
                                             :recursive t
                                             :inner-loop :operation
                                             :do-break-sub :relaxed
                                             :do-break-loop nil
                                             :do-disjoint t
                                             :do-break-touch t
                                             )))

