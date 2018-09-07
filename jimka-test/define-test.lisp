;; Copyright (c) 2018 EPITA Research and Development Laboratory
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

(in-package :jimka-test)

(defvar *tests* nil)
(defvar *current-test* nil)


(defmacro define-test (test-name &body body)
  (declare (type symbol test-name))
  `(progn
     (pushnew ',test-name *tests*)
     (defun ,test-name ()
       ,@body)))

(define-condition test-condition () ((code :initarg :code
					   :reader test-condition-code
					   :initform nil)
				     (test :initarg :test
					   :reader test-condition-test
					   :initform *current-test*
					   :type (or null symbol))))
(define-condition test-pass (test-condition) ())
(define-condition test-fail (test-condition) ((expected :reader test-condition-expected
							:initarg :expected)
					      (received :reader test-condition-received
							:initarg :received)
					      (arguments :reader test-condition-arguments
							 :initarg :arguments)))
(define-condition test-error (test-condition) ((error :initarg :error
						      :reader test-condition-error
						      :type error)))
(define-condition test-does-not-exist (test-condition) ())

(defun test-report (num-passed failed errors)
  (format t "------------------~%")
  (format t "Summary of tests:~%")
  (format t "TOTAL TESTS: ~D~%" (length *tests*))
  (format t "ASSERTIONS PASSED: ~D~%" num-passed)
  (format t "ASSERTIONS FAILED: ~D~%" (length failed))
  (let (tests-failed)
    (dolist (f failed)
      (pushnew (test-condition-test f) tests-failed))
    (dolist (f tests-failed)
      (format t "  ~D failed assertions in ~A~%"
	      (count f failed :key #'test-condition-test ) f)))
  (format t "ERRORS: ~D~%" (length errors))
  (dolist (f errors)
    (format t "  ~A~%" (test-condition-test f))))

(defun run-tests (&key (tests *tests*))
  (let ((num-pass 0)
	(failed nil)
	(errors nil)
	(num-tests (length tests))
	(test-num 0))
    (dolist (*current-test* tests)
      (block break
	(labels ((handle-error (e)
		   (declare (type test-error e))
		   (format t "  Error:  ~A~%" (test-condition-code e))
		   (format t "    Msg:  ~A~%" (test-condition-error e))
		   (push e errors)
		   ;; go to next test
		   (return-from break))
		 (handle-fail (f)
		   (declare (type test-fail f))
		   (format t "  Failed: ~A~%" (test-condition-code f))
		   (mapcar (lambda (operand arg)
			     (unless (equal operand arg)
			       (format t "      ~A~%" operand)
			       (format t "        => ~A~%" arg)))
			   (cdr (test-condition-code f))
			   (test-condition-arguments f))
		   (format t "    Expected: ~A~%" (test-condition-expected f))
		   (format t "    Got:      ~A~%" (test-condition-received f))
		   (push f failed))
		 (handle-pass (p)
		   (declare (type test-pass p)
			    (ignore p))
		   (incf num-pass)))
	  (let ((*package* (find-package :keyword)))
	    (format t "Running: ~D/~D ~S~%" (incf test-num) num-tests *current-test*))
	  (handler-bind ((test-pass #'handle-pass)
			 (test-error #'handle-error)
			 (test-fail #'handle-fail))
	    (funcall *current-test*)))))
    (test-report num-pass failed errors)))

(defun run-1-test (test-name)
  (run-tests :tests (list test-name)))

(defun run-package-tests (packages)
  (let (package-tests)
    (dolist (package (if (listp packages)
			 packages
			 (list packages)))
      (do-symbols (name package)
	(when (member name *tests*)
	  (pushnew name package-tests))))
    (run-tests :tests package-tests)))

(defun test-for (expected test-function gen-arguments code)
  (declare (type (member t nil) expected)
	   (type (function () list) gen-arguments)
	   (type function test-function))
  (let* ((arguments (handler-case (funcall gen-arguments)
		      (error (e)
			(signal 'test-error :error e :code code)
			;; exit the test because of error
			(return-from test-for))))
	 (result (handler-case (apply test-function arguments)
		   (error (e)
		     (signal 'test-error :error e :code code)
		     ;; exit the test because of error
		     (return-from test-for)))))
    (cond
      ((and expected result)
       (signal 'test-pass :code code))
      ((or (and expected (not result))
	   (and (not expected) result))
       (signal 'test-fail :code code :arguments arguments :expected expected :received result))
      ((and (not expected) (not result))
       (signal 'test-pass :code code)))))

(defun non-null (object)
  (not (null object)))

(defmacro assert-true (code)
  (typecase code
    (cons
     `(test-for t
		(function ,(car code))
		(lambda ()
		  (list ,@(cdr code)))
		',code))
    (t
     `(test-for t
		#'not-null
		(lambda ()
		  (list ,code))
		',code))))
		

(defmacro assert-false (code)
  (typecase code
    (cons
     `(test-for nil
		(function ,(car code))
		(lambda ()
		  (list ,@(cdr code)))
		',code))
    (t
     `(test-for t
		#'null
		(lambda ()
		  (list ,code))
		',code))))

(defun raises (thunk)
  (let (conditions)
    (ignore-errors
     (handler-bind ((t (lambda (c)
			 (push c conditions))))
       (funcall thunk)))
    conditions))

(defmacro assert-error (error-type-specifier expr)
  `(assert-true (find ',error-type-specifier
		      (raises (lambda ()
				,expr))
		      :test (lambda (type object)
			      (typep object type)))))
(define-test jimka-test-1
  (assert-false (= 1 2))
  (assert-false (= 1 3))
  (assert-true (= 1 1))
  (assert-true (= 2 2))
  (assert-true (= 2 3))
  (assert-error division-by-zero (/ 1 0))
  (let ((a 2)
	(b 1))
    (assert-true (< a b)))
  (assert-true (string= "abc" "ABC")))
