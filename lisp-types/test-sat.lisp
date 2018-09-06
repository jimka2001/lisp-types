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

(define-test type/sat1
  ;; crashes in allegro
  (let* ((A '(1 2 3 4 5 6 7 8 9 10 12 13))
         (B '(2 3 5 6))
         (C '(3 4 5 7))
         (D '(5 6 7 8 9 13))
         (E '(9))
         (F '(10))
         (G '(11))
         (H '(9 12 13))
         (types `((member ,@A)
                  (member ,@B)
                  (member ,@C)
                  (member ,@D)
                  (member ,@E)
                  (member ,@F)
                  (member ,@G)
                  (member ,@H))))


    (lisp-types::generate-constraints types)
    (caching-types
      (assert-false (set-exclusive-or '((eql 1)
                                        (eql 2)
                                        (eql 3)
                                        (eql 4)
                                        (eql 5)
                                        (eql 6)
                                        (eql 7)
                                        (eql 8)
                                        (eql 9)
                                        (eql 10)
                                        (eql 11)
                                        (eql 12)
                                        (eql 13))
                                      (mdtd-sat types)
                                      :test #'equivalent-types-p))))

  

  )


(define-test types/sat2
  (let (all-numbers decom decom-sat excl)
     (setf all-numbers (valid-subtypes 'number))
     (length all-numbers)
     (setf decom (mdtd-graph all-numbers))
     (length decom)
     (setf decom-sat (mdtd-sat all-numbers))
     (length decom-sat)
     (setf excl (set-exclusive-or decom decom-sat :test #'equivalent-types-p))
     (assert-true (null excl))
  ))

(define-test type/sat3
  ;; crashes in allegro
  (let* ((A '(1 2 3 4 5 6 7 8 9 10 12 13))
         (B '(2 3 5 6))
         (C '(3 4 5 7))
         (D '(5 6 7 8 9 13))
         (E '(9))
         (F '(10))
         (G '(11))
         (H '(9 12 13))
         (types `((member ,@A)
                  (member ,@B)
                  (member ,@C)
                  (member ,@D)
                  (member ,@E)
                  (member ,@F)
                  (member ,@G)
                  (member ,@H))))

    (lisp-types::generate-constraints types)
    (caching-types
      (assert-false (set-exclusive-or '((eql 1)
                                        (eql 2)
                                        (eql 3)
                                        (eql 4)
                                        (eql 5)
                                        (eql 6)
                                        (eql 7)
                                        (eql 8)
                                        (eql 9)
                                        (eql 10)
                                        (eql 11)
                                        (eql 12)
                                        (eql 13))
                                      (mdtd-sat types)
                                      :test #'equivalent-types-p))))

  

  )

(define-test type/sat4
  (let ((types '(bignum unsigned-byte fixnum)))
    (caching-types
      (assert-false (set-exclusive-or (mdtd-sat types)
                                      (mdtd-graph types)
                                      :test #'equivalent-types-p))))  )


(define-test type/sat5
  (let ((types '(rational 
                 bit
                 real 
                 bignum
                 float
                 unsigned-byte
                 number
                 )))
    (caching-types
      (assert-false (set-exclusive-or (mdtd-sat types)
                                      (mdtd-graph types)
                                      :test #'equivalent-types-p))))
  )

(define-test type/sat6

  (let ((types '(rational bit integer long-float real floating-point-inexact
                 double-float bignum signed-byte float unsigned-byte single-float number fixnum
                 test-char-int complex)))
    (caching-types
      (assert-false (set-exclusive-or (mdtd-sat types)
                                      (mdtd-graph types) :test #'equivalent-types-p)))

    (let ((types '(short-float ratio rational bit integer long-float real floating-point-inexact
                   double-float bignum signed-byte float unsigned-byte single-float number fixnum
                   test-char-int complex)))
      (caching-types
        (assert-false (set-exclusive-or (mdtd-sat types)
                                        (mdtd-graph types)
                                        :test #'equivalent-types-p))))


    (let ((types '(short-float ratio rational bit integer long-float real floating-point-inexact
                   double-float bignum signed-byte float unsigned-byte single-float number fixnum
                   test-char-int complex)))
      (caching-types
        (assert-false (set-exclusive-or (mdtd-sat types)
                                        (mdtd-graph types)
                                        :test #'equivalent-types-p)))))

  

  )



(defun types/sanity-test ()
  (let ((numerical-types '(test-array-rank test-array-total-size bignum bit
                           complex fixnum float test-float-digits
                           float-radix integer number ratio rational real
                           test-char-code ;; test-char-int
                           double-float ;; long-float
                           ;;short-float signed-byte single-float
                           unsigned-byte)))
    (types/cmp-perf :types numerical-types)))
                         

(defun types/cmp-perf-sat ()
  (declare (notinline sort))
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (valid-type-p sym)
	(push sym all-types)))
    (setf all-types (set-difference all-types '(compiled-function test-char-code control-error division-by-zero error)))
    (setf all-types (sort all-types #'string<))
    (let ((testing-types (list (pop all-types))))
      (loop :while testing-types
            :do (progn (format t "~A~%" (car testing-types))
                       (types/cmp-perf :types testing-types )
                       (push (pop all-types) testing-types))))))
		  
