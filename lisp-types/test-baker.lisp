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

(in-package :lisp-types-test)

(shadow-all-symbols :package-from :lisp-types :package-into :lisp-types-test)

;; i'm not sure why lisp-unit package is required here
(define-test baker/decompose-simple
  (assert-false (decompose-types-graph (list nil)))
  (assert-false (decompose-types-graph-baker (list nil)))
  (assert-true  (decompose-types-graph (list t)))
  (assert-true  (decompose-types-graph-baker (list t))))



(define-test test123
  (flet ((equal-n (&rest args)
           (and (cdr args)
                (forall a args
                  (equal a (car args))))))
    (assert-true (equal-n 3 3 3 3 3 3 3 ))))

(defun equal-n (&rest args)
  (and (cdr args)
       (forall a args
         (equal a (car args)))))

(defvar *testing-types* '(t
                          nil
                          null
                          list
                          cons
                          string
                          number
                          (and symbol (not keyword))
                          (and number (not unsigned-byte))
                          (and bignum unsigned-byte)
                          (and number (not fixnum))
                          (and number (not (integer 12 18)))
                          (and string (not (eql 0)))
                          (double-float 1d1 10d0)
                          (single-float 1f1 20f0)
                          (eql 1f1)
                          (member 1f1 2f2 3f3)
                          (member 1f1 2f2 3f3 0 :x)
                          (or (integer 12 18) (integer 21 56))
                          (or (integer 12 18) ratio)
                          (float 1.0 3.5)
                          (eql 12)
                          (eql :x)
                          (eql 12.0)
                          (eql 12d0)
                          (eql -1)
                          (member -1 0 1)
                          (member 1 2 3)
                          (member :x 12)
                          (real 3/7)
                          (real (3/7))
                          (or string number)
                          (and string number)
                          (member 1 2 3)
                          (member nil :x -1)
                          (member 1 2)
                          (or (member 1 2) string)
                          (or string (real 1.0 31/7))
                          ))

(defun test-baker-2 ()
  (let ((c 0))
    (flet ((equiv (a b)
             (and (baker:subtypep a b)
                  (baker:subtypep b a))))
      (dolist (t1 *testing-types*)
        (dolist (t2 *testing-types*)
          (labels ((check2 (t1 t2)
                     (format t "~d checking ~A vs ~A~%" (incf c) t1 t2)
                     (format t "  ~A~%" (equiv t1 t2)))
                   (check (a b)
                     (check2 a b)
                     (check2 `(not ,a) b)
                     (check2 a `(not ,b))
                     (check2 `(not ,a) `(not ,b))))
            (check t1 t2)
            (dolist (t3 *testing-types*)
              (check t1 `(and ,t2 ,t3))
              (check t1 `(or ,t2 ,t3))
              (check t1 `(or ,t2 (not ,t3)))
              (check t1 `(and ,t2 (not ,t3)))
              (check t1 `(or (and ,t1 (not ,t2)) (and (not ,t1) ,t2))))))))))

(defun test-baker (verbose)
  (flet ((equiv-control (a b)
           (equivalent-types-p a b))
         (equiv-cl (a b)
           (let ((*subtypep* #'cl:subtypep))
             (equivalent-types-p a b)))
         (equiv-baker (a b)
           (let ((*subtypep* #'baker:subtypep))
             (equivalent-types-p a b))))
    (let ((c 0))
      (dolist (t1 *testing-types*)
        (dolist (t2 *testing-types*)
          (labels ((check2 (t1 t2)
                     (when verbose
                       (format t "~d checking ~A vs ~A~%" (incf c) t1 t2)
                       (format t "  control ~A~%" (equiv-control t1 t2))
                       (format t "  cl      ~A~%" (equiv-cl t1 t2))
                       (format t "  baker   ~A~%" (equiv-baker t1 t2)))
                     (assert-true (equal-n (equiv-control t1 t2)
                                           (equiv-cl t1 t2)
                                           (equiv-baker t1 t2))))
                   (check (a b)
                     (check2 a b)
                     (check2 `(not ,a) b)
                     (check2 a `(not ,b))
                     (check2 `(not ,a) `(not ,b))))
            (check t1 t2)
            (dolist (t3 *testing-types*)
              (check t1 `(and ,t2 ,t3))
              (check t1 `(or ,t2 ,t3))
              (check t1 `(or ,t2 (not ,t3)))
              (check t1 `(and ,t2 (not ,t3)))
              (check t1 `(or (and ,t1 (not ,t2)) (and (not ,t1) ,t2))))))))))

(define-test baker/decompose-2
  (test-baker nil))

(define-test baker/decompose-3
  (ltbdd-with-new-hash ()
    (let ((types '((member 1 2) (member 2 3) (member 1 2 3 4))))
      (assert-false (set-exclusive-or (decompose-types-graph types)
                                      (decompose-types-graph-baker types)
                                      :test #'equivalent-types-p)))
    (assert-false (set-exclusive-or (decompose-types-graph '(UNSIGNED-BYTE FIXNUM RATIONAL))
                                    (decompose-types-graph-baker     '(UNSIGNED-BYTE FIXNUM RATIONAL))
                                    :test #'equivalent-types-p))

    (assert-false (set-exclusive-or (decompose-types-graph '(unsigned-byte bit fixnum rational number float))
                                    (decompose-types-graph-baker  '(unsigned-byte bit fixnum rational number float))
                                    :test #'equivalent-types-p))))
