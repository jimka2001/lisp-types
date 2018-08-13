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

(define-test baker/decompose-2
  (flet ((equiv-control (a b)
           (equivalent-types-p a b))
         (equiv-cl (a b)
           (let ((*subtypep* #'cl:subtypep))
             (equivalent-types-p a b)))
         (equiv-baker (a b)
           (let ((*subtypep* #'baker:subtypep))
             (equivalent-types-p a b))))
    (let ((types '(string
                   number
                   (or string number)
                   (and string number)
                   (member 1 2 3)
                   (member 1 2)
                   (or (member 1 2) string))))
      (dolist (t1 types)
        (dolist (t2 types)
          (format t "~A ~A~%" t1 t2)
          (format t "control ~A~%" (equiv-control t1 t2))
          (format t "cl      ~A~%" (equiv-cl t1 t2))
          (format t "baker   ~A~%" (equiv-baker t1 t2))
          (assert-true (equal-n (equiv-control t1 t2)
                                (equiv-cl t1 t2)
                                (equiv-baker t1 t2))))))))

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
