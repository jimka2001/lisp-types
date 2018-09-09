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

(defclass Z1 () ())
(defclass Z2 () ())
(defclass Z3 () ())
(defclass Z4 () ())
(defclass Z5 () ())
(defclass Z6 () ())
(defclass Z7 () ())
(defclass Z8 () ())
(defclass Z9 () ())
(defclass ZA () ())
(defclass ZB () ())
(defclass ZC () ())
(defclass Z12345678 (Z1 Z2 Z3 Z4 Z5 Z6 Z7 Z8) ())
(defclass ZCBA987654321 (ZC ZB ZA Z9 Z8 Z7 Z6 Z5 Z4 Z3 Z2 Z1) ())

(defvar *bdd-test-classes* '(ZC ZB ZA Z9 Z8 Z7 Z6 Z5 Z4 Z3 Z2 Z1))

;; these are getting undefined somewhere,  not sure where
(deftype test-float-radix () '(integer 0 (64)))
(deftype test-float-digits () '(integer 0 64))
(deftype test-array-rank () '(integer 0 (65529)))
(deftype test-array-total-size () `(integer 0 (,(- most-positive-fixnum 2))))
(deftype test-char-code () '(integer 0 (#x110000)))
(deftype test-char-int () 'test-char-code)


(defun check-decomposition (given calculated)
  "debugging function to assure that a given list of types GIVEN corresponds correctly
to a set of types returned from %mdtd-bdd."
  (ltbdd-with-new-hash ()
    (let ((bdd-given (bdd `(or ,@given)))
          (bdd-calculated (bdd `(or ,@calculated))))
      (unless (bdd-subtypep bdd-given bdd-calculated)
        (error "union of given types ~A is not a subset of union of~%    calculated types ~A~%difference is ~A"
               given calculated (bdd-to-dnf (bdd-and-not bdd-given bdd-calculated))))
      (unless (bdd-subtypep bdd-calculated bdd-given)
        (error "union of calculated types ~A is not a subset of~%    union of given types ~A~%difference is ~A"
               calculated given (bdd-to-dnf (bdd-and-not bdd-calculated bdd-given))))
      (dolist (c calculated)
        (when (bdd-empty-type (bdd c))
          (error "calculated empty type ~A" c))
        (unless (exists g given
                  (bdd-subtypep (bdd c) (bdd g)))
          (error "calculated type ~A is not a subset of any given type ~A"
                 c given))
        (dolist (c2 (remove c calculated))
          (when (bdd-type-equal (bdd c2) (bdd c))
            (error "calculated two equal types ~A = ~A" c c2)))))))


(defun find-decomposition-discrepancy (&optional (type-specs '(test-array-rank test-array-total-size bignum bit
                                                               complex fixnum float test-float-digits
                                                               test-float-radix integer number ratio rational real
                                                               test-char-code ;; char-int
                                                               double-float ;; long-float
                                                               unsigned-byte)))
  (labels ((recure ( type-specs)
             (when (cdr type-specs)
               (recure (cdr type-specs)))
             (format t "~%~%~%n = ~D~%~%~%~%" (length type-specs))
             (let* ((bdd-types (mdtd-bdd type-specs))
                    (def-types (mdtd-baseline type-specs))
                    (common (intersection bdd-types def-types :test #'equivalent-types-p))
                    (bdd-left-over (set-difference bdd-types common :test #'equivalent-types-p))
                    (def-left-over (set-difference def-types common :test #'equivalent-types-p)))
               (unless (= (length def-types)
                          (length bdd-types))
                 (format t "n=~D bdd=~D  def=~D~%" (length type-specs) (length bdd-types) (length def-types))
                 (format t " given  :~A~%" type-specs)
                 (format t " common :~A~%" common)
                 (format t "    bdd :~A~%" bdd-left-over)
                 (format t "    def :~A~%" def-left-over)
                 (dolist (com common)
                   (dolist (types (list bdd-left-over def-left-over))
                     (dolist (spec types)
                       (when (subtypep-wrapper spec com)
                         (format t " ~A <: ~A~%" spec com))
                       (when (subtypep-wrapper com spec)
                         (format t " ~A <: ~A~%" com spec)))))
                 (format t "checking calculated bdd types~%")
                 (check-decomposition type-specs bdd-types)
                 (format t "checking calculated def types~%")
                 (check-decomposition type-specs def-types)
                 (return-from find-decomposition-discrepancy nil)
                 ))))
    (recure type-specs)))

