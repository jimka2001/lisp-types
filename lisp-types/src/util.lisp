;; Copyright (c) 2017 EPITA Research and Development Laboratory
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


(in-package :lisp-types)


(def-cache-fun (cached-subtypep call-with-subtypep-cache :hash *subtypep-hash*) (sub super)
               "Wrapper around CL:SUBTYPEP.  Manages the caching of return values of SUBTYPEP within the
dynamic extend of WITH-SUBTYPEP-CACHE"
  (subtypep-wrapper sub super))

;;; the following are functions used to re-produce the allegro bug/issue spr43795.

(defun count-cells (data)
  (if (listp data)
      (+ (length data)
         (reduce (lambda (sum item)
                   (+ sum (count-cells item)))
                 data
                 :initial-value 0))
      0))


(defun reduce-repetitions (data)
  (let ((hash (make-hash-table :test #'equal)))
    (labels ((rec (data)
               (mapl (lambda (list &aux (head (car list)) (tail (cdr list)))
                       (when (and head (listp head))
                         (multiple-value-bind (value foundp) (gethash head hash)
                           (cond
                             (foundp
                              (setf (car list) value))
                             (t
                              (rec head)
                              (setf (gethash head hash) head)))))
                       (multiple-value-bind (value foundp) (gethash tail hash)
                         (cond
                           (foundp
                            (setf (cdr list) value))
                           (t
                            (rec tail)
                            (setf (gethash tail hash) tail)))))
                     data)))
      (rec data)
      data)))

(defun count-1-bits (n &aux (bits 0))
  (declare (optimize (speed 3) (debug 0))
           (type (and unsigned-byte fixnum) bits)
           (type unsigned-byte n))
  (while (plusp n)
    (when (oddp n)
      (incf bits))
    (setf n (ash n -1)))
  bits)

(defun count-bit-diffs (a b)
  (declare (type unsigned-byte a b) ; warning maybe bignums
           (optimize (speed 3) (debug 0)))
  (count-1-bits (boole boole-xor a b)))

(defun grey-sort (integers)
  "given a list integers (which may be fixnum or bignum or a mix of the two), 
 put them into an order which makes it likely that adjacent 
 entries are close in terms of number of bits different"
  (declare (optimize (debug 0) (speed 3)))
  (let ((path (list (car integers)))
        (integers (copy-list (cdr integers))))
    (labels ((closest (a &aux (guess (car integers)))
               ;; find the closest element to A in the list INTEGERS
               (dolist (b (cdr integers))
                 (when (< (count-bit-diffs a b)
                          (count-bit-diffs a guess))
                   (setf guess b)))
               guess))
      (while integers
        (let ((i0 (closest (car path))))
          (setf integers (delete i0 integers)) ; remove destructively
          (push i0 path)))
      path)))

(defun seconds-to-hh.mm.ss (seconds)
  (multiple-value-bind (minutes ss) (truncate (truncate seconds) 60)
    (multiple-value-bind (hh mm) (truncate minutes 60)
      (list seconds
            (list hh mm ss)
            (format nil "~D:~D:~D" hh mm ss)))))

