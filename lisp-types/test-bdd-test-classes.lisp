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
