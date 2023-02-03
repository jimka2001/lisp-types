;; Copyright (c) 2023 EPITA Research and Development Laboratory
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


(in-package :lisp-types-analysis)

(defun els-2023-analysis ()
  "Generate the files, including the ltxdat files used in the ELS 2023 submission.
These files must be copied from *destination-dir* to
~/Repos/research/publications/src/newton.23.els.includes/.
It is the responsibility of the caller to make this copy."
  (best-2-report :multiplier 1.1
                 :decomposition-functions '(mdtd-bdd-graph
                                            mdtd-bdd
                                            mdtd-padl
                                            mdtd-graph)))

(defclass A1 () ())
(defclass A2 (A1) ())
(defclass A3 (A1) ())
(defclass A23 (A2 A3) ())
(defclass A4 (A1) ())
(defclass A423 (A4 A23) ())
(defclass A5 (A4) ())
(defclass A6 (A1) ())

(mdtd-padl '(A1 A2 A3 A4 A5 A6))
