;; Copyright (c) 2016-18 EPITA Research and Development Laboratory
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


(in-package :lisp-types-baker-analysis)

(defun baker-report (&key (re-run t) (multiplier 1.8) (create-png-p t) (destination-dir *destination-dir*)
                       (bucket-reporters *bucket-reporters*))
  (declare (type string destination-dir)
	   (type number multiplier))
  (big-test-report :re-run re-run
                   :prefix "baker-"
                   :multiplier multiplier
                   :normalize nil
                   :time-out 20
                   :num-tries 2
                   :hilite-min nil
                   :destination-dir destination-dir
                   :create-png-p create-png-p
                   :bucket-reporters bucket-reporters
                   :decomposition-functions '( 
                                              mdtd-bdd-graph
                                              mdtd-bdd-graph-baker
                                              mdtd-graph
                                              mdtd-graph-baker)))

(def-report "baker" (&key re-run destination-dir create-png-p &allow-other-keys)
  (baker-report :re-run re-run
		:create-png-p create-png-p
		:bucket-reporters *bucket-reporters*
		:destination-dir  destination-dir))


(defun mdtd-graph-baker (type-specifiers &key &allow-other-keys)
  (let ((*subtypep* #'baker:baker-subtypep))
    (lisp-types::decompose-by-graph-1 type-specifiers :graph-class 'lisp-types::sexp-graph)))

(defun mdtd-bdd-graph-baker (type-specifiers)
  (mdtd-bdd-graph type-specifiers :subtypep #'baker:baker-subtypep))

(define-mdtd-function :names 'mdtd-bdd-graph-baker)
(define-mdtd-function :names 'mdtd-graph-baker)
