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

(asdf:defsystem :lisp-types
  :version (:read-file-form "version.lisp")
  :author "Jim Newton"
  :description "Lisp type reduction utilities, and CL type-system compatible ROBDD implementation"
  :license "MIT"
  :depends-on (:dispatch
               :cl-robdd
	       :adjuvant)
  :components
  ((:module "src"
    :components
    ((:file "lisp-types-package")
     (:file "subtypep")
     (:file "util")
     (:file "ltbdd")
     (:file "lisp-types")
     (:file "reduce" :depends-on ("lisp-types"))
     (:file "decompose" :depends-on ("reduce"))
     (:file "sat" :depends-on ("lisp-types"))
     (:file "typecase" :depends-on ("lisp-types"))
     (:file "bdd-reduce-generic")
     (:file "bdd-reduce" :depends-on ("bdd-reduce-generic"))
     (:file "bdd-graph" :depends-on ("bdd-reduce"))
     (:file "bdd-reduce-17")
     (:file "decompose-rtev2")
     (:file "bdd-typecase" :depends-on ("bdd-reduce-generic"))
     ))))
