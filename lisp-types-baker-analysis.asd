;; Copyright (c) 2018,2020 EPITA Research and Development Laboratory
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


(asdf:defsystem :lisp-types-baker-analysis
  :version (:read-file-form "version.lisp")
  :author "Jim Newton"
  :description "Extension of lisp-types-analysis and lisp-types-analysis-test for testing the baker subtypep algorithm"
  :license "MIT"
  :depends-on (:lisp-types
	       :cl-robdd
	       :cl-robdd-test
	       :cl-robdd-analysis-test
	       :lisp-types-analysis
	       :lisp-types-test
               ;; in order to test using fr.epita.lrde.subtypep, you need to
               ;;   push :subtypep-debug onto *feature* before loading
               ;;   this system.
               (:feature :subtypep-debug :fr.epita.lrde.subtypep)
	       :adjuvant
	       :scrutiny)
  :components
  ((:module "src"
    :components
    ((:file "baker-work-around")
     (:file "lisp-types-baker-analysis-package")
     (:file "analysis-baker")
     (:file "test-baker")
     ))))
