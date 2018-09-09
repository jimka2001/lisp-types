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


(asdf:defsystem :lisp-types-test
  :depends-on (:lisp-types
               :lisp-types-analysis
               :bordeaux-threads
               :closer-mop
	       :jimka-addons
	       :jimka-test
               ;; :sb-profile
               #+sbcl :sb-sprof)
  :components
  ((:module "lisp-types"
    :components
    ((:file "test-setup")
     (:file "debug")
     (:file "test-classes")
     (:file "test-lisp-types" :depends-on ("test-classes"))
     (:file "test-util")
     (:file "test-perf")
     (:file "test-typecase" :depends-on ("test-lisp-types"))
     (:file "test-sat" :depends-on ("test-lisp-types"
                                    "test-perf"
				    "test-classes"))
     (:file "test-graph" :depends-on ("test-lisp-types"
                                      "test-perf"))
     (:file "test-bdd" :depends-on ("test-classes"
                                    "test-lisp-types"
                                    "test-perf"))
     (:file "test-bdd-reduce-17")
     (:file "test-dnf")
     (:file "test-reduce-lisp-type-once")
     (:file "test-bdd-typecase")
     (:file "test-reduce" :depends-on ("test-classes"))
     ))))
