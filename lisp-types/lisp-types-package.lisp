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

(defpackage :lisp-types
  (:use :cl :cl-robdd :jimka-addons)
  (:export
   "*SUBTYPEP*"
   "AMBIGUOUS-SUBTYPE"
   "AUTO-PERMUTE-TYPECASE"
   "BDD-DISJOINT-TYPES-P"
   "BDD-EMPTY-TYPE"
   "BDD-GRAPH-TO-DOT"
   "BDD-SUBTYPEP"
   "BDD-TYPE-EQUAL"
   "CACHING-TYPES"
   "CHOOSE-RANDOMLY"
   "COUNT-CONNECTIONS-PER-NODE"
   "COUNT-PARENTS-PER-NODE"
   "DISJOINT-TYPES-P"
   "EQUIVALENT-TYPES-P"
   "LISP-TYPE-BDD-NODE"
   "LTBDD"
   "LTBDD-NODE"
   "LTBDD-WITH-NEW-HASH"
   "MDTD-BASELINE"
   "MDTD-BASELINE"
   "MDTD-BDD"
   "MDTD-BDD-GRAPH"
   "MDTD-BDD-GRAPH-STRONG"
   "MDTD-BDD-GRAPH-WEAK"
   "MDTD-BDD-GRAPH-WEAK-DYNAMIC"
   "MDTD-BDD-STRONG"
   "MDTD-BDD-WEAK"
   "MDTD-BDD-WEAK-DYNAMIC"
   "MDTD-GRAPH"
   "MDTD-GRAPH-BAKER"
   "MDTD-RTEV2"
   "MDTD-SAT"
   "PARAMETERIZED-MDTD-BDD-GRAPH"
   "REDUCE-LISP-TYPE"
   "REDUCED-TYPECASE"
   "REMOVE-SUBS"
   "REMOVE-SUPERS"
   "SHUFFLE-LIST"
   "SMARTER-SUBTYPEP"
   "SUBTYPEP-WRAPPER"
   "VALID-TYPE-P"
   "WARN-AMBIGUOUS-SUBTYPE"
   ))
