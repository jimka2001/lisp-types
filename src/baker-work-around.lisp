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

#-:sbtypep-debug
(defpackage :FR.EPITA.LRDE.SUBTYPEP
  (:nicknames :baker)
  (:export
   "BAKER-SUBTYPEP"
   "SUBTYPEP"
))

;; some necessary symbols
#-:sbtypep-debug
'( baker::baker-subtypep
	       baker::numeric-types->ranges
	       baker::split-type
	       baker::recursively-expand-type
	       baker::literal-type-null?
	       baker::type-keep-if
	       baker::type/map-atomic-types)

#-:sbtypep-debug
(defun FR.EPITA.LRDE.SUBTYPEP::BAKER-SUBTYPEP (sub super)
  (cl:subtypep sub super))

#-:sbtypep-debug
(defun FR.EPITA.LRDE.SUBTYPEP::SUBTYPEP (sub super)
  (cl:subtypep sub super))
