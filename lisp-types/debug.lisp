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

(in-package   :lisp-types-test)

(defun dnf-type-p (type)
  "Function useful for debugging to test whether a given type specifier is in DNF form."
  (labels ((unique-p (list)
             (= (length list)
                (length (remove-duplicates list :test #'equal))))
           (and-term-p (term)
             (and (consp term)
                  (eq 'and (car term))
                  (cddr term)
                  (unique-p (cdr term))
                  (forall t2 (cdr term)
                    (or (other-term-p t2)
                        (not-term-p t2)))))
           (or-term-p (term)
             (and (consp term)
                  (eq 'or (car term))
                  (cddr term)
                  (unique-p (cdr term))
                  (forall t2 (cdr term)
                    (or (and-term-p t2)
                        (not-term-p t2)
                        (other-term-p t2)))))
           (not-term-p (term)
             (and (consp term)
                  (eq 'not (car term))
                  (cdr term)
                  (null (cddr term))
                  (other-term-p (cadr term))))
           (other-term-p (term)
             (or (atom term)
                 (not (member (car term) '(and or not))))))
    (or (and-term-p type)
        (not-term-p type)
        (or-term-p type)
        (other-term-p type))))

(defun derive-constraints (types)
  (loop for tail on types
        nconc (loop for t2 in (cdr tail)
                 with t1 = (car tail)
                 when (cached-subtypep t1 t2)
                   collect (list :subtype t1 t2)
                 when (cached-subtypep t2 t1)
                   collect (list :subtype t2 t1)
                 when (and (cached-subtypep t1 t2)
                           (cached-subtypep t2 t1))
                   collect (list :equal t1 t2)
                 when (null (nth-value 1 (cached-subtypep t1 t2)))
                   collect (list :unknown-subtype t1 t2)
                 when (null (nth-value 1 (cached-subtypep t2 t1)))
                   collect (list :unknown-subtype t2 t1)
                 when (cached-subtypep `(and ,t1 t2) nil)
                   collect (list :disjoint t1 t2)
                 when (null (nth-value 1 (cached-subtypep `(and ,t1 t2) nil)))
                   collect (list :unknown-disjoint t1 t2))))
