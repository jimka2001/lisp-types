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

(in-package   :lisp-types)

(defun slow-mdtd-rtev2 (type-specifiers)
  (declare ;;(optimize (speed 3) (compilation-speed 0) (debug 0))
           #+sbcl (notinline union))
  (let ((type-specifiers (setof ts (mapcar #'reduce-lisp-type-simple type-specifiers)
			   (not (smarter-subtypep ts nil))))
        (known-disjoint (make-hash-table :test #'equal))
        decomposition) ;; the list of disjoint type-specifiers
    (labels ((disjoint? (T1 T2 &aux (key (list T1 T2)))
               (multiple-value-bind (hit found?) (gethash key known-disjoint)
                 (cond
                   (found? hit)
                   (t
                    (setf (gethash (reverse key) known-disjoint)
                          (setf (gethash key known-disjoint) (disjoint-types-p T1 T2)))))))
	     (forget (type)
	       (setf type-specifiers (remove type type-specifiers :test #'eq)))
             (already-in-list (a b ;; &aux (already (equivalent-types-p a b)))
			       )
	       ;;(format t "  already pending: ~A = ~A ? ~A~%"  a b (if already "yes" "no"))
	       ;; already)
	       (equivalent-types-p a b))
	     (remember (type)
	       (unless (smarter-subtypep type nil)
		 (pushnew type type-specifiers :test #'already-in-list))))
      (while type-specifiers
        ;; (format t "~D type specifiers + ~D decomposition~%" (length type-specifiers) (length decomposition))
	;; (dolist (A type-specifiers)
	;;   (format t "   type specifier ~A~%" A))
	;; (dolist (A decomposition)
	;;   (format t "   decomposition  ~A~%" A))
        (let* ((A (car type-specifiers))
               (intersecting (setof B (cdr type-specifiers)
				 (not (disjoint? A B)))))
          ;;(format t "  know-disjoint=~A~%"  known-disjoint)
	  ;;(maphash (lambda (two-types disjointp) (format t "  ~A ~A~%" disjointp two-types)) known-disjoint)
	  ;;(format t "  intersecting ~D ~A~%" (length intersecting) A)
	  ;;(dolist (B intersecting)
	  ;;  (format t "  ~A~%" B))
          (forget A)
          (cond
            ((null intersecting)
             (pushnew A decomposition :test #'equivalent-types-p))
            (t
             (dolist (B intersecting)
               (forget B)
	       ;;(format t "   closure of~%     ~A~% and ~A~%" A B)
	       ;;(format t "   considering ~A~%" `(and ,A ,B))
               (remember (type-to-dnf `(and ,A ,B)))
	       ;;(format t "   considering ~A~%" `(and ,A (not ,B)))
               (remember (type-to-dnf `(and ,A (not ,B))))
	       ;; (format t "   considering ~A~%" `(and (not ,A) ,B))
               (remember (type-to-dnf `(and (not ,A) ,B))))))))
      (remove-duplicates
       (remove nil (mapcar 'reduce-lisp-type-full decomposition))
       :test #'equal))))


(defun mdtd-rtev2 (type-specifiers)
  (declare (type list type-specifiers))
  "Given a list TYPE-SPECIFIERS of lisp type names, return a list of disjoint, 
non-nil type-specifiers comprising the same union, with each of the resulting
type-specifiers being a sub-type of one of the given type-specifiers."
  (caching-types
    (slow-mdtd-rtev2 type-specifiers)))
