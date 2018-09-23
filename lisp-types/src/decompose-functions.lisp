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


(in-package :lisp-types-analysis)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (defvar *sort-strategies*
     `((:sort-nodes ,#'(lambda (graph)
                         (shuffle-list graph))
        :sort-strategy "SHUFFLE")
       (:sort-nodes ,(lambda (graph)
                       (declare #+sbcl (notinline sort))
                       (sort graph #'< :key #'lisp-types::count-connections-per-node))
        :sort-strategy "INCREASING-CONNECTIONS")
       (:sort-nodes ,(lambda (graph)
                       (declare #+sbcl (notinline sort))
                       (sort graph #'> :key #'lisp-types::count-connections-per-node))
        :sort-strategy "DECREASING-CONNECTIONS")
       (:sort-nodes ,(lambda (graph)
                       (declare #+sbcl (notinline sort))
                       (sort graph #'> :key #'lisp-types::count-parents-per-node))
        :sort-strategy "BOTTOM-TO-TOP")
       (:sort-nodes ,(lambda (graph)
                       (declare #+sbcl (notinline sort))
                       (sort graph #'< :key #'lisp-types::count-parents-per-node))
        :sort-strategy "TOP-TO-BOTTOM"))))

(defun find-sort-strategy-function (name)
  (getf (find name *sort-strategies* :test #'string= :key (getter :sort-strategy))
        :sort-nodes))

(defmacro make-decompose-fun-combos ()
  (let (fun-defs
        prop-defs
        fun-names
        ( operation-combos '((:do-break-sub :strict  :do-break-loop t)
                             (:do-break-sub :relaxed :do-break-loop nil)
                             (:do-break-sub :relaxed :do-break-loop t)))
        ( inner-loops '((:inner-loop :node      :recursive nil)
                        (:inner-loop :operation :recursive t)
                        (:inner-loop :operation :recursive nil)))
        ( sort-nodes (mapcar (getter :sort-strategy) *sort-strategies*))

        )
    (dolist (sort-nodes-arg sort-nodes)
      (dolist (inner-loop-args inner-loops)
        (destructuring-bind (&key inner-loop recursive) inner-loop-args
          (dolist (operation-combo-args operation-combos)
            (destructuring-bind (&key do-break-sub do-break-loop) operation-combo-args
              (let* ((symbol (concatenate 'string
                                          "MDTD-BDD-GRAPH-"
                                          (symbol-name do-break-sub)
                                          "/"
                                          "BREAK-LOOP="
                                          (if do-break-loop "YES" "NO")
                                          "/"
                                          (symbol-name inner-loop)
                                          "/"
                                          "RECURSIVE="
                                          (if recursive "YES" "NO")
                                          "/"
                                          sort-nodes-arg))
                     (fun-name (intern symbol (find-package :lisp-types-analysis)))
                     (props `(:sort-strategy ,sort-nodes-arg
                              ,@inner-loop-args
                              ,@operation-combo-args)))

                (push `(setf (get ',fun-name 'decompose-properties) ',props) prop-defs)
                (push `(defun ,fun-name (type-specifiers)
                         (ltbdd-with-new-hash ()
                           (let ((*bdd-hash-strength* :weak))
                             (parameterized-mdtd-bdd-graph type-specifiers ,@props))))
                      fun-defs)))))))
    (setf fun-names (mapcar #'cadr fun-defs))
    `(progn
       (defvar *decompose-fun-parameterized-names* ',fun-names)
       ,@prop-defs
       ,@fun-defs)))

(make-decompose-fun-combos)




(defun define-mdtd-function (&key names max-num-types
			       gnu-color
			       color
			       (linewidth 1)
			       (legend t))
  (let ((names (if (listp names)
		   names
		   (list names))))
    (setf *decomposition-function-descriptors*
	  (remove names *decomposition-function-descriptors*
		  :key (getter :names)
		  :test-not #'set-exclusive-or))
    (push (list :names names
		:max-num-types max-num-types
		:gnu-color (or gnu-color
			       ;; find first color in (cdr *colors*) which is not already used in *decomposition-function-descriptors*
			       (find-if-not (lambda (color)
					      (find color *decomposition-function-descriptors*
						    :key (getter :gnu-color)
						    ;; string-equal is case-independent string=
						    :test #'string-equal))
					    (cdr *colors*)))
		:linewidth linewidth
		:legend legend
		:color color)
	  *decomposition-function-descriptors*)
    (setf *decomposition-functions*
	  (set-difference (mapcan (lambda (plist)
				    (copy-list (getf plist :names))) *decomposition-function-descriptors*)
			  (cons 'local-minimum *decompose-fun-parameterized-names*)))))

(define-mdtd-function :names 'mdtd-baseline :max-num-types 15)
(define-mdtd-function :names 'mdtd-rtev2)
(define-mdtd-function :names 'mdtd-sat)
(define-mdtd-function :names 'mdtd-graph)
(define-mdtd-function :names 'mdtd-bdd-strong)
(define-mdtd-function :names 'mdtd-bdd-weak)
(define-mdtd-function :names 'mdtd-bdd-weak-dynamic)
(define-mdtd-function :names 'mdtd-bdd)
(define-mdtd-function :names *decompose-fun-parameterized-names*  :gnu-color "b2daff" :color "light-blue" :legend nil)
(define-mdtd-function :names 'mdtd-bdd-graph-strong)
(define-mdtd-function :names 'mdtd-bdd-graph-weak-dynamic)
(define-mdtd-function :names 'mdtd-bdd-graph)
(define-mdtd-function :names 'mdtd-bdd-graph-weak)
(define-mdtd-function :names 'parameterized-mdtd-bdd-graph :gnu-color "ff4444")
(define-mdtd-function :names 'local-minimum :gnu-color "000000" :color "black" :linewidth 2)

(defun parameterization-report (&key (re-run t) (multiplier 1) (create-png-p t) (destination-dir *destination-dir*)
                                  (bucket-reporters *bucket-reporters*))
  (big-test-report :re-run re-run
                   :prefix "param-"
                   :normalize 'mdtd-bdd-graph
                   :hilite-min t
                   :destination-dir destination-dir
                   :multiplier multiplier
                   :create-png-p create-png-p
                   :bucket-reporters bucket-reporters
                   :decomposition-functions (list* 'mdtd-bdd-graph
						   'parameterized-mdtd-bdd-graph
						   *decompose-fun-parameterized-names*)))

(defun find-decomposition-function-descriptor (name)
  (typecase name
    (symbol
     (find-if (lambda (plist)
                (member name (getf plist :names))) 
              *decomposition-function-descriptors*))
    (string
     (find-if (lambda (plist)
                (exists f (getf plist :names)
                  ;; case independent search
                  (string-equal name (symbol-name f))))
              *decomposition-function-descriptors*))))





