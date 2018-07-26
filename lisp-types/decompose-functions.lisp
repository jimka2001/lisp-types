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
                       (sort graph #'< :key #'count-connections-per-node))
        :sort-strategy "INCREASING-CONNECTIONS")
       (:sort-nodes ,(lambda (graph)
                       (declare #+sbcl (notinline sort))
                       (sort graph #'> :key #'count-connections-per-node))
        :sort-strategy "DECREASING-CONNECTIONS")
       (:sort-nodes ,(lambda (graph)
                       (declare #+sbcl (notinline sort))
                       (sort graph #'> :key #'count-parents-per-node))
        :sort-strategy "BOTTOM-TO-TOP")
       (:sort-nodes ,(lambda (graph)
                       (declare #+sbcl (notinline sort))
                       (sort graph #'< :key #'count-parents-per-node))
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
                                          "DECOMPOSE-TYPES-BDD-GRAPH-"
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
                             (slow-decompose-types-bdd-graph type-specifiers ,@props))))
                      fun-defs)))))))
    (setf fun-names (mapcar #'cadr fun-defs))
    `(progn
       (defvar *decompose-fun-parameterized-names* ',fun-names)
       ,@prop-defs
       ,@fun-defs)))

(make-decompose-fun-combos)


(setf *decomposition-function-descriptors*
  (let ((color 0))
    `((:names (decompose-types) :max-num-types 15 :gnu-color ,(nth (incf color) *colors*) :color "blue" :legend t)
      (:names (decompose-types-rtev2) :max-num-types nil  :gnu-color ,(nth (incf color) *colors*) :color "olive" :legend t)
      (:names (decompose-types-sat)  :gnu-color ,(nth (incf color) *colors*) :color "dark-cyan"  :legend t)
      (:names (decompose-types-graph)  :gnu-color ,(nth (incf color) *colors*) :color "lavender" :legend t)
      (:names (bdd-decompose-types-strong)  :gnu-color ,(nth (incf color) *colors*) :color "orange" :legend t)
      (:names (bdd-decompose-types-weak) :gnu-color ,(nth (incf color) *colors*) :color "gold" :legend t)
      (:names (bdd-decompose-types-weak-dynamic) :gnu-color ,(nth (incf color) *colors*) :color "gold" :legend t)
      (:names (bdd-decompose-types) :gnu-color ,(nth (incf color) *colors*) :color "gold" :legend t)
      (:names ,*decompose-fun-parameterized-names*  :gnu-color "b2daff" :color "light-blue" :legend nil)
      (:names (decompose-types-bdd-graph-strong)  :gnu-color ,(nth (incf color) *colors*) :color "red" :linewidth 1  :legend t)
      (:names (decompose-types-bdd-graph-weak-dynamic)  :gnu-color ,(nth (incf color) *colors*) :color "rust" :linewidth 1  :legend t)
      (:names (decompose-types-bdd-graph)  :gnu-color ,(nth (incf color) *colors*) :color "rust" :linewidth 1  :legend t)
      (:names (decompose-types-bdd-graph-weak)  :gnu-color ,(nth (incf color) *colors*) :color "rust" :linewidth 1  :legend t)
      (:names (decompose-types-bdd-graph)  :gnu-color "991818" :color "rust" :linewidth 2  :legend t)
      (:names (slow-decompose-types-bdd-graph) :gnu-color ,(nth (incf color) *colors*) :linewidth 1 :legend t)
      (:names (local-minimum) :gnu-color "000000" :color "black" :linewidth 2 :legend t))))

(setf *decomposition-functions*
      (set-difference (mapcan (lambda (plist)
				(copy-list (getf plist :names))) *decomposition-function-descriptors*)
		      (cons 'local-minimum *decompose-fun-parameterized-names*)))

(defun parameterization-report (&key (re-run t) (multiplier 1) (create-png-p t) (destination-dir *destination-dir*)
                                  (bucket-reporters *bucket-reporters*))
  (big-test-report :re-run re-run
                   :prefix "param-"
                   :normalize 'decompose-types-bdd-graph
                   :hilite-min t
                   :destination-dir destination-dir
                   :multiplier multiplier
                   :create-png-p create-png-p
                   :bucket-reporters bucket-reporters
                   :decomposition-functions (list* 'decompose-types-bdd-graph
						   'slow-decompose-types-bdd-graph
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





