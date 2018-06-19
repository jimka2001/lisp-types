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

(in-package :lisp-types.test)

#|

|#



(let ((package-into (find-package  :lisp-types.test))
      (package-from (find-package  :lisp-types))
      (*package* (find-package :keyword)))
  (do-symbols (name package-from)
    (when (and (eq package-from (symbol-package name))
               (not (find-symbol (symbol-name name) package-into)))
      (format t "importing name=~A into ~S ~%" name package-into)
      (shadowing-import name package-into))))

(defun user-read (&rest args)
  "Calls read with the specified ARGS, but with *PACKAGE* bound to the CL-USER package.  
The effect of this is that symbols like NIL and - get read as COMMON-LISP:NIL and COMMON-LISP:- rather 
than as keywords."
  (let ((*package* (find-package :cl-user)))
    (apply #'read args)))

(defun locate-symbol (name)
  "Return a list of symbols which is a collection of symbols from all packages which have the given symbol-name"
  (let (symbols)
    (dolist (p (list-all-packages))
      (do-symbols (s p)
        (when (and (symbol-name s)
                   (string= name (symbol-name s)))
          (pushnew s symbols))))
    symbols))

(defun valid-subtypes (super)
  (let (all-types)
    (do-external-symbols (sym :cl)
      (when (and (valid-type-p sym)
                 (subtypep sym super))
	(push sym all-types)))
    (remove-duplicates all-types :test (lambda (t1 t2)
                                         (and (subtypep t1 t2)
                                              (subtypep t2 t1))))))



(defvar *cl-types* '(
                     arithmetic-error                  function            simple-condition           
                     array                             generic-function    simple-error               
                     atom                              hash-table          simple-string              
                     base-char                         integer             simple-type-error          
                     base-string                       ;;keyword ;; sbcl has problems with keyword so let's remove it from this list
                     simple-vector              
                     bignum                            list                simple-warning             
                     bit                               logical-pathname    single-float               
                     bit-vector                        long-float          standard-char              
                     broadcast-stream                  method              standard-class             
                     built-in-class                    method-combination  standard-generic-function  
                     cell-error                        nil                 standard-method            
                     character                         null                standard-object            
                     class                             number              storage-condition          
                     ;;compiled-function  ;; ignoring compiled-function because sbcl has lots
                     ;;  of problems with this type.  E.g., (subtypep 'compiled-function 'error) ==> nil,nil
                     ;;  but it should return nil,t
                     package             stream                     
                     complex                           package-error       stream-error               
                     concatenated-stream               parse-error         string                     
                     condition                         pathname            string-stream              
                     cons                              print-not-readable  structure-class            
                     control-error                     program-error       structure-object           
                     division-by-zero                  random-state        style-warning              
                     double-float                      ratio               symbol                     
                     echo-stream                       rational            synonym-stream             
                     end-of-file                       reader-error        t                          
                     error                             readtable           two-way-stream             
                     extended-char                     real                type-error                 
                     file-error                        restart             unbound-slot               
                     file-stream                       sequence            unbound-variable           
                     fixnum                            serious-condition   undefined-function         
                     float                             short-float         unsigned-byte              
                     floating-point-inexact            signed-byte         vector                     
                     floating-point-invalid-operation  simple-array        warning                    
                     floating-point-overflow           simple-base-string                             
                     floating-point-underflow          simple-bit-vector    ))

(defmacro print-conditions (&body body)
  (let ((conditions (gensym "conditions")))
    `(let (,conditions)
       (handler-bind ((t #'(lambda (condition) (push condition ,conditions))))
         (prog1 (progn ,@body)
           (when ,conditions
             (let ((n 0))
               (format t "Conditions signalled while evaluating: ~A~%" ',body)
               (dolist (condition (nreverse ,conditions))
                 (format t "~D: ~S~%" (incf n) condition)))))))))

(defun call-asserting-conditions (thunk condition-types)
  (handler-bind ((t #'(lambda (condition)
                        (assert (member-if (lambda (c-type)
                                             (typep condition c-type))
                                           condition-types)
                                ()
                                "Evaluating expression raised invalid condition: ~A" condition))))
    (funcall thunk)))

(defmacro allowing-conditions (condition-types &body body)
  `(call-asserting-conditions ',condition-types (lambda () ,@body)))


(defun foo (G)
  (allowing-conditions (warning info db-timeout-error)
    (funcall G)))

;; (defmethod print-object ((c SB-KERNEL:PARSE-UNKNOWN-TYPE) stream)
;;   (print-unreadable-object (c stream :type t :identity nil)
;;     (when (slot-boundp c 'SB-KERNEL::specifier)
;;       (format stream " specifier=~A " (SB-KERNEL::parse-unknown-type-specifier c)))))

(defvar *number-combos*

  (let* ((l1 (shuffle-list (valid-subtypes 'number)))
         (l2 (shuffle-list (valid-subtypes 'number)))
         (l3 (loop for t1 in l1
                   for t2 in l2
                   nconc (list t1 `(and ,t1 (not ,t2)) `(or ,t1 ,t2)))))
    (setof e l3
      (not (subtypep e nil)))))



;;(length  *number-combos*)

(defvar *cl-type-combos*
  (loop for types on *cl-types*
        nconc (loop for t2 in (cdr types)
                 with t1 = (car types)
                 nconc (list t1 `(and ,t1 ,t2) `(or ,t1, t2)))))

(defvar *colors* 
 '(
    ;;"000000"
   "e6194b"      ;;Red	(230, 25, 75)	(0, 100, 66, 0)
   "3cb44b"      ;;Green	(60, 180, 75)	(75, 0, 100, 0)
   "ffe119"	;;Yellow	(255, 225, 25)	(0, 25, 95, 0)
   "0082c8"	;;Blue	(0, 130, 200)	(100, 35, 0, 0)
   "f58231"	;;Orange	(245, 130, 48)	(0, 60, 92, 0)
   "911eb4"	;;Purple	(145, 30, 180)	(35, 70, 0, 0)
   "46f0f0"	;;Cyan	(70, 240, 240)	(70, 0, 0, 0)
   "f032e6"	;;Magenta	(240, 50, 230)	(0, 100, 0, 0)
   "d2f53c"	;;Lime	(210, 245, 60)	(35, 0, 100, 0)
   "fabebe"	;;Pink	(250, 190, 190)	(0, 30, 15, 0)
   "008080"	;;Teal	(0, 128, 128)	(100, 0, 0, 50)
   "e6beff"	;;Lavender	(230, 190, 255)	(10, 25, 0, 0)
   "aa6e28"	;;Brown	(170, 110, 40)	(0, 35, 75, 33)
   "fffac8"	;;Beige	(255, 250, 200)	(5, 10, 30, 0)
   "800000"	;;Maroon	(128, 0, 0)	(0, 100, 100, 50)
   "aaffc3"	;;Mint	(170, 255, 195)	(33, 0, 23, 0)
   "808000"	;;Olive	(128, 128, 0)	(0, 0, 100, 50)
   "ffd8b1"	;;Coral	(255, 215, 180)	(0, 15, 30, 0)
   "000080"	;;Navy	(0, 0, 128)	(100, 100, 0, 50)
   "808080"	;;Grey	(128, 128, 128)	(0, 0, 0, 50)
    ))


(defvar *decomposition-function-descriptors*
  (let ((color 0))
    `((:names (decompose-types) :max-num-types 15 :gnu-color ,(nth (incf color) *colors*) :color "blue" :legend t)
      (:names (decompose-types-rtev2) :max-num-types nil  :gnu-color ,(nth (incf color) *colors*) :color "olive" :legend t)
      (:names (decompose-types-sat)  :gnu-color ,(nth (incf color) *colors*) :color "dark-cyan"  :legend t)
      (:names (decompose-types-graph)  :gnu-color ,(nth (incf color) *colors*) :color "lavender" :legend t)
      (:names (bdd-decompose-types-strong)  :gnu-color ,(nth (incf color) *colors*) :color "orange" :legend t)
      (:names (bdd-decompose-types-weak) :gnu-color ,(nth (incf color) *colors*) :color "gold" :legend t)
      (:names (bdd-decompose-types-weak-dynamic) :gnu-color ,(nth (incf color) *colors*) :color "gold" :legend t)
      (:names ,*decompose-fun-parameterized-names*  :gnu-color ,(nth (incf color) *colors*) :color "light-blue" :legend nil)
      (:names (decompose-types-bdd-graph-strong)  :gnu-color ,(nth (incf color) *colors*) :color "red" :linewidth 1  :legend t)
      (:names (decompose-types-bdd-graph-weak-dynamic)  :gnu-color ,(nth (incf color) *colors*) :color "rust" :linewidth 1  :legend t)
      (:names (decompose-types-bdd-graph-weak)  :gnu-color ,(nth (incf color) *colors*) :color "rust" :linewidth 1  :legend t)
      (:names (decompose-types-bdd-graph)  :gnu-color ,(nth (incf color) *colors*) :color "rust" :linewidth 2  :legend t)
      (:names (local-minimum) :gnu-color "000000" :color "black" :linewidth 2 :legend t))))

(defvar *decomposition-functions*
  (set-difference (mapcan (lambda (plist)
                            (copy-list (getf plist :names))) *decomposition-function-descriptors*)
                  (cons 'local-minimum *decompose-fun-parameterized-names*)))

(defun encode-time (time &aux (decoded-time (multiple-value-list (decode-universal-time time))))
  "Create a string similar to the UNIX date command: e.g., \"Thu Aug  3 10:39:18 2017\""
  (destructuring-bind (second minute hour date month year day-of-week ;; (0 = Monday)
                       daylight-savings-times ;; T (daylight savings times) or NIL (standard time)
                       timezone) decoded-time
    (declare (ignore timezone daylight-savings-times))
    (let ((day-of-week (aref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week))
          (month (aref #("no-month" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)))
      (with-output-to-string (str)
        (format str "~A ~A" day-of-week month)
        (format str " ~2D ~2D:~2,'0D:~2,'0D ~S" date hour minute second year)))))

(defun types/cmp-perfs (&key
                          (re-run t)
                          (verify nil)
                          (num-tries 2)
                          (suite-time-out (* 60 10))
                          (randomize nil)
                          (summary "")
                          (limit 15)
                          sample
                          (types (choose-randomly (valid-subtypes 'number) limit))
                          (time-out nil)
                          tag
                          (decompose *decomposition-functions*)
                          normalize
                          hilite-min
                          (create-png-p t)
                          (profile nil)
                          destination-dir
                          (prefix "")
                          file-name)
  (declare (type (or list (and symbol (satisfies symbol-function))) decompose)
           (type string file-name prefix destination-dir))
  (let ((*package* (find-package "KEYWORD"))
        (start-time (get-universal-time))
        (fraction-completion 0)
        (time-out-time (+ (get-universal-time) suite-time-out))
        delayed)
    (labels ((handle (descr len thunk)
               (cond
                 ((and (getf descr :max-num-types)
                       (>= len (getf descr :max-num-types)))
                  nil)
                 (randomize
                  (push thunk delayed))
                 (t
                  (funcall thunk))))
             (log-data ()
               (print-report :re-run re-run
                             :profile profile
                             :include-decompose decompose
                             :destination-dir destination-dir
                             :prefix prefix
                             :file-name file-name
                             :create-png-p create-png-p
                             :summary summary
                             :normalize normalize
                             :time-out time-out
                             :limit limit
                             :hilite-min hilite-min
                             :tag tag))
             (run1 (f len types)
               (lambda (&aux results)
                 (when (> (get-universal-time) time-out-time)
                   (log-data)
                   (return-from types/cmp-perfs 'timed-out))
                 (let ((now (get-universal-time)))
                   (format t "    date:  ~A" (encode-time now))
                   (when (plusp fraction-completion)
                     (format t "  estim finish:  ~A~%" (encode-time
                                                      (truncate (+ start-time
                                                                   (/ (- now start-time)
                                                                      fraction-completion))))))
                   (terpri t))
                 (format t "function:  ~A~%" f)
                 (format t "   tag:    ~A~%" tag)
                 (format t "   length:  ~D~%" len)
                 (let ((result (types/cmp-perf :num-tries num-tries
                                               :types (choose-randomly types len)
                                               :decompose f
                                               :time-out time-out
                                               :profile profile)))
                   (format t " run-time:  ~A~%" (getf result :run-time))
                   (when verify
                     (push result results)
                     (compare/results results)))))
             (make-gausian (len &aux (half (truncate len 2)))
               (+ 2 (random half)
                  (random half)))
             (run (types)
               (dolist (f (if (listp decompose)
                              decompose
                              (list decompose)))
                 (declare (type symbol f))
                 (let ((descr (find-decomposition-function-descriptor f)))
                   (loop :for len :from 2 :to limit
                         :do (let ((g (make-gausian len)))
                               (handle descr g (run1 f g types))
                               (handle descr len (run1 f len types))))))))
      (when re-run
        (run (choose-randomly types limit))
        (when randomize
          (let* ((c (length delayed))
                 (len c))
            (setf delayed (shuffle-list delayed))
            (while delayed
              (format t "sample: ~A~%" sample)
              (format t "countdown ~D/~D  ~D/~D~%" (decf c) len (- time-out-time (get-universal-time)) suite-time-out)
              (setf fraction-completion (- 1
                                           (min (/ c len)
                                                (/ (- time-out-time (get-universal-time)) suite-time-out))))
              (funcall (pop delayed))))))
      (garbage-collect)
      (log-data)))
  t)


(defvar *perf-results* nil)
(defun types/cmp-perf (&key types (decompose 'bdd-decompose-types-weak) (time-out 15) (num-tries 2) profile
                       &aux (f (symbol-function decompose)))
  (declare (type list types)
           (type symbol decompose)
           (type function f))
  (setf types (remove nil types))
  (cond
    ((null types)
     nil)
    ((exists plist *perf-results*
       (and (eq decompose (getf plist :decompose))
            (equal types (getf plist :types))))
     (format t "skipping duplicate ~A ~A~%" decompose types)
     nil)
    (t
     (garbage-collect)
     (let ((result (call-with-timeout time-out
                                      (lambda ()
                                        (funcall f types))
                                      num-tries
                                      :profile profile))
           (num-unknown 0)
           (num-known 0))
       (declare (type (and unsigned-byte fixnum) num-known num-unknown))
       (dolist (t1 types)
         (dolist (t2 types)
           (dolist (t3 (list t1 `(not ,t1)))
             (dolist (t4 (list t2 `(not ,t2)))
               (if (nth-value 1 (subtypep t3 t4))
                   (incf num-unknown)
                   (incf num-known))))))
       (assert (or (typep (getf result :run-time) 'number )
                   (getf result :time-out)) (result))
       (destructuring-bind (&key time-out (run-time 0) (wall-time 0) value profile-plists) result
         (declare (type (or null fixnum) time-out)
                  (type list value)
                  (type number run-time wall-time)
                  (type (or cons null) profile-plists))
         (push
          (cond
            (time-out
             (format t "timed-out: ~D~%" time-out)
             (format t "given: ~D~%" (length types))
             (let ((*package* (find-package "KEYWORD")))
               (format t "given: ~S~%" types))
             ;; :known = number of elements of types X types for which A<B is known
             ;; :unknown = number of elements of types X types for which A<B is unknown
             (list :given (length types)
                   :types types
                   :decompose decompose
                   :known num-known
                   :unknown num-unknown
                   :wall-time (/ wall-time 1.0)
                   :run-time (/ run-time 1.0)
                   :time-out time-out))
            (t
             (unless value
               (format t "   types: ~A~%" types)
               (format t "   value: nil~%"))
             `(:given ,(length types)
               :types ,types
               :decompose ,decompose
               :known ,num-known
               :unknown ,num-unknown
               :time ,(/ run-time 1.0)
               :wall-time ,(/ wall-time 1.0)
               :run-time ,(/ run-time 1.0)
               :calculated ,(length value)
               :value ,value
               ,@(when profile
                   (list :profile-plists profile-plists)))))
          *perf-results*))
       (car *perf-results*)))))


(defun get-all-types ()
  (set-difference (valid-subtypes t) '(t nil class built-in-class
                                       keyword compiled-function ;; sbcl has problems with keyword and compiled-function, so lets ignore these
                                       )))

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

(defun find-decomposition-discrepancy (&optional (type-specs '(test-array-rank test-array-total-size bignum bit
                                                               complex fixnum float test-float-digits
                                                               test-float-radix integer number ratio rational real
                                                               test-char-code ;; char-int
                                                               double-float ;; long-float
                                                               unsigned-byte)))
  (labels ((recure ( type-specs)
             (when (cdr type-specs)
               (recure (cdr type-specs)))
             (format t "~%~%~%n = ~D~%~%~%~%" (length type-specs))
             (let* ((bdd-types (bdd-decompose-types type-specs))
                    (def-types (decompose-types type-specs))
                    (common (intersection bdd-types def-types :test #'equivalent-types-p))
                    (bdd-left-over (set-difference bdd-types common :test #'equivalent-types-p))
                    (def-left-over (set-difference def-types common :test #'equivalent-types-p)))
               (unless (= (length def-types)
                          (length bdd-types))
                 (format t "n=~D bdd=~D  def=~D~%" (length type-specs) (length bdd-types) (length def-types))
                 (format t " given  :~A~%" type-specs)
                 (format t " common :~A~%" common)
                 (format t "    bdd :~A~%" bdd-left-over)
                 (format t "    def :~A~%" def-left-over)
                 (dolist (com common)
                   (dolist (types (list bdd-left-over def-left-over))
                     (dolist (spec types)
                       (when (subtypep spec com)
                         (format t " ~A <: ~A~%" spec com))
                       (when (subtypep com spec)
                         (format t " ~A <: ~A~%" com spec)))))
                 (format t "checking calculated bdd types~%")
                 (check-decomposition type-specs bdd-types)
                 (format t "checking calculated def types~%")
                 (check-decomposition type-specs def-types)
                 (return-from find-decomposition-discrepancy nil)
                 ))))
    (recure type-specs)))

(defun compare/results (all-results &aux (good-results (setof res all-results
                                                         (and res
                                                              (null (getf res :time-out)))))
                                      (good-decomp (mapcar (lambda (plist) (getf plist :decompose)) good-results)))
  ;; results is a list of plists
  ;; each plist has one of two forms
  ;;  keys: (:types :given :decompose :time-out)
  ;;        or
  ;;        (:types :given :calculated :decompose :value :time)
  ;;   :value designates a list of calculated types according to the algorithm :decompose
  ;;   This function compare/results assures that the list of types is the same for each algorithm
  ;;   Ignoring ones which timed out, i.e., :time-out exists in the plist.
  ;;  If a difference is found, an attempt is made to find a smaller input list which also results
  ;;  in different types being calculated
  (labels ((equiv-type-sets (set1 set2)
             (and (= (length set1) (length set2))
                  (bdd-with-new-hash ()
                    (or (null (set-exclusive-or set1 set2 :test #'%equal))
                        (let ((bdd-set1 (bdd `(or ,@set1)))
                              (bdd-set2 (bdd `(or ,@set2))))
                          (and (eq *bdd-false* (bdd-and-not bdd-set1 bdd-set2))
                               (eq *bdd-false* (bdd-and-not bdd-set2 bdd-set1))))))))
           (%equal (t1 t2)
             (or (bdd-type-equal (bdd t1) (bdd t2))
                 (equivalent-types-p t1 t2)))
           (compare (res1 res2)
             (cond ((equiv-type-sets (getf res1 :value) (getf res2 :value)))
                   (t
                    (find-small-difference res1 res2))))
           (touching-pairs (types)
             (loop for tail on types
                   nconc (loop for type2 in (cdr types)
                               with type1 = (car types)
                               if (not (subtypep `(and ,type1 ,type2) nil))
                                 collect (list type1 type2))))
           (find-small-difference (res1 res2)
             (let ((*package* (find-package "KEYWORD")))
               (format t "found difference given=~D ~A=~D ~A=~D~%"
                       (length (getf res1 :types))
                       (getf res1 :decompose)
                       (length (getf res1 :value))
                       (getf res2 :decompose)
                       (length (getf res2 :value)))
               (let* ((smaller (find-smaller (getf res1 :types) (getf res1 :decompose) (getf res2 :decompose)))
                      (v1 (funcall (getf res1 :decompose) smaller))
                      (v2 (funcall (getf res2 :decompose) smaller))
                      (o1 (touching-pairs v1))
                      (o2 (touching-pairs v2))
                      (v1-v2 (bdd-and-not (bdd `(or ,@v1)) (bdd `(or ,@v2))))
                      (v2-v1 (bdd-and-not (bdd `(or ,@v2)) (bdd `(or ,@v2)))))

                 (dolist (pair o1)
                   (warn "~A touching pair: ~A~%" (getf res1 :decompose) pair))
                 (dolist (pair o2)
                   (warn "~A touching pairs: ~A~%" (getf res2 :decompose) pair))

                 (let ((msg (format nil "given=~A calculated~%   ~A=[~D]~A~%   vs ~A=[~D]~A~%  a\\b=~A~%  b\\a=~A~%   common=~A~%  a-b=~A~%  b-a=~A"
                                    smaller
                                    (getf res1 :decompose) (length v1) v1 
                                    (getf res2 :decompose) (length v2) v2
                                    (set-difference v1 v2 :test #'%equal)
                                    (set-difference v2 v1 :test #'%equal)
                                    (intersection v1 v2 :test #'%equal)
                                    v1-v2
                                    v2-v1)))
                   (warn msg)
                   (error msg)))))
           (find-smaller (given f1 f2 &aux v1 v2)
             (format t "searching for smaller error than ~S~%" given)
             (let ((ts (exists t1 given
                         (not (equiv-type-sets (setf v1 (funcall f1 (remove t1 given)))
                                               (setf v2 (funcall f2 (remove t1 given))))))))
               (cond
                 (ts
                  (format t "   found smaller difference given=~D~%" (1- (length given)))
                  (find-smaller (remove (car ts) given) f1 f2))
                 (t
                  given))))
           (check-1 (given-types calculated-types decompose-function)
             (when given-types
               (loop :for types :on calculated-types
                     :do (loop :for t2 :in (cdr types)
                               :with t1 = (car types)
                               :with bdd1 = (bdd (car types))
                               :do (let ((*package* (find-package "KEYWORD"))
                                         (bdd2 (bdd t2)))
                                     (unless (bdd-disjoint-types-p bdd1 bdd2)
                                       (dolist (type given-types)
                                         (let ((fewer (remove type given-types :test #'eq)))
                                           (check-1 fewer
                                                    (funcall decompose-function fewer)
                                                    decompose-function)))
                                       (error "Calculated touching types: ~S touches ~S~%Given types ~S~%Calculated: ~S~% Decomp ~S"
                                              t1 t2 given-types calculated-types good-decomp)))))
               (let* ((bdd-given (bdd `(or ,@given-types)))
                      (bdd-calc  (bdd `(or ,@calculated-types))))
                 (let ((*package* (find-package "KEYWORD")))
                   (unless (bdd-type-equal bdd-given bdd-calc)
                     (warn "found problem with ~S" given-types)
                     (dolist (type given-types)
                       (let ((fewer (remove type given-types)))
                         (warn "  checking with ~S" fewer)
                         (check-1 fewer (funcall decompose-function fewer) decompose-function)))
                     (error "Calculated types not equivalent to given types~% given: ~S~% calculated: ~S~% decompose: ~S~% calculated - given: ~S~% given - calculated: ~S"
                            given-types
                            calculated-types
                            decompose-function
                            (bdd-to-dnf (bdd-and-not bdd-calc bdd-given))
                            (bdd-to-dnf (bdd-and-not bdd-given bdd-calc)))))))))
    (when good-results
      (let ((res1 (car good-results)))
        (bdd-with-new-hash ()
          (check-1 (getf res1 :types) (getf res1 :value) (getf res1 :decompose))))
      
      (dolist (res (cdr good-results))
        (compare (car good-results) res)))))

(defun count-pairs (data predicate)
  (let ((c 0))
    (loop :for tail :on data
          :do (loop :for d2 :in tail
                    :with d1 = (car tail)
                    :do (when (funcall predicate d1 d2)
                          (incf c))))
    c))

(defun group-by (data &key key (test #'eql))
  (declare (type list data)
           (type (function (t) t) key)
           (type (function (t t) t) test))
  (let ((hash (make-hash-table :test test)))
    (dolist (item data)
      (push item (gethash (funcall key item) hash nil)))
    (loop for key being the hash-keys of hash
          collect (list key (gethash key hash)))))

(defun build-string (delimiter data)
  (cond
    ((null data)
     "")
    (t
     (with-output-to-string (str)
       (format str "~A" (car data))
       (dolist (next (cdr data))
         (format str "~A~A" delimiter next))))))

(defun create-gnuplot (sorted-file gnuplot-file png-filename normalize hilite-min include-decompose key create-png-p comment)
  (declare (type (member :smooth :xys) key))
  (let ((min-num-points 1)
        (content (with-open-file (stream sorted-file :direction :input)
                   (format t "reading    ~A~%" sorted-file)
                   (user-read stream nil nil))))
    (with-open-file (stream gnuplot-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format t "[writing to ~A~%" gnuplot-file)
      (destructuring-bind (&key summary sorted &allow-other-keys &aux min-curve min-curve-line-style) content
        (declare (type string summary))
        (if (not sorted)
            (warn "skipping ~S too few points" summary)
            (progn
              (assert (typep sorted 'cons))
              ;; sort DATA so that the order agrees with *decomposition-function-descriptors*
              (setf sorted (mapcan (lambda (desc &aux (names (getf desc :names)))
                                     (setof plist sorted
                                       (exists name names
                                         (string= (getf plist :decompose) (symbol-name name)))))
                                   (setof plist *decomposition-function-descriptors*
                                     (exists name include-decompose
                                       (member name (getf plist :names))))))
              ;; if we are trying to normalize, we need at least two points in the graph
              ;;    so in this case remove DATA which has less that 2 points to plot
              (when normalize
                (setf sorted (setof plist sorted
                               (cdr (getf plist key)))))
              (when hilite-min
                (setf min-curve (reduce (lambda (curve1 curve2)
                                          (if (< (getf curve1 :integral)
                                                 (getf curve2 :integral))
                                              curve1
                                              curve2)) (cdr sorted) :initial-value (car sorted)))
                (assert (typep min-curve 'cons))
                (when (cdr (getf (find-decomposition-function-descriptor (getf min-curve :decompose)) :names))
                  ;; this is one of the 45 curves of the parameterized functions
                  (setf min-curve `(:decompose "LOCAL-MINIMUM" ,@min-curve))))
              (format stream "# summary ~A~%" summary)
              (format stream "# ~A~%" comment)
              (format stream "set term png~%")
              (format stream "set key below~%") ;; enable legend
              (format stream "set title ~S~%" summary)
              (unless normalize
                (format stream "set logscale xy~%"))
              (labels ((xys (curve)
                         (declare #+sbcl (notinline sort))
                         (if (atom (getf curve key))
                             nil
                             (remove-duplicates (sort (copy-list (getf curve key))
                                                      (lambda (a b)
                                                        (if (= (car a) (car b))
                                                            (< (cadr a) (cadr b))
                                                            (< (car a) (car b)))))
                                                :key #'car
                                                :test #'equal))))
                (let* ((line-style 0)
                       (mapping (mapcar (lambda (descr)
                                          (incf line-style)
                                          (format stream "set style line ~D linewidth ~D linecolor rgb \"#~A\"~%"
                                                  line-style
                                                  (or (getf descr :linewidth) 1)
                                                  (getf descr :gnu-color))
                                          ;; collect
                                          `(:line-style ,line-style ,@descr))
                                        *decomposition-function-descriptors*))
                       (normalize-to-xys (when normalize
                                           (let ((plist (or (find (symbol-name normalize) sorted
                                                                  :key (getter :decompose) :test #'string=)
                                                            (error "cannot find :decompose ~A~%   with key=~A~%  with include-decompose=~A~%  in sorted=~A"
                                                                   (symbol-name normalize)
                                                                   key
                                                                   include-decompose
                                                                   sorted))))
                                             (or (xys plist)
                                                 (error "failed to compute points from sorted=~A" plist))))))
                  (assert (or (not normalize) normalize-to-xys)
                          (normalize normalize-to-xys sorted))
                  (incf line-style)
                  (when hilite-min
                    (setf min-curve-line-style line-style)
                    (format stream "set style line ~D linewidth 2 linecolor rgb ~S~%" min-curve-line-style "black"))
                  (labels ((interpolate-y (x0 x1 y1 x2 y2)
                             (+ y1 (* (- y2 y1)
                                      (/ (float (- x0 x1)) (- x2 x1)))))
                           (interpolate (x)
                             (declare (type integer x))
                             (mapl (lambda (tail &aux (xy1 (car tail)) (xy2 (cadr tail)) (xy-tail (cddr tail)))
                                     ;; normalize-to-xys is a list of x-y pairs already ordered by x value, with no duplicate x's
                                     ;; for the given x, interpolate the value of y
                                     ;; 1) if x < the min x from normalize-to-xys
                                     ;; 2) if x is found exactly in normalize-to-xys
                                     ;; 3) if x is strictly between two x's from normalize-to-xys
                                     ;; 4) if x > the max x from normalize-to-xys
                                     (assert xy2 (x normalize tail normalize-to-xys)
                                             "interpolation failed to find x=~A" x)
                                     (cond
                                       ((or (< x (car xy1)) ; case 1
                                            (and xy2        ; case 3
                                                 (> x (car xy1))
                                                 (< x (car xy2)))
                                            (and xy2 ; case 4
                                                 (null xy-tail)
                                                 (> x (car xy2))))
                                        (return-from interpolate (interpolate-y x (car xy1) (cadr xy1) (car xy2) (cadr xy2))))
                                       ((= x (car xy1)) ; case 2
                                        (return-from interpolate (cadr xy1)))
                                       ((and xy2
                                             (= x (car xy2))) ; case 2
                                        (return-from interpolate (cadr xy2)))))
                                   normalize-to-xys))
                           (plot-curve (curve)
                             (destructuring-bind (&key decompose
                                                  &allow-other-keys
                                                  &aux (color (or (getf (find-decomposition-function-descriptor decompose) :gnu-color)
                                                                  (getf (find-decomposition-function-descriptor decompose) :color)))
                                                    (num-points 0))
                                 curve
                               (format stream "# ~A ~A~%" color decompose)
                               (dolist (pair (xys curve))
                                 (destructuring-bind (x y) pair
                                   (cond
                                     (normalize
                                      (incf num-points)
                                      (format stream "~A ~A~%" x (- y (the number (interpolate x)))))
                                     ((zerop y))
                                     (t
                                      (incf num-points)
                                      (format stream "~A ~A~%" x y)))))
                               (setf min-num-points (min min-num-points num-points))
                               (format stream "end #~D~%" num-points)))
                           (null-curves (curves)
                             (or (null curves)
                                     (forall curve curves
                                       (null (getf curve :xys))))))
                    (unless (null-curves sorted)
                      (format stream "# plot ~D curves~%" (length sorted))
                      (format stream "plot ~A"
                              (build-string (format nil ",\\~%")
                                            (mapcar (lambda (data-plist &aux (decompose (getf data-plist :decompose)))
                                                      (let ((mapping-plist (find-if (lambda (mapping-plist)
                                                                                      (exists name (getf mapping-plist :names)
                                                                                        (string= decompose
                                                                                                 (symbol-name name))))
                                                                                    mapping)))
                                                        (with-output-to-string (str)
                                                          (format str "   \"-\" using 1:2 with lines ls ~D"
                                                                  (getf mapping-plist :line-style))
                                                          (format str " title ~S" (string-downcase decompose)))))
                                                    sorted)))
                      (when hilite-min
                        (format stream ",\\~%\   \"-\" using 1:2 with lines ls ~D" min-curve-line-style)
                        (format stream " title ~S~%" "unknown"))
                      (mapc #'plot-curve sorted)
                      (when hilite-min
                        (plot-curve min-curve)))))))))
      (format t "~A ]~%" gnuplot-file))

    (when (and create-png-p
               (plusp min-num-points))
      (run-program "gnuplot" (list gnuplot-file)
                   :search t 
                   :output png-filename
                   :error *error-output*
                   :if-output-exists :supersede))))

(defun integral (xys &key logx)
  "given a list of xy pairs (car cadr) calculate the area under the curve formed by the trapizoids.
the list of xys need not be already ordered."
  (let ((acc 0.0)
        (sorted (sort (copy-list xys) (lambda (p1 p2 &aux (x1 (car p1)) (x2 (car p2)) (y1 (cadr p1)) (y2 (cadr p2)))
                                        (cond ((= x1 x2)
                                               (< y1 y2))
                                              (t
                                               (< x1 x2)))))))
    (mapl (lambda (xys-tail)
            (destructuring-bind ((x1 y1) &optional ((x2 y2) '(nil nil)) &rest tail) xys-tail
              (when tail
                ;; increment by trapizoid area
                (incf acc (* (if logx
                                 (log (/ x2 x1) 10)
                                 (- x2 x1))
                             (/ (+ y2 y1) 2))))))
          sorted)
    (when (minusp acc)
      (warn "negative integral ~A: ~A~%" acc sorted))
    (/ acc (length xys))))

(defun statistics (data)
  (destructuring-bind (&key summary
                         time-out-count
                         run-count
                         time-out-run-time
                         run-time
                         wall-time
                         limit
                         unknown
                         known
                         sorted &allow-other-keys) data
    (let ((integrals (mapcar (getter :integral) sorted)))
      (destructuring-bind (&key count sum) (reduce (lambda (accum this)
                                                     (destructuring-bind (&key count sum) accum
                                                       (list :count (1+ count) :sum (+ this sum)))) integrals
                                                       :initial-value '(:count 0 :sum 0))
        (labels ((sqr (x) (* x x))
                 (stdev (count mean data)
                   (sqrt (/ (reduce (lambda (sum this)
                                      (+ sum (sqr (- this mean))))
                                    data :initial-value 0.0)
                            count))))
          (let* ((mean (unless (zerop count)
                         (/ sum count)))
                 (sigma (when mean
                          (stdev count mean integrals)))
                 previous)
            (list :summary summary
                  :time-out-count time-out-count
                  :run-count run-count
                  :time-out-run-time time-out-run-time
                  :run-time run-time
                  :wall-time wall-time
                  :limit limit
                  ;; :known = number of elements of types X types for which A<B is known
                  ;; :unknown = number of elements of types X types for which A<B is unknown
                  :unknown unknown
                  :known known
                  :sigma sigma
                  :mean mean
                  :sorted (mapcar (lambda (plist)
                                    (destructuring-bind (&key integral samples decompose arguments xys smooth &allow-other-keys
                                                         &aux (score (unless (member sigma '(0 0.0 nil))
                                                                       (/ (- integral mean) sigma)))) plist
                                    (list :integral integral
                                          :score score
                                          :delta (when score
                                                   (prog1 (when previous
                                                            (- previous score))
                                                     (setf previous score)))
                                          :samples samples
                                          :decompose decompose
                                          :arguments arguments
                                          :smooth smooth
                                          :xys xys)))
                                sorted))))))))

(defun append-tail (l1 l2 value)
  (append l1 (mapcar (constantly value) (nthcdr (length l1) l2))))

;; (append-tail '(a b c d) '(0 0 0 0 1 2 3 4 5 6 7) -1)
;;(append-tail '(0 0 0 0 1 2 3 4 5 6 7) '(a b c d) -1)



;; (reduce (lambda (string num) (format nil "~D~S" num string)) '(1 2 3) :initial-value "")


(defun smoothen (xys)
  "Takes a list of (x y) pairs and returns a new list of (x y) pairs with some of the noise filtered out.
The smoothening method is an average of all the points in an x-radius of the given point, 
i.e., of all the points whose xcoord is between x/2 and x*2."
  (flet ((between (x y z)
           (and (<= x y)
                (<= y z)))
         (mean (xs)
           (/ (reduce #'+ xs :initial-value 0.0)
              (float (length xs)))))

    (let* ((radius 2))
      (loop :for xy :in xys
            :for x0 = (car xy)
            :for close = (loop :for sample :in xys
                               :when (between (/ x0 radius)
                                              (car sample)
                                              (* x0 radius))
                                 :collect (cadr sample))
            :collect (list x0 (mean close))))))

(defun sort-results (in out &rest options)
  (declare #+sbcl (notinline sort))
  (cond
    ((eq in nil) nil)
    ((typep in '(or pathname string))
     (format t "[reading from ~A~%" in)
     (with-open-file (stream in :direction :input :if-does-not-exist nil)
       (prog1 (apply #'sort-results stream out options)
         (format t "]~%"))))
    ((typep out '(or pathname string))
     (ensure-directories-exist out)
     (with-open-file (stream out :direction :output :if-exists :supersede :if-does-not-exist :create)
       (format t "[writing to ~A~%" out)
       (prog1 (apply #'sort-results in stream options)
         (format t "]~%"))))
    ((eq out :return)
     ;; (reduce (lambda (num string) (format nil "~D~S" num string)) '(1 2 3) :initial-value "")
     (destructuring-bind (&key summary limit data time-out-count run-count time-out-run-time run-time wall-time unknown known)
         (user-read in nil nil)
       (declare (type string summary))
       (let (observed-max-time observed-min-time observed-min-prod observed-max-prod)
         (dolist (plist data)
           (mapc (lambda (given calculated time)
                   (setf observed-max-prod (if observed-max-prod
                                               (max observed-max-prod (* given calculated))
                                               (* given calculated)))
                   (setf observed-min-prod (if observed-min-prod
                                               (min observed-min-prod (* given calculated))
                                               (* given calculated)))
                   (setf observed-max-time (if observed-max-time
                                               (max observed-max-time time)
                                               time))
                   (setf observed-min-time (if observed-min-time
                                               (min observed-min-time time)
                                               time)))
                 (getf plist :given) (getf plist :calculated) (getf plist :run-time)))
         (statistics
          (list :summary summary
                :time-out-count time-out-count
                :run-count run-count
                :time-out-run-time time-out-run-time
                :run-time run-time
                :wall-time wall-time
                :limit limit
                ;; :known = number of elements of types X types for which A<B is known
                ;; :unknown = number of elements of types X types for which A<B is unknown
                :unknown unknown
                :known known
                :sorted (sort 
                         (loop for plist in data
                               collect (destructuring-bind (&key decompose given calculated run-time &allow-other-keys) plist
                                         (flet ((cmp (a b)
                                                  (if (= (car a) (car b))
                                                      (< (cadr a) (cadr b))
                                                      (< (car a) (car b)))))
                                           (let* ((xys (mapcar (lambda (given calculated run-time)
                                                                 (declare (type fixnum given calculated)
                                                                          (type number run-time))
                                                                 (list (* given calculated) run-time)) given calculated run-time))
                                                  (xys-extended xys))
                                             (unless (find observed-min-prod xys-extended :key #'car)
                                               (push (list observed-min-prod observed-min-time) xys-extended))
                                             (unless (find observed-max-prod xys-extended :key #'car)
                                               (push (list observed-max-prod observed-max-time) xys-extended))
                                             ;; if two adjacent points have the same x, remove the second, because
                                             ;;  its y value is larger, thanks to the sort #'cmp
                                             (setf xys (remove-duplicates (sort (copy-list xys) #'cmp)
                                                                          :key #'car :from-end t))
                                             (setf xys-extended (remove-duplicates (sort (copy-list xys-extended) #'cmp)
                                                                                   :key #'car :from-end t))
                                             (list :integral (integral xys-extended)
                                                   :xys xys
                                                   :smooth (smoothen xys)
                                                   :samples (length xys)
                                                   :decompose decompose
                                                   :arguments (get (intern decompose (find-package :lisp-types))
                                                                   'decompose-properties))))))
                         #'< :key (lambda (obj) (getf obj :integral))))))))
    (t
     (let ((*package* (find-package "CL")))
       (format out "~S~%" (apply #'sort-results in :return options))))))

(defun print-sexp (stream summary limit)
  (let ((groups (group-by (cdr *perf-results*) :key (getter :decompose))))
    (format stream "(")
    (when summary
      (format stream " :SUMMARY ~S~%" summary))
    (format stream "  :LIMIT ~D~%" limit)
    (format stream "  :TIME-OUT-COUNT ~D~%" (count-if (getter :time-out) *perf-results*))
    (format stream "  :RUN-COUNT ~D~%" (count-if (getter :calculated) *perf-results*))
    (format stream "  :TIME-OUT-RUN-TIME ~A~%" (reduce (lambda (total next)
                                                         (if (getf next :time-out)
                                                             (+ total (getf next :run-time))
                                                             total)) *perf-results*
                                                             :initial-value 0))
    (format stream "  :RUN-TIME ~A~%" (reduce (lambda (total next)
                                            (if (getf next :calculated)
                                                (+ total (getf next :run-time))
                                                total))
                                              *perf-results*
                                              :initial-value 0))
    (format stream "  :WALL-TIME ~A~%" (reduce (lambda (total next)
                                            (if (getf next :calculated)
                                                (+ total (getf next :wall-time))
                                                total))
                                               *perf-results*
                                               :initial-value 0))
    (format stream " :DATA  (")
    (flet ((print-group (group)
             (destructuring-bind (decompose data) group
               (format stream "( :DECOMPOSE ~S" (symbol-name decompose))
               (when (get decompose 'lisp-types::decompose-properties)
                 (let ((*package* (find-package "KEYWORD")))
                   (format stream "~%:OPTIONS ~S" (get decompose 'lisp-types::decompose-properties))))
               (let ((time-outs (setof item data
                                  (getf item :time-out)))
                     (no-time-out (setof item data
                                    (null (getf item :time-out)))))
                 (when time-outs
                   (format stream "~%:TIME-OUT (:TIME ~D :COUNT ~D~%"
                           (getf (car time-outs) :time-out) 
                           (length time-outs))
                   (format stream ":GIVEN ~A)"
                           (mapcar (getter :given) time-outs)))

                 (dolist (tag `(:given :calculated :run-time :wall-time :known :unknown :profile-plists :value :types))
                   (format stream "~%~S (" tag)
                   (when no-time-out
                     (let ((*package* (find-package "CL")))
                       (format stream "~S" (getf (car no-time-out) tag))))
                   (dolist (item (cdr no-time-out))
                     (when (consp (getf item tag))
                       (format stream "~%"))
                     (let ((*package* (find-package "CL")))
                       (format stream " ~S" (getf item tag))))
                   (format stream ")")))
               (format stream ")~%"))))
      (when (car groups)
        (print-group (car groups)))
      (dolist (group (cdr groups))
        (format stream " ")
        (print-group group)))
    (format stream "))~%"))
  nil)

(defun print-ltxdat (ltxdat-name sorted-name include-decompose legendp tag)
  (let ((content (with-open-file (stream sorted-name :direction :input :if-does-not-exist :error)
                   (user-read stream))))
    (destructuring-bind (&key sorted &allow-other-keys) content
      (ensure-directories-exist ltxdat-name)
      (with-open-file (stream ltxdat-name :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format t "writing to ~A~%" ltxdat-name)
        (format stream "\\begin{tikzpicture}~%")
        (format stream "\\begin{axis}[~% ")
        (when tag
          (format stream "title=~A,~% " tag))
        (format stream "xlabel=Size,~% ylabel=Time,~% xmode=log,~% ymode=log,~% legend style={at={(0.5,-0.2)},anchor=north},~% xmajorgrids,~% xminorgrids,~% ymajorgrids,~% legend style={font=\\tiny},~% xticklabel style={font=\\tiny},~% yticklabel style={font=\\tiny},~% label style={font=\\tiny}~%]~%")
        (let ((*print-case* :downcase)
              (min-curve (reduce (lambda (curve1 curve2)
                                  (if (< (getf curve1 :integral)
                                         (getf curve2 :integral))
                                      curve1
                                      curve2)) (cdr sorted) :initial-value (car sorted)))
              legend)
          (setf min-curve (if (cdr (getf (find-decomposition-function-descriptor (getf min-curve :decompose)) :names))
                              ;; this is one of the 45 curves of the parameterized functions
                              `(:decompose "LOCAL-MINIMUM" ,@min-curve)
                              nil))
          (flet ((plot (xys decompose descr)
                   (let* ((decimal (parse-integer (getf descr :gnu-color) :radix 16))
                          (red   (/ (logand decimal #xff0000) #x10000))
                          (green (/ (logand decimal #x00ff00) #x100))
                          (blue  (logand decimal #x0000ff)))
                     (format stream "\\definecolor{color~A}{RGB}{~A,~A,~A}~%" (getf descr :gnu-color) red green blue)
                     (format stream "\\addplot[color=color~A] coordinates {~%" (getf descr :gnu-color)))
                   (dolist (xy xys)
                     (format stream "(~D, ~S)~%" (car xy) (cadr xy)))
                   (format stream "};~%")
                   (push (if (getf descr :legend)
                             (format nil "~A" decompose)
                             "") legend)))
            (dolist (descr *decomposition-function-descriptors*)
              (dolist (curve sorted)
                (destructuring-bind (&key decompose xys &allow-other-keys
                                     &aux (name (find-if (lambda (name)
                                                           (string= (symbol-name name) decompose))
                                                         include-decompose))
                                       (descr2 (find-decomposition-function-descriptor name)))
                    curve
                  ;; decompose is a string such as "DECOMPOSE-TYPES-BDD-GRAPH"
                  ;; include-decompose contains symbols such as DECOMPOSE-TYPES-BDD-GRAPH
                  (when (and (eq descr descr2) name)
                    (plot xys decompose descr)))))
            (when min-curve
              (let ((descr (find-decomposition-function-descriptor (getf min-curve :decompose))))
                (plot (getf min-curve :xys)
                      (getf min-curve :decompose)
                      descr))))
          (format stream "~A\\legend{~A};~%" (if legendp "" "%% ") (build-string ", " (reverse legend)))
          (format stream "~A\\legend{};~%" (if legendp "%% " "")))
        (format stream "\\end{axis}~%")
        (format stream "\\end{tikzpicture}~%")))))
            
(defun print-dat (dat-name include-decompose)
  (with-open-file (stream dat-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "given calculated sum product touching disjoint disjoint*given touching*given new time decompose~%")    
    (let ((domain (remove-duplicates *perf-results*
                                     :test #'equal
                                     :key #'(lambda (plist)
                                              (list (getf plist :decompose)
                                                    (getf plist :types))))))
      (dolist (plist domain)
        (destructuring-bind (&key given calculated decompose value types time time-out &allow-other-keys) plist
          (when time-out
            (setf time time-out))
          (unless calculated
            (let ((hit (find-if (lambda (plist)
                                  (equal given (getf plist :given)))
                                domain)))
              (setf calculated (getf hit :calculated))))
           
          (cond
            ((or (not given) (not calculated)))
            ((< time 0.0011))
            ((= 0 time))
            ((member decompose include-decompose)
             (let ((*print-case* :downcase)
                   (num-disjoint (count-pairs types #'disjoint-types-p))
                   (num-touching (count-pairs types #'(lambda (x y)
                                                        (not (disjoint-types-p x y))))))
               (format stream "~D ~D ~D ~D ~D ~D ~D ~D ~D ~S ~A~%"
                       given calculated
                       (+ given calculated)    ;; sum
                       (* given calculated)    ;; product
                       num-touching            ;; touching
                       num-disjoint            ;; disjoint
                       (* num-disjoint given)  ;; disjoint * given
                       (* num-touching given)  ;; touching * given
                       (length (set-difference value types :test #'equivalent-types-p)) ;; new
                       time decompose)))))))))

(defun change-extension (filename new-extension)
  "change file name extension:
E.g., (change-extension \"/path/to/file.gnu\" \"png\") --> \"/path/to/file.png\""
  (let ((index (search "." filename :from-end t)))
    (when index
      (let ((head (subseq filename 0 index)))
        (concatenate 'string head "." new-extension)))))

(defun insert-suffix (filename suffix)
  "insert the given SUFFIX before the filename extension: 
 E.g., (insert-suffix \"/path/to/file.gnu\" \"-smooth\") --> \"/path/to/file-smooth.gnu\""
  ;; find the final "."
  (let ((index (search "." filename :from-end t)))
    (when index
      (let ((tail (subseq filename index))
            (head (subseq filename 0 index)))
        (concatenate 'string head suffix tail)))))

(defun print-report (&key (re-run t) (profile nil)
                       limit (summary nil) normalize destination-dir
                       prefix file-name (create-png-p t) (include-decompose *decomposition-functions*) (tag "NO TITLE") (hilite-min nil)
                     &allow-other-keys
                     &aux
                       (sexp-name (make-output-file-name :sexp-name destination-dir prefix file-name))
                       (sorted-name (make-output-file-name :sorted-name destination-dir prefix file-name))
                       (gnuplot-name (make-output-file-name :gnuplot-name destination-dir prefix file-name))
                       (ltxdat-name  (make-output-file-name :ltxdat-name destination-dir prefix file-name))
                       (ltxdat-no-legend-name (make-output-file-name :ltxdat-no-legend-name destination-dir prefix file-name))
                       (dat-name (make-output-file-name :dat-name destination-dir prefix file-name))
                       (png-name (make-output-file-name :png-name destination-dir prefix file-name))
                       (gnuplot-normalized-name (make-output-file-name :gnuplot-normalized-name destination-dir prefix file-name))
                       (png-normalized-name (make-output-file-name :png-normalized-name destination-dir prefix file-name)))
  (declare (type string prefix destination-dir))
  (format t "report ~A~%" summary)
  (when re-run
    (with-open-file (stream sexp-name :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format t "writing to ~A~%" sexp-name)
      (print-sexp stream summary limit)))
  (sort-results sexp-name sorted-name)

  (unless profile
    (create-gnuplot sorted-name gnuplot-name png-name
                    nil hilite-min include-decompose :xys create-png-p "1314")
    (create-gnuplot sorted-name (insert-suffix gnuplot-name "-smooth") (insert-suffix png-name "-smooth")
                    nil hilite-min include-decompose :smooth create-png-p "1316")
    (when normalize
      (create-gnuplot sorted-name gnuplot-normalized-name png-normalized-name
                      normalize hilite-min include-decompose :xys create-png-p "1319")
      (create-gnuplot sorted-name (insert-suffix gnuplot-normalized-name "-smooth") (insert-suffix png-normalized-name "-smooth")
                      normalize hilite-min include-decompose :smooth create-png-p "1321")))

  (when profile
    (create-profile-scatter-plot sexp-name destination-dir prefix file-name create-png-p :smooth nil :comment "1324")
    (create-profile-scatter-plot sexp-name destination-dir prefix file-name create-png-p :smooth t   :comment "1325"))


  (print-ltxdat ltxdat-name           sorted-name include-decompose t nil)
  (print-ltxdat ltxdat-no-legend-name sorted-name include-decompose nil tag)
  (print-dat dat-name include-decompose))

(defun create-profile-scatter-plot (sexp-name destination-dir prefix file-name create-png-p
                                    &key smooth (threshold 0.15) (comment ""))
  (let ((sexp (with-open-file (stream sexp-name :direction :input)
                (user-read stream nil nil))))
    (destructuring-bind (&key summary data &allow-other-keys) sexp
      (dolist (data-plist data)
        (destructuring-bind (&key decompose profile-plists &allow-other-keys) data-plist
          (let ((gnu-name (insert-suffix (make-output-file-name :gnuscatter-name destination-dir prefix file-name)
                                         (concatenate 'string "-" decompose
                                                      (if smooth "-smooth" "")))))
            (with-open-file (gnu gnu-name
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (format t "[writing to ~A~%" gnu-name)
              (format gnu "# ~A~%" comment)
              (format gnu "# scatter plot for ~A ~S~%" summary decompose)
              (format gnu "set term png~%")
              (format gnu "set logscale x~%")
              (format gnu "set xlabel ~S~%" "execution time (seconds)")
              (format gnu "set ylabel ~S~%" "profile percentage")
              (format gnu "set key bmargin~%")
              (format gnu "set title ~S~%" (format nil "~A ~A" summary decompose))
              (let ((hash (make-hash-table :test #'equal))
                    top-names)
                (declare (notinline sort))
                (dolist (profile-plist profile-plists)
                  (destructuring-bind (&key dprofile-total dprof &allow-other-keys) profile-plist
                    (dolist (dprof-plist (subseq dprof 0 1))
                      (destructuring-bind (&key (seconds 0.0) name &allow-other-keys) dprof-plist
                        (when (plusp seconds)
                          (let ((y (/ seconds dprofile-total)))
                            (when (> y threshold)
                              (pushnew name top-names :test #'string=))))))))

                (dolist (profile-plist profile-plists)
                  (destructuring-bind (&key n-dtimes dprofile-total dprof &allow-other-keys) profile-plist
                    (dolist (dprof-plist dprof)
                      (destructuring-bind (&key (seconds 0.0) name &allow-other-keys) dprof-plist
                        (when (and (plusp seconds)
                                   (member name top-names :test #'string=))
                          (let ((x (/ dprofile-total n-dtimes))
                                (y (/ seconds dprofile-total)))
                            (push (list x y) (gethash name hash nil))))))))
                (when (< 5 (length top-names))
                  (setf top-names (subseq (sort top-names #'> :key (lambda (name)
                                                                     (unless (gethash name hash nil)
                                                                       (format t "~A has no xy points~%" name))
                                                                     (integral (gethash name hash nil) :logx t)))
                                          0 5)))
                (setf top-names (sort top-names #'string<))
                (when top-names
                  (format gnu "plot ")
                  (let ((header (make-string-output-stream))
                        (footer (make-string-output-stream)))
                    (flet ((plot (function-name)
                             (declare (notinline sort) (type string function-name))
                             (format header "~S using 1:2" "-")
                             (if (cdr (gethash function-name hash))
                                 (format header " with linespoints")
                                 (format header " with points"))
                             (format header " title ~S" function-name)
                             (format footer "# ~S~%" function-name)
                             (let ((xys (sort (gethash function-name hash) #'< :key #'car)))
                               (dolist (xy (if smooth (smoothen xys) xys))
                                 (format footer "~A ~A~%" (car xy) (* 100 (cadr xy)))))
                             (format footer "end~%")))
                      (plot (car top-names))
                      (dolist (function-name (cdr top-names))
                        (format header ",\\~%    ")
                        (plot function-name)))
                    (format gnu "~A~%" (get-output-stream-string header))
                    (format gnu "~A" (get-output-stream-string footer))))))
            (format t "   ~A]~%" gnu-name)
            (when create-png-p
              (run-program "gnuplot" (list gnu-name)
                           :search t
                           :output (change-extension gnu-name "png")
                           :error *error-output*
                           :if-output-exists :supersede))))))))

(defvar *destination-dir* "/Users/jnewton/newton.16.edtchs/src")
(defun test-report (&key sample (prefix "") (re-run t) (suite-time-out (* 60 60 4))
                      (time-out (* 3 60)) normalize (destination-dir *destination-dir*)
                      types file-name (limit 15) tag hilite-min (num-tries 2) profile (create-png-p t)
                    &allow-other-keys)
  "TIME-OUT is the number of seconds to allow for one call to a single decompose function.
SUITE-TIME-OUT is the number of time per call to TYPES/CMP-PERFS."
  (declare (type string file-name prefix))
  (when re-run
    (setf *perf-results* nil))
  (let ((type-specifiers (shuffle-list types)))
    (types/cmp-perfs :re-run re-run
                     :types type-specifiers
                     :suite-time-out suite-time-out
                     :randomize t
                     :limit (truncate limit)
                     :tag tag
                     :summary file-name
                     :time-out time-out
                     :decompose  *decomposition-functions*
                     :normalize normalize
                     :sample sample
                     :num-tries num-tries
                     :hilite-min hilite-min
                     :profile profile
                     :create-png-p create-png-p
                     :destination-dir destination-dir
                     :prefix prefix
                     :file-name file-name)))

(defun make-output-file-name (purpose destination-dir prefix file-name)
  (declare (type keyword purpose)
           (type string destination-dir prefix file-name))
  (ensure-directories-exist
   (format nil (ecase purpose
                 (:ltxdat-name
                  "~A/~A~A.ltxdat")
                 (:ltxdat-no-legend-name
                  "~A/no-legend/~A~A.ltxdat")
                 (:dat-name
                  "~A/~A~A.dat")
                 (:png-name
                  "~A/~A~A.png")
                 (:png-normalized-name
                  "~A/~A~A-normalized.png")
                 (:gnuscatter-name
                  "~A/~A~A-scatter.gnu")
                 (:gnuplot-name
                  "~A/~A~A.gnu")
                 (:gnuplot-normalized-name
                  "~A/~A~A-normalized.gnu")
                 (:sexp-name
                  "~A/~A~A.sexp")
                 (:sorted-name
                  "~A/~A~A.sorted"))
           destination-dir prefix file-name)))

(defun random-subset-of-range (min max)
  (loop for i from min to max
        when (zerop (random 2))
          collect i))

(defun ranges (type min max)
  (when (> min max)
    (rotatef min max))
  (list `(,type ,min ,max)
        `(,type (,min) ,max)
        `(,type ,min (,max))
        `(,type (,min) (,max))))

(defvar *real-number-range-types* 
  (remove-duplicates
   (loop for i from 1 to 25
         nconc (let* ((a (random 100))
                      (b (random 100))
                      (c (random 100))
                      (d (random 100))
                      (min (/ a (1+ b)))
                      (max (/ c (1+ d))))
                 (ranges 'real min max))
         nconc (ranges 'float (random 100.0) (random 100.0))
         nconc (ranges 'integer (random 100) (random 100))
         )
   :test #'equal))

(defvar *integer-range-types*
  (remove-duplicates
   (loop for i from 1 to 25
         nconc (let* ((min (random (expt 10 (random i))))
                      (max (random (expt 10 (random i)))))
                 (ranges 'integer min max)))
   :test #'equal))

(defvar *member-types* (remove-duplicates
                        (loop for i from 1 to 25
                              collect (let ((s (random-subset-of-range 0 10)))
                                        (cond ((cdr s)
                                               (cons 'member s))
                                              (s
                                               (cons 'eql s))
                                              (t
                                               'null))))
                        :test #'equal))

(defun analysis (file-names)
  (declare #+sbcl (notinline sort))
  (let* ((measures '(:recursive :inner-loop :sort-strategy :do-break-sub :do-break-loop))
        (table (make-hash-table :test #'eq))
        (data (sort (mapcan (lambda (file-name)
                              (with-open-file (stream file-name)
                                (destructuring-bind (&key summary sorted &allow-other-keys) (user-read stream)
                                  (mapcar (lambda (sorted-plist)
                                            (destructuring-bind (&key score integral arguments &allow-other-keys) sorted-plist
                                              (dolist (measure measures)
                                                (pushnew (getf arguments measure) (gethash measure table) :test #'equal))
                                              (destructuring-bind (&key sort-strategy inner-loop recursive do-break-sub do-break-loop &allow-other-keys) arguments
                                                (list :score score
                                                      :recursive recursive
                                                      :inner-loop inner-loop
                                                      :sort-strategy sort-strategy
                                                      :do-break-sub do-break-sub
                                                      :do-break-loop do-break-loop
                                                      :integral integral
                                                      :summary summary))))
                                          sorted))))
                            file-names)
                    #'< :key (getter :score))))
    (flet ((mean (numbers &aux (c 0))
             (/ (reduce (lambda (acc next)
                          (incf c)
                          (+ acc next)) numbers :initial-value 0.0) c)))
      (list :attributes
            (sort (mapcan (lambda (measure)
                      (mapcar (lambda (value)
                                (list measure value (mean (mapcar (getter :score) (setof plist data
                                                                                    (equal value (getf plist measure)))))))
                              (gethash measure table)))
                    measures)
                  #'< :key #'caddr)
            :data data))))

(defun do-analysis ()
  (analysis '("/Users/jnewton/newton.16.edtchs/src/bdd-graph-cl-combos.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-cl-types.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-member.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-pcl-types.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-condition.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-number-or-condition.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-number.sorted"
              "/Users/jnewton/newton.16.edtchs/src/bdd-graph-subtypes-of-t.sorted")))

(defvar *bucket-reporters* nil)

(defun make-bucket-reporter (&key tag scale types file-name)
  (declare (type string file-name))
  (lambda (multiplier sample options &key (create-png-p t) (destination-dir *destination-dir*))
    (apply #'test-report :limit (* multiplier scale)
                         :tag tag
                         :types types
                         :file-name file-name
                         :sample sample
                         :create-png-p create-png-p
                         :destination-dir destination-dir
                         options)))

(defun add-bucket-reporter (&key tag scale types file-name)
  (declare (type string file-name))
  (let ((pair (assoc tag *bucket-reporters* :test #'equal)))
    (setf *bucket-reporters* (remove pair *bucket-reporters*))
    (push (list tag (make-bucket-reporter :tag tag :scale scale :types types :file-name file-name))
          *bucket-reporters*)))

(add-bucket-reporter :scale 20
                     :tag "Real number ranges"
                     :types *real-number-range-types*
                     :file-name "number-ranges")

(add-bucket-reporter :scale 20
                     :tag "Integer ranges"
                     :types *integer-range-types*
                     :file-name "integer-ranges")

(add-bucket-reporter :scale 18
                     :tag "Subtypes of CONDITION"
                     :types (valid-subtypes 'condition)
                     :file-name "subtypes-of-condition")

(add-bucket-reporter :scale 30
                     :tag "MEMBER types"
                     :types *member-types*
                     :file-name "member")

(add-bucket-reporter :scale 21
                     :tag "OBJECT SYSTEM types"
                     :types (loop :for name being the external-symbols in #+sbcl "SB-PCL" #+allegro "ACLMOP"
                                  :when (find-class name nil)
                                    :collect name)
                     :file-name "pcl-types")

(add-bucket-reporter :scale 30
                     :tag "CL types"
                     :types *cl-types*
                     :file-name "cl-types")

(add-bucket-reporter :scale 18
                     :tag "Subtypes of NUMBER or CONDITION"
                     :types (setof e (remove-duplicates (loop for t1 in (shuffle-list (union (valid-subtypes 'number)
                                                                                             (valid-subtypes 'condition)))
                                                              for t2 in (shuffle-list (union (valid-subtypes 'number)
                                                                                             (valid-subtypes 'condition)))
                                                              nconc (list t1 `(or ,t1 ,t2)
                                                                          `(and ,t1 (not ,t2))))
                                                        :test #'equal)
                              (null (subtypep e nil)))
                     :file-name "subtypes-of-number-or-condition")

(add-bucket-reporter :scale 25
                     :tag "Subtypes of NUMBER"
                     :types *number-combos*
                     :file-name "subtypes-of-number")

(add-bucket-reporter :scale 33
                     :tag "CL combinations"
                     :types (choose-randomly  *cl-type-combos* 13850)
                     :file-name "cl-combos")

(add-bucket-reporter :scale 22
                     :tag "Subtypes of T"
                     :types (set-difference (valid-subtypes t) '(keyword compiled-function) :test #'eq)
                     :file-name "subtypes-of-t")

(defun call-with-caffeinate (thunk)
  (let ((caff (sb-ext:run-program "caffeinate"
                                  '("-i" "sleep" "31449600") ;; sleep for 1 year or until killed
                                  :search t :wait nil)))
    (prog1 (funcall thunk)
      (sb-ext:process-kill caff 1))))

(defmacro caffeinate (&body body)
  "Evaluate the given code body, but using the caffeinate MACOS program to prevent the system from
sleeping before the code finishes evaluating."
  `(call-with-caffeinate (lambda () ,@body)))

(defmacro with-dup-stream ((stream file) &body body)
  "Rebind the named STREAM to a broadcast-stream which duplicates it to the named FILE, evaluating the BODY within the dynamic extend of the rebinding."
  (let ((log-file (gensym)))
    `(with-open-file (,log-file ,file
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
       (let ((,stream (make-broadcast-stream ,stream ,log-file)))
         ,@body))))

(defun big-test-report (&rest options &key (num-tries 2) (multiplier 1) (prefix "") (re-run t)
                                        (suite-time-out (* 60 60 4)) (time-out 100) normalize hilite-min
                                        (decomposition-functions *decomposition-functions*)
                                        (bucket-reporters *bucket-reporters*)
                                        profile
                                        (create-png-p t)
                                        (destination-dir *destination-dir*))
  (declare (ignore prefix re-run suite-time-out time-out num-tries hilite-min profile))
  (when normalize
    (assert (member normalize decomposition-functions) (normalize decomposition-functions)))
  (dolist (df decomposition-functions)
    (unless (exists plist *decomposition-function-descriptors*
              (member df (getf plist :names)))
      (let ((*package* (find-package :keyword)))
        (error "No plist has :name ~A in ~A"
               df *decomposition-function-descriptors*))))
  (let ((*bdd-hash-struct* (bdd-new-hash))
        (*decomposition-functions*  decomposition-functions))
    (loop for (tag bucket-reporter) in bucket-reporters
          for sample = (/ 1 (length bucket-reporters)) then (+ sample (/ 1 (length bucket-reporters)))
          do (funcall bucket-reporter multiplier sample options :create-png-p create-png-p :destination-dir destination-dir))))

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
                   :decomposition-functions (cons 'decompose-types-bdd-graph
                                                  *decompose-fun-parameterized-names*)))

(defun best-2-report (&key (re-run t) (multiplier 1.8) (create-png-p t) (destination-dir *destination-dir*)
                        (bucket-reporters *bucket-reporters*))
  (big-test-report :re-run re-run
                   :prefix "best-2-" ;; should change to best-4-
                   :multiplier multiplier
                   :normalize nil
                   :time-out 20
                   :num-tries 4
                   :hilite-min nil
                   :destination-dir destination-dir
                   :create-png-p create-png-p
                   :bucket-reporters bucket-reporters
                   :decomposition-functions '(decompose-types-bdd-graph-strong
                                              decompose-types-bdd-graph-weak
                                              decompose-types-bdd-graph-weak-dynamic
                                              bdd-decompose-types-strong
                                              bdd-decompose-types-weak
                                              bdd-decompose-types-weak-dynamic
                                              decompose-types-rtev2
                                              decompose-types-graph)))

(defun bdd-report (&key (re-run t) (multiplier 2.5) (create-png-p t) (bucket-reporters *bucket-reporters*) (destination-dir *destination-dir*))
  (big-test-report :re-run re-run
                   :prefix "bdd-ws-" ;; should change to best-4-
                   :multiplier multiplier
                   :normalize nil
                   :time-out 20
                   :num-tries 4
                   :hilite-min nil
                   :destination-dir destination-dir
                   :create-png-p create-png-p
                   :bucket-reporters bucket-reporters
                   :decomposition-functions '(decompose-types-bdd-graph-strong
                                              decompose-types-bdd-graph-weak
                                              decompose-types-bdd-graph-weak-dynamic
                                              bdd-decompose-types-strong
                                              bdd-decompose-types-weak
                                              bdd-decompose-types-weak-dynamic)))

(defun bdd-report-profile (&key (re-run t) (multiplier 0.2) (destination-dir *destination-dir*)
                             (num-tries 4) (prefix "bdd-profile-1-") (decomposition-functions *decomposition-functions*)
                             (bucket-reporters *bucket-reporters*)
                             (create-png-p t))
  (big-test-report :re-run re-run
                   :profile t
                   :prefix prefix
                   :multiplier multiplier
                   :bucket-reporters bucket-reporters
                   :normalize nil
                   :time-out 200
                   :num-tries num-tries
                   :hilite-min nil
                   :destination-dir destination-dir
                   :create-png-p create-png-p
                   :decomposition-functions decomposition-functions))

(defun rebuild-plots ()
  (dotimes (bucket-index (length *bucket-reporters*))
    (let ((*bucket-reporters* (list (nth bucket-index *bucket-reporters*))))
      (big-test-report :re-run nil
                       :create-png-p t
                       :bucket-reporters *bucket-reporters*
                       :prefix "big-"
                       :destination-dir "/Users/jnewton/analysis")
      (best-2-report :re-run nil
                     :create-png-p t
                     :bucket-reporters *bucket-reporters*
                     :destination-dir  "/Users/jnewton/analysis")
      (parameterization-report :re-run nil
                               :create-png-p t
                               :bucket-reporters *bucket-reporters*
                               :destination-dir  "/Users/jnewton/analysis")
      (bdd-report :re-run nil
                  :create-png-p nil
                  :bucket-reporters *bucket-reporters*
                  :destination-dir  "/Users/jnewton/analysis")
      (dotimes (decompose-function-index (length *decomposition-functions*))
        (big-test-report :re-run nil
                         :profile t
                         :decomposition-functions (list (nth decompose-function-index *decomposition-functions*))
                         :bucket-reporters *bucket-reporters*
                         :create-png-p t
                         :destination-dir "/Users/jnewton/analysis"
                         :prefix (format nil "bdd-profile-~D-~D-" decompose-function-index bucket-index))))))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defun qstat-f ()
  "call qstat -f and write the output to a file with the .sXXXX extension
similar to where current Output_Path is indicating."
  (let ((pbs-jobid (sb-posix:getenv "PBS_JOBID"))
        (delimeter (format nil "~%~a" '#\Tab )))
    ;; call qstat -f and get the output;
    ;; qstat -f wraps long lines, so we first have to unwrap them by finding
    ;; "\n\t" and replacing with ""
    (let* ((qstat-out (replace-all (with-output-to-string (str)
                                    (sb-ext:run-program "qstat" (list "-f" pbs-jobid)
                                                        :search t
                                                        :output str))
                                   delimeter ""))
           ;; we want to write into file.sXXXXX, so we have to
           ;; find the Output_Path = /path/to/file.oXXXXXX and parse that
           ;; to get the desired name of the new status output file.
           (pos-host (search "Output_Path = " qstat-out))
           (pos-path (search ":" qstat-out :start2 pos-host))
           (pos-eol  (search (format nil "~%") qstat-out :start2 pos-host))
           ;; output-path = "/path/to/file.oXXXXXX"
           (output-path (subseq qstat-out (1+ pos-path) pos-eol))
           (pos-extension (1+ (search "." output-path :from-end t)))
           ;; extension = "XXXXXX" ; excluding the ".o"
           (extension (subseq output-path (1+ pos-extension)))
           ;; leading = "/path/to/file." ;; including the "."
           (leading   (subseq output-path 0 pos-extension))
           ;; status-filename = "/path/to/file.sXXXXXX"
           (status-filename (concatenate 'string leading "s" extension)))
      
      (with-open-file (stream status-filename
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format stream "~%")
        (write-line qstat-out stream)))))

;; (bdd-report-profile :num-tries 1 :multiplier 0.2 :create-png-p nil)
