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


(in-package :lisp-types)

(defun shadow-all-symbols (&key package-from package-into)
  (let ((package-into (or (find-package  package-into)
                          (error "cannot find package ~A" package-into)))
        (package-from (or (find-package  package-from)
                          (error "cannot find package ~A" package-from)))
        (*package* (find-package :keyword)))
    (let (names)
      (do-symbols (name package-from)
        (push name names))
      (dolist (name (sort names #'string< ))
        (when (and (eq package-from (symbol-package name))
                   (not (find-symbol (symbol-name name) package-into)))
          (format t "importing name=~S into ~S ~%" name package-into)
          (shadowing-import name package-into))))))

(defun run-program (program args &rest options)
  #+sbcl (apply #'sb-ext:run-program program args :search t options)
  #+allegro (apply #'excl:run-shell-command
                   (apply #'vector (cons program args))
                   :wait t
                   options
                   )
  )


(defun garbage-collect ()
  #+sbcl (sb-ext::gc :full t)
  #+allegro (excl:gc t)
)

(defvar *verbose-caching* nil)
(defvar *caching-thresh* 2048)
(defvar *secret-default-value* (list nil))

(defun caching-call (thunk key hash fun-name access increment)
  "Helper function used by the expansion of DEF-CACHE-FUN.  This function
 manages the caching and cache search of the function defined by DEF-CACHE-FUN."
  (declare (type (function () t) thunk)
           (type (function () unsigned-byte) access increment)
           (type symbol fun-name)
           (type list key)
           (type (or null hash-table) hash))
  (cond
    ((null hash)
     (funcall thunk))
    (t
     (when (and *verbose-caching*
                (= 0 (mod (funcall increment) *caching-thresh*)))
       (format t "~D ~A ~A~%" (funcall access) fun-name hash))
     (apply #'values
            ;; (multiple-value-bind (value foundp) (gethash key hash)
            ;;   (cond
            ;;     (foundp value)
            ;;     (t
            ;;      (setf (gethash key hash) (multiple-value-list (funcall thunk))))))

            ;; trying this optimization to see if it is faster.
            (let ((value (gethash key hash *secret-default-value*)))
              (if (eq value *secret-default-value*)
                  (setf (gethash key hash) (multiple-value-list (funcall thunk)))
                  value))

            ))))

(defmacro def-cache-fun ((fun-name with-name
                          &key (hash (gensym "hash"))
                            (access-count (gensym "count"))
                            (caching-call (intern (concatenate 'string (symbol-name fun-name) "-CACHING-CALL")
                                                  (symbol-package fun-name))))
                         lam-list doc-string  &body body)
  "Define three functions named by FUN-NAME and WITH-NAME a derived name.  The lambda list of the 
 first function is given by LAM-LIST.  The semantics of the first function will be
 to normally simply return the value of BODY.  However, if the call site to the
 first function is within the dymamic extent of the second function, the
 the return value will be cached, and the arguments are found in the cache
 BODY is not evaluated but simply the cached value of the return value will be
 returned."
  (declare (type string doc-string)
           (type symbol fun-name with-name access-count))
  `(progn
     (defvar ,hash nil)
     (defvar ,access-count 0)
     (defun ,caching-call (thunk key hash fun-name access increment)
       "Helper function used by the expansion of DEF-CACHE-FUN.  This function
 manages the caching and cache search of the function defined by DEF-CACHE-FUN."
       (declare (type (function () t) thunk)
                (type (function () unsigned-byte) access increment)
                (type symbol fun-name)
                (type list key)
                (type (or null hash-table) hash))
       (cond
         ((null hash)
          (funcall thunk))
         (t
          (when (and *verbose-caching*
                     (= 0 (mod (funcall increment) *caching-thresh*)))
            (format t "~D ~A ~A~%" (funcall access) fun-name hash))
          (apply #'values
                 ;; (multiple-value-bind (value foundp) (gethash key hash)
                 ;;   (cond
                 ;;     (foundp value)
                 ;;     (t
                 ;;      (setf (gethash key hash) (multiple-value-list (funcall thunk))))))

                 ;; trying this optimization to see if it is faster.
                 (let ((value (gethash key hash *secret-default-value*)))
                   (if (eq value *secret-default-value*)
                       (setf (gethash key hash) (multiple-value-list (funcall thunk)))
                       value))
                 ))))
     (defun ,with-name (thunk)
       (declare (type (function () t) thunk))
       (if (null ,hash)
           (let ((,hash (make-hash-table :test #'equal)))
             (declare (ignorable ,hash))
             (prog1 (funcall thunk)
               (when *verbose-caching*
                 (format t "finished with ~A=~A~%" ',fun-name ,hash))))
           (funcall thunk)))
       
     (defun ,fun-name (&rest arg)
       ,doc-string
       (destructuring-bind ,lam-list arg
         (,caching-call (lambda () ,@body)
                       arg
                       ,hash
                       ',fun-name
                       (lambda () ,access-count)
                       (lambda () (incf ,access-count)))))))

(def-cache-fun (cached-subtypep call-with-subtypep-cache :hash *subtypep-hash*) (sub super)
               "Wrapper around CL:SUBTYPEP.  Manages the caching of return values of SUBTYPEP within the
dynamic extend of WITH-SUBTYPEP-CACHE"
  (subtypep sub super))

;;; the following are functions used to re-produce the allegro bug/issue spr43795.

(defun count-cells (data)
  (if (listp data)
      (+ (length data)
         (reduce (lambda (sum item)
                   (+ sum (count-cells item)))
                 data
                 :initial-value 0))
      0))


(defun reduce-repetitions (data)
  (let ((hash (make-hash-table :test #'equal)))
    (labels ((rec (data)
               (mapl (lambda (list &aux (head (car list)) (tail (cdr list)))
                       (when (and head (listp head))
                         (multiple-value-bind (value foundp) (gethash head hash)
                           (cond
                             (foundp
                              (setf (car list) value))
                             (t
                              (rec head)
                              (setf (gethash head hash) head)))))
                       (multiple-value-bind (value foundp) (gethash tail hash)
                         (cond
                           (foundp
                            (setf (cdr list) value))
                           (t
                            (rec tail)
                            (setf (gethash tail hash) tail)))))
                     data)))
      (rec data)
      data)))

(defun read-subtypep-data ()
  (with-open-file (stream "/Users/jnewton/subtypep.lisp"
                          :direction :input)
    (let ((EOF '(EOF))
          (items 3000)
          data)
      (loop :while (and (plusp items)
                        (not (eq EOF (setf data (read stream nil EOF)))))
            :do (decf items)
            :collect (reduce-repetitions data)))))

(defun time-subtypep ()
  (let ((pairs (read-subtypep-data)))
    (format t "starting timing~%")
    (let ((start-time (get-internal-real-time)))
      (dolist (pair pairs)
        (apply #'subtypep pair))
      (let ((stop-time (get-internal-real-time)))
        (float (/ (- stop-time start-time)
                  internal-time-units-per-second))))))

(defun count-1-bits (n &aux (bits 0))
  (declare (optimize (speed 3) (debug 0))
           (type (and unsigned-byte fixnum) bits)
           (type unsigned-byte n))
  (if (typep n 'fixnum)
      (let ()
        (declare (type fixnum n))
        (while (plusp n)
          (when (oddp n)
            (incf bits))
          (setf n (ash n -1))))
      (let ()
        (while (plusp n)
          (when (oddp n)
            (incf bits))
          (setf n (ash n -1)))))
  bits)

(defun count-bit-diffs (a b)
  (declare (type unsigned-byte a b) ; warning maybe bignums
           (optimize (speed 3) (debug 0)))
  (count-1-bits (boole boole-xor a b)))

(defun grey-sort (integers)
  "given a list integers (which may be fixnum or bignum or a mix of the two), 
 put them into an order which makes it likely that adjacent 
 entries are close in terms of number of bits different"
  (declare (optimize (debug 0) (speed 3)))
  (let ((path (list (car integers)))
        (integers (copy-list (cdr integers))))
    (labels ((closest (a &aux (guess (car integers)))
               ;; find the closest element to A in the list INTEGERS
               (dolist (b (cdr integers))
                 (when (< (count-bit-diffs a b)
                          (count-bit-diffs a guess))
                   (setf guess b)))
               guess))
      (while integers
        (let ((i0 (closest (car path))))
          (setf integers (delete i0 integers)) ; remove destructively
          (push i0 path)))
      path)))

(defun demand-env-var (env-var-name)
  (or (sb-posix:getenv env-var-name)
      (error "Missing env var ~s" env-var-name)))

(defun seconds-to-hh.mm.ss (seconds)
  (multiple-value-bind (minutes ss) (truncate (truncate seconds) 60)
    (multiple-value-bind (hh mm) (truncate minutes 60)
      (list seconds
            (list hh mm ss)
            (format nil "~D:~D:~D" hh mm ss)))))

(defun sci-notation (bignum)
  ;; bignum = alpha * 10^beta
  (let* ((log_x (log bignum 10))
         (beta (truncate (log bignum 10.0)))
         (log_alpha (- log_x beta))
         (alpha (expt 10.0 log_alpha)))
    
    (list alpha beta)))

