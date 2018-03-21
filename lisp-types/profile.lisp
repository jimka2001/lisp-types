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

(in-package :lisp-types.test)

#|

|#

(let ((lisp-types-test (find-package  :lisp-types.test))
      (lisp-types (find-package  :lisp-types)))
  (do-symbols (name :lisp-types)
    (when (and (eq lisp-types (symbol-package name))
               (not (find-symbol (symbol-name name) lisp-types-test)))
      (format t "1 importing name=~A into  :lisp-types.test~%" name)
      (shadowing-import name :lisp-types.test))))

#+nil(defun gather-profiling (thunk)
  (let* ((graph       (sb-sprof::make-call-graph most-positive-fixnum))
         (interesting (map 'list (lambda (name)
                                   (find name (sb-sprof::call-graph-vertices graph)
                                         :key #'sb-sprof::node-name))
                           (funcall thunk))))
    (map 'list (lambda (node)
                 (cons (sb-sprof::node-name node)
                       (/ (sb-sprof::node-accrued-count node)
                          (sb-sprof::call-graph-nsamples graph))))
         interesting)))

(defun delimiterp (c)
  (or (char= c #\Newline)))

(defun split-str (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
      :while end))

(defun parse-sprofiler-output (profiler-text n-times)
  "PROFILER-TEXT is the string printed by sb-sprof:report"
  (flet ((dashes (str)
           (every (lambda (c)
                    (char= c #\-)) str)))
    (let* ((lines-str profiler-text)
           ;; dash-1 and dash-2 are lists starting with the  1st and 2nd
           ;; occurance of "-----..." in line-str after being split into a list of lines.
           (dash-1 (member-if #'dashes (split-str lines-str)))
           (dash-2 (member-if #'dashes (cdr dash-1)))
           ;; profile-lines is the list of lines between the dashes
           (profile-lines (ldiff (cdr dash-1) dash-2))
           (*package* (find-package :cl-user)))
           
      ;; "           Self        Total        Cumul"
      ;; "  Nr  Count     %  Count     %  Count     %    Calls  Function"
      ;; "------------------------------------------------------------------------"
      ;; "   1    121  15.6    121  15.6    121  15.6        -  LDIFF"
      ;; "   2    113  14.5    176  22.6    234  30.1        -  (LABELS TO-DNF :IN TYPE-TO-DNF)"
      ;; "   3     69   8.9    553  71.1    303  38.9        -  DISJOINT-TYPES-P"
      ;; "   4     66   8.5    199  25.6    369  47.4        -  CACHED-SUBTYPEP"
      ;; "------------------------------------------------------------------------"
      ;; "          0   0.0                                     elsewhere")
      (loop :for line :in profile-lines
            :for stream = (make-string-input-stream line)
            :collect (prog1 (list :nr (read stream nil nil)
                                  :self (list :count (truncate (read stream nil nil) n-times)
                                              :percent (read stream nil nil))
                                  :total (list :count (truncate (read stream nil nil) n-times)
                                               :percent (read stream nil nil))
                                  :cumul (list :count (truncate (read stream nil nil) n-times)
                                               :percent (read stream nil nil))
                                  :function (progn (read stream nil nil) ;; skip calls because don't know whether to call / or truncate
                                                   (format nil "~A" (read stream nil nil))))
                       (close stream))))))

(defun call-with-sprofiling (thunk consume-prof consume-n get-n-stimes)
  (declare (type (function () t) thunk)
           (type (function (list) t) consume-prof)
           (type (function ((and fixnum unsigned-byte)) t) consume-n))
  (labels ((recur (n-times)
             ;;(format t "recur ~D~%" n-times)
             (sb-sprof:reset)
             (let ((val (block nil
                          (handler-bind ((warning (lambda (w &aux (filter-me "No sampling progress;"))
                                                    ;; No sampling progress; run too short, sampling interval too
                                                    ;; long, inappropriate set of sampled thread, or possibly a
                                                    ;; profiler bug.
                                                    (when (string= filter-me
                                                                   (subseq (format nil "~A" w)
                                                                           0
                                                                           (length filter-me)))
                                                      (return nil)))))
                            (sb-sprof:with-profiling (:loop nil)
                              (dotimes (n n-times)
                                (funcall consume-n n)
                                (funcall thunk))))))
                   (prof (parse-sprofiler-output
                          (with-output-to-string (str)
                            (let ((*standard-output* str))
                              (sb-sprof:report :type :flat)))
                          (1+ (funcall get-n-stimes)))))
               (cond
                 (prof
                  (funcall consume-prof prof)
                  val)
                 (t
                  (recur (* 2 n-times)))))))
    (recur 1)))
 
(defun skip-char (stream c)
  (unless (char= c (read-char stream nil nil))
    (skip-char stream c)))

(defun parse-dprofiler-output (profiler-text n-times)
  "PROFILER-TEXT is the string printed by sb-sprof:report"
  (flet ((dashes (str)
           (every (lambda (c)
                    (char= c #\-)) str)))
    (let* ((lines-str profiler-text)
           ;; dash-1 and dash-2 are lists starting with the  1st and 2nd
           ;; occurance of "-----..." in line-str after being split into a list of lines.
           (dash-1 (member-if #'dashes (split-str lines-str)))
           (dash-2 (member-if #'dashes (cdr dash-1)))
           ;; profile-lines is the list of lines between the dashes
           (profile-lines (ldiff (cdr dash-1) dash-2))
           (*package* (find-package :cl-user)))

      ;;   seconds  |     gc     |    consed   |  calls |  sec/call  |  name  
      ;; ----------------------------------------------------------
      ;;      1.314 |      0.000 | 763,854,800 | 14,718 |   0.000089 | RND-ELEMENT
      ;;      0.974 |      0.967 |           0 |     10 |   0.097396 | GARBAGE-COLLECT
      ;;      0.317 |      0.000 |     293,328 |     20 |   0.015849 | RUN-PROGRAM
      ;;      0.007 |      0.000 |     360,448 |     10 |   0.000707 | CHOOSE-RANDOMLY
      ;;      0.004 |      0.000 |           0 |     10 |   0.000397 | REPORT-HASH-LOSSAGE
      ;;      0.001 |      0.000 |           0 |  2,120 |   0.000000 | FIXED-POINT
      ;;      0.000 |      0.000 |           0 |    520 |   0.000001 | CACHED-SUBTYPEP
      ;;      0.000 |      0.000 |           0 |    520 |   0.000000 | ALPHABETIZE
      ;;      0.000 |      0.000 |           0 |    840 |   0.000000 | CMP-OBJECTS
      ;;      0.000 |      0.000 |           0 |    520 |   0.000000 | REDUCE-MEMBER-TYPE
      ;;      0.000 |      0.000 |           0 |     10 |   0.000000 | BDD-RECENT-COUNT
      ;;      0.000 |      0.000 |           0 |     10 |   0.000000 | GETTER
      ;;      0.000 |      0.000 |           0 |      1 |   0.000000 | BDD-NEW-HASH
      ;;      0.000 |      0.000 |           0 |     10 |   0.000000 | SHUFFLE-LIST
      ;;      0.000 |      0.000 |           0 |  2,120 |   0.000000 | TYPE-TO-DNF
      ;;      0.000 |      0.000 |   1,622,880 |  1,040 |   0.000000 | CACHING-CALL
      ;;      0.000 |      0.000 |           0 |     20 |   0.000000 | BDD-HASH
      ;;      0.000 |      0.000 |           0 |  3,160 |   0.000000 | ALPHABETIZE-TYPE
      ;;      0.000 |      0.000 |          16 |    520 |   0.000000 | SLOW-DISJOINT-TYPES-P
      ;;      0.000 |      0.000 |           0 |    520 |   0.000000 | DISJOINT-TYPES-P
      ;; ----------------------------------------------------------
      ;;      2.618 |      0.967 | 766,131,472 | 26,699 |            | Total

      (loop :for line :in profile-lines
            :for stream = (make-string-input-stream
                           (remove #\| (remove #\,  line)
                                   :count 5))
            :collect (prog1 (list :seconds (/ (read stream nil nil) n-times)
                                  :gc      (/ (read stream nil nil) n-times)
                                  :cons    (truncate (read stream nil nil) n-times)
                                  :calls   (truncate (read stream nil nil) n-times)
                                  :sec/call (read stream nil nil)
                                  :name    (format nil "~A" (read stream nil nil)))
                       (close stream))))))

(defun call-with-dprofiling (thunk packages consume-prof consume-n get-n-dtimes)
  (declare (type (function () t) thunk)
           (type list packages) ;; list of strings or symbols
           (type (function (list) t) consume-prof)
           (type (function ((and fixnum unsigned-byte)) t) consume-n))
  (labels ((recur (n-times)
             (sb-profile:unprofile)
             (sb-profile:reset)
             ;;(sb-profile:profile "package")
             (sb-profile::mapc-on-named-funs #'sb-profile::profile-1-fun packages)
             (let ((val (dotimes (n n-times)
                          (funcall consume-n n)
                          (funcall thunk)))
                   (prof (parse-dprofiler-output
                          (with-output-to-string (str)
                            (let ((*trace-output* str))
                              (sb-profile:report :print-no-call-list nil)))
                          (1+ (funcall get-n-dtimes)))))
               (sb-profile:unprofile)
               ;; did the profiler produce any output?
               (cond
                 (prof
                  ;; if yes, then consume the lines
                  (funcall consume-prof prof)
                  val)
                 (t
                  ;; if no, then try again by running the thunk twice as many times as before.
                  (recur (* 2 n-times)))))))
    (recur 1)))


(defun test-profiler ()

  (labels ((loc1 (x y)
             (let ((z 0.0))
               (dotimes (i 10000 z)
                 (setf z (* z (max (abs (sin (* x i)))
                                   (abs (cos (* y i))))))))))
    (format t "test-profiler~%")
    (let ((m 0.0))
      (do ((x 1.1 (1+ x))
           (y 2.1 (1+ y))
           (n 0 (1+ n)))
          ((> n 1000) m)
        (setf m (max m (loc1 x y)))))))

(defun test-profile ()
  (let (s-prof-plists
        d-prof-plists
        (n-stimes 1)
        (n-dtimes 1))
    (labels ((set-sprofile-plists (plists)
               (setf s-prof-plists plists))
             (set-n-stimes (n)
               (format t "set n-times=~D~%" n)
               (setf n-stimes n))
             (get-n-stimes ()
               n-stimes)
             (get-n-dtimes ()
               n-dtimes)
             (set-dprofile-plists (plists)
               (setf d-prof-plists plists))
             (set-n-dtimes (n)
               (setf n-dtimes n)))
             
      (call-with-sprofiling (lambda ()
                              (test-profiler))
                            #'set-sprofile-plists
                            #'set-n-stimes
                            #'get-n-stimes)
      
      (call-with-dprofiling (lambda ()
                              (test-profiler))
                            '("LISP-TYPES" "LISP-TYPES.TEST" "CL")
                            #'set-dprofile-plists
                            #'set-n-dtimes
                            #'get-n-dtimes)
      )))
