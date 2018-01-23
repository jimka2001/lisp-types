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

#nil(defun gather-profiling (thunk)
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

(defun call-with-profiling (thunk)
  (sb-sprof:reset)
  (sb-sprof:start-profiling)
  (funcall thunk)
  (sb-sprof:stop-profiling)
  (flet ((dashes (str)
           (every (lambda (c)
                    (char= c #\-)) str)))
    
    (let* ((lines-str (with-output-to-string (str)
                        (let ((*standard-output* str))
                          (sb-sprof:report :type :flat))))
           (dash-1 (member-if #'dashes (split-str lines-str)))
           (dash-2 (member-if #'dashes (cdr dash-1)))
           (profile-lines (ldiff (cdr dash-1) dash-2)))
           
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
                                  :self (list :count (read stream nil nil)
                                              :percent (read stream nil nil))
                                  :total (list :count (read stream nil nil)
                                               :percent (read stream nil nil))
                                  :cumul (list :count (read stream nil nil)
                                               :percent (read stream nil nil))
                                  :calls (read stream nil nil)
                                  :function (read stream nil nil))
                       (close stream))))))
    
