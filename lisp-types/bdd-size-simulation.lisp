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

(in-package :lisp-types.test)


(defun int-to-boolean-expression (n vars)
  "Returns a Boolean expression which is a Boolean combination of the given variable names.
VARS is a list of symbols indicating Boolean variable names
N is an integer: 0 <= N < 2^2^(length vars)

Denote M = (length VARS)
The truth table for a Boolean function of M has 2^M rows
   e.g., a 3 variable truth table has 8 rows.
If we want to generate such a truth table, we must supply 8 bits, i.e., an integer between
   0 and 255 (inclusive), i.e., 0 <= N < 2^2^M.
This function, INT-TO-BOOLEAN-EXPRESSION, iterates from 0 to 2^M - 1, and for each
iteration generates a min-term, by calling local function GEN-MIN-TERM.
E.g., (INT-TO-BOOLEAN-EXPRESSION #b00010011 '(a b c))
--> (OR (AND (NOT A) (NOT B) (NOT C)) 
        (AND A       (NOT B) (NOT C))
        (AND (NOT A) (NOT B) C))
Why?  Because the truth table of this function is:
 CBA|
 000|1
 001|1
 010|0
 011|0
 100|1
 101|0
 110|0
 111|1
"
  (let ((num-vars (length vars)))
    (let ((max-n (expt 2 (expt 2 num-vars))))
      (assert (< n max-n) (n vars)
              "N=~D must be less than ~D for ~D variables=~A"
              n max-n num-vars vars))
    (flet ((gen-min-term (i)
             ;; interpret the given I as a bit-mask
             ;; and generate an (AND ...) expression
             ;; the arguments of AND are the symbols in order in VAR
             ;; either as is or wrapped in (NOT ...)
             ;; e.g. if VARS='(a b), then 2 with bitmask 10 -> (and A (not B))
             ;; bits from right to left correspond to variables from left to right
             (prog1
                 (when (oddp n)
                   (list (cons 'and (mapcar (lambda (var)
                                              (prog1 (if (oddp i)
                                                         var
                                                         `(not ,var))
                                                (setf i (ash i -1))))
                                            vars))))
               (setf n (ash n -1)))))
      (cons 'or (loop for i from 0 below (expt 2 num-vars)
                      nconc (gen-min-term i))))))


(defun random-boolean-combination (vars)
  "return a randomly selection boolean combination of the given BOOLEAN variables in sum-of-minterms form (or (and ...) (and ...) ...)"
  ;; vars is a list of symbols
  (int-to-boolean-expression (random (expt 2 (expt 2 (length vars))))
                             vars))

(defun median-a-list (a-list)
  (let ((a-list (copy-list a-list)))
    (loop while (cdr a-list)
          do (let ((couple (cons (car a-list) (last a-list))))
               (destructuring-bind ((low-index low-count)
                                    (high-index high-count)) couple
                 (setf a-list (if (and (null (cddr a-list))
                                       (= low-count high-count))
                                  (list (list (/ (+ low-index high-index) 2) 1))
                                  (merge 'list (cond ((= low-count high-count)
                                                      nil)
                                                     ((< low-count high-count)
                                                      (list (list high-index (- high-count low-count))))
                                                     (t
                                                      (list (list low-index (- low-count high-count)))))
                                         (set-difference a-list couple :test #'eq)
                                         #'<
                                         :key #'car))))))
    (values (caar a-list) a-list)))

(defun make-announcement-timer (min max interval announce)
  (declare (type (function (integer number) t) announce)
           (type integer min max)
           (type real interval))
  (let* ((start-time (get-internal-real-time))
         (internal-interval (* interval internal-time-units-per-second))
         (previous-announcement start-time))
    (lambda (iteration &aux (now (get-internal-real-time)))
      (cond
        ((equal min iteration))
        ((< (+ previous-announcement internal-interval)
            now)
         (setf previous-announcement now)
         (let* ((fraction-done (/ (- iteration min) (- max min)))
                (elapsed-seconds (/ (- now start-time) internal-time-units-per-second))
                (total-seconds (/ elapsed-seconds fraction-done))
                (remaining-seconds (- total-seconds elapsed-seconds)))
           (funcall announce iteration (coerce remaining-seconds 'double-float))))))))

(defun calc-plist (histogram num-vars randomp)
  (declare (type cons histogram)
           (type fixnum num-vars))
  ;; histogram is a list of pairs, each pair is (sample occurances)
  (flet ((sqr (x) (* x x)))
    (let* ((num-samples (reduce (lambda (sum this)
                                  (declare (type integer sum)
                                           (type (cons integer (cons integer)) this))
                                  (destructuring-bind (sample occurances) this
                                    (declare (ignore sample))
                                    (+ sum occurances)))
                                histogram
                                :initial-value 0))
           (normalized-histogram (mapcar (lambda (pair)
                                           (destructuring-bind (sample count) pair
                                             (list sample (/ count num-samples))))
                                         histogram))
           (mean (reduce (lambda (sum this)
                           (destructuring-bind (sample probability) this
                             (+ sum (* probability sample))))
                         normalized-histogram
                         :initial-value 0))
           (stdev (sqrt (reduce (lambda (sum this)
                                  (destructuring-bind (sample probability) this
                                    (+ sum (* probability (sqr (- sample mean))))))
                                normalized-histogram :initial-value 0.0)))
           (ffff (1- (expt 2 (expt 2 num-vars))))
           (density (/ num-samples (1+ ffff))))

      (let (sum average-size median)
        (declare #+sbcl (notinline sort))
        (setf sum (reduce #'+ histogram :initial-value 0 :key #'cadr))
        (setf average-size (/ (reduce (lambda (old item)
                                        (destructuring-bind (sample occurances) item
                                          (+ old (* sample occurances)))) histogram :initial-value 0) sum))
        (setf median (median-a-list histogram))
        (list :sum sum
              :num-samples num-samples
              :randomp randomp
              :num-vars num-vars
              :density density
              :average-size mean
              :sigma stdev
              :median median
              :possible-sizes (mapcar #'car histogram)
              :normalized-histogram normalized-histogram
              :counts (mapcar (lambda (pair)
                                (declare (type (cons integer (cons integer)) pair))
                                (list (car pair) ;; a bdd size
                                      (float (/ (cadr pair) sum)) ;; normalized number of bdds of this size as a fraction of total sample
                                      (cadr pair) ;; number of bdds of this size in sample
                                      ;; extrapolation
                                      (truncate (cadr pair) density) ;; estimated number of unique bdds of this size
                                      ))
                              histogram))))))

(defun gen-random-samples (min max num-samples)
  (declare (type unsigned-byte min max num-samples))
  (cond
    ((> (- max min) num-samples)
     (let ((hash (make-hash-table :test #'eql)))
       (dotimes (_ num-samples)
         (setf (gethash (+ min (random (- max min))) hash) t))
       (loop while (< (hash-table-count hash) num-samples)
             do (setf (gethash (+ min (random (- max min))) hash) t))
       (sort (loop for k being the hash-keys of hash
                   collect k)
             #'<)))
    (t
     (loop for k from min to max
           collect k))))

(defun measure-bdd-size (vars num-samples)
  (setf num-samples (min (expt 2 (expt 2 (length vars)))
                         num-samples))
  (let* ((hash (make-hash-table))
         (ffff (1- (expt 2 (expt 2 (length vars)))))
         (randomp (< num-samples (1+ ffff))))
    (flet ((measure (try)
             (garbage-collect)
             (bdd-with-new-hash (&aux (boolean-combo (int-to-boolean-expression try vars))
                                      (bdd (bdd boolean-combo))
                                      (node-count (bdd-count-nodes bdd)))
               (incf (gethash node-count hash 0)))))

      (let ((announcer (make-announcement-timer
                        2 (1- num-samples) 2
                        (lambda (iteration remaining-seconds)
                          (format t "~D iteration=~D: " (length vars) iteration)
                          (let ((seconds (truncate remaining-seconds))
                                (minutes (coerce (/ remaining-seconds 60) 'float))
                                (hours (coerce (/ remaining-seconds (* 60 60)) 'float)))
                            (format t "seconds remaining ~D" seconds)
                            (when (> minutes 1)
                              (format t " = ~D minutes" (truncate minutes)))
                            (when (> hours 1)
                              (format t " = ~D hours ~D minutes" (truncate hours)
                                      (truncate (- minutes (* 60 (truncate hours))))))
                            (format t "~%")))))
            (samples (gen-random-samples 0 ffff num-samples)))
        (pushnew 0 samples)
        (pushnew 1 samples)
        (pushnew ffff samples)
        (format t "generating ~D " (length samples))
        (when randomp (format t "randomly chosen "))
        (format t "BDDs of possible ~D (~a%) with ~D variables ~S~%"  (1+ ffff)
                (* 100.0 (/ (length samples) (1+ ffff))) (length vars) vars)
        (loop :for try :in samples
              :for iteration = 0 :then (1+ iteration)
              :do (measure try)
              :do (funcall announcer iteration))))
    (let (histogram)
      (declare #+sbcl (notinline sort))
      (maphash (lambda (&rest args)
                 (push args histogram))
               hash)
      (setf histogram (sort histogram #'< :key #'car))
      (calc-plist histogram (length vars) randomp))))

(defun measure-bdd-sizes (vars num-samples min max)
  (mapcon (lambda (vars)
            (cond
              ((> min (length vars))
               nil)
              ((> (length vars) max)
               nil)
              (t
               (list (measure-bdd-size vars
                                       (min (expt 2 (expt 2 (length vars)))
                                            num-samples))))))
          vars))

(defun convert-double-notation (stream string)
  (declare (type stream stream)
           (type string string))
  (loop for char across string
        do (if (char= #\d char)
               (format stream "e")
               (format stream "~A" char))))

(defun write-one-bdd-distribution-data (plist prefix)
  (let* ((num-vars (getf plist :num-vars))
         (data-file (format nil "~A/bdd-distribution-data-~D.sexp" prefix num-vars)))
    (with-open-file (stream data-file
                            :direction :output :if-does-not-exist :create :if-exists :supersede)
      (when stream
        (format t "writing to ~A~%" data-file)
        (format stream "  (~%")
        (while plist
          (destructuring-bind (keyword obj &rest _others) plist
            (declare (ignore _others))
            (let ((*package* (find-package :keyword)))
              (format stream "    ~S ~A~%" keyword obj)))
          (pop plist)
          (pop plist))
          (format stream "  )~%")))))

(defun write-bdd-distribution-data (data prefix)
  (declare (type list data)
           (type string prefix))
  (dolist (plist data)
    (write-one-bdd-distribution-data plist prefix)))

(defun read-bdd-distribution-data (prefix &key (min 1) (max 8) vars)
  (declare (ignore vars))
  (loop for var from min to max
        for data-file = (format nil "~A/bdd-distribution-data-~D.sexp" prefix var)
        nconc (with-open-file (stream data-file
                                      :direction :input :if-does-not-exist nil)
                (let* ((plist (read stream))
                       (histogram (mapcar (lambda (this &aux (sample (car this)) (occurances (caddr this)))
                                            (list sample occurances))
                                          (getf plist :counts))))
                  (when stream
                    (list (calc-plist histogram (getf plist :num-vars) (getf plist :randomp))))))))

(defun measure-and-write-bdd-distribution (prefix num-vars num-samples)
  (write-bdd-distribution-data (measure-bdd-sizes *bdd-test-classes*
                                                  num-samples num-vars num-vars)
                               prefix))

(defun latex-measure-bdd-sizes (prefix vars num-samples &key (min 1) (max (length vars)) (re-run t))
  (declare (type string prefix)
           (type list vars)
           (type fixnum num-samples)
           #+sbcl (notinline sort))
  (ensure-directories-exist prefix)
  ;; prefix = "/Users/jnewton/newton.16.edtchs/src"
  (let* (legend
         (colors '("red" "goldenrod" "olive" "blue" "lavender" "greeny" "dark-cyan" "color-7" "color-8"))
         (data (if re-run
                   (sort (measure-bdd-sizes vars num-samples min max) #'< :key (getter :num-vars))
                   (read-bdd-distribution-data prefix :vars vars))))
    (when re-run
      (write-bdd-distribution-data data prefix))

  (flet ((individual-plot (stream num-vars &aux (plist (find num-vars data :key (getter :num-vars))))
           (format stream "\\begin{tikzpicture}~%")
           (format stream "\\begin{axis}[~% xlabel=ROBDD node count for ~D variables,~% ymajorgrids,~% yminorgrids,~% xmajorgrids,~% xminorgrids,~% ylabel=Number of Boolean functions,~% legend style={font=\tiny}~%]~%" num-vars)
           (format stream "\\addplot[color=blue,mark=*] coordinates {~%")
           (dolist (item (getf plist :counts))
             (convert-double-notation
              stream
              (with-output-to-string (str)
                (format str "(~D,~D)~%" (car item) (coerce (nth 3 item) 'double-float)))))
           (format stream "};~%")
           (format stream "\\legend{}~%")
           (format stream "\\end{axis}~%")

           (format stream "\\end{tikzpicture}~%"))
         (sigma-plot (stream)
           (format stream "\\begin{tikzpicture}~%")
           (format stream "\\begin{axis}[~% ymajorgrids,~% xmin=0,~% ymin=0,~% yminorgrids,~% xmajorgrids,~% xlabel=Number of variables,~% ylabel=Standard deviation,~% legend style={anchor=west,font=\tiny},")
           (format stream "xtick={1")
           (loop for xtick from 2
                   to (reduce (lambda (max item)
                                (max max (getf item :num-vars)))
                              (cdr data)
                              :initial-value (getf (car data) :num-vars))
                 do (format stream ",~D" xtick))
           (format stream "}~%]~%")
           (format stream "\\addplot[color=blue,mark=*] coordinates {~%")
           (dolist (plist data)
             (destructuring-bind (&key num-vars sigma &allow-other-keys) plist
               (format stream "(~D,~D)~%"
                       num-vars
                       (coerce sigma 'float))))
           (format stream "};~%")
           (format stream "\\end{axis}~%")
           (format stream "\\end{tikzpicture}~%"))             
         (average-plot (stream)
           (format stream "\\begin{tikzpicture}~%")
           (format stream "\\begin{axis}[~% ymin=0,~% ymajorgrids,~% yminorgrids,~% xmajorgrids,~% xlabel=Number of variables,~% ylabel=ROBDD size,~% legend style={at={(0,1)},anchor=north west,font=\tiny},~%")
           (format stream " xtick={0,1")
           (loop for xtick from 2
                   to (reduce (lambda (max item)
                                (max max (getf item :num-vars)))
                              (cdr data)
                              :initial-value (getf (car data) :num-vars))
                 do (format stream ",~D" xtick))
           (format stream "}~%]~%")
           ;; worst case size
           (format stream "\\addplot[color=red,mark=*] coordinates {~%")
           (dolist (plist data)
             (destructuring-bind (&key num-vars counts &allow-other-keys) plist
               (format stream "(~D,~D)~%"
                       num-vars
                       (reduce #'max counts :key #'car :initial-value 0))))
           (format stream "};~%")
           ;; average size
           (format stream "\\addplot[color=green,mark=*] coordinates {~%")
           (dolist (plist data)
             (destructuring-bind (&key num-vars average-size &allow-other-keys) plist
               (format stream "(~D,~D)~%"
                       num-vars
                       (coerce average-size 'float))))
           (format stream "};~%")
           (format stream "\\addplot[color=blue,mark=*] coordinates {~%")
           (dolist (plist data)
             (destructuring-bind (&key num-vars median &allow-other-keys) plist
               (format stream "(~D,~D)~%"
                       num-vars
                       median)))
           (format stream "};~%")
           (format stream "\\legend{Worst case, Average, Median}~%")
           (format stream "\\end{axis}~%")
           (format stream "\\end{tikzpicture}~%"))
         (size-plots (stream)
           (format stream "\\begin{tikzpicture}~%")
           (format stream "\\begin{axis}[~% xlabel=BDD Size,~% ymajorgrids,~% yminorgrids,~% xmajorgrids,~% xminorgrids,~% ylabel=Probability,~%legend style={font=\\tiny},~% label style={font=\\tiny}~%]~%")
                
           (dolist (datum data)
             (destructuring-bind (&key num-vars counts &allow-other-keys) datum
               (when (> num-vars 1)
                 (push (format nil "Size with ~D variables" num-vars) legend)
                 (format stream "\\addplot[color=~A,mark=*] coordinates {~%"
                         (or (pop colors) "black"))
                 (dolist (xy counts)
                   (format stream "  (~D,~A)~%" (car xy) (cadr xy)))
                 (format stream "};~%"))))
                
           (format stream "\\legend{")
           (let ((first t))
             (dolist (label (reverse legend))
               (unless first
                 (format stream ","))
               (format stream "~S" label)
               (setf first nil)))
           (format stream "}~%")
           (format stream "\\end{axis}~%")
           (format stream "\\end{tikzpicture}~%")))


    (with-open-file (stream (format nil "~A/bdd-distribution-sigma.ltxdat" prefix)
                            :direction :output :if-does-not-exist :create :if-exists :supersede)
      (sigma-plot stream))
    (with-open-file (stream (format nil "~A/bdd-distribution.ltxdat" prefix)
                            :direction :output :if-does-not-exist :create :if-exists :supersede)
      (size-plots stream))
    (with-open-file (stream (format nil "~A/bdd-distribution-expected.ltxdat" prefix)
                            :direction :output :if-does-not-exist :create :if-exists :supersede)
      (average-plot stream))
    (loop for num-vars from min to max
          do (with-open-file (stream (format nil "~A/bdd-distribution-~D.ltxdat" prefix num-vars)
                                     :direction :output :if-does-not-exist :create :if-exists :supersede)
               (individual-plot stream num-vars)))
    )
  data))

(defun all-possible-bdds (prefix vars &aux (num-vars (length vars)))
  (declare #+sbcl (notinline sort)
           (type string prefix)
           (type list vars)
          )
  (let ((bdd-data (bdd-with-new-hash ()
                    (loop for try from 0 below (expt 2 (expt 2 num-vars))
                          collect (let* ((expr (int-to-boolean-expression try vars))
                                         (bdd (bdd expr)))
                                    (list :bdd bdd
                                          :node-count (bdd-count-nodes bdd)
                                          :expr (bdd-to-dnf bdd))))
                       :verbose nil))
        (uniq 1000))
    
    (sort (loop for data in (sort bdd-data #'< :key (getter :node-count))
          collect (destructuring-bind (&key bdd expr node-count &allow-other-keys) data
                    (list :node-count node-count
                          :num-vars num-vars
                          :path (bdd-to-png bdd :reduced t
                                                :basename (format nil "~A/vars=~D-~D-~D"
                                                                  prefix num-vars node-count (incf uniq)))
                          :expr expr)))
          #'< :key (getter :node-count))))

(defun all-possible-bdds-latex (prefix vars)
  (with-open-file (latex (format nil "~A/all-robdds-~A.ltx" prefix (length vars))
                         :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format latex "\\begin{table}~%")
    (format latex "\\begin{center}~%")
    (format latex "\\begin{tabular}{c|c|l}~%")
    (format latex "No. Nodes & ROBDD & Boolean Expression\\\\~%")
    (format latex "\\hline~%")
    (dolist (data (all-possible-bdds prefix vars))
      (destructuring-bind (&key path node-count expr &allow-other-keys) data
        (format latex "~D " node-count)
        (format latex "& \\includegraphics[width=0.5in]{~A}~%" (pathname-name (pathname path)))
        (format latex "&~%")
        (format latex "\\begin{minipage}{2in}")
        (format latex "\\begin{verbatim}~%")
        (format latex "~A~%" expr)
        (format latex "\\end{verbatim}~%")
        (format latex "\\end{minipage}\\\\~%")))
    (format latex "\\hline~%")
    (format latex "\\end{tabular}~%")
    (format latex "\\end{center}~%")
    (format latex "\\caption{All ROBDDs of ~r Variable~:p }~%" (length vars))
    (format latex "\\label{fig.robdds.of.size.~D}~%" (length vars))
    (format latex "\\end{table}~%")))
