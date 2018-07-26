(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-arrows")
  (ql:quickload "cl-ppcre")
  (ql:quickload "alexandria"))

(defpackage :day25
  (:use :cl :cl-arrows)
  (:export :day-25-a
           :day-25-b))

(in-package :day25)

(defparameter +prologue+ "Begin in state (\\w).
Perform a diagnostic checksum after (\\d) steps.")

(defparameter +transitions+ "In state (\\w):
  If the current value is (\\d):
    - Write the value (\\d).
    - Move one slot to the (.*?).
    - Continue with state (\\w).
  If the current value is (\\d):
    - Write the value (\\d).
    - Move one slot to the (.*?).
    - Continue with state (\\w).")

(defun read-input (path-str)
  (let* ((inp (alexandria:read-file-into-string path-str))
         (parts (cl-ppcre:split "(?m)\\n^\\n" inp))
         (start-and-steps (car parts))
         (transitions (cdr parts)))
    (cl-ppcre:do-register-groups
        ((#'(lambda (x) (char x 0)) start)
         (#'parse-integer num-iterations))
        (+prologue+ start-and-steps)
      (setf *start-state* start
            +num-iterations+ num-iterations))
    (loop for trans in transitions do
         (cl-ppcre:do-register-groups
             ((#'(lambda (x) (char x 0)) curr-state)
              (#'parse-integer cond1)
              (#'parse-integer res1)
              (#'(lambda (x) (if (string= x "left") -1 1)) dir1)
              (#'(lambda (x) (char x 0)) next-state1)
              (#'parse-integer cond2)
              (#'parse-integer res2)
              (#'(lambda (x) (if (string= x "left") -1 1)) dir2)
              (#'(lambda (x) (char x 0)) next-state2))
             (+transitions+ (string-trim (list #\Newline) trans))
           (declare (ignore cond1 cond2))
           (setf (gethash curr-state *transition-functions*)
                 (lambda (pos tape)
                   (declare (special pos))
                   (if (zerop (gethash pos tape))
                       (progn
                         (setf (gethash pos tape) res1)
                         (incf pos dir1)
                         (format t "[pos@0:~d]~%" pos)
                         next-state1)
                       (progn
                         (setf (gethash pos tape) res2)
                         (incf pos dir2)
                         (format t "[pos@1:~d]~%" pos)
                         next-state2))))))))
