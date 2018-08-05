(Eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-arrows")
  (ql:quickload "cl-ppcre")
  (ql:quickload "alexandria"))

(defpackage :day25
  (:use :cl :cl-arrows)
  (:export :day-25-a
           :day-25-b))

(in-package :day25)

(defparameter +prologue+ "Begin in state (\\w).
Perform a diagnostic checksum after (\\d+?) steps.")

(defparameter +transitions+ "In state (\\w):
  If the current value is (\\d):
    - Write the value (\\d).
    - Move one slot to the (.*?).
    - Continue with state (\\w).
  If the current value is (\\d):
    - Write the value (\\d).
    - Move one slot to the (.*?).
    - Continue with state (\\w).")

(defstruct env
  (tape (make-instance 'tape :len 100001))
  numiters
  start
  (funcs (make-hash-table))
  curr-state)

(defun read-input (path-str env)
  (let* ((inp (alexandria:read-file-into-string path-str))
         (parts (cl-ppcre:split "(?m)\\n^\\n" inp))
         (start-and-steps (car parts))
         (transitions (cdr parts)))
    (start-and-steps start-and-steps env)
    (loop for tr in transitions do
         (state-function tr env))))

(defun start-and-steps (prologue env)
  (cl-ppcre:do-register-groups
      ((#'(lambda (x) (char x 0)) starting-state)
       (#'parse-integer num-iterations))
      (+prologue+ prologue)
    (setf (env-start env) starting-state
          (env-numiters env) num-iterations))
  env)

(defun state-function (state-block env)
  (let (curr-state
        curr-val1 val1 dir1 nxt1
        curr-val2 val2 dir2 nxt2)
    (cl-ppcre:do-register-groups
        ((#'(lambda (x) (char x 0)) cs)
         (#'parse-integer cv1)
         (#'parse-integer v1)
         (#'(lambda (x) (if (string= x "left") -1 1)) d1)
         (#'(lambda (x) (char x 0)) n1)
         (#'parse-integer cv2)
         (#'parse-integer v2)
         (#'(lambda (x) (if (string= x "left") -1 1)) d2)
         (#'(lambda (x) (char x 0)) n2))
        (+transitions+ (string-trim (list #\Newline) state-block))
      (setf curr-state cs
            curr-val1 cv1 val1 v1 dir1 d1 nxt1 n1
            curr-val2 cv2 val2 v2 dir2 d2 nxt2 n2))
    (setf (gethash curr-state (env-funcs env))
          (lambda (env)
            (if (= curr-val1 (val-at-curr-pos (env-tape env)))
                (progn
                  (setf (val-at-curr-pos (env-tape env)) val1)
                  (go-dir dir1 (env-tape env))
                  (setf (env-curr-state env) nxt1))
                (progn
                  (setf (val-at-curr-pos (env-tape env)) val2)
                  (go-dir dir2 (env-tape env))
                  (setf (env-curr-state env) nxt2))))))
  env)

(defclass tape ()
  ((cells :type (simple-array *))
   (zero :type 'integer)
   (pos :type 'integer :initform 0)))

(defmethod initialize-instance :after ((tape tape) &key len)
  (setf (slot-value tape 'zero) (ceiling len 2)
        (slot-value tape 'cells) (make-array len :initial-element 0)))

(defmethod (setf val-at) (val idx (tape tape))
  "Absolute indexing"
  (with-slots  (cells zero pos) tape
    (setf (aref cells (+ zero idx)) val)))

(defmethod (setf val-at-curr-pos) (val (tape tape))
  "Indexing relative to current position"
  (with-slots  (cells zero pos) tape
    (setf (aref cells (+ zero pos)) val)))

(defmethod val-at-curr-pos ((tape tape))
  "Indexing relative to current position"
  (with-slots  (cells zero pos) tape
    (aref cells (+ zero pos))))

(defmethod val-at (idx (tape tape))
  (with-slots (cells zero pos) tape
    (aref cells (+ zero pos idx))))

(defmethod curr-pos ((tape tape))
  (slot-value tape 'pos))

(defmethod go-dir (dir (tape tape))
  (incf (slot-value tape 'pos) dir))

(defmethod checksum ((tape tape))
  (with-slots (cells) tape
    (loop for cell across cells if (= 1 cell) count cell into cnt
         finally (return cnt))))

(defun run (problem)
  (let* ((env (make-env)))
    (read-input problem env)
    (setf (env-curr-state env) (env-start env))
    (dotimes (i (env-numiters env))
      (funcall (gethash (env-curr-state env) (env-funcs env)) env))
    (checksum (env-tape env))))

(defun day-25-a ()
  (run "./input"))
