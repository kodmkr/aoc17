(ql:quickload "cl-ppcre")

(defpackage :day16
  (:use :cl)
  (:export :day-16-a
           :day-16-b))

(in-package :day16)

(defconstant +len+ 16)

(defparameter *names* (make-array +len+ :element-type 'character :initial-contents "abcdefghijklmnop"))

(defparameter *test* "abcde")
(defparameter *test-seq* '(("s" "1") ("x" "3" "4") ("p" "e" "b")))

(defun reset ()
  (setf *test* "abcde")
  (setf *names* (make-array +len+ :element-type 'character :initial-contents "abcdefghijklmnop"))
  (values))

(defun read-input (path-str)
  (with-open-file (inp path-str)
    (read-line inp)))

(defun process-input (input)
  (loop for instr in (cl-ppcre:split "," input)
     collect (multiple-value-bind (m regs)
                 (cl-ppcre:scan-to-strings "(.)(.+)/(.+)|(.)(.+)" instr)
               (declare (ignore m))
               (if (aref regs 0)
                   (list (aref regs 0) (aref regs 1) (aref regs 2))
                   (list (aref regs 3) (aref regs 4))))))

(defun nspin (x arr)
  (let* ((border (- (array-dimension arr 0) x))
         (rst (subseq arr border)))
    (replace arr arr :start1 x)
    (replace arr rst))
  arr)

(defun nswitch-places (pos1 pos2 arr)
  (rotatef (aref arr pos1) (aref arr pos2))
  arr)

(defun nswitch-positions (name1 name2 arr)
  "NAME1 and NAME2 have to be strings of (at least) length 1."
  (let* ((n1 (aref name1 0))
         (n2 (aref name2 0))
         (p1 (position n1 arr :test #'char=))
         (p2 (position n2 arr :test #'char=)))
    (rotatef (aref arr p1) (aref arr p2))
    arr))

(defun ndance (progs moves)
  (loop for move in moves do
       (cond ((string= "s" (car move))
              (nspin (parse-integer (cadr move)) progs))
             ((string= "x" (car move))
              (nswitch-places (parse-integer (cadr move)) (parse-integer (caddr move)) progs))
             (t
              (nswitch-positions (cadr move) (caddr move) progs))))
  progs)

(defun day-16-a ()
  (let* ((inp (read-input "./input"))
         (moves (process-input inp)))
    (reset)
    (ndance *names* moves)))

(defun day-16-b ()
  (let* ((inp (read-input "./input"))
         (moves (process-input inp)))
    (reset)
    (loop for x from 1 to (mod 1000000000 36) do ; the progs repeat after 36 iterations of the dance
         (ndance *names* moves)
         finally (return *names*))))
