(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre"))

(defpackage :day10
  (:use :cl)
  (:import-from :cl-ppcre :split)
  (:export :day-10-a
           :day-10-b
           :calc-hash))

(in-package :day10)

(defparameter *length* 256)
(defparameter *chunk-sz* 16)

(defun read-input (path-str)
  (with-open-file (in path-str)
    (read-line in nil)))

(defun input-to-list (input)
  (loop for n in (split "," input)
     collect (parse-integer n)))

(defun process-input (input)
  (append (loop for c across input if (char/= c #\Newline)
             collect (char-code c))
          '(17 31 73 47 23)))

(defun mk-array (size)
  (labels ((mil-aux (count acc)
             (if (zerop count)
                 acc
                 (mil-aux (1- count) (cons (1- count) acc)))))
    (make-array (list size) :initial-contents (mil-aux size nil))))

(defun nrev (arr s len)
  (let* ((arr-len (array-dimension arr 0))
         (end (+ s len)) )
    (loop for x from s for y downfrom (1- end) while (<= x y)
       do (rotatef (aref arr (mod x arr-len))
                   (aref arr (mod y arr-len))))
    arr))

(defun test (l)
  (let* ((in (read-input "./test"))
         (lens (input-to-list in))
         (ini (mk-array l))
         (curr-pos 0)
         (skip 0))
    (loop for len in lens do
         (nrev ini curr-pos len)
         (setf curr-pos (+ curr-pos len skip))
         (incf skip))
    ini))

(defun process-array (arr)
  (loop for x from 0 by *chunk-sz* below (array-dimension arr 0)
     collect (reduce #'logxor (subseq arr x (+ x *chunk-sz*)))))

(defun calc-hash (strin)
  (let* ((lens (process-input strin))
         (arr (mk-array *length*))
         (curr-pos 0)
         (skip 0))
    (loop for i from 0 below 64 do
         (loop for len in lens do
              (nrev arr curr-pos len)
              (setf curr-pos (+ curr-pos len skip))
              (incf skip)))
    (with-output-to-string (stream)
      (loop for x in (process-array arr)
         do (format stream "~(~2,'0,x~)" x)))))

(defun day-10-a ()
  (let* ((in (read-input "./input"))
         (lens (input-to-list in))
         (arr (mk-array *length*))
         (curr-pos 0)
         (skip 0))
    (loop for len in lens do
         (nrev arr curr-pos len)
         (setf curr-pos (+ curr-pos len skip))
         (incf skip))
    (* (aref arr 0) (aref arr 1))))

(defun day-10-b ()
  (let ((in (read-input "./input")))
    (calc-hash in)))
