(defpackage :day01
  (:use :cl :cl-user)
  (:export :day-01))

(in-package :day01)

(defparameter *input* (get-input-as-list "./input.txt"))

(defparameter *half* (/ (length *input*) 2))

(defun get-input-as-list (path-string)
  (with-open-file (in path-string)
    (coerce (read-line in) 'list)))

(defun day-01 (input-list stride)
  (let* ((input-copy (copy-seq input-list)))
    (setf (cdr (last input-copy)) input-copy)
    (let ((turned (nthcdr stride input-copy))
          (acc nil))
      (mapcar #'(lambda (x y)
                  (when (char= x y)
                    (push (digit-char-p x) acc)))
              input-list
              turned)
      (reduce #'+ acc))))
