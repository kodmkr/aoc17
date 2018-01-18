(ql:quickload "split-sequence")

(defpackage :day02
  (:use :cl :split-sequence)
  (:export :day-02))

(in-package :day02)

(defparameter *delim* #\Tab)

(defparameter *input* (get-input "./input.txt"))

(defun get-input (path-str &optional (delim #\Tab))
  (with-open-file (in path-str)
    (loop :for line = (read-line in nil) :while line
       :collect (sort (mapcar #'(lambda (x) (parse-integer x))
                              (split-sequence delim line))
                      #'>))))

(defun get-least-and-largest (list)
  "Assumes that the list is sorted descending"
  (cons (car list) (car (last list))))

(defun get-divisors (list)
  "Assumes that the list is sorted descending"
  (loop named outer for subl on list :do
       (loop for x in (cdr subl) when (zerop (mod (car subl) x)) :do
            (return-from outer (cons (car subl) x)))))

(defun day-02 (input func op)
  (let* ((cons-list (mapcar func input))
         (num-list (mapcar #'(lambda (x)
                               (funcall op (car x) (cdr x)))
                           cons-list)))
    (reduce #'+ num-list)))
