(defpackage :day19
  (:use :cl)
  (:export :day-19-a
           :day-19-b))

(in-package :day19)

(defparameter +horizontal-segment+ "-")
(defparameter +vertical-segment+ "|")
(defparameter +junction+ "+")

(defun read-input (path-str)
  (with-open-file (in path-str)
    (multiple-value-bind (lines max-len)
        (loop
           :for line := (read-line in nil)
           :while line :collect (cons (length line) line) :into lines
           :maximize (array-dimension line 0)  into max-len
           :finally (return (values lines max-len)))
      (values lines max-len))))

(defun process-input (lens-n-lines max-len)
  (let* ((num-lines (length lens-n-lines))
         (str (make-string (* num-lines max-len)
                           :initial-element #\Space)))
    (loop :for (len . line) :in lens-n-lines :for i :from 0 :by max-len :do
       (replace str line :start1 i :end1 (+ i len)))
    str))

(defun render (str line-len)
  (loop :for i :from 0 :by line-len :while (< i (length str)) :do
     (print (subseq str i (+ i line-len)))))



(defun day-19-a ())
(defun day-19-b ())
