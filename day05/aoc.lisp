(defpackage :day05
  (:use :cl)
  (:export :day-05-a
           :day-05-b))

(in-package :day05)

(defun number-array-from-file (path-str)
  (with-open-file (in path-str)
    (let* ((l (loop for line = (read-line in nil) while line collect
                   (parse-integer line :junk-allowed t)))
           (len (length l)))
      (make-array len :initial-contents l))))
          
(defmacro follow (in-array &body body)
  `(let ((len (array-dimension ,in-array 0)))
    ;; the horror...
    (do* ((offset 0) ;; 
          (delta (aref ,in-array offset))
          (cnt 0))
         (nil)
      (setf delta (aref ,in-array offset))
      ,@body
      (setf offset (+ offset delta))
      (incf cnt)
      (when (>= offset len)
        (return cnt)))))

(defun day-05-a ()
  (let ((inp (number-array-from-file "./input.txt")))
    (follow inp
      (incf (aref inp offset)))))

(defun day-05-b ()
  (let ((inp (number-array-from-file "./input.txt")))
    (follow inp
      (if (>= (aref inp offset) 3)
          (decf (aref inp offset))
          (incf (aref inp offset))))))
