(ql:quickload "cl-ppcre")

(defpackage :day20
  (:use :cl)
  (:export :day-20-a
           :day-20-b))

(in-package :day20)

(defvar pts)

(defstruct (point-data (:conc-name pts-))
  (pos (make-array 3 :element-type 'fixnum))
  (vel (make-array 3 :element-type 'fixnum))
  (acc (make-array 3 :element-type 'fixnum))
  (idx (make-array 3 :element-type 'fixnum)))

(defun manh-dist (pt)
  (loop for c across (pts-pos pt) sum (abs c)))

(defun extract-coordinates (point-spec)
  (let* ((numbers "\\s*(-?\\d+),\\s*(-?\\d+),\\s*(-?\\d+)")
         (line-pattern (concatenate 'string
                                    "p=<" numbers ">,\\s*"
                                    "v=<" numbers ">,\\s*"
                                    "a=<" numbers ">$")))
    (cl-ppcre:register-groups-bind
        ((#'parse-integer p1 p2 p3)
         (#'parse-integer v1 v2 v3)
         (#'parse-integer a1 a2 a3))
        (line-pattern point-spec)
      (values (vector p1 p2 p3)
              (vector v1 v2 v3)
              (vector a1 a2 a3)))))

(defun sort-by-distance (pts &optional &key (test #'manh-dist))
  (sort pts #'< :key test))

(defun read-input (path-str)
  (with-open-file (str path-str)
    (let* ((points (loop :for line = (read-line str nil)
                      :for i :from 0 :while line
                      :collect (multiple-value-bind (pos vel acc) (extract-coordinates line)
                                 (make-point-data :pos pos :vel vel :acc acc :idx i))))
           (points-len (length points)))
      (sort-by-distance (make-array points-len :initial-contents points)))))

(defun update-points-data (ptd)
  (symbol-macrolet ((pos (pts-pos ptd))
                    (vel (pts-vel ptd))
                    (acc (pts-acc ptd)))
    (setf vel (map 'vector #'+ vel acc)
          pos (map 'vector #'+ pos vel)))
  ptd)

(defun tick (points-data)
  (loop :for ptd :across points-data :do
       (update-points-data ptd))
  (sort-by-distance points-data))

(defun do-ticks (point-data &optional (count 1000))
  "Ticks COUNT times. COUNT is randomly set to 1000. This is quadratic, so there is
a noticeable delay."
  (loop for x below count do
       (tick point-data))
  point-data)

(defun day-20-a ()
  (let* ((inp (read-input "./input")))
    (pts-idx (aref (do-ticks inp) 0))))


(defun day-20-b ())
