(ql:quickload "cl-ppcre")

(defpackage :day20
  (:use :cl)
  (:export :day-20-a
           :day-20-b))

(in-package :day20)

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

(defun do-ticks (point-data &optional &key (count 500) retainer)
  "Ticks COUNT times. COUNT is randomly set to 500. This is quadratic, so there is
a noticeable delay."
  (if (null retainer)
      (progn
        (loop for x from 1 below count do
             (tick point-data))
        (return-from do-ticks point-data))
      (let ((cpy (copy-seq point-data)))
        (loop for x from 1 below count do
             (setf cpy (funcall retainer (tick cpy))))
        (return-from do-ticks cpy))))

(defun coincidep (p1 p2)
  (let ((v1 (pts-pos p1))
        (v2 (pts-pos p2)))
    (and (= (svref v1 0) (svref v2 0))
         (= (svref v1 1) (svref v2 1))
         (= (svref v1 2) (svref v2 2)))))

(defun find-next-different-pos (pt-datas &optional (start 0))
  (let ((elt (aref pt-datas start)))
    (position-if #'(lambda (x) (not (coincidep x elt)))
                 pt-datas
                 :start start)))

(defun retain (pt-datas)
  (let ((len (array-dimension pt-datas 0))
        (start 0)
        (retained nil))
    (loop named lp do
         (let ((nxt (find-next-different-pos pt-datas start)))
           ;; (format t "[start:~d][nxt:~d]~%" start nxt)
           (if (not (null nxt))
               (if (= nxt (1+ start))
                   (progn
                     (push start retained)
                     (setf start nxt))
                   (setf start nxt))
               (when (<= start (1- len))
                 (push start retained)
                 (return-from lp)))))
    (make-array (length retained) :initial-contents
                (loop for x in (nreverse retained) collect (aref pt-datas x)))))

(defun day-20-a ()
  (let ((inp (read-input "./input")))
    (pts-idx (aref (do-ticks inp) 0))))

(defun day-20-b ()
  (let ((inp (read-input "./input")))
    (array-dimension (do-ticks inp :retainer #'retain) 0)))
