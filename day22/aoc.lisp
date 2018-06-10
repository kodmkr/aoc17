(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-arrows")
  (ql:quickload  "alexandria"))

(defpackage :day22
  (:use :cl :cl-arrows)
  (:export :day-22-a
           :day-22-b))

(in-package :day22)

(defparameter *positions* (make-hash-table :test #'eql))
(defparameter +up+ #c(0 1))
(defparameter +down+ #c(0 -1))
(defparameter +left+ #c(-1 0))
(defparameter +right+ #c(1 0))

(defun read-input (path-str)
  (clrhash *positions*)
  (let* ((contents (alexandria:read-file-into-string path-str))
         (rows (count-if (lambda (c) (char= c #\Newline)) contents))
         (cols (position #\Newline contents))
         (max-r (1- rows))
         (max-c (1- cols))
         (contents (remove #\Newline contents)))
    (labels ((idx-from-coords (i j)
               (+ (* rows i) j))
             (infp (idx)
               (char= (char contents idx) #\#))
             (tr-cpx (i j)
               (complex (+ (- max-c) j) (- max-r i))))
      (loop for i below rows do
           (loop for j below cols do
                (let ((idx (idx-from-coords i j)))
                  (when (infp idx)
                    (setf (gethash (tr-cpx i j) *positions*) t)))))
      (values *positions* (tr-cpx (floor max-r 2) (floor max-c 2))))))

(defstruct (state (:conc-name st-))
  (position #c(0 0))
  (direction +up+)
  (num-infected 0))

(defun init (path-str)
  (multiple-value-bind (poss start)
      (read-input path-str)
    (declare (ignore poss))
  (make-state :position start)))

(defun toggle-infected (pos)
  (multiple-value-bind (inf? present?)
      (gethash pos *positions*)
    (declare (ignore inf?))
    (if present?
        (progn
          (remhash pos *positions*)
          nil)
        (setf (gethash pos *positions*) t))))

(defun infectedp (pos)
  (multiple-value-bind (inf? present?)
      (gethash pos *positions*)
    (when present?
      inf?)))

(defun go-in-dir (state)
  (setf (st-position state) (+ (st-position state) (st-direction state))))

(defun turn-left (state)
  (setf (st-direction state) (* (st-direction state) #c(0 1))))

(defun turn-right (state)
  (setf (st-direction state) (* (st-direction state) #c(0 -1))))

(defun burst (state &optional (cnt 1))
  (dotimes (i cnt state)
    (let ((curr-pos (st-position state)))
      (if (infectedp curr-pos)
          (turn-right state)
          (turn-left state))
      (when (toggle-infected (st-position state))
        (incf (st-num-infected state)))
      (go-in-dir state)))
  state)

(defun day-22-a (&key (cnt 10000) (input "./input"))
  (-<> (init input)
       (burst <> cnt)
       (st-num-infected <>)))


(defun day-22-b ())
