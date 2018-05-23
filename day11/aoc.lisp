(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre"))

(defpackage :day11
  (:use :cl)
  (:import-from :cl-ppcre :split)
  (:export :day-11-a
           :day-11-b))

(in-package :day11)

;; http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/

(defun read-input (path-str)
  "Reads the (whole) input into a string."
  (with-open-file (in path-str)
    (read-line in nil)))

(defun map-dir-to-vector (card-dir)
  (cond ((string= card-dir "n") '(-1 1 0))
        ((string= card-dir "ne") '(0 1 1))
        ((string= card-dir "se") '(1 0 1))
        ((string= card-dir "s") '(1 -1 0))
        ((string= card-dir "sw") '(0 -1 -1))
        ((string= card-dir "nw") '(-1 0 -1))
        (t (error "No such dir"))))

(defun translate-input (input-str)
  (let ((s (split "," input-str)))
    (loop for dir in s collect (map-dir-to-vector dir))))

(defun nvec-add (x y)
  "Destructively adds vector Y to vector X"
  (setf (first x) (+ (first x) (first y))
        (second x) (+ (second x) (second y))
        (third x) (+ (third x) (third y)))
  x)

(defun travel (trail)
  (let ((curr (list 0 0 0))
        (furthest 0))
    (loop for pt in trail do
         (nvec-add curr pt)
       ;; don't know why this does not work
       ;; with maximize
         (let ((d (hex-distance curr)))
           (if (> d furthest)
               (setf furthest d))))
    (values (hex-distance curr) furthest)))

(defun hex-distance (v)
  "Distance in relation to origin '(0 0 0 )."
  (max (abs (first v))
       (abs (second v))
       (abs (third v))))

(defun day-11-a ()
  (let* ((in (read-input "./input"))
         (vecs (translate-input in)))
    (travel vecs)))

(defun day-11-b ()
  (let* ((in (read-input "./input"))
         (vecs (translate-input in)))
    (multiple-value-bind (dist furthest) (travel vecs)
      (declare (ignore dist))
      furthest)))
