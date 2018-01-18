(defpackage :day17
  (:use :cl)
  (:export :day-17-a
           :day-17-b))

(in-package :day17)

(defconstant +step+ 348)

(defparameter *sz* 2018)

(defparameter *circ-buff*
  (make-array (list *sz*)
              :adjustable t
              :element-type 'integer
              :initial-element 0))

(defun reset ()
  (setf *circ-buff*
        (make-array (list *sz*)
                    :adjustable t
                    :element-type 'integer
                    :initial-element 0))
  (values))

(defun ninsert-after (arr idx val)
  (let ((after (1+ idx)))
    (replace arr arr :start1 (1+ after) :start2 after)
    (setf (aref arr after) val)
    arr))

(defun run (&key (stride +step+) (arr *circ-buff*) (times *sz*)
              (really-do t))
  (let ((curr 0))
    (loop for val from 1 below times do
         (setf curr (mod (+ curr stride) val))
         (if really-do
             (ninsert-after arr curr val)
             (when (= curr 0)
               (ninsert-after arr curr val)))
         (incf curr))
    curr))

(defun day-17-a ()
  (reset)
  (let ((last-idx (run)))
    (aref *circ-buff* (1+ last-idx))))

(defun day-17-b ()
  (reset)
  (run :times 49000000 :really-do nil)
  (aref *circ-buff* 1))
