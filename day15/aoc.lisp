(defpackage :day15
  (:use :cl)
  (:export :day-15-a
           :day-15-b))

(in-package :day15)

(defconstant +fact-a+ 16807)
(defconstant +fact-b+ 48271)
(defconstant +div+ (1- (expt 2 31)))

(defconstant +start-a+ 516)
(defconstant +start-b+ 190)

(defconstant +start-a-test+ 65)
(defconstant +start-b-test+ 8921)

(defconstant +times-p1+ 40000000)
(defconstant +times-p2+ 5000000)

(defvar *curr-a*)
(defvar *curr-b*)

(defun next-a ()
  (setf *curr-a* (mod (* +fact-a+ *curr-a*) +div+))
  *curr-a*)

(defun next-b ()
  (setf *curr-b* (mod (* +fact-b+ *curr-b*) +div+))
  *curr-b*)

(defun mk-div-by-pred (num)
  #'(lambda (x) (zerop (mod x num))))

(defun run-for (times &optional (pred-a (mk-div-by-pred 1)) (pred-b (mk-div-by-pred 1)))
  (loop for i from 1 to times
     for a = (loop for x = (next-a) while (not (funcall pred-a x)) finally (return x))
     for b = (loop for x = (next-b) while (not (funcall pred-b x)) finally (return x))
     count (and (= (logand a #xffff) (logand b #xffff)) 1)))

(defun day-15-a ()
  (let ((*curr-a* +start-a+)
        (*curr-b* +start-b+))
    (run-for +times-p1+)))

(defun day-15-b ()
  (let ((*curr-a* +start-a+)
        (*curr-b* +start-b+))
    (run-for +times-p2+ (mk-div-by-pred 4) (mk-div-by-pred 8))))
