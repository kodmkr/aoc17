(load "../day10/aoc.lisp")

(defpackage :day14
  (:use :cl)
  (:import-from :day10 :calc-hash)
  (:export :day-14-a
           :day-14-b))

(in-package :day14)

(defparameter *input* "wenycdww")
(defparameter *bitfmt* "~4,'0,,b")
(defparameter *char-map* #("0000" "0001" "0010" "0011"
                           "0100" "0101" "0110" "0111"
                           "1000" "1001" "1010" "1011"
                           "1100" "1101" "1110" "1111"))

(defparameter *tab* exmp)
(defparameter *len* 128)
(defparameter *side* (isqrt *len*))

(defparameter *unseen* (make-array *len* :initial-element t))

(defun cmref (chr)
  (cond ((char<= #\0 chr #\9) (aref *char-map* (- (char-code chr) 48)))
        ((char<= #\a chr #\f) (aref *char-map* (- (char-code chr) 87)))
        (t nil)))

(defun add-num (num str)
  (concatenate 'string str "-" (write-to-string num)))

(defun hex-to-bin (hex-string)
  (let ((res (make-array *len* :element-type 'character :fill-pointer 0)))
    (with-output-to-string (strm res)
      (loop for c across hex-string do
           (princ (cmref c) strm)))
    res))

(defun gen ()
  (let ((str (make-array (expt *len* 2) :element-type 'character  :fill-pointer 0)))
    (with-output-to-string (s str)
      (loop for x from 0 below *len* do
           (princ (hex-to-bin (calc-hash (add-num x *input*))) s)))
    str))

(defun neighbors (x y)
  "Return a list of neighboring positions for the given position.
NOTE: The returned positions are list literals and should not be modified."
  (let ((max-idx (1- *side*)))
    (flet ((maxip (i)
             (= i max-idx)))
      (cond ((and (zerop x) (zerop y)) ;; upper left
             (list `(,x ,(1+ y))
                   `(,(1+ x) ,y)))
            ((and (zerop x) (maxip y)) ;; upper right
             (list `(,x ,(1- y))
                   `(,(1+ x) ,y)))
            ((and (maxip x) (maxip y)) ;; lower right
             (list `(,(1- x) ,y)
                   `(,x ,(1- y))))
            ((and (maxip x) (zerop y)) ;; lower left
             (list `(,(1- x) ,y)
                   `(,x ,(1+ y))))
            ((zerop x) ;; upper
             (list `(,x ,(1- y))
                   `(,x ,(1+ y))
                   `(,(1+ x) ,y)))
            ((maxip y) ;; right
             (list `(,(1- x) ,y)
                   `(,x ,(1- y))
                   `(,(1+ x) ,y)))
            ((maxip x) ;; lower
             (list `(,(1- x) ,y)
                   `(,x ,(1- y))
                   `(,x ,(1+ y))))
            ((zerop y) ;; left
             (list `(,(1- x) ,y)
                   `(,x ,(1+ y))
                   `(,(1+ x) ,y)))
            (t ;; middle
             (list `(,(1- x) ,y)
                   `(,x ,(1- y))
                   `(,x ,(1+ y))
                   `(,(1+ x) ,y)))))))

(declaim (inline pos-from-idx))
(defun pos-from-idx (idx)
  (multiple-value-bind (x y) (floor idx *side*)
    (list x y)))

(declaim (inline idx-from-pos))
(defun idx-from-pos (pos)
  (destructuring-bind (x y) pos
    (+ y (* x *side*))))

(declaim (inline czerop))
(defun czerop (c)
  (char= c #\0))

(defun neighbors-if-free (x y)
  (let ((nbrs (neighbors x y)))
    (remove-if-not #'(lambda (p) (viable-p (idx-from-pos p))) nbrs)))

(defun filter-zeros ()
  (loop :for i :below *len* :do
     (when (czerop (aref *tab* i))
       (setf (aref *unseen* i) 0)))
  *unseen*)

(defun init-unseen ()
  (setf *unseen* (make-array *len* :initial-element t))
  (filter-zeros))

(defun viable-p (idx)
  (let ((elt (aref *unseen* idx)))
    (and (not (numberp elt))
         (eql elt t))))

(defun find-next ()
  (loop :for i :below *len* :do
     (when (viable-p i)
       (return i))))

(defun fill-group (num p)
  (let ((nbrs (apply #'neighbors-if-free p)))
    (setf (aref *unseen* (idx-from-pos p)) num)
    (loop for q in nbrs do
         (fill-group num q))))

(defun fill-all-groups ()
  (let ((n 0))
    (loop while (some #'(lambda (x) (eql x t)) *unseen*) do
         (incf n)
         (fill-group n (pos-from-idx (find-next))))
    n))

(defun day-14-a ()
  (let* ((*tab* (gen))
         (*len* (length *tab*)))
    (count-if #'(lambda (x) (char= #\1 x)) *tab*)))

(defun day-14-b ()
  (let* ((*tab* (gen))
         (*len* (length *tab*))
         (*side* (isqrt *len*)))
    (init-unseen)
    (fill-all-groups)))
