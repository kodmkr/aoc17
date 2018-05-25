(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre"))

(defpackage :day21
  (:use :cl)
  (:export :day-21-a
           :day-21-b))

(in-package :day21)

(defvar *rules* (make-hash-table :test #'equal))

(defvar *pattern* ".#./..#/###")
(defparameter *two* ".#/#.")

(defun pat-size (pat)
  (let ((pos-sep (position-if #'(lambda (c) (char= c #\/)) pat)))
    (when pos-sep
      pos-sep)))

(defun num-ons (pattern)
  (count-if #'(lambda (x) (char= x #\#)) pattern))

;; maybe should just have used fact that a 90° rotation would be a mapcar over
;; the list resulting from splitting the pattern at `/`
(defmacro mk-transformation (pref perm)
  (let* ((pat (gensym "PAT"))
         (cnt (gensym "CNT"))
         (perm-len (length `,perm))
         (name (format nil "~a-sz-~d" pref (if (> perm-len 5) 3 2))))
    `(defun ,(intern (string-upcase name)) (,pat &optional (,cnt 1))
       (let* ((pat-len (length ,pat))
              (res (copy-seq ,pat))
              (tmp (copy-seq ,pat)))
         (dotimes (c ,cnt res)
           (loop :for i :below pat-len :for d :in ',perm :do
                (setf (char res d) (char tmp i)))
           (setf tmp (copy-seq res)))))))

;; clockwise 90° rotation and horizontal mirroring suffice for
;; generating the Dihedral Group D_n.
(mk-transformation "rot" (1 4 2 0 3))
(mk-transformation "hflip" (3 4 2 0 1))
(mk-transformation "rot" (2 6 10 3 1 5 9 7 0 4 8))
(mk-transformation "hflip" (8 9 10 3 4 5 6 7 0 1 2))

(defun gen-grp (pattern &key rotator flipper)
  "Returns unique elements of D_n in the sense that their pattern is different."
  (delete-duplicates
   (loop for i below 8 collect
        (if (<= i 3)
            (funcall rotator pattern i)
            ;; could use some function composition here...
            (funcall flipper
                     (funcall rotator pattern (mod i 4)))))
   :test #'string=))



(defun day-21-a ())
(defun day-21-b ())
