(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre"))

(defpackage :day21
  (:use :cl)
  (:export :day-21-a
           :day-21-b))

(in-package :day21)

(defvar *rules* (make-hash-table :test #'equal))

(defvar *pattern* ".#./..#/###")

(defun pat-size (pat)
  (let ((pos-sep (position-if #'(lambda (c) (char= c #\/)) pat)))
    (when pos-sep
      pos-sep)))

;; maybe should just have used fact that a 90° rotation would be a mapcar over
;; the list resulting from splitting the pattern at `/`
(defmacro mk-transformation (pref perm)
  (let* ((pat (gensym "PAT"))
         (cnt (gensym "CNT"))
         (perm-len (length `,perm))
         (name (format nil "~a-sz-~d" pref (if (> perm-len 5) 3 2))))
    `(defun ,(intern (string-upcase name)) (,pat &optional (,cnt 1))
       (let* ((pat-len (length ,pat))
              (res (make-string pat-len :initial-element #\X))
              (tmp (copy-seq ,pat)))
         (dotimes (c ,cnt res)
           (loop :for i :below pat-len :for d :in ',perm :do
                (setf (char res d) (char tmp i)))
           (setf tmp (copy-seq res)))))))

(mk-transformation "rot" (1 4 2 0 3))
(mk-transformation "rot" (2 6 10 3 1 5 9 7 0 4 8))
(mk-transformation "hflip" (3 4 2 0 1))
(mk-transformation "hflip" (8 9 10 3 4 5 6 7 0 1 2))

(defun num-ons (pattern)
  (count-if #'(lambda (x) (char= x #\#)) pattern))


(defun day-21-a ())
(defun day-21-b ())
