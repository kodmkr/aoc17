(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-arrows")
  (ql:quickload "cl-ppcre")
  (ql:quickload "anaphora")
  (ql:quickload "alexandria"))

(defpackage :day23
  (:use :cl :cl-arrows :anaphora-basic)
  (:export :day-23-a
           :day-23-b))

(in-package :day23)

(defparameter *registers* (make-hash-table))
(defparameter *program* nil)
(defparameter +regs+ "abcdefgh")

(defun read-input (path-str)
  (with-open-file (in path-str)
    (flet ((reg-or-val (rov)
             (let ((maybereg (aref rov 0)))
               (if (alpha-char-p maybereg)
                   maybereg
                   (parse-integer rov :junk-allowed t))))
           (mk-func (func-string)
             (intern (string-upcase (concatenate 'string "%" func-string)))))
      (let* ((insns (loop for line = (read-line in nil) while line collect
                         (cl-ppcre:split " " line)))
             (insns-len (length insns)))
        (map-into insns (lambda (opvw)
                          (destructuring-bind (op v w) opvw
                            (list (mk-func op) (reg-or-val v) (reg-or-val w))))
                  insns)
        (make-array insns-len :initial-contents insns)))))

(defun init ()
  (clrhash *registers*)
  (loop for c across +regs+ do
       (setf (gethash c *registers*) 0))
  *registers*)

(defun value (?)
  (etypecase ?
    (standard-char (gethash ? *registers*))
    (integer ?)))

(defun %set (reg vr)
  (declare (special counter))
  (setf (gethash reg *registers*) (value vr))
  (incf counter))

(defun %sub (reg vr)
  (declare (special counter))
  (setf (gethash reg *registers*) (- (value reg) (value vr)))
  (incf counter))

(defun %mul (reg vr)
  (declare (special counter))
  (declare (special mulcnt))
  (setf (gethash reg *registers*) (* (value reg) (value vr)))
  (incf counter)
  (incf mulcnt))

(defun %jnz (vr wr)
  (declare (special counter))
  (incf counter (if (not (zerop (value vr))) (value wr) 1)))


;; ======================================================================

(defun day-23-a ()
  (let* ((program (read-input "./input"))
         (plen (array-dimension program 0))
         (counter 0)
         (mulcnt 0))
    (declare (special counter))
    (declare (special mulcnt))
    (init)
    (loop while (< counter plen) do
         (destructuring-bind (f op1 op2) (aref program counter)
           (funcall f op1 op2)))
    mulcnt))

(defun day-23-b ()
  (let* ((program (read-input "./input"))
         (plen (array-dimension program 0))
         (counter 0)
         (mulcnt 0))
    (declare (special counter))
    (declare (special mulcnt))
    (init)
    (setf (gethash #\a *registers*) 1)
    (loop while (< counter plen) do
         (format t "[counter:~a]~%" counter)
         (destructuring-bind (f op1 op2) (aref program counter)
           (funcall f op1 op2))))
  (gethash #\h *registers*))
