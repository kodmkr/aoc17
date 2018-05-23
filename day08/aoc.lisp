(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre"))

(defpackage :day08
  (:use :cl)
  (:import-from :cl-ppcre :scan-to-strings :split)
  (:export :day-08-a
           :day-08-b))

(in-package :day08)

(defparameter *regex* "(\\w+)\\s*(\\w+)\\s*(-?\\d+)\\s*(\\w+)\\s*(\\w+)\\s*([=!<>]+)\\s*(-?\\d+)")

(defparameter *vars* (make-hash-table :test #'equal))

(defun reset-vars ()
  (clrhash *vars*))

(defun read-input (path-str)
  (with-open-file (in path-str)
    (loop for line = (read-line in nil) while line
       collect line)))

(defmacro define-translation (trans-name &body body)
  (let ((arg (gensym)))
    `(defun ,(intern (concatenate 'string
                                  "TRANSLATE-"
                                  (string-upcase trans-name))) (,arg)
       (cond ,@(loop for (src dest . rest) in body collect
                    `((string= ,arg ,src) ,dest))
             (t nil)))))

(define-translation "rel-op"
  ("==" '=)
  (">" '>)
  ("<" '<)
  ("!=" '/=)
  ("<=" '<=)
  (">=" '>=))

(define-translation "op"
  ("inc" 'incf)
  ("dec" 'decf))

(define-translation "cond"
  ("if" 'when))

(defun parse-line (line)
  (multiple-value-bind (match grps)
      (scan-to-strings *regex* line)
    (declare (ignore match))
    (list (aref grps 0)
          (aref grps 1)
          (parse-integer (aref grps 2))
          (aref grps 3)
          (aref grps 4)
          (aref grps 5)
          (parse-integer (aref grps 6)))))

(defun parse-lines (lines)
  (loop for line in lines collect
       (parse-line line)))

(defun initialize-vars (psds &optional (vars *vars*))
  (loop for (var . rest) in psds do
       (setf (gethash var vars) 0))
  (values))

;; now that should probably not be done like that...
(defun make-instruction (psd)
  (let ((arg (gensym)))
    (destructuring-bind (n1 op num1 cd n2 rop num2) psd
      `(lambda (,arg)
         (,(translate-cond cd) (,(translate-rel-op rop) (gethash ,n2 ,arg) ,num2)
           (,(translate-op op) (gethash ,n1 ,arg) ,num1))))))

(defun make-instructions (psds)
  (loop for psd in psds collect
       (eval (make-instruction psd))))

(defun run (prgr &optional (vars *vars*))
  (loop for ins in prgr
     do (funcall ins vars)
     maximize (find-var-value vars)))

(defun find-var-value (&optional (vars *vars*))
  (loop for v being the hash-values of vars maximize v))

(defun day-08-a ()
  (let* ((inp (read-input "./input.txt"))
         (descs (parse-lines inp))
         (prgr (make-instructions descs)))
    (reset-vars)
    (initialize-vars descs)
    (run prgr)
    (find-var-value)))

(defun day-08-b ()
  (let* ((inp (read-input "./input.txt"))
         (descs (parse-lines inp))
         (prgr (make-instructions descs)))
    (reset-vars)
    (initialize-vars descs)
    (run prgr)))
