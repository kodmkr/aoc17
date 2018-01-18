(ql:quickload "cl-ppcre")

(defpackage :day09
  (:use :cl)
  (:import-from :cl-ppcre :regex-replace-all)
  (:export :day-09-a
           :day-09-b))

(in-package :day09)

(defconstant +open-brace+ #\{)
(defconstant +close-brace+ #\})
(defconstant +open-garbage+ #\<)
(defconstant +close-garbage+ #\>)
(defconstant +bang+ #\!)

(defun read-input (path-str)
  (with-open-file (in path-str)
    (let ((outp (make-array '(0) :element-type 'base-char
                            :fill-pointer 0 :adjustable t)))
      (with-output-to-string (s outp)
        (loop for c = (read-char in nil nil) while c do
             (if (char= c #\!)
                 (read-char in nil)
                 (princ c s)))
        (string-trim '(#\Space #\Newline) outp)))))

(defun clean (string)
  (regex-replace-all "<([^>]*?)>" string ""))

(defun count-groups (cleaned-string)
  (let ((level 0)
        (score 0)
        (seen nil))
    (loop for c across cleaned-string do
         (cond ((char= c +open-brace+)
                (push c seen)
                (incf level))
               ((char= c +close-brace+)
                (when (char= (first seen) +open-brace+)
                  (pop seen)
                  (incf score level)
                  (decf level)))
               (t nil)))
    score))

(defun count-garbage-contents (pristine-string)
  (let ((num-chars 0)
        (inside-garbage nil)
        (canceled nil))
    (labels ((toggle-bang ()
               (setf canceled (not canceled))))
      (loop for c across pristine-string do
           (block cancel
             (cond (canceled
                    (toggle-bang)
                    (return-from cancel))
                   ((char= c +bang+)
                    (toggle-bang))
                   ((char= c +open-garbage+)
                    (if inside-garbage
                        (incf num-chars)
                        (setf inside-garbage t)))
                   ((char= c +close-garbage+) 
                    (setf inside-garbage nil))
                   (t (when inside-garbage
                        (incf num-chars)))))))
    num-chars))

(defun day-09-a ()
  (let* ((inp (read-input "./input.txt"))
         (cleaned (clean inp)))
    (count-groups cleaned)))

(defun day-09-b ()
  (let ((inp (read-input "./input.txt")))
    (count-garbage-contents inp)))
