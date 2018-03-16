(ql:quickload "cl-ppcre")

(defpackage :day18
  (:use :cl)
  (:import-from :cl-ppcre :split)
  (:export :day-18-a
           :day-18-b))

(in-package :day18)

(defun read-input (path-str)
  (with-open-file (st path-str)
    (loop for line = (read-line st nil) while line collect (split "\\s" line))))

(defun reg-p (maybe-reg)
  "Registers are alphabetic characters."
  (if (characterp maybe-reg)
      (alpha-char-p maybe-reg)
      (when (stringp maybe-reg)
        (alpha-char-p (char maybe-reg 0)))))

(defun day-18-a ())
(defun day-18-b ())
