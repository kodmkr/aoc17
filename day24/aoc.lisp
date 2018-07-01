(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-arrows")
  (ql:quickload "cl-ppcre")
  (ql:quickload "alexandria"))

(defpackage :day24
  (:use :cl :cl-arrows)
  (:export :day-24-a
           :day-24-b))

(in-package :day24)
