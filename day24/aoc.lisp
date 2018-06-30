(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-arrows")
  (ql:quickload "cl-ppcre")
  (ql:quickload "alexandria"))

(defpackage :day24
  (:use :cl :cl-arrows)
  (:export :day-24-a
           :day-24-b))

(in-package :day24)

(defun read-input (path-str)
  (with-open-file (s path-str)
    (sort (loop for line = (read-line s nil) while line collect
               (let ((spl (cl-ppcre:split "/" line)))
                 (cons (parse-integer (first spl)) (parse-integer (second spl)))))
          (lambda (x y)
            (and (<= (car x) (car y))
                 (<= (cdr x) (cdr y)))))))

(defun val (component)
  (+ (car component) (cdr component)))

(defun find-init (segs)
  (remove-if-not (lambda (x)
                   (or (zerop (car x))
                       (zerop (cdr x))))
                 segs))

(defun find-largest (target segs)
  (sort (remove-if-not (lambda (x)
                              (or (= (car x) target)
                                  (= (cdr x) target)))
                            segs)
             (lambda (x y)
               (> (+ (car x) (cdr x))
                  (+ (car y) (cdr y))))))

(defun the-other (the-one seg)
  (if (= (car seg) the-one)
      (cdr seg)
      (car seg)))

(defun build (tgt segs acc)
  (format t "[acc:~a]~%" acc)
  (let ((nxt (find-largest tgt segs)))
    (when (null nxt)
      (return-from build acc))
    (build (the-other tgt nxt)
           (remove nxt segs :test #'equal)
           (cons nxt acc))))

(defun path-sum (path)
  (loop for s in path sum (+ (car s) (cdr s))))

(defun pbuild (segs)
  (car (sort (loop for init in (find-init segs) collect
                  (build (the-other 0 init)
                         (remove init segs :test #'equal)
                         (list init)))
             (lambda (p q) (> (path-sum p) (path-sum q))))))

(defun day-24-a (&optional (prob "./input"))
  (-<> (read-input prob)
       (pbuild <>)
       (path-sum <>)))




(defun day-24-b ())
