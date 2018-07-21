(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-arrows")
  (ql:quickload "cl-ppcre")
  (ql:quickload "alexandria"))

(defpackage :day24
  (:use :cl :cl-arrows)
  (:export :day-24-a
           :day-24-b
           :inits))

(in-package :day24)

(defun read-input (path-str)
  (with-open-file (s path-str)
    (loop for line = (read-line s nil) while line collect
         (destructuring-bind (f s)
             (cl-ppcre:split "/" line)
           (cons (parse-integer f) (parse-integer s))))))

(defun has-val-p (val segment)
  (or (= val (car segment))
      (= val (cdr segment))))

(defun initp (s)
  (has-val-p 0 s))

(defun partition (segments)
  (loop for s in segments
     if (initp s) collect s into inits
     else collect s into not-inits
     finally (return (values inits not-inits))))

(defun the-other (val segment)
  (let ((seg-car (car segment)))
    (if (= seg-car val)
        (cdr segment)
        seg-car)))

(defun find-successors (val candidates)
  (remove-if-not (lambda (x)
                   (has-val-p val x))
                 candidates))

(defun without (to-be-removeds segments)
  (set-difference segments to-be-removeds :test #'equal))

(defun build-path (start-val avail-segs path)
  (let* ((succs (find-successors start-val avail-segs)))
    (if (null succs)
        (list path)
        (alexandria:mappend (lambda (succ)
                              (let ((nexts (without (list succ)  avail-segs)))
                                (build-path (the-other start-val succ)
                                            nexts
                                            (cons succ path))))
                            succs))))

(defun all-paths (segments)
  (multiple-value-bind (starts rest)
      (partition segments)
    (loop for start in starts append
         (build-path (the-other 0 start)
                     rest
                     (list start)))))

(defun path-strength (path)
  (loop for (x . y) in path sum (+ x y)))

(defun strongest-path (paths)
  (loop for path in paths maximize (path-strength path)))

(defun longest-path (paths)
  (loop for path in paths maximize (length path)))

(defun day-24-a ()
  (-<> (read-input "./input")
       (all-paths <>)
       (strongest-path <>)))

(defun day-24-b ()
  (let* ((inp (read-input "./input"))
         (paths (all-paths inp))
         (max-length (longest-path paths))
         (longest-paths (remove-if-not (lambda (p)
                                         (= (length p) max-length))
                                       paths)))
    (strongest-path longest-paths)))
