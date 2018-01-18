(ql:quickload "cl-ppcre")

(defpackage :day13
  (:use :cl)
  (:export :day-13-a
           :day-13-b))

(in-package :day13)

(defun read-input (path-str)
  (with-open-file (in path-str)
    (loop for l = (read-line in nil) while l collect l)))

(defun process-input (input)
  (loop for entry in input collect
       (let ((s (cl-ppcre:split "\\s*:\\s*" entry)))
         (list (parse-integer (car s)) (parse-integer (cadr s))))))

(defun mk-firewall (processed)
  (let* ((srtd (sort (copy-seq processed) #'> :key #'first))
         (num-layers (1+ (caar srtd)))
         (a (make-array num-layers :initial-element nil)))
    (loop for (l d) in srtd do
         (setf (aref a l) (list 0 d)))
    a))

(defun tri (x &key amp)
  "https://en.wikipedia.org/wiki/Triangle_wave"
  (let* ((depth (1- amp))
         (period (* depth 2)))
    (abs (- (mod (abs (- x depth)) period) depth))))

(defun step-fw (firewall time)
  (loop :for layer :across firewall :for i :from 0 :when layer :do
     (setf (car (aref firewall i)) (tri time :amp (cadr (aref firewall i)))))
  firewall)

(defun run (firewall &optional (starting-point 0))
  (let ((caught nil)
        (num-layers (array-dimension firewall 0)))
    (step-fw firewall starting-point)
    (loop
       :for pos :from 0 :below num-layers
       :for time :from (+ 0 starting-point) :do
       (when (and (aref firewall pos)
                  (zerop (car (aref firewall pos))))
         (push (cons pos (cadr (aref firewall pos))) caught))
       (step-fw firewall (1+ time)))
    caught))

(defun safe (firewall &optional (starting-point 0))
  (step-fw firewall starting-point)
  (loop :for layer :across firewall :for i :from (+ starting-point 0)
     :if (and layer (< (tri i :amp (cadr layer)) 1)) :return nil
     :finally (return t)))

(defun severity (layers)
  (if layers
      (loop for (l . d) in layers sum (* l d))
      0))

(defun day-13-a ()
  (let* ((in (read-input "./input"))
         (procd (process-input in))
         (firewall (mk-firewall procd)))
    (severity (run firewall))))

(defun day-13-b ()
  (let* ((in (read-input "./input"))
         (procd (process-input in))
         (firewall (mk-firewall procd)))
    (loop :for delay :from 1 :while (not (safe firewall delay))
       :finally (return delay))))
