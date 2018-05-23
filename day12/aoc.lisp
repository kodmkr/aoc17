(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre"))

(defpackage :day12
  (:use :cl)
  (:export :day-12-a
           :day-12-b))

(in-package :day12)

(defun read-input (path-str)
  (with-open-file (in path-str)
    (loop for line = (read-line in nil) while line
       collect line)))

(defun process-input (str-list)
  (sort (loop for entry in str-list collect
             (destructuring-bind (id rest) (cl-ppcre:split "\\s*<->\\s*" entry)
               (list (parse-integer id)
                     (sort (loop for nb in (cl-ppcre:split ",\\s*" rest) collect
                                (parse-integer nb))
                           #'<))))
        #'< :key #'first))

(defun build-group (adj-list &optional (to-visit (cadar adj-list)) visited group)
  (if (null to-visit)
      (sort (remove-duplicates group) #'<)
      (let* ((curr (car to-visit))
             (rst (cdr to-visit))
             (nbrs-temp (cadr (find-if #'(lambda (x)
                                          (= x curr))
                                      adj-list :key #'car)))
             (nbrs (remove-if #'(lambda (x)
                                  (member x visited))
                              nbrs-temp))
             (to-visit-n (append rst nbrs))
             (visited-n (cons curr visited))
             (grp-n (append group nbrs-temp)))
        (build-group adj-list to-visit-n visited-n grp-n))))

(defun day-12-a ()
  (let* ((in (read-input "./input"))
         (adj-list (process-input in))
         (clique (build-group adj-list)))
    (length clique)))

(defun day-12-b ()
  (let* ((in (read-input "./input"))
         (adj-list (process-input in))
         (seen nil))
    (loop for cnt from 0 while (not (null adj-list)) do
         (setf seen (append seen (build-group adj-list)))
         (setf adj-list (remove-if #'(lambda (x)
                                       (member x seen))
                                   adj-list :key #'first))
         finally (return cnt))))
