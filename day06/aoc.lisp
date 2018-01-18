(ql:quickload "split-sequence")

(defpackage :day06
  (:use :cl :split-sequence)
  (:export :day-06-a
           :day-06-b))

(in-package :day06)

(defparameter *seen* (make-hash-table :test #'equal))
(defparameter *counts* (make-hash-table :test #'equal))

(defun reset-seen ()
  (setf *seen* (make-hash-table :test #'equal)))

(defun readd-counts ()
  (setf *counts* (make-hash-table :test #'equal)))

(defun reset-hashes ()
  (reset-seen)
  (readd-counts)
  (values))

(defun read-input (path-str)
  (with-open-file (in path-str)
    (let* ((line (read-line in nil))
           (split (split-sequence #\Tab line)))
      (when line
        (let* ((l (loop for n in split collect (parse-integer n :junk-allowed t)))
               (len (length l)))
          (make-array len :initial-contents l))))))

(defun mk-number-str (array)
  (let ((out (make-array
              0
              :element-type 'character
              :fill-pointer 0
              :adjustable t)))
    (with-output-to-string (s out)
      (loop for n across array do
         ;; Mind that some arrays,despite being differe,
         ;; can result in the same string
         ;; That's the reason for the `X`
           (format s "~dX" n)))
    out))

(defun find-largest-idx (array)
  (let* ((keyed (loop for x upfrom 0 for e across array collect (cons e x)))
         (sorted-keyed (stable-sort keyed #'> :key #'car)))
    (car (loop for (e . i) in sorted-keyed collect i))))

(defun set-or-add-one (str &optional (hash *seen*))
  (let ((val (gethash str hash)))
    (if val
        (incf (gethash str hash))
        (setf (gethash str hash) 0))))

(defun add-count (str cnt &optional (hash *counts*))
  (setf (gethash str hash) (cons cnt (gethash str hash))))

(defun distribute-largest-bank (array cnt)
  (let ((s (mk-number-str array)))
    (set-or-add-one s)
    (add-count s cnt))
  (let* ((a-len (array-dimension array 0))
         (largest-bank-idx (find-largest-idx array))
         (largest-bank-val (aref array largest-bank-idx))
         (a-cpy (copy-seq array)))
    (setf (aref a-cpy largest-bank-idx) 0)
    (labels ((wrap (idx)
               (if (>= idx a-len)
                   (mod idx a-len)
                   idx)))
      (do ((curr-idx (wrap (1+ largest-bank-idx)) (wrap (1+ curr-idx))))
          ((zerop largest-bank-val) a-cpy)
        (decf largest-bank-val)
        (incf (aref a-cpy curr-idx))))))

(defun distribute-while (array cnd)
  (let ((a-cpy (copy-seq array))
        (a-str (mk-number-str array)))
    (loop for cnt from -1 do
         (if (funcall cnd)
             (setf a-cpy (distribute-largest-bank a-cpy cnt))
             (return cnt)))))

(defun stop-p ()
  (every #'zerop (loop for x being the hash-values of *seen* collect x)))

(defun day-06-a ()
  (let ((inp (read-input "./input.txt")))
    (reset-hashes)
    (distribute-while inp #'stop-p)))

(defun day-06-b ()
  (let ((inp (read-input "./input.txt")))
    (reset-hashes)
    (distribute-while inp #'stop-p)
    (destructuring-bind (last first . rest)
        (loop for v being the hash-values of *counts*
           if (>= (length v) 2) append v)
      (declare (ignore rest))
      (- last first))))
