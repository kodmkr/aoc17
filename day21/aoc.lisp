(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre")
  (ql:quickload "cl-arrows"))

(defpackage :day21
  (:use :cl :cl-arrows)
  (:export :day-21-a
           :day-21-b))

(in-package :day21)

(defvar *rules* (make-hash-table :test #'equal))

(defvar *pattern* ".#./..#/###")

(defun pat-size (pat)
  (let ((pos-sep (position-if (lambda (c) (char= c #\/)) pat)))
    (when pos-sep
      pos-sep)))

(defun num-ons (pattern)
  (count-if (lambda (x) (char= x #\#)) pattern))

;; maybe should just have used fact that a 90° rotation would be a mapcar over
;; the list resulting from splitting the pattern at `/`
(defmacro mk-transformation (pref perm)
  (let* ((pat (gensym "PAT"))
         (cnt (gensym "CNT"))
         (perm-len (length `,perm))
         (name (format nil "~a-sz-~d" pref (if (> perm-len 5) 3 2))))
    `(defun ,(intern (string-upcase name)) (,pat &optional (,cnt 1))
       (let* ((pat-len (length ,pat))
              (res (copy-seq ,pat))
              (tmp (copy-seq ,pat)))
         (dotimes (c ,cnt res)
           (loop :for i :below pat-len :for d :in ',perm :do
                (setf (char res d) (char tmp i)))
           (setf tmp (copy-seq res)))))))

;; clockwise 90° rotation and horizontal mirroring suffice for
;; generating the Dihedral Group D_n.
(mk-transformation "rot" (1 4 2 0 3))
(mk-transformation "hflip" (3 4 2 0 1))
(mk-transformation "rot" (2 6 10 3 1 5 9 7 0 4 8))
(mk-transformation "hflip" (8 9 10 3 4 5 6 7 0 1 2))

(defun gen-grp (pattern &key rotator flipper)
  "Returns unique elements of D_n in the sense that their pattern is different."
  (delete-duplicates
   (loop for i below 8 collect
        (if (<= i 3)
            (funcall rotator pattern i)
            ;; could use some function composition here...
            (funcall flipper
                     (funcall rotator pattern (mod i 4)))))
   :test #'string=))

(defun read-input (in-path)
  (clrhash *rules*)
  (with-open-file (s in-path)
    (loop for line = (read-line s nil) while line do
         (destructuring-bind (left right) (cl-ppcre:split "\\s+=>\\s+" line)
           (let ((grp (case (pat-size left)
                        (2 (gen-grp left :rotator #'rot-sz-2 :flipper #'hflip-sz-2))
                        (3 (gen-grp left :rotator #'rot-sz-3 :flipper #'hflip-sz-3)))))
             (dolist (e grp *rules*)
               (setf (gethash e *rules*) right))))))
  *rules*)

(defun group-by (num seq)
  "Groups NUM adjacent element in SEQ together.
The length of SEQ _has_ to be a  multiple of NUM."
  (when (plusp num)
    (let* ((len (length seq))
           (q (floor len num))
           (grp nil))
      (dotimes (i q (nreverse grp))
        (push (subseq seq (* i num) (* (1+ i) num)) grp)))))

(defun split-pattern (pattern)
  (let* ((pat-size (pat-size pattern))
         (side-len (if (evenp pat-size) 2 3))
         (split-pattern (cl-ppcre:split "/" pattern))
         (groups (group-by side-len split-pattern))
         (split-groups (loop for group in groups collect
                            (mapcar (lambda (x) (group-by side-len x))
                                    group))))
    (labels ((concat (x y)
               (concatenate 'string x "/" y)))
      (loop for split-group in split-groups nconc
           (reduce (lambda (f r) (mapcar #'concat f r)) split-group)))))

(defun match-rules (patterns)
  (mapcar (lambda (x) (gethash x *rules*)) patterns))

;; maybe split-pattern and join-patterns
;; can be combined into one function
;; one being the "inverse" of the other
(defun join-patterns (patterns)
  (when (= 1 (length patterns))
    (return-from join-patterns (car patterns)))
  (let* ((side-len (isqrt (length patterns)))
         (split-patterns
          (mapcar (lambda (pattern)
                      (cl-ppcre:split "/" pattern))
                  patterns))
         (grouped (group-by side-len split-patterns)))
    (labels ((zip (xs ys &key (sep ""))
               (mapcar (lambda (x y) (concatenate 'string x sep y)) xs ys)))
      (reduce #'(lambda (xs ys) (concatenate 'string xs "/" ys))
              (loop for group in grouped nconc
                   (reduce #'zip group))))))


(defun do-step (&optional (count 1))
  (let ((pattern *pattern*))
    (loop for i below count do
         (-<> pattern
              (split-pattern <>)
              (match-rules <>)
              (join-patterns <>)
              (setf pattern <>)))
    pattern))

(defun count-on (pattern)
  (count-if (lambda (c) (char= c #\#)) pattern))

(defun day-21-a ()
  (read-input "./input")
  (count-on (do-step 5)))

;; takes about a minute...
(defun day-21-b ()
  (read-input "./input")
  (count-on (do-step 18)))
