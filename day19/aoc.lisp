(defpackage :day19
  (:use :cl)
  (:export :day-19-a
           :day-19-b))

(in-package :day19)

(defparameter +horizontal-segment+ #\-)
(defparameter +vertical-segment+ #\|)
(defparameter +junction+ #\+)
(defparameter +empty+ #\Space)

(defparameter +north+ '(-1 0))
(defparameter +south+ '(1 0))
(defparameter +east+ '(0 1))
(defparameter +west+ '(0 -1))

(defvar *start*)
(defvar *dim*)
(defvar *str*)

(defun read-input (path-str)
  (with-open-file (in path-str)
    (multiple-value-bind (lines max-len)
        (loop
           :for line := (read-line in nil)
           :while line
           :collect (cons (length line) line)
           :into lines
           :maximize (array-dimension line 0)  into max-len
           :finally (return (values lines max-len)))
      (values lines max-len))))

(defun process-input (lens-n-lines max-len)
  (let* ((num-lines (length lens-n-lines))
         (str (make-string (* num-lines max-len)
                           :initial-element #\Space)))
    (setf *start* `(0 ,(1- (car (car lens-n-lines)))))
    (setf *dim* `(,num-lines ,max-len))
    (loop :for (len . line) :in lens-n-lines :for i :from 0 :by max-len :do
       (replace str line :start1 i :end1 (+ i len)))
    (setf *str* str)
    (values str max-len)))

(defun empty-p (char)
  (char= char +empty+))

(defun go-in-dir (curr-pos dir)
  "Returns the next position from the current position CURR-POS in direction DIR."
  `(,(+ (car curr-pos) (car dir)) ,(+ (cadr curr-pos) (cadr dir))))

(defun render (str line-len)
  (loop :for i :from 0 :by line-len :while (< i (length str)) :do
     (print (subseq str i (+ i line-len)))))

(defun inner-p (pos)
  "Indicates wether the POS is any position
not on the 'outer' ring."
  (destructuring-bind (x y) pos
    (and (< 0 x (1- (car *dim*)))
         (< 0 y (1- (cadr *dim*))))))

(defun nw-p (pos)
  "Indicates wether POS is the upper left corner."
  (destructuring-bind (x y) pos
    (and (zerop x) (zerop y))))

(defun ne-p (pos)
  "Indicates wether POS is the upper right corner."
  (destructuring-bind (x y) pos
    (and (zerop x)
         (= y (1- (cadr *dim*))))))

(defun se-p (pos)
  "Indicates wether POS is the lower right corner."
  (destructuring-bind (x y) pos
    (and (= x (1- (car *dim*)))
         (= y (1- (cadr *dim*))))))

(defun sw-p (pos)
  "Indicates wether POS is the lower left corner."
  (destructuring-bind (x y) pos
    (and (= x (1- (car *dim*)))
         (zerop y))))

(defun north-p (pos)
  "Indicates wether POS is any position in the
upper horizontal row."
  (destructuring-bind (x y) pos
    (declare (ignore y))
    (zerop x)))

(defun east-p (pos)
  "Indicates wether POS is any position in the
right vertical row."
  (destructuring-bind (x y) pos
    (declare (ignore x))
    (= y (1- (cadr *dim*)))))

(defun south-p (pos)
  "Indicates wether POS is any position in the
right vertical row."
  (destructuring-bind (x y) pos
    (declare (ignore y))
    (= x (1- (car *dim*)))))

(defun west-p (pos)
  "Indicates wether POS is any position in the
left vertical row."
  (destructuring-bind (x y) pos
    (declare (ignore x))
    (zerop y)))

(defun p2i (pos)
  "Converts a position POS into an index for an array."
  (destructuring-bind (x y) pos
    (+ y (* x (cadr *dim*)))))

(defun i2p (idx)
  "Converts an array index IDX to a position in the 2d
representation of the array."
  (multiple-value-bind (x y) (floor idx (cadr *dim*))
    (list x y)))

(defun char-at (pos)
  "Retrieves the character that is located at position POS."
  (let ((idx (p2i pos)))
    (aref *str* idx)))

(defun next-char-in-dir (curr-pos dir)
  (char-at (go-in-dir curr-pos dir)))

(defun finished-p (dir curr-pos)
  "Indicates whether the trail is finished, i.e. if there is no
 successor after the current position CURR-POS."
  (or (and (inner-p curr-pos)
           (not (equal (char-at curr-pos) +junction+))
           (empty-p (next-char-in-dir curr-pos dir)))
      (and (or (and (north-p curr-pos) (equal dir +north+))
               (and (east-p curr-pos) (equal dir +east+))
               (and (south-p curr-pos) (equal dir +south+))
               (and (west-p curr-pos) (equal dir +west+)))
           (not (equal (char-at curr-pos) +junction+)))))

(defun find-dir (dir curr-pos)
  "Finds the next direction at the `+` junction.
DIR is the direction we came from, CURR-POS is the position of the `+`.
It assumes that the direction will change after a `+`, i.e. if we encountered
the `+` by going north, then the next direction will not be north or south."
  (if (inner-p curr-pos)
      (cond ((or (equal dir +north+) (equal dir +south+))
             (let* ((west-pos (go-in-dir curr-pos +west+))
                    (west-char (char-at west-pos)))
               (if (or (empty-p west-char)
                       (equal +vertical-segment+ west-char))
                   +east+
                   +west+)))
            ((or (equal dir +west+) (equal dir +east+))
             (let* ((north-pos (go-in-dir curr-pos +north+))
                    (north-char (char-at north-pos)))
               (if (or (empty-p north-char)
                       (equal north-char +horizontal-segment+))
                   +south+
                   +north+)))
            (t (error "Case impossible")))
      ;; corners and outer segments
      ;; first, corners
      (cond ((nw-p curr-pos)
             (if (equal dir +north+)
                 +east+
                 +south+))
            ((ne-p curr-pos)
             (if (equal dir +east+)
                 +south+
                 +west+))
            ((se-p curr-pos)
             (if (equal dir +south+)
                 +west+
                 +north+))
            ((sw-p curr-pos)
             (if (equal dir +west+)
                 +north+
                 +east+))
            ;; the above checks do not explicitly test if the character is
            ;; empty or not.
            ;; ====================
            ;; outer segments without corners (they already have been dealt with)

            ;; we come either from the east or from the west. Assuming the
            ;; direction has to change, we can only go south.
            ;; The same reasoning applies (mutatis mutandis) to the other cases.
            ((north-p curr-pos)
             (if (equal dir +north+) ;; should probably put into a macro
                 (let ((ncid (next-char-in-dir curr-pos +east+)))
                   (if (or (empty-p ncid)
                           (equal ncid +vertical-segment+))
                       +west+
                       +east+))
                 +south+))
            ((east-p curr-pos)
             (if (equal dir +east+)
                 (let ((ncid (next-char-in-dir curr-pos +north+)))
                   (if (or (empty-p ncid)
                           (equal ncid +horizontal-segment+))
                       +south+
                       +north+))
                 +west+))
            ((south-p curr-pos)
             (if (equal dir +south+)
                 (let ((ncid (next-char-in-dir curr-pos +east+)))
                   (if (or (empty-p ncid)
                           (equal ncid +vertical-segment+))
                       +west+
                       +east+))
                 +north+))
            ((west-p curr-pos)
             (if (equal dir +west+)
                 (let ((ncid (next-char-in-dir curr-pos +north+)))
                   (if (or (empty-p ncid)
                           (equal ncid +horizontal-segment+))
                       +south+
                       +north+))
                 +east+))
            (t (error "Case impossible"))))))

(defun travel (start dir)
  (let ((curr-pos start)
        (curr-dir dir)
        (letters nil)
        (steps 0))
    (loop :named path :do
       (let ((curr-char (char-at curr-pos)))
         (cond ((alpha-char-p curr-char)
                (setf letters (cons curr-char letters)))
               ((char= curr-char +junction+)
                (setf curr-dir (find-dir curr-dir curr-pos)))))
       (incf steps)
       (when (finished-p curr-dir curr-pos)
         (return-from path))
       (setf curr-pos (go-in-dir curr-pos curr-dir)))
    (values (coerce (reverse letters) 'string) steps)))

(defun day-19-a ()
  (multiple-value-call #'process-input (read-input "./input"))
  (travel *start* +south+))

(defun day-19-b ()
  (multiple-value-call #'process-input (read-input "./input"))
  (multiple-value-bind (str steps) (travel *start* +south+)
    (declare (ignore str))
    steps))
