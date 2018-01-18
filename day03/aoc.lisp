(defpackage :day03
  (:use :cl)
  (:export :day-03-a
           :day-03-b))

(in-package :day03)

(defparameter *target* 312051)

(defun ring (number)
  (if (= number 0)
      (list 1 1 1 1)
      (let* ((odd (1+ (* 2 number)))
             (odd-sq (expt odd 2)))
        (loop for x in '(0 1 2 3) collect (- odd-sq (* x (1- odd)))))))

(defun find-segment (number ring)
  (destructuring-bind  (se sw nw ne) ring
    (let* ((d (- se sw 1)) ;; for the corner case (pun intended)
           (x (- ne d)))
      (cond ((and (< sw number) (<= number se))
             (list :s sw se))
            ((and (< nw number) (<= number sw))
             (list :w nw sw))
            ((and (< ne number) (<= number nw))
             (list :n ne nw))
            ((and (<= x number) (<= number ne))
             (list :e x se))
            (t nil)))))

(defun in-ring-p (number ring)
  (destructuring-bind  (se sw nw ne) ring
    (if (> number 1)
        (find-segment number ring)
        (and (= se sw nw ne 1)
             (list :n 1 1)))))

(defun distance-from-middle (num segment)
  (destructuring-bind (dir lo hi) segment
    (declare (ignore dir))
    (let* ((d (- hi lo))
           (half-d (floor d 2))
           (mid (+ lo half-d)))
      (abs (- num mid)))))

(defun find-ring-ord (number)
  (loop for i from 0 while (not (in-ring-p number (ring i)))
       finally (return (values (ring i) i))))

(defun moves-to-center (number)
  (multiple-value-bind (rng ord) (find-ring-ord number)
    (let ((d (distance-from-middle number (find-segment number rng))))
      (+ ord d))))

(defun day-03-a ()
  (moves-to-center *target*))

(defun ar-last (ar)
  (aref ar (1- (array-dimension ar 0))))

(defun mk-odd (ord)
  (1+ (* 2 ord)))

(defun sq (x)
  (expt x 2))

;; well, that escalated quickly...
(defun zip-arrays (o-ar n-ar)
  (let ((sd (/ (array-dimension n-ar 0) 4)))

    ;; handles the first segment of `SD`-1 numbers
    (setf (aref n-ar 0) (+ (ar-last o-ar) (aref o-ar 0)))
    (setf (aref n-ar 1) (+ (aref n-ar 0) (ar-last o-ar) (aref o-ar 0) (aref o-ar 1)))
    ;; rest of the elements not counting the last two
    (loop for i from 2 below (- sd 2) do
         (setf (aref n-ar i) (+ (aref n-ar (1- i))
                                (aref o-ar (- i 2))
                                (aref o-ar (- i 1))
                                (aref o-ar i))))
    (let ((bli (- sd 2)))
      (setf (aref n-ar bli) (+ (aref n-ar (1- bli))
                               (aref o-ar (- bli 2))
                               (aref o-ar (1- bli)))))
    (let ((li (1- sd)))
      (setf (aref n-ar li) (+ (aref n-ar (1- li)) 
                               (aref o-ar (- li 2)))))

    ;; handles the second segment
    (let ((msi (* sd 1))) ; middle-segment-index
      (setf (aref n-ar msi) (+ (aref n-ar (- msi 2))
                               (aref n-ar (- msi 1))
                               (aref o-ar (- msi 3))
                               (aref o-ar (- msi 2))))
      ;; rest of the elements in segment not counting the last two
      (loop for i from (+ msi 1) below (- (+ msi sd) 2) do
           (setf (aref n-ar i) (+ (aref n-ar (1- i))
                                  (aref o-ar (- i 4))
                                  (aref o-ar (- i 3))
                                  (aref o-ar (- i 2)))))
      ;; the last two elements in segment
      (let ((bli (- (+ msi sd) 2))) ; but-last-index
        (setf (aref n-ar bli) (+ (aref n-ar (1- bli))
                                 (aref o-ar (- bli 4))
                                 (aref o-ar (- bli 3)))))
      (let ((li (1- (+ msi sd))))
        (setf (aref n-ar li) (+ (aref n-ar (1- li))
                                (aref o-ar (- li 4))))))
    
    ;; handles the third segment
    (let ((msi (* sd 2))) ; middle-segment-index
      (setf (aref n-ar msi) (+ (aref n-ar (- msi 2))
                               (aref n-ar (- msi 1))
                               (aref o-ar (- msi 5))
                               (aref o-ar (- msi 4))))
      ;; rest of the elements in segment not counting the last two
      (loop for i from (+ msi 1) below (- (+ msi sd) 2) do
           (setf (aref n-ar i) (+ (aref n-ar (1- i))
                                  (aref o-ar (- i 6))
                                  (aref o-ar (- i 5))
                                  (aref o-ar (- i 4)))))
      ;; the last two elements in segment
      (let ((bli (- (+ msi sd) 2))) ; but-last-index
        (setf (aref n-ar bli) (+ (aref n-ar (1- bli))
                                 (aref o-ar (- bli 6))
                                 (aref o-ar (- bli 5)))))
      (let ((li (1- (+ msi sd))))
        (setf (aref n-ar li) (+ (aref n-ar (1- li))
                                (aref o-ar (- li 6))))))

    ;; the last segment
    (let ((lss (* sd 3))) ; last-segment-start
      (setf (aref n-ar lss) (+ (aref n-ar (- lss 2))
                               (aref n-ar (- lss 1))
                               (aref o-ar (- lss 7))
                               (aref o-ar (- lss 6))))
      ;; rest of the elements in segment not counting the last two
      (loop for i from (1+ lss) below (- (+ lss sd) 2) do
           (setf (aref n-ar i) (+ (aref n-ar (1- i))
                                  (aref o-ar (- i 8))
                                  (aref o-ar (- i 7))
                                  (aref o-ar (- i 6)))))
      ;; the last two elements in segment
      (let ((bli (- (+ lss sd) 2))) ; but-last-index
        (setf (aref n-ar bli) (+ (aref n-ar (1- bli))
                                 (aref o-ar (- bli 8))
                                 (aref o-ar (- bli 7))
                                 (aref n-ar 0))))
      (let ((li (1- (+ lss sd))))
        (setf (aref n-ar li) (+ (aref n-ar (1- li))
                                (ar-last o-ar )
                                (aref n-ar 0))))
      )
    n-ar))

(defun gen-ring (ord)
  (cond ((zerop ord)
         (make-array 1 :initial-contents '(1)))
        ((= 1 ord)
         (make-array 8 :initial-contents '(1 2 4 5 10 11 23 25)))
        (t (let* ((prev-ring (gen-ring (1- ord)))
                  (ord-th-odd (mk-odd ord))
                  (prev-odd (mk-odd (1- ord)))
                  (new-len (- (sq ord-th-odd) (sq prev-odd)))
                  (new-ring (make-array new-len :initial-element 0)))
             (zip-arrays prev-ring new-ring)))))
             
(defun day-03-b ()
  (loop named outer for x from 3 do ; we already know that we can disregard the first three rings
       (loop for y across (gen-ring x) do
            (when (>= y *target*)
              (return-from outer y)))))
