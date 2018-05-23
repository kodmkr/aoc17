(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre"))

(defpackage :day07
  (:use :cl)
  (:import-from :cl-ppcre :scan-to-strings :split)
  (:export :day-07-a
           :day-07-b))

(in-package :day07)

(defun read-input (path-str)
  (with-open-file (in path-str)
    (loop for line = (read-line in nil) while line collect line)))

(defun parse-name-and-weight (name-and-weight)
  (multiple-value-bind (match cgrps)
      (scan-to-strings "([^ ]+) *\\((\\d+)\\)" name-and-weight)
    (declare (ignore match))
    (if (>= (array-dimension cgrps 0) 2)
        (list (aref cgrps 0) (parse-integer (aref cgrps 1)))
        nil)))

(defun parse-children (children)
  (let ((spl (split "," children)))
    (sort (loop for x in spl collect (string-trim '(#\Space) x)) #'string<)))

(defun parse-line (line)
  (destructuring-bind (n+w . cs) (split "->" line)
    (list (parse-name-and-weight n+w) (when cs
                                        (parse-children (car cs))))))

(defun parse-lines (lines)
  (loop for l in lines collect (parse-line l)))

(defun find-root-name (tree-data-internals) ; internal nodes
  (let ((names (loop for x in tree-data-internals collect (name x)))
        (children (loop for x in tree-data-internals append (children x))))
    (car (intersection ;; there should be only one root
          (set-exclusive-or
           names
           children
           :test #'string=)
          names
          :test #'string=))))

(defun parse-tree-data (s)
  (let* ((in-l (read-input s)))
    (parse-lines in-l)))

(defun tree-data-internals (tree-data)
  (remove-if #'(lambda (x) (leaf-p x)) tree-data))

(defun leaves (tree-data)
  (remove-if-not #'(lambda (x) (leaf-p x)) tree-data))

(defun tree-data-find-by-name (name tree-data)
  (find-if #'(lambda (x) (string= name (name x))) tree-data))

(defun without (name tree-data)
  (remove-if #'(lambda (x) (string= name (name x))) tree-data))

(defun build-tree (tree-data)
  (labels ((build-tree-aux (elt tree-data)
             (let ((elt-cpy (copy-seq elt)))
               (if (null (children elt))
                   elt-cpy
                   (when (consp tree-data)
                     ;; exchange the list of children (string)
                     ;; with list of children (structures)
                     (rplacd elt-cpy
                             (list (loop for c in (children elt-cpy)
                                      collect (build-tree-aux
                                               (tree-data-find-by-name c tree-data)
                                               (without c tree-data))))))))))
    (let* ((root-name (find-root-name (tree-data-internals tree-data)))
           (r (tree-data-find-by-name root-name tree-data))
           (rest (without root-name tree-data)))
      (build-tree-aux r rest))))

(defun name (tree)
  (caar tree))

(defun weight (tree)
  (cadar tree))

(defun children (tree)
  (cadr tree))

(defun leaf-p (tree)
  (null (children tree)))

(defun calc-weight (tree)
  (if (null (children tree))
      (weight tree)
      (apply #'+ (weight tree) (loop for c in (children tree)
                                  collect (calc-weight c)))))

(defun disk-balanced-p (tree)
  (let ((c-ws (loop for c in (children tree) collect (calc-weight c))))
    (if (consp c-ws)
        (apply #'= c-ws)
        t)))

(defun find-by-name (name tree &optional (still-to-search (children tree)))
  (if (string= name (name tree))
      tree
      (progn
        (let ((next (first still-to-search))
              (rst (rest still-to-search)))
          (when next
            (find-by-name name next (append rst (children next))))))))

(defun find-imbalance (tree &optional (to-check (children tree)))
  (if (and (every #'(lambda (x) (not (null x))) (loop for c in to-check
                                              collect (disk-balanced-p c)))
           (not (disk-balanced-p tree)))
      tree
      (progn
        (let ((nxt (first to-check))
              (rst (rest to-check)))
          (when nxt
            (find-imbalance nxt (append rst (children nxt))))))))

(defun find-imbalance-cause (tree)
  (let* ((cdn (loop for c in (children tree)
                 collect (list (calc-weight c) ; full weight
                               (name c)        ; name of the sub-tree
                               (weight c))))   ; weight of the node itself
         (srtd (sort cdn #'< :key #'first)))
    (if (= (first (first srtd)) (first (second srtd)))
        ;; due to sorting the outlier can only be at the end
        (let* ((cause (first (last srtd)))
               (diff (- (first cause) (first (first srtd)))))
          (values (- (third cause) diff) (second cause) :down diff))
        ;; due to sorting the outlier must be the first element
        (let* ((cause (first srtd))
               (diff (- (first (second srtd)) (first cause))))
          (values (+ (third cause) diff) (second cause) :up diff)))))



(defun day-07-a ()
  (let* ((tree-data (parse-tree-data "./input.txt"))
        (internals (tree-data-internals tree-data)))
    (find-root-name internals)))

(defun day-07-b ()
  (let* ((tree-data (parse-tree-data "./input.txt"))
         (tree (build-tree tree-data))
         (imb (find-imbalance tree)))
    (find-imbalance-cause imb)))
