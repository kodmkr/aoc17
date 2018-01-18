(ql:quickload "split-sequence")

(defpackage :day04
  (:use :cl :split-sequence)
  (:export :day-04-a
           :day-04-b))

(in-package :day04)

(defun passphrase-valid-p (phrase-list)
  (let ((orig-len (length phrase-list))
        (dedup-len (length (remove-duplicates phrase-list :test #'string=))))
    (= orig-len dedup-len)))
  
(defun check-passphrases (in-path check)
  (with-open-file (in in-path)
    (loop for line = (read-line in nil) while line
         if (funcall check (split-sequence #\Space line)) count line)))

(defun anagram-p (s1 s2)
  (let ((sorted-s1 (sort s1 #'char<))
        (sorted-s2 (sort s2 #'char<)))
    (string= sorted-s1 sorted-s2)))

(defun anagram-valid-p (word-list)
  (loop for (head . rest) on word-list do
       (if (some #'(lambda (x) (anagram-p head x)) rest)
           (return nil))
     finally (return t)))
       

(defun day-04-a ()
  (check-passphrases "./input.txt" #'passphrase-valid-p))

(defun day-04-b ()
  (check-passphrases "./input.txt" #'anagram-valid-p))
