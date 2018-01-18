(ql:quickload "split-sequence")

(defpackage :day18
  (:use :cl)
  (:export :day-18-a
           :day-18-b))

(in-package :day18)

(defparameter *registers* (make-hash-table :test #'equal))
(defvar *last-frequency* nil)
(defvar *recovered* nil)
(defvar *instructions* nil)
(defvar *next* nil)

(defconstant +snd+ "snd")
(defconstant +set+ "set")
(defconstant +add+ "add")
(defconstant +mul+ "mul")
(defconstant +mod+ "mod")
(defconstant +rcv+ "rcv")
(defconstant +jgz+ "jgz")

(defun read-input (path-str)
  (with-open-file (in path-str)
    (let ((temp-instr (loop for line = (read-line in nil) while line
                         collect (split-sequence:split-sequence #\Space line))))
      (setf *instructions*
            (make-array (length temp-instr)
                        :adjustable t
                        :initial-contents temp-instr)))))

(defun init-registers (input)
  "Reads the instructions from the input file and adds
registers that it finds to the *REGISTERS* variable."
  (loop for instr across input do
       (when (or (string= (car instr) +add+)
                 (string= (car instr) +set+)
                 (string= (car instr) +mul+)
                 (string= (car instr) +mod+))
         (setf (gethash (cadr instr) *registers*) 0))))

(defun register-p (unk)
  "Helper function to discern whether an argument in the instructions
is an immediate value or a register."
  (let ((chr (char unk 0)))
    (alpha-char-p chr)))

(defun reg-or-imm (unk)
  (if (register-p unk)
      (gethash unk *registers*)
      (parse-integer unk)))

(defun reg-set (reg val)
  (setf (gethash reg *registers*)
        (reg-or-imm val))
  (incf *next*))

(defun reg-mul (reg val)
  (let ((prev-val (gethash reg *registers*)))
    (setf (gethash reg *registers*)
          (* prev-val (reg-or-imm val)))
    (incf *next*)))

(defun reg-add (reg val)
  (let ((prev-val (gethash reg *registers*)))
    (setf (gethash reg *registers*)
          (+ prev-val (reg-or-imm val)))
    (incf *next*)))

(defun reg-mod (reg val)
  (let ((prev-val (gethash reg *registers*)))
    (setf (gethash reg *registers*)
          (mod prev-val (reg-or-imm val)))
    (incf *next*)))

(defun reg-snd (val)
  (setf *last-frequency*
        (reg-or-imm val))
  (incf *next*))

(defun reg-rcv (val)
  (when (> (reg-or-imm val) 0)
    (push *last-frequency* *recovered*))
  (incf *next*))

(defun reg-jgz (val offset)
  (cond ((and (register-p val)
              (register-p offset))
         (if (> (gethash val *registers*) 0)
             (setf *next* (+ *next* (gethash offset *registers*)))
             (incf *next*)))
        ((and (register-p val)
              (not (register-p offset)))
         (if (> (gethash val *registers*) 0)
             (setf *next* (+ *next* (parse-integer offset)))
             (incf *next*)))
        (t (if (> (parse-integer val) 0)
               (setf *next* (+ *next* (parse-integer offset)))
               (incf *next*)))))

(defun dispatch (instr)
  (cond ((string= (car instr) +snd+)
         (reg-snd (cadr instr)))
        ((string= (car instr) +set+)
         (reg-set (cadr instr) (caddr instr)))
        ((string= (car instr) +add+)
         (reg-add (cadr instr) (caddr instr)))
        ((string= (car instr) +mul+)
         (reg-mul (cadr instr) (caddr instr)))
        ((string= (car instr) +mod+)
         (reg-mod (cadr instr) (caddr instr)))
        ((string= (car instr) +rcv+)
         (reg-rcv (cadr instr)))
        ((string= (car instr) +jgz+)
         (reg-jgz (cadr instr) (caddr instr)))))

(defun run (&optional (in "./input"))
  (clrhash *registers*)
  (setf *recovered* nil)
  (read-input in)
  (init-registers *instructions*)
  (setf *next* 0)
  (loop while (not *recovered*) do
       (let ((instr (aref *instructions* *next*)))
         ;; (format t
         ;;         "[instr:~a][idx:~d][regs:~a][freq:~a]~%"
         ;;         instr
         ;;         *next*
         ;;         (loop for x being the hash-keys of *registers*
         ;;            for y being the hash-values of *registers*
         ;;            collect (format nil "~a=>~a" x y))
         ;;         *last-frequency*)
         (dispatch instr))))


(defun day-18-a ()
  (run)
  (car *recovered*))

(defun day-18-b ())
