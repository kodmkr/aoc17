(ql:quickload "cl-ppcre")

(defpackage :day18
  (:use :cl)
  (:export :day-18-a
           :day-18-b))

(in-package :day18)

(defparameter +ctr+ "ctr")
(defparameter +snd+ "snd")
(defparameter +rcv+ "rcv")

(defun reg-p (maybe-reg)
  "Registers are alphabetic characters."
  (if (characterp maybe-reg)
      (alpha-char-p maybe-reg)
      (when (stringp maybe-reg)
        (alpha-char-p (char maybe-reg 0)))))

(defun read-input (path-str)
  (with-open-file (st path-str)
    (let* ((insns (loop :for line = (read-line st nil)
                     :while line
                     :collect (cl-ppcre:split "\\s" line)))
           (l (length insns)))
      (make-array l :initial-contents insns))))

(defun mk-env (insn-list)
  "Make environment (HASH) with registers occurring in the program"
  (let ((env (make-hash-table :test #'equal)))
    (loop :for reg
       :in (remove-duplicates
            (loop :for (op reg . rest)
               :across insn-list
               :if (reg-p reg)
               :collect reg)
            :test #'equal)
       :do (setf (gethash reg env) 0))
    (setf (gethash +ctr+ env) 0)
    (setf (gethash +rcv+ env) nil)
    (setf (gethash +snd+ env) nil)
    env))

(defun env-val (key env)
  (gethash key env))

(defun (setf env-val) (val key env)
  (setf (gethash key env) val))

(defun value-of (reg-or-immediate env)
  (let ((maybe-val (parse-integer reg-or-immediate :junk-allowed t)))
    (if maybe-val
        maybe-val
        (env-val reg-or-immediate env))))

(defun op-set (env args)
  (destructuring-bind (reg maybe-reg) args
    (setf (env-val reg env) (value-of maybe-reg env))
    (incf (env-val +ctr+ env))
    (values)))

(defun op-add (env args)
  (destructuring-bind (reg maybe-reg) args
    (let ((reg-val (env-val reg env))
          (val (value-of maybe-reg env)))
      (setf (env-val reg env) (+ reg-val val)))
    (incf (env-val +ctr+ env))
    (values)))

(defun op-mul (env args)
  (destructuring-bind (reg maybe-reg) args
    (let ((reg-val (env-val reg env))
          (val (value-of maybe-reg env)))
      (setf (env-val reg env) (* reg-val val)))
    (incf (env-val +ctr+ env))
    (values)))

(defun op-mod (env args)
  (destructuring-bind (reg maybe-reg) args
    (let ((reg-val (env-val reg env))
          (val (value-of maybe-reg env)))
      (setf (env-val reg env) (mod reg-val val)))
    (incf (env-val +ctr+ env))
    (values)))

(defun op-snd (env args)
  (destructuring-bind (maybe-reg) args
    (push (value-of maybe-reg env) (env-val +snd+ env))
    (incf (env-val +ctr+ env))
    (values)))

(defun op-rcv (env args)
  (destructuring-bind (maybe-reg) args
    (let ((val (value-of maybe-reg env)))
      (when (not (zerop val))
        (push (car (env-val +snd+ env)) (env-val +rcv+ env))))
    (incf (env-val +ctr+ env))
    (values)))

(defun op-jgz (env args)
  (destructuring-bind (maybe-reg1 maybe-reg2) args
    (let ((jmp-cond (value-of maybe-reg1 env))
          (offset (value-of maybe-reg2 env)))
      (incf (env-val +ctr+ env) (if (> jmp-cond 0)
                                    offset
                                    1)))
    (values)))

(defun mk-funcs (funcslist)
  (let ((funcs (make-hash-table :test #'equal)))
    (loop for f in funcslist do
         (let* ((func-name (symbol-name f))
                (suff (string-downcase (subseq func-name (1+ (position #\- func-name))))))
           (setf (gethash suff funcs) (symbol-function f))))
    funcs))

(defun run1 ()
  (let* ((prg (read-input "./input"))
         (env (mk-env prg))
         (ops (mk-funcs '(op-add op-jgz op-mod op-mul op-rcv op-set op-snd))))
    (loop while (not (env-val +rcv+ env)) do
         (let ((instr (aref prg (env-val +ctr+ env))))
           (funcall (gethash (car instr) ops) env (cdr instr))))
    (env-val +rcv+ env)))

(defun day-18-a ()
  (car (run1)))

(defun day-18-b ())
