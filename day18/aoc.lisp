(ql:quickload "cl-ppcre")

(defpackage :day18
  (:use :cl)
  (:export :day-18-a
           :day-18-b))

(in-package :day18)

(defparameter +ctr+ "ctr")
(defparameter +snd+ "snd")
(defparameter +rcv+ "rcv")
(defparameter +stat+ "stat")
(defparameter +id+ "id")

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

(defun mk-env (insn-list &optional id)
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
    (setf (gethash +ctr+ env) 0
          (gethash +rcv+ env) nil
          (gethash +snd+ env) 0)
    (when id
      (setf (gethash "p" env) id
            (gethash +id+ env) id
            (gethash +stat+ env) t))
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

(defun op-jgz (env args)
  (destructuring-bind (maybe-reg1 maybe-reg2) args
    (let ((jmp-cond (value-of maybe-reg1 env))
          (offset (value-of maybe-reg2 env)))
      (incf (env-val +ctr+ env) (if (> jmp-cond 0)
                                    offset
                                    1)))
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

(defun mk-funcs (funcslist &optional &key snd rcv)
  (let ((funcs (make-hash-table :test #'equal)))
    (loop for f in funcslist do
         (let* ((func-name (symbol-name f))
                (suff (string-downcase (subseq func-name (1+ (position #\- func-name))))))
           (setf (gethash suff funcs) (symbol-function f))))
    (when (and snd rcv)
      (setf (gethash +snd+ funcs) snd
            (gethash +rcv+ funcs) rcv))
    funcs))

(defun run1 ()
  (let* ((prg (read-input "./input"))
         (env (mk-env prg))
         (ops (mk-funcs '(op-add op-jgz op-mod op-mul op-set op-rcv op-snd))))
    (loop while (not (env-val +rcv+ env)) do
         (let ((instr (aref prg (env-val +ctr+ env))))
           (funcall (gethash (car instr) ops) env (cdr instr))))
    (env-val +rcv+ env)))

(defun send (env args oth-env)
  (destructuring-bind (reg-or-immediate) args
    (let ((val (value-of reg-or-immediate env)))
      (setf (env-val +rcv+ oth-env) (nconc (env-val +rcv+ oth-env) (list val)))
      (incf (env-val +ctr+ env))
      (when (= 1 (env-val +id+ env))
        (incf (env-val +snd+ env)))))
  (values))

(defun receive (env args)
  (destructuring-bind (reg) args
    (let ((q (env-val +rcv+ env)))
      (if (null q)
          (setf (env-val +stat+ env) nil)
          (let ((val (pop (env-val +rcv+ env))))
            (setf (env-val reg env) val
                  (env-val +stat+ env) t)
            (incf (env-val +ctr+ env))))))
  (values))

(defun run-both ()
  (let* ((prg (read-input "./input"))
         (env0 (mk-env prg 0))
         (env1 (mk-env prg 1))
         (ops (mk-funcs '(op-add op-jgz op-mod op-mul op-set) :snd #'send :rcv #'receive)))
    (loop while (or (env-val +stat+ env0) (env-val +stat+ env1)) do
         (let ((instr0 (aref prg (env-val +ctr+ env0)))
               (instr1 (aref prg (env-val +ctr+ env1))))

           (if (string= (car instr0) +snd+)
               (funcall (gethash (car instr0) ops) env0 (cdr instr0) env1)
               (funcall (gethash (car instr0) ops) env0 (cdr instr0)))
           (if (string= (car instr1) +snd+)
               (funcall (gethash (car instr1) ops) env1 (cdr instr1) env0)
               (funcall (gethash (car instr1) ops) env1 (cdr instr1)))))
    (env-val +snd+ env1)))


(defun day-18-a ()
  (car (run1)))

(defun day-18-b ()
  (run-both))
