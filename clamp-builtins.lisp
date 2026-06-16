(defpackage "CLAMP.__builtins__"
  (:use :cl)
  (:shadow :print :min :max :sum :sorted :abs :round :filter :map :hash :list :tuple :slice :chr :hex :type :str)
  (:export :test :dir :plus :times :__import__ :print :len :bool :callable :isinstance :repr :str :type :id :iter :next :reversed :min :max :sum :sorted :list :tuple :abs :round :hash :pow :divmod :all :any :enumerate :zip :filter :map :range :slice :bin :oct :hex :chr :ord :assign :notimplemented))

(in-package "CLAMP.__builtins__")

(defmacro assign (binding &body body)
  "Assign semantics for translated Python:
  - (assign (var value) body...) => lexical binding like LET
  - (assign (:global var value) body...) => top-level/global assignment via SETQ, then body
  This macro lets the compiler uniformly express assignment while preserving
  Python's module-level global assignment and function-local lexical bindings."
  (cond
    ;; Lexical binding
    ((and (consp binding)
          (symbolp (first binding))
          (not (keywordp (first binding))))
     (destructuring-bind (var val) binding
       `(let ((,var ,val))
          ,@body)))
    ;; Explicit global assignment form
    ((and (consp binding)
          (eq (first binding) :global))
     (destructuring-bind (_ var val) binding
       `(progn
          (setq ,var ,val)
          ,@body)))
    (t
     (error "Invalid ASSIGN syntax: ~S" binding))))

(defvar NotImplemented
  |CLAMP.__CLAMP_INTERNALS__|:*PY-NOT-IMPLEMENTED*)

(defvar plus
  (lambda (&rest xs)
    (apply #'+ xs)))

(defvar times
  (lambda (&rest xs)
    (apply #'* xs)))

(defvar __import__
  (lambda (name &optional
                (globals |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
                (locals |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
                (fromlist |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
                (level 0))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-BUILTIN
     name globals locals fromlist level)))

(defvar test
  (lambda ()
    (write-line "__builtins__.test() invoked")
    42))

(defvar dir
  (lambda (&optional (package-object-or-name *package*))
    (write-line (package-name package-object-or-name))
    (do-external-symbols (sym package-object-or-name)
      (write-line (symbol-name sym)))))

(defvar print
  (lambda (&rest xs)
    (loop for x in xs
          for first = t then nil
          do (progn
               (unless first
                 (princ #\Space))
               (|CLAMP.__CLAMP_INTERNALS__|:PY-DISPLAY x)))
    (terpri)))

(defvar len
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-LEN obj)))

(defvar bool
  (lambda (&optional (obj |CLAMP.__CLAMP_INTERNALS__|:*PY-FALSE*))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-BOOL
     (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P obj))))

(defvar callable
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-CALLABLE obj)))

(defvar isinstance
  (lambda (obj class-or-tuple)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ISINSTANCE obj class-or-tuple)))

(defvar repr
  (lambda (obj)
    (with-output-to-string (stream)
      (|CLAMP.__CLAMP_INTERNALS__|:PY-REPR obj stream))))

(defvar str
  (lambda (&optional (obj ""))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-STR obj)))

(defvar type
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-OF obj)))

(defvar id
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ID obj)))

(defvar iter
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ITER obj)))

(defvar next
  (lambda (iterator &optional
                    (default |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE* default-supplied-p))
    (if default-supplied-p
        (common-lisp:multiple-value-bind (item found)
            (|CLAMP.__CLAMP_INTERNALS__|:PY-NEXT-ITEM iterator)
          (if found item default))
        (|CLAMP.__CLAMP_INTERNALS__|:PY-NEXT iterator))))

(defvar reversed
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-REVERSED obj)))

(defvar min
  (lambda (&rest xs)
    (apply #'|CLAMP.__CLAMP_INTERNALS__|:PY-MIN xs)))

(defvar max
  (lambda (&rest xs)
    (apply #'|CLAMP.__CLAMP_INTERNALS__|:PY-MAX xs)))

(defvar sum
  (lambda (iterable &optional (start 0))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-SUM iterable start)))

(defvar sorted
  (lambda (iterable)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-SORTED iterable)))

(defvar list
  (lambda (&optional (iterable |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-LIST iterable)))

(defvar tuple
  (lambda (&optional (iterable |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-TUPLE iterable)))

(defvar abs
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ABS obj)))

(defvar round
  (lambda (obj &optional (ndigits |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ROUND obj ndigits)))

(defvar hash
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-HASH obj)))

(defvar pow
  (lambda (base exp &optional (mod |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-POW base exp mod)))

(defvar divmod
  (lambda (left right)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-DIVMOD left right)))

(defvar all
  (lambda (iterable)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ALL iterable)))

(defvar any
  (lambda (iterable)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ANY iterable)))

(defvar enumerate
  (lambda (iterable &optional (start 0))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ENUMERATE iterable start)))

(defvar zip
  (lambda (&rest iterables)
    (apply #'|CLAMP.__CLAMP_INTERNALS__|:PY-ZIP iterables)))

(defvar filter
  (lambda (predicate iterable)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-FILTER predicate iterable)))

(defvar map
  (lambda (function &rest iterables)
    (apply #'|CLAMP.__CLAMP_INTERNALS__|:PY-MAP function iterables)))

(defvar range
  (lambda (&rest args)
    (apply #'|CLAMP.__CLAMP_INTERNALS__|:PY-RANGE args)))

(defvar bin
  (lambda (value)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-BIN value)))

(defvar oct
  (lambda (value)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-OCT value)))

(defvar hex
  (lambda (value)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-HEX value)))

(defvar chr
  (lambda (value)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-CHR value)))

(defvar ord
  (lambda (value)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ORD value)))

(defvar slice
  (lambda (&rest args)
    (case (length args)
      (1 (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-SLICE
          |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*
          (first args)
          |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
      (2 (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-SLICE
          (first args)
          (second args)
          |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
      (3 (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-SLICE
          (first args)
          (second args)
          (third args)))
      (otherwise
       (error "slice expected at least 1 argument and at most 3 arguments, got ~A"
              (length args))))))
