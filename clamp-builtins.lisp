(defpackage "CLAMP.__builtins__"
  (:use :cl)
  (:shadow :print :min :max :sum :abs)
  (:export :test :dir :plus :times :print :len :bool :repr :iter :next :reversed :min :max :sum :abs :all :any :enumerate :zip :assign))

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

(defvar plus
  (lambda (&rest xs)
    (apply #'+ xs)))

(defvar times
  (lambda (&rest xs)
    (apply #'* xs)))

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

(defvar repr
  (lambda (obj)
    (with-output-to-string (stream)
      (|CLAMP.__CLAMP_INTERNALS__|:PY-REPR obj stream))))

(defvar iter
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ITER obj)))

(defvar next
  (lambda (iterator)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-NEXT iterator)))

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

(defvar abs
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ABS obj)))

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
