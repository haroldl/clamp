(defpackage "CLAMP.__builtins__"
  (:use :cl)
  (:shadow :print :getattr :setattr :delattr :classmethod :staticmethod :property :super :open :compile :exec :issubclass :format :float :complex :min :max :sum :sorted :abs :round :filter :map :hash :list :tuple :dict :set :slice :chr :hex :type :str :warning :userwarning :deprecationwarning :runtimewarning :memoryerror)
  (:export :test :dir :plus :times :__import__ :print :len :bool :globals :getattr :setattr :delattr :hasattr :classmethod :staticmethod :property :super :int :float :complex :bytes :object :open :compile :exec :callable :isinstance :issubclass :format :vars :repr :ascii :str :type :id :iter :next :aiter :anext :reversed :min :max :sum :sorted :list :tuple :dict :set :frozenset :memoryview :bytearray :abs :round :hash :pow :divmod :all :any :enumerate :zip :filter :map :range :slice :bin :oct :hex :chr :ord :assign :notimplemented :ellipsis :baseexception :exception :warning :userwarning :deprecationwarning :runtimewarning :memoryerror :runtimeerror :recursionerror :assertionerror :typeerror :valueerror :lookuperror :keyerror :indexerror :importerror :modulenotfounderror :attributeerror :nameerror :oserror :filenotfounderror :timeouterror :stopiteration :stopasynciteration :|BaseException| :|Exception| :|Warning| :|UserWarning| :|DeprecationWarning| :|RuntimeWarning| :|MemoryError| :|RuntimeError| :|RecursionError| :|AssertionError| :|TypeError| :|ValueError| :|LookupError| :|KeyError| :|IndexError| :|ImportError| :|ModuleNotFoundError| :|AttributeError| :|NameError| :|OSError| :|FileNotFoundError| :|TimeoutError| :|StopIteration| :|StopAsyncIteration|))

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

(defvar Ellipsis
  |CLAMP.__CLAMP_INTERNALS__|:*PY-ELLIPSIS*)

(defvar memoryview nil)

(defvar bytearray nil)


(defvar BaseException
  |CLAMP.__CLAMP_INTERNALS__|:*PY-BASE-EXCEPTION-TYPE*)

(defvar Exception
  |CLAMP.__CLAMP_INTERNALS__|:*PY-EXCEPTION-TYPE*)

(defvar Warning
  |CLAMP.__CLAMP_INTERNALS__|:*PY-WARNING-TYPE*)

(defvar UserWarning
  |CLAMP.__CLAMP_INTERNALS__|:*PY-USER-WARNING-TYPE*)

(defvar DeprecationWarning
  |CLAMP.__CLAMP_INTERNALS__|:*PY-DEPRECATION-WARNING-TYPE*)

(defvar RuntimeWarning
  |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-WARNING-TYPE*)

(defvar MemoryError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-MEMORY-ERROR-TYPE*)

(defvar StopIteration
  |CLAMP.__CLAMP_INTERNALS__|:*PY-STOP-ITERATION-TYPE*)

(defvar RuntimeError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*)

(defvar RecursionError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-RECURSION-ERROR-TYPE*)

(defvar AssertionError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-ASSERTION-ERROR-TYPE*)

(defvar TypeError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-ERROR-TYPE*)

(defvar ValueError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-VALUE-ERROR-TYPE*)

(defvar LookupError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-LOOKUP-ERROR-TYPE*)

(defvar KeyError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-KEY-ERROR-TYPE*)

(defvar IndexError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-INDEX-ERROR-TYPE*)

(defvar ImportError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-IMPORT-ERROR-TYPE*)

(defvar ModuleNotFoundError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-MODULE-NOT-FOUND-ERROR-TYPE*)

(defvar AttributeError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-ATTRIBUTE-ERROR-TYPE*)

(defvar NameError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-NAME-ERROR-TYPE*)

(defvar OSError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-OS-ERROR-TYPE*)

(defvar FileNotFoundError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-FILE-NOT-FOUND-ERROR-TYPE*)

(defvar TimeoutError
  |CLAMP.__CLAMP_INTERNALS__|:*PY-TIMEOUT-ERROR-TYPE*)

(defvar StopAsyncIteration
  |CLAMP.__CLAMP_INTERNALS__|:*PY-STOP-ASYNC-ITERATION-TYPE*)

(defvar |BaseException| BaseException)
(defvar |Exception| Exception)
(defvar |Warning| Warning)
(defvar |UserWarning| UserWarning)
(defvar |DeprecationWarning| DeprecationWarning)
(defvar |RuntimeWarning| RuntimeWarning)
(defvar |MemoryError| MemoryError)
(defvar |RuntimeError| RuntimeError)
(defvar |RecursionError| RecursionError)
(defvar |AssertionError| AssertionError)
(defvar |TypeError| TypeError)
(defvar |ValueError| ValueError)
(defvar |LookupError| LookupError)
(defvar |KeyError| KeyError)
(defvar |IndexError| IndexError)
(defvar |ImportError| ImportError)
(defvar |ModuleNotFoundError| ModuleNotFoundError)
(defvar |AttributeError| AttributeError)
(defvar |NameError| NameError)
(defvar |OSError| OSError)
(defvar |FileNotFoundError| FileNotFoundError)
(defvar |TimeoutError| TimeoutError)
(defvar |StopIteration| StopIteration)
(defvar |StopAsyncIteration| StopAsyncIteration)

(defvar plus
  (lambda (&rest xs)
    (apply #'+ xs)))

(defvar times
  (lambda (&rest xs)
    (apply #'* xs)))

(defvar __import__
  (lambda (&rest call-args)
    (destructuring-bind (name globals locals fromlist level)
        (|CLAMP.__CLAMP_INTERNALS__|:PY-BIND-ARGS
         "__import__"
         (common-lisp:list "name" "globals" "locals" "fromlist" "level")
         1
         (common-lisp:list |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*
               |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*
               |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*
               0)
         call-args)
      (|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-BUILTIN
       name globals locals fromlist level))))

(defvar test
  (lambda ()
    (write-line "__builtins__.test() invoked")
    42))

(defun py-dir-hash-keys (table)
  (let ((names '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (stringp key)
                 (push key names)))
             table)
    (apply #'|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-LIST (sort names #'string<))))

(defvar dir
  (lambda (&optional (obj |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
    (cond
      ((|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-P obj)
       (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-KEYS-LIST obj))
      ((|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P obj)
       (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-KEYS-LIST
        (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-DICT obj)))
      ((and (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P obj)
            |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-DIR*)
       (funcall |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-DIR* obj))
      ((|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-P obj)
       (py-dir-hash-keys (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-ATTRS obj)))
      ((|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-P obj)
       (py-dir-hash-keys (|CLAMP.__CLAMP_INTERNALS__|:PY-OBJECT-ATTRS obj)))
      (t
       (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-LIST)))))

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
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "bool"
   :FN (lambda (&optional (obj |CLAMP.__CLAMP_INTERNALS__|:*PY-FALSE*))
         (|CLAMP.__CLAMP_INTERNALS__|:PY-BOOL
          (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P obj)))))

(defvar globals
  (lambda ()
    (|CLAMP.__CLAMP_INTERNALS__|:PY-GLOBALS)))

(defvar getattr
  (lambda (obj name &optional (default |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE* default-supplied-p))
    (if default-supplied-p
        (|CLAMP.__CLAMP_INTERNALS__|::PY-LOOKUP-ATTR-OR-DEFAULT obj name default)
        (|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR obj name))))

(defvar setattr
  (lambda (obj name value)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-SETATTR obj name value)))

(defvar delattr
  (lambda (obj name)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-DELATTR obj name)))

(defvar hasattr
  (lambda (obj name)
    (handler-case
        (progn
          (|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR obj name)
          |CLAMP.__CLAMP_INTERNALS__|:*PY-TRUE*)
      (|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION ()
        |CLAMP.__CLAMP_INTERNALS__|:*PY-FALSE*))))

(defvar staticmethod
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "staticmethod"
   :FN (lambda (fn)
         (if (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-P fn)
             (let ((wrapper (|CLAMP.__CLAMP_INTERNALS__|::PY-COPY-CALLABLE fn :binding-kind :static-method)))
               (common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-UNDERLYING wrapper)
                                 (or (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-UNDERLYING fn) fn))
               wrapper)
             fn))))

(defvar classmethod
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "classmethod"
   :FN (lambda (fn)
         (if (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-P fn)
             (let ((wrapper (|CLAMP.__CLAMP_INTERNALS__|::PY-COPY-CALLABLE fn :binding-kind :class-method)))
               (common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-UNDERLYING wrapper)
                                 (or (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-UNDERLYING fn) fn))
               wrapper)
             fn))))

(defvar property
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "property"
   :FN #'|CLAMP.__CLAMP_INTERNALS__|::PY-PROPERTY-CALL))

(defun make-minimal-super-object ()
  (let ((obj (|CLAMP.__CLAMP_INTERNALS__|::MAKE-PY-OBJECT
              :type |CLAMP.__CLAMP_INTERNALS__|:*PY-OBJECT-TYPE*)))
    (common-lisp:setf
     (|CLAMP.__CLAMP_INTERNALS__|:PY-OBJECT-ATTR obj "__new__")
     (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
      :NAME "super.__new__"
      :BINDING-KIND :static-method
      :FN (lambda (&rest args)
            (common-lisp:destructuring-bind (mcls name bases namespace &rest rest-args) args
              (common-lisp:apply #'|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-NEW
                                 mcls name bases namespace rest-args)))))
    (common-lisp:setf
     (|CLAMP.__CLAMP_INTERNALS__|:PY-OBJECT-ATTR obj "register")
     (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
      :NAME "super.register"
      :BINDING-KIND :static-method
      :FN (lambda (subclass) subclass)))
    obj))

(defvar super
  (lambda (&rest args)
    (cond
      ((= (common-lisp:length args) 0)
       (make-minimal-super-object))
      ((= (common-lisp:length args) 2)
       (|CLAMP.__CLAMP_INTERNALS__|::MAKE-PY-SUPER-OBJECT
        :TYPE |CLAMP.__CLAMP_INTERNALS__|:*PY-OBJECT-TYPE*
        :START-TYPE (first args)
        :BOUND-OBJECT (second args)))
      (t
       (common-lisp:error "super() expects 0 or 2 arguments")))))

(defvar int
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "int"
   :FN (lambda (&optional (obj 0))
         (cond
           ((eq obj |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*) 0)
           ((stringp obj) (parse-integer obj))
           ((|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P obj)
            (truncate
             (|CLAMP.__CLAMP_INTERNALS__|::PY-NORMALIZE-BOOL-NUMBER
              (|CLAMP.__CLAMP_INTERNALS__|:PY-CALL-ATTR obj "__int__"))))
           (t (truncate (|CLAMP.__CLAMP_INTERNALS__|::PY-NORMALIZE-BOOL-NUMBER obj)))))))

(defvar float
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "float"
   :FN (lambda (&optional (obj 0.0d0))
         (cond
           ((stringp obj) (common-lisp:coerce (read-from-string obj) 'double-float))
           ((|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P obj)
            (common-lisp:coerce
             (|CLAMP.__CLAMP_INTERNALS__|::PY-NORMALIZE-BOOL-NUMBER
              (|CLAMP.__CLAMP_INTERNALS__|:PY-CALL-ATTR obj "__float__"))
             'double-float))
           (t
            (common-lisp:coerce
             (|CLAMP.__CLAMP_INTERNALS__|::PY-NORMALIZE-BOOL-NUMBER obj)
             'double-float))))))

(defvar complex nil)

(defvar bytes
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "bytes"
   :FN (lambda (&optional (source nil source-supplied-p) (encoding |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*) (errors |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
         (declare (ignore errors))
         (cond
           ((not source-supplied-p)
            (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-BYTES-FROM-VECTOR
             (make-array 0 :element-type '(unsigned-byte 8))))
           ((|CLAMP.__CLAMP_INTERNALS__|::PY-BYTES-OBJECT-P source)
            source)
           ((stringp source)
            (when (eq encoding |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
              (|CLAMP.__CLAMP_INTERNALS__|:PY-RAISE-TYPE
               |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-ERROR-TYPE*
               "string argument without an encoding"))
            (let ((external-format (if (or (eq encoding |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
                                           (and (stringp encoding)
                                                (member (string-downcase encoding)
                                                        '("utf-8" "utf8")
                                                        :test #'string=)))
                                       :utf-8
                                       :utf-8)))
              (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-BYTES-FROM-VECTOR
               (sb-ext:string-to-octets source :external-format external-format))))
           ((|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P source)
            (|CLAMP.__CLAMP_INTERNALS__|:PY-CALL-ATTR source "__bytes__"))
           (t
            (let* ((items (|CLAMP.__CLAMP_INTERNALS__|:PY-ITERABLE-TO-LIST source))
                   (storage (make-array (common-lisp:length items)
                                        :element-type '(unsigned-byte 8))))
              (loop for item in items
                    for index from 0
                    for value = (|CLAMP.__CLAMP_INTERNALS__|::PY-NORMALIZE-BOOL-NUMBER item)
                    do (progn
                         (unless (and (integerp value) (<= 0 value 255))
                           (|CLAMP.__CLAMP_INTERNALS__|:PY-RAISE-TYPE
                            |CLAMP.__CLAMP_INTERNALS__|:*PY-VALUE-ERROR-TYPE*
                            "bytes must be in range(0, 256)"))
                         (common-lisp:setf (aref storage index) value)))
              (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-BYTES-FROM-VECTOR storage)))))))

(defvar object nil)

(defvar open nil)
(defvar compile nil)
(defvar exec nil)

(defvar callable
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-CALLABLE obj)))

(defvar isinstance
  (lambda (obj class-or-tuple)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ISINSTANCE obj class-or-tuple)))

(defvar issubclass
  (lambda (cls class-or-tuple)
    (cond
      ((and |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-ISSUBCLASS*
            (or (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P cls)
                (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P class-or-tuple)))
       (|CLAMP.__CLAMP_INTERNALS__|:PY-BOOL
        (funcall |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-ISSUBCLASS* cls class-or-tuple)))
      (t
       (labels ((one (candidate)
                  (cond
                    ((|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P candidate)
                     (|CLAMP.__CLAMP_INTERNALS__|:PY-BOOL
                      (and (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P cls)
                           (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-SUBTYPE-P cls candidate))))
                    (t |CLAMP.__CLAMP_INTERNALS__|:*PY-FALSE*))))
         (if (|CLAMP.__CLAMP_INTERNALS__|::PY-TUPLE-OBJECT-P class-or-tuple)
             (|CLAMP.__CLAMP_INTERNALS__|:PY-BOOL
              (loop for item in (|CLAMP.__CLAMP_INTERNALS__|:PY-ITERABLE-TO-LIST class-or-tuple)
                    thereis (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P (one item))))
             (one class-or-tuple)))))))

(defvar vars
  (lambda (&optional (obj nil supplied-p))
    (if supplied-p
        (|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR obj "__dict__")
        (|CLAMP.__CLAMP_INTERNALS__|:PY-GLOBALS))))

(defvar repr
  (lambda (obj)
    (with-output-to-string (stream)
      (|CLAMP.__CLAMP_INTERNALS__|:PY-REPR obj stream))))

(defvar ascii
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-ASCII obj)))

(defvar str
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "str"
   :FN (lambda (&optional (obj ""))
         (|CLAMP.__CLAMP_INTERNALS__|:PY-STR obj))))

(defvar format
  (lambda (obj &optional (format-spec ""))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-FORMAT obj format-spec)))

(defvar type
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "type"
   :FN (lambda (obj)
         (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-OF obj))))

(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-OBJECT-ATTR type "__new__")
                  (lambda (&rest args)
                    (let ((actual-args
                            (if (and (= (common-lisp:length args) 5)
                                     (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-P (first args))
                                     (common-lisp:string= (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-NAME (first args)) "type"))
                                (rest args)
                                args)))
                      (common-lisp:destructuring-bind (mcls name bases namespace &rest rest-args) actual-args
                        (common-lisp:apply #'|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-NEW
                                           mcls name bases namespace rest-args)))))

(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-OBJECT-ATTR type "__instancecheck__")
                  (lambda (cls instance)
                    (|CLAMP.__CLAMP_INTERNALS__|:PY-ISINSTANCE instance cls)))

(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-OBJECT-ATTR type "__subclasscheck__")
                  (lambda (cls subclass)
                    (|CLAMP.__CLAMP_INTERNALS__|:PY-BOOL
                     (and (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P subclass)
                          (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-SUBTYPE-P subclass cls)))))

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

(defvar aiter
  (lambda (obj)
    (|CLAMP.__CLAMP_INTERNALS__|:PY-AITER obj)))

(defvar anext
  (lambda (iterator &optional
                    (default |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE* default-supplied-p))
    (if default-supplied-p
        (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-COROUTINE
         "anext"
         (lambda ()
           (common-lisp:multiple-value-bind (item found)
               (|CLAMP.__CLAMP_INTERNALS__|:PY-ANEXT-ITEM iterator)
             (if found item default))))
        (|CLAMP.__CLAMP_INTERNALS__|:PY-CALL-ATTR iterator "__anext__"))))

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
  (lambda (&rest args)
    (apply #'|CLAMP.__CLAMP_INTERNALS__|:PY-SORTED args)))

(defvar list
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "list"
   :FN (lambda (&optional (iterable |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
         (|CLAMP.__CLAMP_INTERNALS__|:PY-LIST iterable))))

(defvar tuple
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "tuple"
   :FN (lambda (&optional (iterable |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
         (|CLAMP.__CLAMP_INTERNALS__|:PY-TUPLE iterable))))

(defvar dict
  (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
   :NAME "dict"
   :FN #'|CLAMP.__CLAMP_INTERNALS__|:PY-DICT))

(defvar set nil)

(defvar frozenset nil)

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
