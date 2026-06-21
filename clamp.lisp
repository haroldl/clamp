#!/usr/bin/sbcl --script

(require :uiop)

;; Define Clamp's internal runtime types and helpers.
(load "clamp-internals.lisp")

;; Define the __builtins__ module.
(load "clamp-builtins.lisp")

;; This Lisp package models the top level nameless Python module.
(defpackage :clamp-impl
  (:use :cl "SB-ALIEN" "UIOP"))

(in-package :clamp-impl)

;; https://docs.python.org/3/c-api/veryhigh.html
(load-shared-object "/usr/lib/python3.12/config-3.12-x86_64-linux-gnu/libpython3.12.so")

;; Read in the Python -> Lisp compiler so that it will be in memory, even in the saved lisp core.
(defconstant *clamp-compiler-source* (uiop:read-file-string "clamp_compiler.py"))
(defparameter *verbose* nil)
(defparameter *compile-only* nil)

;;
;; Map the Python C API into Lisp:
;;

;; The SBCL alien type (* t) is a pointer to anything, and is used in many places here to
;; store a pointer to a PyObject which is an opaque type to us - we just need to store
;; references to PyObject values and pass them back to Python later.

(define-alien-routine ("Py_Initialize" py-initialize) void)
(define-alien-routine ("Py_Finalize" py-finalize) void)
(define-alien-routine ("Py_IncRef" py-inc-ref) void (obj (* t)))
(define-alien-routine ("Py_DecRef" py-dec-ref) void (obj (* t)))
(define-alien-routine ("PyGILState_Ensure" py-gil-state-ensure) int)
(define-alien-routine ("PyGILState_Release" py-gil-state-release) void (state int))

(define-alien-routine ("PyRun_SimpleString" py-run-simple-string) int (str c-string))

(define-alien-routine ("PyDict_New" py-new-dict) (* t))

;; https://docs.python.org/3/c-api/unicode.html
(define-alien-routine ("PyUnicode_FromString" py-unicode-from-string) (* t) (input c-string))
(define-alien-routine ("PyUnicode_FromStringAndSize" py-unicode-from-string-and-size-c) (* t) (input (* unsigned-char)) (size long))

;; The int* is an output parameter for the string length which we don't need;
;; it is safe to just pass in nil.
(define-alien-routine ("PyUnicode_AsUTF8AndSize" py-value-to-utf8) c-string
  (python-value (* t)) (output-size (* int)))

;; Call the str() function on a value to convert it to a Python string value.
(define-alien-routine ("PyObject_Str" py-str) (* t) (python-object (* t)))
(define-alien-routine ("PyObject_Repr" py-repr-c) (* t) (python-object (* t)))
(define-alien-routine ("PyObject_Format" py-object-format-c) (* t) (obj (* t)) (format-spec (* t)))

(defun python-unicode-to-lisp-string (python-value)
  (py-value-to-utf8 python-value nil))

(defun python-to-lisp-string (python-value)
  (python-unicode-to-lisp-string (py-str python-value)))

;; PyObject *PyRun_String(const char *str, int start, PyObject *globals, PyObject *locals)
;; The `start` param should be one of the constants below:
;; (py-single-input, py-file-input, py-eval-input)
(define-alien-routine ("PyRun_String" py-run-string) (* t)
  (str c-string) (start int) (globals (* t)) (locals (* t)))

;; Need to get a reference after initializing Python.
;; For some reason, (define-alien-variable "Py_None" (* t)) does not work.
(defvar *py-none* nil)
(defvar *py-cpython-ellipsis* nil)
(defvar *py-cpython-none-type* nil)
(defvar *py-cpython-bool-type* nil)
(defvar *py-cpython-int-type* nil)
(defvar *py-cpython-float-type* nil)
(defvar *py-cpython-str-type* nil)
(defvar *py-cpython-bytes-type* nil)
(defvar *py-cpython-list-type* nil)
(defvar *py-cpython-tuple-type* nil)
(defvar *py-cpython-dict-type* nil)
(defvar *py-cpython-type-callable* nil)
(defvar *py-cpython-object-setattr* nil)
(defvar *py-cpython-types-prepare-class* nil)
(defvar *py-cpython-abc-abstractmethod* nil)
(defvar *py-cpython-exception-types* nil)
(defvar *cpython-proxy-to-clamp* (make-hash-table :test #'eql))
(defvar *cpython-callable-proxies* (make-hash-table :test #'eql))
(defvar *cpython-callable-proxy-next-id* 0)
(defvar *cpython-callable-method-defs* nil)
(defvar *syncing-cpython-proxy* nil)
(defvar *syncing-cpython-module-globals* nil)
(defvar *cpython-finalizing* nil)

(defun cpython-pointer-key (pointer)
  (and pointer
       (not (null-alien pointer))
       (sb-sys:sap-int (sb-alien:alien-sap pointer))))

(defun remember-cpython-proxy (pointer value)
  (let ((key (cpython-pointer-key pointer)))
    (when key
      (setf (gethash key *cpython-proxy-to-clamp*) value)))
  pointer)

(defun cpython-proxy-original (pointer)
  (let ((key (cpython-pointer-key pointer)))
    (when key
      (multiple-value-bind (value found) (gethash key *cpython-proxy-to-clamp*)
        (when found value)))))

;; Constants from the Python.h include file and its friends that we need:
(defconstant py-single-input 256)
(defconstant py-file-input 257)
(defconstant py-eval-input 258)

(define-alien-routine ("PyTuple_New" py-tuple-new) (* t) (arity int))
(define-alien-routine ("PyTuple_SetItem" py-tuple-set-item) int (tuple (* t)) (position int) (value (* t)))
(define-alien-routine ("PyList_New" py-list-new-c) (* t) (size int))
(define-alien-routine ("PyList_Append" py-list-append-c) int (list (* t)) (item (* t)))
(define-alien-routine ("PyList_Size" py-list-size-c) long (list (* t)))
(define-alien-routine ("PyList_GetItem" py-list-get-item-c) (* t) (list (* t)) (position long))
(define-alien-routine ("PyTuple_Size" py-tuple-size-c) long (tuple (* t)))
(define-alien-routine ("PyTuple_GetItem" py-tuple-get-item-c) (* t) (tuple (* t)) (position long))
(define-alien-routine ("Py_GenericAlias" py-generic-alias-c) (* t) (origin (* t)) (args (* t)))
(define-alien-routine ("PySlice_New" py-slice-new-c) (* t) (start (* t)) (stop (* t)) (step (* t)))

(define-alien-routine ("PyObject_Call" py-call) (* t) (callable (* t)) (args (* t)) (kwargs (* t)))
(define-alien-routine ("PyObject_CallObject" py-call-object) (* t) (callable (* t)) (args (* t)))
(define-alien-routine ("PyImport_ImportModule" py-import-module-c) (* t) (name c-string))
(define-alien-routine ("PyImport_AddModule" py-import-add-module-c) (* t) (name c-string))
(define-alien-routine ("PyModule_GetDict" py-module-get-dict-c) (* t) (module (* t)))
(define-alien-routine ("PyObject_GetAttrString" py-object-get-attr-string) (* t) (obj (* t)) (name c-string))
(define-alien-routine ("PyObject_SetAttrString" py-object-set-attr-string) int (obj (* t)) (name c-string) (value (* t)))
(define-alien-routine ("PyBool_FromLong" py-bool-from-long) (* t) (value long))
(define-alien-routine ("PyLong_FromLongLong" py-long-from-long-long) (* t) (value long-long))
(define-alien-routine ("PyLong_AsLongLong" py-long-as-long-long) long-long (obj (* t)))
(define-alien-routine ("PyFloat_FromDouble" py-float-from-double) (* t) (value double))
(define-alien-routine ("PyComplex_FromDoubles" py-complex-from-doubles) (* t) (real double) (imag double))
(define-alien-routine ("PyFloat_AsDouble" py-float-as-double) double (obj (* t)))
(define-alien-routine ("PyObject_IsTrue" py-object-is-true) int (obj (* t)))
(define-alien-routine ("PyObject_Type" py-object-type-c) (* t) (obj (* t)))
(define-alien-routine ("PyObject_IsInstance" py-object-is-instance) int (obj (* t)) (type (* t)))
(define-alien-routine ("PyObject_IsSubclass" py-object-is-subclass) int (derived (* t)) (class (* t)))
(define-alien-routine ("PyObject_RichCompareBool" py-object-rich-compare-bool-c) int (left (* t)) (right (* t)) (op int))
(define-alien-routine ("PyObject_Hash" py-object-hash-c) long (obj (* t)))
(define-alien-routine ("PyObject_Length" py-object-length-c) long (obj (* t)))
(define-alien-routine ("PyObject_Dir" py-object-dir-c) (* t) (obj (* t)))
(define-alien-routine ("PyObject_GetItem" py-object-get-item) (* t) (obj (* t)) (key (* t)))
(define-alien-routine ("PyObject_SetItem" py-object-set-item) int (obj (* t)) (key (* t)) (value (* t)))
(define-alien-routine ("PyObject_DelItem" py-object-del-item) int (obj (* t)) (key (* t)))
(define-alien-routine ("PyObject_GetIter" py-object-get-iter) (* t) (obj (* t)))
(define-alien-routine ("PyIter_Next" py-iter-next-c) (* t) (iterator (* t)))
(define-alien-routine ("PyCallable_Check" py-callable-check) int (obj (* t)))
(define-alien-routine ("PySequence_Contains" py-sequence-contains-c) int (container (* t)) (item (* t)))
(define-alien-routine ("PyNumber_Add" py-number-add-c) (* t) (left (* t)) (right (* t)))
(define-alien-routine ("PyNumber_Subtract" py-number-subtract-c) (* t) (left (* t)) (right (* t)))
(define-alien-routine ("PyNumber_Multiply" py-number-multiply-c) (* t) (left (* t)) (right (* t)))
(define-alien-routine ("PyNumber_TrueDivide" py-number-true-divide-c) (* t) (left (* t)) (right (* t)))
(define-alien-routine ("PyNumber_FloorDivide" py-number-floor-divide-c) (* t) (left (* t)) (right (* t)))
(define-alien-routine ("PyNumber_Remainder" py-number-remainder-c) (* t) (left (* t)) (right (* t)))
(define-alien-routine ("PyNumber_Divmod" py-number-divmod-c) (* t) (left (* t)) (right (* t)))
(define-alien-routine ("PyNumber_Power" py-number-power-c) (* t) (left (* t)) (right (* t)) (modulus (* t)))
(define-alien-routine ("PyNumber_Negative" py-number-negative-c) (* t) (obj (* t)))
(define-alien-routine ("PyNumber_Positive" py-number-positive-c) (* t) (obj (* t)))
(define-alien-routine ("PyNumber_Absolute" py-number-absolute-c) (* t) (obj (* t)))
(define-alien-routine ("PyBytes_FromStringAndSize" py-bytes-from-string-and-size-c) (* t) (bytes (* unsigned-char)) (size long))
(define-alien-routine ("PyBytes_Size" py-bytes-size-c) long (obj (* t)))
(define-alien-routine ("PyBytes_AsString" py-bytes-as-string-c) (* unsigned-char) (obj (* t)))
(define-alien-routine ("PyCFunction_NewEx" py-cfunction-new-ex) (* t) (method-def (* t)) (self (* t)) (module (* t)))

(defconstant +py-meth-varargs+ 1)
(defconstant +py-meth-keywords+ 2)

(define-alien-type py-method-def
  (struct py-method-def
    (ml-name c-string)
    (ml-meth (* t))
    (ml-flags int)
    (ml-doc c-string)))

;; https://docs.python.org/3/c-api/dict.html
(define-alien-routine ("PyDict_SetItem" py-dict-set-item) int (dict (* t)) (key (* t)) (value (* t)))
(define-alien-routine ("PyDict_Merge" py-dict-merge) int (target-dict (* t)) (source-dict (* t)) (override int))
(define-alien-routine ("PyDict_Next" py-dict-next-c) int
  (dict (* t)) (position (* long)) (key (* (* t))) (value (* (* t))))

;; Exception handling
;; https://docs.python.org/3/c-api/exceptions.html
(define-alien-routine ("PyErr_Occurred" py-err-occurred) (* t))
(define-alien-routine ("PyErr_Print" py-err-print) void)
(define-alien-routine ("PyErr_Clear" py-err-clear) void)
(define-alien-routine ("PyErr_SetString" py-err-set-string) void (type (* t)) (message c-string))
(define-alien-routine ("PyErr_Fetch" py-err-fetch) void
  (ptype (* (* t))) (pvalue (* (* t))) (ptraceback (* (* t))))

(defun cpython-error-occurred-p ()
  (let ((error-pointer (py-err-occurred)))
    (and error-pointer
         (not (null-alien error-pointer)))))


(defun cpython-error-type-name (type-pointer)
  (if (or (not type-pointer) (null-alien type-pointer))
      "RuntimeError"
      (let ((name-pointer (py-object-get-attr-string type-pointer "__name__")))
        (if (or (not name-pointer) (null-alien name-pointer))
            "RuntimeError"
            (python-to-lisp-string name-pointer)))))

(defun cpython-error-message (value-pointer type-name)
  (if (or (not value-pointer) (null-alien value-pointer))
      type-name
      (python-to-lisp-string (py-str value-pointer))))

(defun cpython-error-clamp-type (type-name)
  (cond
    ((string= type-name "TypeError") |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-ERROR-TYPE*)
    ((string= type-name "ValueError") |CLAMP.__CLAMP_INTERNALS__|:*PY-VALUE-ERROR-TYPE*)
    ((string= type-name "KeyError") |CLAMP.__CLAMP_INTERNALS__|:*PY-KEY-ERROR-TYPE*)
    ((string= type-name "IndexError") |CLAMP.__CLAMP_INTERNALS__|:*PY-INDEX-ERROR-TYPE*)
    ((string= type-name "AttributeError") |CLAMP.__CLAMP_INTERNALS__|:*PY-ATTRIBUTE-ERROR-TYPE*)
    ((string= type-name "NameError") |CLAMP.__CLAMP_INTERNALS__|:*PY-NAME-ERROR-TYPE*)
    ((string= type-name "ImportError") |CLAMP.__CLAMP_INTERNALS__|:*PY-IMPORT-ERROR-TYPE*)
    ((string= type-name "ModuleNotFoundError") |CLAMP.__CLAMP_INTERNALS__|:*PY-MODULE-NOT-FOUND-ERROR-TYPE*)
    ((string= type-name "RuntimeWarning") |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-WARNING-TYPE*)
    ((string= type-name "MemoryError") |CLAMP.__CLAMP_INTERNALS__|:*PY-MEMORY-ERROR-TYPE*)
    ((string= type-name "RuntimeError") |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*)
    ((string= type-name "RecursionError") |CLAMP.__CLAMP_INTERNALS__|:*PY-RECURSION-ERROR-TYPE*)
    ((string= type-name "AssertionError") |CLAMP.__CLAMP_INTERNALS__|:*PY-ASSERTION-ERROR-TYPE*)
    (t nil)))

(defun cpython-exception-type-pointer (type-name)
  (or (cpython-builtin-object-pointer type-name)
      (cpython-builtin-object-pointer "RuntimeError")))

(defun cpython-set-error-string (type-name message)
  (let ((type-pointer (cpython-exception-type-pointer type-name)))
    (if (and type-pointer (not (null-alien type-pointer)))
        (py-err-set-string type-pointer message)
        (py-err-set-string (cpython-builtin-object-pointer "Exception") message))))

(defun cpython-copy-exception-attr (exception pvalue name)
  (when (and pvalue (not (null-alien pvalue)))
    (let ((attr (py-object-get-attr-string pvalue name)))
      (if (and attr (not (null-alien attr)))
          (let ((value (cpython-object-to-clamp attr)))
            (when value
              (setf (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTR exception name) value)))
          (when (cpython-error-occurred-p)
            (py-err-clear))))))

(defun cpython-make-mapped-exception (clamp-type message pvalue)
  (let ((exception (|CLAMP.__CLAMP_INTERNALS__|::MAKE-PY-EXCEPTION clamp-type message)))
    (when (and pvalue (not (null-alien pvalue)))
      (setf (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTR exception "__cpython_proxy__")
            (wrap-cpython-object pvalue))
      (cpython-copy-exception-attr exception pvalue "name")
      (cpython-copy-exception-attr exception pvalue "path"))
    exception))

(defun cpython-set-error-from-clamp-exception (condition)
  (let ((value (|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION-VALUE condition)))
    (if (|CLAMP.__CLAMP_INTERNALS__|::PY-EXCEPTION-OBJECT-P value)
        (let* ((type (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-TYPE value))
               (type-name (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-NAME type))
               (message (|CLAMP.__CLAMP_INTERNALS__|::PY-EXCEPTION-MESSAGE value)))
          (cpython-set-error-string type-name message))
        (cpython-set-error-string "RuntimeError" (princ-to-string value)))))

(defun cpython-raise-current-error (fallback-message)
  (sb-alien:with-alien ((ptype (* t))
                        (pvalue (* t))
                        (ptraceback (* t)))
    (py-err-fetch (sb-alien:addr ptype) (sb-alien:addr pvalue) (sb-alien:addr ptraceback))
    (let* ((type-name (cpython-error-type-name ptype))
           (clamp-type (cpython-error-clamp-type type-name))
           (message (if (and pvalue (not (null-alien pvalue)))
                        (cpython-error-message pvalue type-name)
                        fallback-message)))
      (if clamp-type
          (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE
           (cpython-make-mapped-exception clamp-type message pvalue))
          (let ((exception (|CLAMP.__CLAMP_INTERNALS__|::MAKE-PY-EXCEPTION
                            (wrap-cpython-object ptype)
                            message)))
            (when (and pvalue (not (null-alien pvalue)))
              (setf (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTR exception "__cpython_proxy__")
                    (wrap-cpython-object pvalue))
              (cpython-copy-exception-attr exception pvalue "name")
              (cpython-copy-exception-attr exception pvalue "path"))
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE exception))))))

(defun wrap-cpython-object (pointer &key borrowed)
  (when (and pointer (not (null-alien pointer)))
    ;; Borrowed references from container/dict APIs must be retained before a
    ;; Clamp wrapper can outlive the native owner. Releasing those refs needs a
    ;; GIL-aware lifecycle; SBCL finalizers are not safe for Py_DecRef.
    (when borrowed
      (py-inc-ref pointer))
    (|CLAMP.__CLAMP_INTERNALS__|::MAKE-PY-CPYTHON-OBJECT-WRAPPER pointer)))

(defun cpython-install-native-builtins ()
  (let ((builtins (py-import-module-c "builtins")))
    (setf |CLAMP.__builtins__|:INT
          (wrap-cpython-object (py-object-get-attr-string builtins "int")))
    (setf |CLAMP.__builtins__|:FLOAT
          (wrap-cpython-object (py-object-get-attr-string builtins "float")))
    (setf |CLAMP.__builtins__|:COMPLEX
          (wrap-cpython-object (py-object-get-attr-string builtins "complex")))
    (setf |CLAMP.__builtins__|:BYTES
          (wrap-cpython-object (py-object-get-attr-string builtins "bytes")))
    (setf |CLAMP.__builtins__|:OBJECT
          (wrap-cpython-object (py-object-get-attr-string builtins "object")))
    (setf |CLAMP.__builtins__|:DICT
          (wrap-cpython-object (py-object-get-attr-string builtins "dict")))
    (setf |CLAMP.__builtins__|:SET
          (wrap-cpython-object (py-object-get-attr-string builtins "set")))
    (setf |CLAMP.__builtins__|:FROZENSET
          (wrap-cpython-object (py-object-get-attr-string builtins "frozenset")))
    (setf |CLAMP.__builtins__|:MEMORYVIEW
          (wrap-cpython-object (py-object-get-attr-string builtins "memoryview")))
    (setf |CLAMP.__builtins__|:BYTEARRAY
          (wrap-cpython-object (py-object-get-attr-string builtins "bytearray")))
    (setf |CLAMP.__builtins__|:OPEN
          (wrap-cpython-object (py-object-get-attr-string builtins "open")))
    (setf |CLAMP.__builtins__|:COMPILE #'cpython-compile-builtin)
    (setf |CLAMP.__builtins__|:EXEC #'cpython-exec-builtin)))

(defun cpython-cache-builtin-types (globals-and-locals)
  (declare (ignore globals-and-locals))
  (let ((builtins (py-import-module-c "builtins")))
    (setf *py-cpython-none-type* (py-object-type-c *py-none*))
    (setf *py-cpython-bool-type* (py-object-get-attr-string builtins "bool"))
    (setf *py-cpython-int-type* (py-object-get-attr-string builtins "int"))
    (setf *py-cpython-float-type* (py-object-get-attr-string builtins "float"))
    (setf *py-cpython-str-type* (py-object-get-attr-string builtins "str"))
    (setf *py-cpython-bytes-type* (py-object-get-attr-string builtins "bytes"))
    (setf *py-cpython-list-type* (py-object-get-attr-string builtins "list"))
    (setf *py-cpython-tuple-type* (py-object-get-attr-string builtins "tuple"))
    (setf *py-cpython-dict-type* (py-object-get-attr-string builtins "dict"))
    (setf *py-cpython-type-callable* (py-object-get-attr-string builtins "type"))
    (let ((object-type (py-object-get-attr-string builtins "object")))
      (setf *py-cpython-object-setattr*
            (and object-type
                 (not (null-alien object-type))
                 (py-object-get-attr-string object-type "__setattr__"))))
    (let ((types-module (py-import-module-c "types")))
      (setf *py-cpython-types-prepare-class*
            (and types-module
                 (not (null-alien types-module))
                 (py-object-get-attr-string types-module "prepare_class"))))
    (let ((abc-module (py-import-module-c "abc")))
      (setf *py-cpython-abc-abstractmethod*
            (and abc-module
                 (not (null-alien abc-module))
                 (py-object-get-attr-string abc-module "abstractmethod"))))
    (setf *py-cpython-exception-types*
          (list (cons "BaseException" (py-object-get-attr-string builtins "BaseException"))
                (cons "Exception" (py-object-get-attr-string builtins "Exception"))
                (cons "TypeError" (py-object-get-attr-string builtins "TypeError"))
                (cons "ValueError" (py-object-get-attr-string builtins "ValueError"))
                (cons "KeyError" (py-object-get-attr-string builtins "KeyError"))
                (cons "IndexError" (py-object-get-attr-string builtins "IndexError"))
                (cons "AttributeError" (py-object-get-attr-string builtins "AttributeError"))
                (cons "NameError" (py-object-get-attr-string builtins "NameError"))
                (cons "ImportError" (py-object-get-attr-string builtins "ImportError"))
                (cons "ModuleNotFoundError" (py-object-get-attr-string builtins "ModuleNotFoundError"))
                (cons "RuntimeWarning" (py-object-get-attr-string builtins "RuntimeWarning"))
                (cons "MemoryError" (py-object-get-attr-string builtins "MemoryError"))
                (cons "RuntimeError" (py-object-get-attr-string builtins "RuntimeError"))
                (cons "RecursionError" (py-object-get-attr-string builtins "RecursionError"))
                (cons "AssertionError" (py-object-get-attr-string builtins "AssertionError"))))))

(defun cpython-pointer= (left right)
  (and left
       right
       (not (null-alien left))
       (not (null-alien right))
       (sb-sys:sap= (sb-alien:alien-sap left)
                    (sb-alien:alien-sap right))))

(defun cpython-object-exact-type-p (pointer type-pointer)
  (cpython-pointer= (py-object-type-c pointer) type-pointer))

(defmacro with-cpython-floating-point-boundary (&body body)
  (let ((modes (gensym "MODES")))
    `(let ((,modes (sb-int:get-floating-point-modes)))
       (unwind-protect
            (progn
              (sb-int:set-floating-point-modes
               :traps nil
               :current-exceptions nil
               :accrued-exceptions nil)
              ,@body)
         (sb-int:set-floating-point-modes
          :current-exceptions nil
          :accrued-exceptions nil
          :traps (getf ,modes :traps)
          :rounding-mode (getf ,modes :rounding-mode))))))

(defmacro with-cpython-gil (&body body)
  (let ((state (gensym "GIL-STATE")))
    `(let ((,state (py-gil-state-ensure)))
       (unwind-protect
            (progn ,@body)
         (py-gil-state-release ,state)))))

(defmacro with-cpython-runtime-boundary (&body body)
  `(with-cpython-gil
     (with-cpython-floating-point-boundary
       ,@body)))

(defun cpython-compile-builtin (source filename mode &rest args)
  (declare (ignore filename mode args))
  source)

(defun cpython-copy-dict-entries-to-clamp-dict (dict-pointer clamp-dict)
  (sb-alien:with-alien ((position long 0)
                        (key (* t))
                        (value (* t)))
    (loop while (/= (py-dict-next-c dict-pointer
                                    (sb-alien:addr position)
                                    (sb-alien:addr key)
                                    (sb-alien:addr value))
                    0)
          do (let ((clamp-key (cpython-object-to-clamp key :borrowed t)))
               (when (stringp clamp-key)
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-SET-ENTRY
                  clamp-dict
                  clamp-key
                  (cpython-object-to-clamp value :borrowed t)))))))

(defun cpython-exec-builtin (code &optional globals locals)
  (declare (ignore locals))
  (with-cpython-runtime-boundary
    (let* ((source (if (stringp code)
                       code
                       (with-output-to-string (stream)
                         (|CLAMP.__CLAMP_INTERNALS__|:PY-DISPLAY code stream))))
           (target (if (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-P globals)
                       globals
                       (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS))))
      (when (and (>= (length source) 4)
                 (string= (subseq source 0 4) "def ")
                 (search "return _decorate_(" source :test #'char=))
        (let* ((open-pos (position #\( source))
               (name (and open-pos (subseq source 4 open-pos))))
          (when (and name (> (length name) 0))
            (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-SET-ENTRY
             target
             name
             (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE
              :name name
              :signature-vararg-name "args"
              :fn (lambda (&rest call-args)
                    (let ((decorate (|CLAMP.__CLAMP_INTERNALS__|:PY-GETITEM target "_decorate_"))
                          (caller (|CLAMP.__CLAMP_INTERNALS__|:PY-GETITEM target "_call_")))
                      (apply #'|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE
                             decorate
                             (append call-args (list caller)))))))
            (return-from cpython-exec-builtin |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))))
      (let* ((dict-pointer (clamp-dict-to-cpython target))
             (builtins (py-import-module-c "builtins")))
        (when (and builtins (not (null-alien builtins)))
          (py-dict-set-item dict-pointer (py-unicode-from-string "__builtins__") builtins))
        (let ((result (py-run-string source py-file-input dict-pointer dict-pointer)))
          (if (and result (not (null-alien result)))
              (progn
                (cpython-copy-dict-entries-to-clamp-dict dict-pointer target)
                |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
              (if (cpython-error-occurred-p)
                  (cpython-raise-current-error "CPython exec failed")
                  (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
                   |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
                   "CPython exec failed"))))))))

(defun cpython-list-to-clamp (pointer)
  (let ((items '())
        (size (py-list-size-c pointer)))
    (loop for index from 0 below size
          do (push (cpython-object-to-clamp (py-list-get-item-c pointer index) :borrowed t) items))
    (apply #'|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-LIST (nreverse items))))

(defun cpython-tuple-to-clamp (pointer)
  (let ((items '())
        (size (py-tuple-size-c pointer)))
    (loop for index from 0 below size
          do (push (cpython-object-to-clamp (py-tuple-get-item-c pointer index) :borrowed t) items))
    (apply #'|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-TUPLE (nreverse items))))

(defun cpython-bytes-to-clamp (pointer)
  (let* ((size (py-bytes-size-c pointer))
         (data (py-bytes-as-string-c pointer))
         (storage (make-array size :element-type '(unsigned-byte 8))))
    (when (or (< size 0) (not data) (null-alien data))
      (cpython-raise-current-error "CPython bytes conversion failed"))
    (loop for index from 0 below size
          do (setf (aref storage index) (sb-alien:deref data index)))
    (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-BYTES-FROM-VECTOR storage)))

(defun cpython-dict-to-clamp (pointer)
  (let ((pairs '()))
    (sb-alien:with-alien ((position long 0)
                          (key (* t))
                          (value (* t)))
      (loop while (/= (py-dict-next-c pointer
                                      (sb-alien:addr position)
                                      (sb-alien:addr key)
                                      (sb-alien:addr value))
                      0)
            do (push (list (cpython-object-to-clamp key :borrowed t)
                           (cpython-object-to-clamp value :borrowed t))
                     pairs)))
    (apply #'|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS (nreverse pairs))))

(sb-alien:define-alien-callable clamp-cpython-callable-callback (* t) ((self (* t)) (args (* t)) (kwargs (* t)))
  (handler-case
      (let* ((callable-id (py-long-as-long-long self))
             (callable (gethash callable-id *cpython-callable-proxies*))
             (clamp-positional-args (if (and args (not (null-alien args)))
                                        (|CLAMP.__CLAMP_INTERNALS__|::PY-ITERABLE-TO-LIST
                                         (cpython-tuple-to-clamp args))
                                        '()))
             (clamp-keyword-args (if (and kwargs (not (null-alien kwargs)))
                                     (|CLAMP.__CLAMP_INTERNALS__|::PY-KWARGS-TO-CALL-ARGS
                                      (cpython-dict-to-clamp kwargs))
                                     '()))
             (clamp-args (append clamp-positional-args clamp-keyword-args))
             (param-names (and callable
                               (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-SIGNATURE-PARAM-NAMES callable)))
             (owner-type (and callable
                              (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-OWNER-TYPE callable)))
             (invoke-args (if (and owner-type
                                   param-names
                                   (string= (first param-names) "cls"))
                              (cons owner-type clamp-args)
                              clamp-args))
             (result (apply #'|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE callable invoke-args)))
        (clamp-value-to-cpython result))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION (condition)
      (cpython-set-error-from-clamp-exception condition)
      nil)
    (error (condition)
      (cpython-set-error-string "RuntimeError" (princ-to-string condition))
      nil)))

(defun clamp-callable-to-cpython-proxy (value)
  (handler-case
      (let ((cached (gethash "__cpython_proxy__" (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-ATTRS value))))
        (when (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P cached)
          (return-from clamp-callable-to-cpython-proxy
            (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER cached))))
    (error () nil))
  (let* ((callable-id (incf *cpython-callable-proxy-next-id*))
         (self (py-long-from-long-long callable-id))
         (method-def (make-alien py-method-def))
         (callback (sb-alien:alien-callable-function 'clamp-cpython-callable-callback)))
    (setf (gethash callable-id *cpython-callable-proxies*) value)
    (setf (slot method-def 'ml-name) (or (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-NAME value) "clamp_callable"))
    (setf (slot method-def 'ml-meth) (sb-alien:alien-sap callback))
    (setf (slot method-def 'ml-flags) (logior +py-meth-varargs+ +py-meth-keywords+))
    (setf (slot method-def 'ml-doc) "Clamp callable proxy")
    (push method-def *cpython-callable-method-defs*)
    (let* ((callback-proxy (py-cfunction-new-ex method-def self nil))
           (globals (py-new-dict))
           (builtins (py-import-module-c "builtins"))
           (wrapper-name (format nil "__clamp_callable_proxy_~D" callable-id)))
      (when (cpython-error-occurred-p)
        (cpython-raise-current-error "CPython callable proxy creation failed"))
      (when (/= (py-dict-set-item globals (py-unicode-from-string "__builtins__") builtins) 0)
        (cpython-raise-current-error "CPython callable proxy wrapper setup failed"))
      (when (/= (py-dict-set-item globals (py-unicode-from-string "__clamp_callable") callback-proxy) 0)
        (cpython-raise-current-error "CPython callable proxy wrapper setup failed"))
      (let ((result (py-run-string
                     (format nil "def ~A(*args, **kwargs):~%    return __clamp_callable(*args, **kwargs)~%" wrapper-name)
                     py-file-input
                     globals
                     globals)))
        (declare (ignore result))
        (when (cpython-error-occurred-p)
          (cpython-raise-current-error "CPython callable proxy wrapper creation failed")))
      (let ((proxy (py-object-get-item globals (py-unicode-from-string wrapper-name))))
        (unless (and proxy (not (null-alien proxy)))
          (cpython-raise-current-error "CPython callable proxy wrapper lookup failed"))
        (remember-cpython-proxy proxy value)
        (setf (gethash "__cpython_proxy__" (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-ATTRS value))
              (wrap-cpython-object proxy))
        proxy))))

(defun cpython-object-to-clamp (pointer &key borrowed)
  (let ((original (cpython-proxy-original pointer)))
    (when original
      (return-from cpython-object-to-clamp original)))
  (cond
    ((or (not pointer) (null-alien pointer)) nil)
    ((cpython-object-exact-type-p pointer *py-cpython-none-type*)
     |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
    ((and *py-cpython-ellipsis*
          (cpython-pointer= pointer *py-cpython-ellipsis*))
     |CLAMP.__CLAMP_INTERNALS__|:*PY-ELLIPSIS*)
    ((cpython-object-exact-type-p pointer *py-cpython-bool-type*)
     (if (= (py-object-is-true pointer) 1)
         |CLAMP.__CLAMP_INTERNALS__|:*PY-TRUE*
         |CLAMP.__CLAMP_INTERNALS__|:*PY-FALSE*))
    ((cpython-object-exact-type-p pointer *py-cpython-int-type*)
     (py-err-clear)
     (let ((value (py-long-as-long-long pointer)))
       (if (cpython-error-occurred-p)
           (progn
             (py-err-clear)
             (wrap-cpython-object pointer :borrowed borrowed))
           value)))
    ((cpython-object-exact-type-p pointer *py-cpython-float-type*)
     (py-err-clear)
     (let ((value (py-float-as-double pointer)))
       (if (cpython-error-occurred-p)
           (progn
             (py-err-clear)
             (wrap-cpython-object pointer :borrowed borrowed))
           value)))
    ((cpython-object-exact-type-p pointer *py-cpython-str-type*)
     (python-unicode-to-lisp-string pointer))
    ((cpython-object-exact-type-p pointer *py-cpython-bytes-type*)
     (cpython-bytes-to-clamp pointer))
    ((cpython-object-exact-type-p pointer *py-cpython-list-type*)
     (cpython-list-to-clamp pointer))
    ((cpython-object-exact-type-p pointer *py-cpython-tuple-type*)
     (cpython-tuple-to-clamp pointer))
    ((cpython-object-exact-type-p pointer *py-cpython-dict-type*)
     (cpython-dict-to-clamp pointer))
    (t (wrap-cpython-object pointer :borrowed borrowed))))

(defun clamp-bytes-to-cpython (value)
  (let* ((storage (|CLAMP.__CLAMP_INTERNALS__|::PY-BYTES-STORAGE value "CPython bridge"))
         (size (length storage))
         (buffer (make-alien unsigned-char (max size 1))))
    (unwind-protect
         (progn
           (loop for index from 0 below size
                 do (setf (sb-alien:deref buffer index) (aref storage index)))
           (let ((pointer (py-bytes-from-string-and-size-c buffer size)))
             (if (and pointer (not (null-alien pointer)))
                 pointer
                 (cpython-raise-current-error "CPython bytes conversion failed"))))
      (free-alien buffer))))

(defun clamp-string-to-cpython (value)
  (let* ((octets (sb-ext:string-to-octets value :external-format :utf-8))
         (size (length octets))
         (buffer (make-alien unsigned-char (max size 1))))
    (unwind-protect
         (progn
           (loop for index from 0 below size
                 do (setf (sb-alien:deref buffer index) (aref octets index)))
           (let ((pointer (py-unicode-from-string-and-size-c buffer size)))
             (if (and pointer (not (null-alien pointer)))
                 pointer
                 (cpython-raise-current-error "CPython string conversion failed"))))
      (free-alien buffer))))

(defun clamp-list-to-cpython (value)
  (let* ((storage (|CLAMP.__CLAMP_INTERNALS__|::PY-LIST-STORAGE value "CPython bridge"))
         (size (or (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-SIZE value) 0))
         (list-pointer (py-list-new-c 0)))
    (loop for index from 0 below size
          do (let ((result (py-list-append-c list-pointer
                                             (clamp-value-to-cpython (aref storage index)))))
               (when (/= result 0)
                 (cpython-raise-current-error "CPython list conversion failed"))))
    list-pointer))

(defun cpython-incref-if-valid (pointer)
  (when (and pointer (not (null-alien pointer)))
    (py-inc-ref pointer))
  pointer)

(defun clamp-value-to-cpython-stealable (value)
  (let ((pointer (clamp-value-to-cpython value)))
    (when (or (eq value |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
              (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P value)
              (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P value)
              (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-P value)
              (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-P value))
      (cpython-incref-if-valid pointer))
    pointer))

(defun clamp-tuple-to-cpython (value)
  (let* ((storage (|CLAMP.__CLAMP_INTERNALS__|::PY-TUPLE-STORAGE value "CPython bridge"))
         (size (or (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-SIZE value) 0))
         (tuple-pointer (py-tuple-new size)))
    (loop for index from 0 below size
          do (let ((result (py-tuple-set-item tuple-pointer index
                                             (clamp-value-to-cpython-stealable (aref storage index)))))
               (when (/= result 0)
                 (cpython-raise-current-error "CPython tuple conversion failed"))))
    tuple-pointer))

(defun clamp-dict-to-cpython (value)
  (let ((dict-pointer (py-new-dict))
        (storage (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-STORAGE value "CPython bridge"))
        (keys (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-KEYS value)))
    (loop for index from 0 below (fill-pointer keys)
          for key = (aref keys index)
          do (let ((result (py-dict-set-item dict-pointer
                                             (clamp-value-to-cpython key)
                                             (clamp-value-to-cpython (gethash key storage)))))
               (when (/= result 0)
                 (cpython-raise-current-error "CPython dict conversion failed"))))
    dict-pointer))

(defun clamp-slice-to-cpython (value)
  (let ((pointer (py-slice-new-c
                  (clamp-value-to-cpython (|CLAMP.__CLAMP_INTERNALS__|:PY-SLICE-OBJECT-START value))
                  (clamp-value-to-cpython (|CLAMP.__CLAMP_INTERNALS__|:PY-SLICE-OBJECT-STOP value))
                  (clamp-value-to-cpython (|CLAMP.__CLAMP_INTERNALS__|:PY-SLICE-OBJECT-STEP value)))))
    (if (and pointer (not (null-alien pointer)))
        pointer
        (cpython-raise-current-error "CPython slice conversion failed"))))

(defun clamp-type-bases-to-cpython-tuple (value)
  (let* ((bases (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-BASES value))
         (tuple (py-tuple-new (length bases))))
    (loop for base in bases
          for index from 0
          do (let ((result (py-tuple-set-item tuple index (clamp-value-to-cpython-stealable base))))
               (when (/= result 0)
                 (cpython-raise-current-error "CPython type proxy base conversion failed"))))
    tuple))

(defun clamp-method-callable-to-cpython-function (name callable)
  (let* ((globals (py-new-dict))
         (builtins (py-import-module-c "builtins"))
         (callable-proxy (clamp-callable-to-cpython-proxy callable)))
    (when (/= (py-dict-set-item globals (py-unicode-from-string "__builtins__") builtins) 0)
      (cpython-raise-current-error "CPython method wrapper globals setup failed"))
    (when (/= (py-dict-set-item globals (py-unicode-from-string "__clamp_callable") callable-proxy) 0)
      (cpython-raise-current-error "CPython method wrapper globals setup failed"))
    (let ((result (py-run-string
                   (format nil "def ~A(self, context=None):~%    return __clamp_callable(self, context)~%" name)
                   py-file-input
                   globals
                   globals)))
      (declare (ignore result))
      (when (cpython-error-occurred-p)
        (cpython-raise-current-error "CPython method wrapper creation failed"))
      (let ((function (py-object-get-item globals (py-unicode-from-string name))))
        (if (and function (not (null-alien function)))
            function
            (cpython-raise-current-error "CPython method wrapper lookup failed"))))))

(defun clamp-copy-type-attrs-to-cpython-namespace (value namespace)
  (unless (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-SUBTYPE-P value |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-TYPE*)
    (maphash
     (lambda (name attr)
       (when (and (stringp name)
                  (not (member name '("__cpython_proxy__" "__dict__" "__weakref__") :test #'string=)))
         (cond
           ((and (string= name "model_post_init")
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-P attr))
            (let ((result (py-dict-set-item namespace
                                            (py-unicode-from-string name)
                                            (clamp-method-callable-to-cpython-function name attr))))
              (when (/= result 0)
                (cpython-raise-current-error "CPython type proxy namespace conversion failed"))))
           ((or (and (clamp-cpython-proxy-attr-value-p attr)
                     (not (|CLAMP.__CLAMP_INTERNALS__|::PY-LIST-OBJECT-P attr))
                     (not (|CLAMP.__CLAMP_INTERNALS__|::PY-TUPLE-OBJECT-P attr)))
                (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-P attr))
            (let ((result (py-dict-set-item namespace
                                            (py-unicode-from-string name)
                                            (clamp-value-to-cpython attr))))
              (when (/= result 0)
                (cpython-raise-current-error "CPython type proxy namespace conversion failed")))))))
     (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTRS value))))


(defun clamp-cpython-module-global-value-p (value)
  (or (clamp-cpython-proxy-attr-value-p value)
      (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-P value)
      (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-P value)
      (and (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-P value)
           (not (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P value))
           (not (|CLAMP.__CLAMP_INTERNALS__|::PY-MODULE-OBJECT-P value))
           (not (|CLAMP.__CLAMP_INTERNALS__|::PY-MODULE-SPEC-OBJECT-P value))
           (not (|CLAMP.__CLAMP_INTERNALS__|::PY-SOURCE-FILE-LOADER-OBJECT-P value))
           (not (|CLAMP.__CLAMP_INTERNALS__|::PY-FILE-READER-OBJECT-P value))
           (not (|CLAMP.__CLAMP_INTERNALS__|::PY-PATH-OBJECT-P value)))))

(defun clamp-sync-module-globals-to-cpython (module-name)
  (when (and (not *syncing-cpython-module-globals*)
             (stringp module-name)
             (> (length module-name) 0)
             (not (string= module-name "__main__"))
             (not (or (string= module-name "numpy")
                      (string= module-name "scipy")
                      (and (> (length module-name) 6)
                           (string= (subseq module-name 0 6) "numpy."))
                      (and (> (length module-name) 6)
                           (string= (subseq module-name 0 6) "scipy.")))))
    (let ((*syncing-cpython-module-globals* t)
          (module (gethash module-name |CLAMP.__CLAMP_INTERNALS__|::*PY-SYS-MODULES*)))
      (when (|CLAMP.__CLAMP_INTERNALS__|::PY-MODULE-OBJECT-P module)
        (let* ((cpython-module (py-import-add-module-c module-name))
               (namespace (and cpython-module
                               (not (null-alien cpython-module))
                               (py-module-get-dict-c cpython-module))))
          (when (and namespace (not (null-alien namespace)))
            (maphash
             (lambda (name attr-value)
               (when (and (stringp name)
                          (not (member name '("__cpython_proxy__" "__loader__" "__spec__" "__builtins__")
                                       :test #'string=))
                          (clamp-cpython-module-global-value-p attr-value))
                 (handler-case
                     (let ((converted (clamp-value-to-cpython attr-value)))
                       (unless (= (py-dict-set-item namespace
                                                     (py-unicode-from-string name)
                                                     converted)
                                  0)
                         (when (cpython-error-occurred-p)
                           (py-err-clear))))
                   (error ()
                     (when (cpython-error-occurred-p)
                       (py-err-clear))))))
             (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTRS module))))))))

(setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-SYNC-MODULE-GLOBALS*
      #'clamp-sync-module-globals-to-cpython)

(defun clamp-builtin-type-name (value)
  (cond
    ((eq value |CLAMP.__CLAMP_INTERNALS__|::*PY-BOOL-TYPE*) "bool")
    ((eq value |CLAMP.__CLAMP_INTERNALS__|::*PY-INT-TYPE*) "int")
    ((eq value |CLAMP.__CLAMP_INTERNALS__|::*PY-FLOAT-TYPE*) "float")
    ((eq value |CLAMP.__CLAMP_INTERNALS__|::*PY-STR-TYPE*) "str")
    ((eq value |CLAMP.__CLAMP_INTERNALS__|::*PY-BYTES-TYPE*) "bytes")
    ((eq value |CLAMP.__CLAMP_INTERNALS__|::*PY-TYPE-TYPE*) "type")
    ((eq value |CLAMP.__CLAMP_INTERNALS__|::*PY-LIST-TYPE*) "list")
    ((eq value |CLAMP.__CLAMP_INTERNALS__|::*PY-TUPLE-TYPE*) "tuple")
    ((eq value |CLAMP.__CLAMP_INTERNALS__|::*PY-DICT-TYPE*) "dict")))

(defun clamp-type-to-cpython-proxy (value)
  (let ((builtin-name (clamp-builtin-type-name value)))
    (when builtin-name
      (let ((pointer (cpython-builtin-object-pointer builtin-name)))
        (when pointer
          (return-from clamp-type-to-cpython-proxy pointer)))))
  (handler-case
      (let ((cached (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-ATTR value "__cpython_proxy__")))
        (when (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P cached)
          (return-from clamp-type-to-cpython-proxy
            (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER cached))))
    (error () nil))
  (let* ((name (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-NAME value))
         (module (handler-case
                     (let ((module-value (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-ATTR value "__module__")))
                       (if (stringp module-value) module-value "__main__"))
                   (error () "__main__")))
         (qualname (handler-case
                       (let ((qualname-value (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-ATTR value "__qualname__")))
                         (if (stringp qualname-value) qualname-value name))
                     (error () name)))
         (namespace (py-new-dict))
         (bases (clamp-type-bases-to-cpython-tuple value))
         (args (py-tuple-new 3)))
    (clamp-sync-module-globals-to-cpython module)
    (when (/= (py-dict-set-item namespace (py-unicode-from-string "__module__") (py-unicode-from-string module)) 0)
      (cpython-raise-current-error "CPython type proxy namespace conversion failed"))
    (when (/= (py-dict-set-item namespace (py-unicode-from-string "__qualname__") (py-unicode-from-string qualname)) 0)
      (cpython-raise-current-error "CPython type proxy namespace conversion failed"))
    (clamp-copy-type-attrs-to-cpython-namespace value namespace)
    (when (/= (py-tuple-set-item args 0 (py-unicode-from-string name)) 0)
      (cpython-raise-current-error "CPython type proxy argument conversion failed"))
    (when (/= (py-tuple-set-item args 1 bases) 0)
      (cpython-raise-current-error "CPython type proxy argument conversion failed"))
    (when (/= (py-tuple-set-item args 2 namespace) 0)
      (cpython-raise-current-error "CPython type proxy argument conversion failed"))
    (let ((proxy (py-call-object *py-cpython-type-callable* args)))
      (when (cpython-error-occurred-p)
        (cpython-raise-current-error "CPython type proxy creation failed"))
      (remember-cpython-proxy proxy value)
      (setf (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-ATTR value "__cpython_proxy__")
            (wrap-cpython-object proxy))
      proxy)))

(defun clamp-cpython-proxy-attr-value-p (value)
  (or (eq value |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
      (eq value |CLAMP.__CLAMP_INTERNALS__|:*PY-TRUE*)
      (eq value |CLAMP.__CLAMP_INTERNALS__|:*PY-FALSE*)
      (stringp value)
      (integerp value)
      (floatp value)
      (|CLAMP.__CLAMP_INTERNALS__|::PY-BYTES-OBJECT-P value)
      (|CLAMP.__CLAMP_INTERNALS__|::PY-LIST-OBJECT-P value)
      (|CLAMP.__CLAMP_INTERNALS__|::PY-TUPLE-OBJECT-P value)
      (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P value)))

(defun clamp-sync-instance-attrs-to-cpython-proxy (value proxy)
  (labels ((set-proxy-attr (name attr-value)
             (when (and (stringp name)
                        (not (string= name "__cpython_proxy__"))
                        (clamp-cpython-proxy-attr-value-p attr-value))
               (let ((result (py-object-set-attr-string proxy name (clamp-value-to-cpython attr-value))))
                 (when (/= result 0)
                   (when (cpython-error-occurred-p)
                     (py-err-clear))))))
           (sync-type-properties (type)
             (when (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P type)
               (dolist (mro-type (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-MRO-LIST type))
                 (when (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P mro-type)
                   (maphash (lambda (name attr)
                              (when (stringp name)
                                (cond
                                  ((clamp-cpython-proxy-attr-value-p attr)
                                   (set-proxy-attr name attr))
                                  ((|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-P attr)
                                   (multiple-value-bind (fget found)
                                       (gethash "__property_fget__"
                                                (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTRS attr))
                                     (when found
                                       (handler-case
                                           (let ((result (|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE fget value)))
                                             (set-proxy-attr name result))
                                         (error () nil))))))))
                            (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTRS mro-type)))))))
    (when (and (not *syncing-cpython-proxy*)
               (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-P value)
               (not (|CLAMP.__CLAMP_INTERNALS__|::PY-MODULE-OBJECT-P value)))
      (let ((*syncing-cpython-proxy* t))
        (maphash #'set-proxy-attr
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTRS value))
        (sync-type-properties (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-TYPE value)))))
  proxy)

(defun clamp-instance-to-cpython-proxy (value)
  (handler-case
      (let ((cached (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTR value "__cpython_proxy__")))
        (when (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P cached)
          (let ((proxy (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER cached)))
            (unless *syncing-cpython-proxy*
              (clamp-sync-instance-attrs-to-cpython-proxy value proxy))
            (return-from clamp-instance-to-cpython-proxy proxy))))
    (error () nil))
  (let* ((class-proxy (clamp-type-to-cpython-proxy
                       (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-TYPE value)))
         (args (py-tuple-new 0))
         (proxy (py-call-object class-proxy args)))
    (when (cpython-error-occurred-p)
      (cpython-raise-current-error "CPython instance proxy creation failed"))
    (remember-cpython-proxy proxy value)
    (setf (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTR value "__cpython_proxy__")
          (wrap-cpython-object proxy))
    (clamp-sync-instance-attrs-to-cpython-proxy value proxy)
    proxy))

(defun cpython-builtin-object-pointer (name)
  (when (member name '("bool" "int" "float" "str" "bytes" "type" "list" "tuple" "dict" "set" "frozenset" "memoryview" "bytearray" "open" "range" "property"
                      "Exception" "RuntimeError" "RecursionError" "TypeError" "ValueError" "KeyError" "IndexError"
                      "AttributeError" "ImportError" "ModuleNotFoundError" "RuntimeWarning" "MemoryError" "AssertionError")
                :test #'string=)
    (let* ((builtins (py-import-module-c "builtins"))
           (pointer (and builtins
                         (not (null-alien builtins))
                         (py-object-get-attr-string builtins name))))
      (if (and pointer (not (null-alien pointer)))
          pointer
          (progn
            (when (cpython-error-occurred-p)
              (py-err-clear))
            nil)))))


(defun clamp-enum-member-to-cpython-value (value)
  (when (and (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-P value)
             (not (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P value)))
    (multiple-value-bind (enum-class found)
        (gethash "__clamp_enum_class__"
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTRS value))
      (declare (ignore enum-class))
      (when found
        (multiple-value-bind (enum-value value-found)
            (gethash "value" (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTRS value))
          (when value-found
            (clamp-value-to-cpython enum-value)))))))

(defun clamp-range-to-cpython (value)
  (let ((range-type (cpython-builtin-object-pointer "range")))
    (unless range-type
      (cpython-raise-current-error "CPython range lookup failed"))
    (cpython-call-pointer-with-clamp-args
     range-type
     "CPython range conversion failed"
     (|CLAMP.__CLAMP_INTERNALS__|::PY-RANGE-OBJECT-START value)
     (|CLAMP.__CLAMP_INTERNALS__|::PY-RANGE-OBJECT-STOP value)
     (|CLAMP.__CLAMP_INTERNALS__|::PY-RANGE-OBJECT-STEP value))))

(defun clamp-property-to-cpython (value)
  (when (|CLAMP.__CLAMP_INTERNALS__|::PY-PROPERTY-OBJECT-P value)
    (let ((property-type (cpython-builtin-object-pointer "property"))
          (attrs (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTRS value)))
      (unless property-type
        (cpython-raise-current-error "CPython property lookup failed"))
      (cpython-call-pointer-with-clamp-args
       property-type
       "CPython property conversion failed"
       (gethash "fget" attrs |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
       (gethash "fset" attrs |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
       (gethash "fdel" attrs |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
       (gethash "__doc__" attrs |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)))))

(defun clamp-mapping-object-to-cpython-dict (value)
  (when (and (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-P value)
             (not (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-P value))
             (string= (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-NAME
                       (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-TYPE value))
                      "LazyLocalNamespace"))
    (handler-case
        (let ((data (|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR value "data")))
          (when (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-P data)
            (clamp-dict-to-cpython data)))
      (error () nil))))

(defun clamp-value-to-cpython (value)
  (cond
    ((eq value |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*) *py-none*)
    ((eq value |CLAMP.__CLAMP_INTERNALS__|:*PY-TRUE*) (py-bool-from-long 1))
    ((eq value |CLAMP.__CLAMP_INTERNALS__|:*PY-FALSE*) (py-bool-from-long 0))
    ((stringp value) (clamp-string-to-cpython value))
    ((and (numberp value) (not (realp value)))
     (py-complex-from-doubles (coerce (realpart value) 'double-float)
                              (coerce (imagpart value) 'double-float)))
    ((integerp value) (py-long-from-long-long value))
    ((floatp value) (py-float-from-double (coerce value 'double-float)))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-BYTES-OBJECT-P value)
     (clamp-bytes-to-cpython value))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-LIST-OBJECT-P value)
     (clamp-list-to-cpython value))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-TUPLE-OBJECT-P value)
     (clamp-tuple-to-cpython value))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-P value)
     (clamp-dict-to-cpython value))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-SLICE-OBJECT-P value)
     (clamp-slice-to-cpython value))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-RANGE-OBJECT-P value)
     (clamp-range-to-cpython value))
    ((clamp-property-to-cpython value))
    ((clamp-enum-member-to-cpython-value value))
    ((or (|CLAMP.__CLAMP_INTERNALS__|::PY-GENERATOR-OBJECT-P value)
         (|CLAMP.__CLAMP_INTERNALS__|::PY-ZIP-OBJECT-P value))
     (clamp-list-to-cpython
      (apply #'|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-LIST
             (|CLAMP.__CLAMP_INTERNALS__|::PY-ITERABLE-TO-LIST value))))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P value)
     (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER value))
    ((clamp-mapping-object-to-cpython-dict value))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P value)
     (clamp-type-to-cpython-proxy value))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-P value)
     (or (cpython-builtin-object-pointer (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-NAME value))
         (clamp-callable-to-cpython-proxy value)))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-P value)
     (clamp-instance-to-cpython-proxy value))
    (t (py-unicode-from-string
        (with-output-to-string (stream)
          (|CLAMP.__CLAMP_INTERNALS__|:PY-DISPLAY value stream))))))


(defun cpython-truthy-bridge (obj)
  (let ((result (py-object-is-true
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj))))
    (cond
      ((= result 1) t)
      ((= result 0) nil)
      (t
       (if (cpython-error-occurred-p)
           (cpython-raise-current-error "CPython truthiness check failed")
           (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
            |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
            "CPython truthiness check failed"))))))

(defun cpython-callable-bridge (obj)
  (not (zerop (py-callable-check
               (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)))))

(defun cpython-type-of-bridge (obj)
  (wrap-cpython-object
   (py-object-type-c (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj))))

(defun cpython-clamp-type-native-exception-pointer (class)
  (and (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P class)
       (cdr (assoc (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-NAME class)
                   *py-cpython-exception-types*
                   :test #'string=))))

(defun cpython-builtin-wrapper-clamp-type (class)
  (when (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P class)
    (let ((pointer (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER class)))
      (cond
        ((cpython-pointer= pointer *py-cpython-bool-type*) |CLAMP.__CLAMP_INTERNALS__|::*PY-BOOL-TYPE*)
        ((cpython-pointer= pointer *py-cpython-int-type*) |CLAMP.__CLAMP_INTERNALS__|::*PY-INT-TYPE*)
        ((cpython-pointer= pointer *py-cpython-float-type*) |CLAMP.__CLAMP_INTERNALS__|::*PY-FLOAT-TYPE*)
        ((cpython-pointer= pointer *py-cpython-str-type*) |CLAMP.__CLAMP_INTERNALS__|::*PY-STR-TYPE*)
        ((cpython-pointer= pointer *py-cpython-bytes-type*) |CLAMP.__CLAMP_INTERNALS__|::*PY-BYTES-TYPE*)
        ((cpython-pointer= pointer *py-cpython-list-type*) |CLAMP.__CLAMP_INTERNALS__|::*PY-LIST-TYPE*)
        ((cpython-pointer= pointer *py-cpython-tuple-type*) |CLAMP.__CLAMP_INTERNALS__|::*PY-TUPLE-TYPE*)
        ((cpython-pointer= pointer *py-cpython-dict-type*) |CLAMP.__CLAMP_INTERNALS__|::*PY-DICT-TYPE*)
        ((cpython-pointer= pointer *py-cpython-type-callable*) |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-TYPE*)))))

(defun cpython-isinstance-bridge (obj class-or-tuple)
  (cond
    ((and (not (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P obj))
          (cpython-builtin-wrapper-clamp-type class-or-tuple))
     (let ((native-class (cpython-builtin-wrapper-clamp-type class-or-tuple)))
       (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-SUBTYPE-P
        (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-OF obj)
        native-class)))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P class-or-tuple)
     (= (py-object-is-instance
         (clamp-value-to-cpython obj)
         (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER class-or-tuple))
        1))
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P obj)
     (let ((native-class
             (or (cpython-clamp-type-native-exception-pointer class-or-tuple)
                 (and (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P class-or-tuple)
                      (clamp-type-to-cpython-proxy class-or-tuple))
                 (and (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-P class-or-tuple)
                      (cpython-builtin-object-pointer
                       (|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-NAME class-or-tuple)))
                 (clamp-value-to-cpython class-or-tuple))))
       (let ((result (py-object-is-instance
                      (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)
                      native-class)))
         (cond
           ((= result 1) t)
           ((= result 0) nil)
           (t
            (if (cpython-error-occurred-p)
                (cpython-raise-current-error "CPython isinstance failed")
                nil))))))
    (t nil)))

(defun cpython-generic-class-wrapper-p (value)
  (and (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P value)
       (search "typing.Generic"
               (with-output-to-string (stream)
                 (|CLAMP.__CLAMP_INTERNALS__|:PY-DISPLAY value stream))
               :test #'char=)))

(defun clamp-generic-type-p (value)
  (and (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P value)
       (handler-case
           (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P
            (|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR value "__parameters__"))
         (error () nil)
         (|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION () nil))))

(defun cpython-issubclass-bridge (derived class-or-tuple)
  (when (and (clamp-generic-type-p derived)
             (cpython-generic-class-wrapper-p class-or-tuple))
    (return-from cpython-issubclass-bridge t))
  (let ((result (py-object-is-subclass
                 (clamp-value-to-cpython derived)
                 (clamp-value-to-cpython class-or-tuple))))
    (cond
      ((= result 1) t)
      ((= result 0) nil)
      (t
       (if (cpython-error-occurred-p)
           (cpython-raise-current-error "CPython issubclass failed")
           (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
            |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
            "CPython issubclass failed"))))))

(defun cpython-richcompare-op-code (op)
  (case op
    (:lt 0)
    (:le 1)
    (:eq 2)
    (:ne 3)
    (:gt 4)
    (:ge 5)
    (otherwise (error "unknown CPython rich comparison op ~S" op))))

(defun cpython-richcompare-bridge (left right op)
  (let ((result (py-object-rich-compare-bool-c
                 (clamp-value-to-cpython left)
                 (clamp-value-to-cpython right)
                 (cpython-richcompare-op-code op))))
    (cond
      ((= result 1) t)
      ((= result 0) nil)
      (t
       (if (cpython-error-occurred-p)
           (cpython-raise-current-error "CPython rich comparison failed")
           (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
            |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
            "CPython rich comparison failed"))))))

(defun cpython-hash-bridge (obj)
  (let ((result (py-object-hash-c
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj))))
    (if (and (= result -1) (cpython-error-occurred-p))
        (cpython-raise-current-error "CPython hash failed")
        result)))

(defun cpython-len-bridge (obj)
  (let ((result (py-object-length-c
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj))))
    (if (and (= result -1) (cpython-error-occurred-p))
        (cpython-raise-current-error "CPython len failed")
        result)))

(defun cpython-dir-bridge (obj)
  (let* ((pointer (py-object-dir-c
                   (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)))
         (value (cpython-object-to-clamp pointer)))
    (if value
        value
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error "CPython dir failed")
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             "CPython dir failed")))))

(defun cpython-getitem-bridge (obj key)
  (let* ((pointer (py-object-get-item
                   (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)
                   (clamp-value-to-cpython key)))
         (value (if (and pointer
                         (not (null-alien pointer))
                         (or (cpython-object-exact-type-p pointer *py-cpython-list-type*)
                             (cpython-object-exact-type-p pointer *py-cpython-dict-type*)))
                    (wrap-cpython-object pointer)
                    (cpython-object-to-clamp pointer))))
    (if value
        value
        (if (cpython-error-occurred-p)
            (progn
              (py-err-clear)
              (let ((display (with-output-to-string (stream)
                               (|CLAMP.__CLAMP_INTERNALS__|:PY-DISPLAY obj stream))))
                (cond
                  ((or (search "typing" display :test #'char=)
                       (search "Union[" display :test #'char=))
                   (py-err-clear)
                   obj)
                  ((and (stringp key)
                        (> (length display) 0)
                        (char= (char display 0) #\{))
                   (py-err-clear)
                   (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
                    |CLAMP.__CLAMP_INTERNALS__|:*PY-KEY-ERROR-TYPE*
                    key))
                  (t
                   (cpython-raise-current-error "CPython __getitem__ failed")))))
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             "CPython __getitem__ failed")))))

(defun cpython-generic-alias-bridge (origin args)
  (let ((result (py-generic-alias-c (clamp-value-to-cpython origin)
                                    (clamp-value-to-cpython args))))
    (if (and result (not (null-alien result)))
        (wrap-cpython-object result)
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error "CPython generic alias creation failed")
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             "CPython generic alias creation failed")))))

(defun cpython-setitem-bridge (obj key value)
  (let ((result (py-object-set-item
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)
                 (clamp-value-to-cpython key)
                 (clamp-value-to-cpython value))))
    (if (= result 0)
        |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error "CPython __setitem__ failed")
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             "CPython __setitem__ failed")))))

(defun cpython-delitem-bridge (obj key)
  (let ((result (py-object-del-item
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)
                 (clamp-value-to-cpython key))))
    (if (= result 0)
        |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error "CPython __delitem__ failed")
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             "CPython __delitem__ failed")))))

(defun cpython-iter-bridge (obj)
  (let* ((pointer (py-object-get-iter
                   (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)))
         (value (cpython-object-to-clamp pointer)))
    (if value
        value
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error "CPython __iter__ failed")
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             "CPython __iter__ failed")))))

(defun cpython-contains-bridge (item container)
  (let ((result (py-sequence-contains-c
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER container)
                 (clamp-value-to-cpython item))))
    (cond
      ((= result 1) t)
      ((= result 0) nil)
      (t
       (if (cpython-error-occurred-p)
           (cpython-raise-current-error "CPython containment check failed")
           (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
            |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
            "CPython containment check failed"))))))

(defun cpython-number-binary-bridge (left right operation function fallback-message)
  (let* ((pointer (funcall function
                           (clamp-value-to-cpython left)
                           (clamp-value-to-cpython right)))
         (value (cpython-object-to-clamp pointer)))
    (if value
        value
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error fallback-message)
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             (format nil "CPython number ~A failed" operation))))))

(defun cpython-number-unary-bridge (obj operation function fallback-message)
  (let* ((pointer (funcall function
                           (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)))
         (value (cpython-object-to-clamp pointer)))
    (if value
        value
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error fallback-message)
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             (format nil "CPython number ~A failed" operation))))))

(defun cpython-add-bridge (left right)
  (cpython-number-binary-bridge left right "add" #'py-number-add-c
                                "CPython number add failed"))

(defun cpython-sub-bridge (left right)
  (cpython-number-binary-bridge left right "subtract" #'py-number-subtract-c
                                "CPython number subtract failed"))

(defun cpython-mul-bridge (left right)
  (cpython-number-binary-bridge left right "multiply" #'py-number-multiply-c
                                "CPython number multiply failed"))

(defun cpython-truediv-bridge (left right)
  (cpython-number-binary-bridge left right "true divide" #'py-number-true-divide-c
                                "CPython number true divide failed"))

(defun cpython-floordiv-bridge (left right)
  (cpython-number-binary-bridge left right "floor divide" #'py-number-floor-divide-c
                                "CPython number floor divide failed"))

(defun cpython-mod-bridge (left right)
  (cpython-number-binary-bridge left right "remainder" #'py-number-remainder-c
                                "CPython number remainder failed"))

(defun cpython-divmod-bridge (left right)
  (cpython-number-binary-bridge left right "divmod" #'py-number-divmod-c
                                "CPython number divmod failed"))

(defun cpython-pow-bridge (left right)
  (let* ((pointer (py-number-power-c
                   (clamp-value-to-cpython left)
                   (clamp-value-to-cpython right)
                   *py-none*))
         (value (cpython-object-to-clamp pointer)))
    (if value
        value
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error "CPython number power failed")
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             "CPython number power failed")))))

(defun cpython-neg-bridge (obj)
  (cpython-number-unary-bridge obj "negative" #'py-number-negative-c
                               "CPython number negative failed"))

(defun cpython-pos-bridge (obj)
  (cpython-number-unary-bridge obj "positive" #'py-number-positive-c
                               "CPython number positive failed"))

(defun cpython-abs-bridge (obj)
  (cpython-number-unary-bridge obj "absolute" #'py-number-absolute-c
                               "CPython number absolute failed"))

(defun cpython-next-bridge (iterator)
  (let* ((pointer (py-iter-next-c
                   (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER iterator)))
         (value (cpython-object-to-clamp pointer)))
    (cond
      (value value)
      ((cpython-error-occurred-p)
       (cpython-raise-current-error "CPython iterator next failed"))
      (t
       (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE
        |CLAMP.__CLAMP_INTERNALS__|:*PY-STOP-ITERATION*)))))

(defun cpython-object-to-string (obj)
  (python-to-lisp-string
   (py-str (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj))))

(defun cpython-object-to-repr (obj)
  (let ((pointer (py-repr-c
                  (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj))))
    (if (and pointer (not (null-alien pointer)))
        (python-to-lisp-string pointer)
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error "CPython repr failed")
            "<CPython object>"))))

(defun cpython-format-bridge (value format-spec)
  (let* ((pointer (py-object-format-c
                   (clamp-value-to-cpython value)
                   (clamp-value-to-cpython format-spec)))
         (result (cpython-object-to-clamp pointer)))
    (if result
        result
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error "CPython format failed")
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             "CPython format failed")))))

(defun cpython-identity-bridge (left right)
  (cpython-pointer=
   (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER left)
   (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER right)))

(defun python-code-string-literal (value)
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for char across value
          do (case char
               (#\\ (write-string "\\\\" stream))
               (#\" (write-string "\\\"" stream))
               (#\Newline (write-string "\\n" stream))
               (t (write-char char stream))))
    (write-char #\" stream)))

(defun cpython-copy-module-dict-to-clamp (module-pointer clamp-module)
  (let ((dict (and module-pointer
                   (not (null-alien module-pointer))
                   (py-module-get-dict-c module-pointer))))
    (unless (and dict (not (null-alien dict)))
      (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
       |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
       "CPython module dictionary is unavailable"))
    (sb-alien:with-alien ((position long 0)
                          (key (* t))
                          (value (* t)))
      (loop while (/= (py-dict-next-c dict
                                      (sb-alien:addr position)
                                      (sb-alien:addr key)
                                      (sb-alien:addr value))
                      0)
            do (let ((clamp-key (cpython-object-to-clamp key :borrowed t)))
                 (when (and (stringp clamp-key)
                            (not (member clamp-key
                                         '("__builtins__" "__loader__" "__spec__"
                                           "__name__" "__package__" "__file__" "__cached__")
                                         :test #'string=)))
                   (setf (|CLAMP.__CLAMP_INTERNALS__|:PY-OBJECT-ATTR clamp-module clamp-key)
                         (cpython-object-to-clamp value :borrowed t)))))))
  clamp-module)

(defun cpython-exec-pyc-module-bridge (module-name pyc-path clamp-module)
  (with-cpython-runtime-boundary
    (cpython-sync-sys-path)
    (let* ((name-literal (python-code-string-literal module-name))
           (path-literal (python-code-string-literal pyc-path))
           (code (format nil
                         "import importlib.util as _clamp_importlib_util, sys as _clamp_sys~%_clamp_name = ~A~%_clamp_path = ~A~%_clamp_spec = _clamp_importlib_util.spec_from_file_location(_clamp_name, _clamp_path)~%_clamp_module = _clamp_importlib_util.module_from_spec(_clamp_spec)~%_clamp_sys.modules[_clamp_name] = _clamp_module~%_clamp_spec.loader.exec_module(_clamp_module)~%"
                         name-literal
                         path-literal))
           (result (py-run-simple-string code)))
      (when (/= result 0)
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error "CPython .pyc execution failed")
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             "CPython .pyc execution failed")))
      (cpython-copy-module-dict-to-clamp (py-import-add-module-c module-name) clamp-module)
      |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)))

(defun cpython-sync-sys-path ()
  (let* ((paths (|CLAMP.__CLAMP_INTERNALS__|::PY-CURRENT-MODULE-SEARCH-PATHS))
         (literal (format nil "[~{~A~^, ~}]"
                          (mapcar #'python-code-string-literal paths))))
    (py-run-simple-string
     (format nil "import sys~%for p in ~A:~%    if p not in sys.path:~%        sys.path.insert(0, p)~%"
             literal))
    (py-run-simple-string
     "import sys, types
try:
    import typing_extensions as _clamp_te
except Exception:
    _clamp_te = types.ModuleType('typing_extensions')
    sys.modules['typing_extensions'] = _clamp_te
if not hasattr(_clamp_te, 'Sentinel'):
    class Sentinel:
        def __init__(self, name, repr=None):
            self._name = name
            self._repr = repr
        def __repr__(self):
            return self._repr or f'<{self._name}>'
    Sentinel.__module__ = 'typing_extensions'
    _clamp_te.Sentinel = Sentinel
")))

(defun cpython-find-spec-bridge (name &optional path)
  (with-cpython-runtime-boundary
    (cpython-sync-sys-path)
    (let* ((machinery (py-import-module-c "importlib.machinery"))
           (path-finder (and machinery (py-object-get-attr-string machinery "PathFinder")))
           (find-spec (and path-finder (py-object-get-attr-string path-finder "find_spec")))
           (spec (and find-spec
                      (if (and path (not (eq path |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)))
                          (cpython-call-pointer-with-clamp-args
                           find-spec
                           "CPython PathFinder.find_spec failed"
                           name
                           path)
                          (cpython-call-pointer-with-clamp-args
                           find-spec
                           "CPython PathFinder.find_spec failed"
                           name)))))
      (cond
        ((or (not spec) (null-alien spec)) |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
        ((cpython-object-exact-type-p spec *py-cpython-none-type*) |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
        (t (wrap-cpython-object spec))))))

(defun cpython-find-extension-origin-bridge (name)
  (let ((spec (cpython-find-spec-bridge name)))
    (if (and spec (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P spec))
        (let* ((origin (py-object-get-attr-string
                       (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER spec)
                       "origin"))
               (origin-value (cpython-object-to-clamp origin)))
          (if (stringp origin-value) origin-value nil))
        nil)))

(defun cpython-import-module-bridge (name)
  (with-cpython-runtime-boundary
    (cpython-sync-sys-path)
    (let* ((pointer (py-import-module-c name))
           (module (wrap-cpython-object pointer)))
      (if module
          module
          (progn
            (when (cpython-error-occurred-p)
              (py-err-clear))
            nil)))))

(defun cpython-get-attr-bridge (obj name)
  (with-cpython-runtime-boundary
    (let* ((pointer (py-object-get-attr-string
                     (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)
                     name))
           (value (if (and (string= name "__dict__")
                           (cpython-object-exact-type-p pointer *py-cpython-dict-type*))
                      (wrap-cpython-object pointer)
                      (cpython-object-to-clamp pointer))))
      (if value
          value
          (progn
            (when (and (string= name "__globals__")
                       (cpython-error-occurred-p))
              (py-err-clear)
              (return-from cpython-get-attr-bridge
                (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS)))
            (if (cpython-error-occurred-p)
                (cpython-raise-current-error (format nil "CPython object has no attribute ~S" name))
                (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
                 |CLAMP.__CLAMP_INTERNALS__|:*PY-ATTRIBUTE-ERROR-TYPE*
                 (format nil "CPython object has no attribute ~S" name))))))))

(defun cpython-set-attr-bridge (obj name value)
  (let ((result (py-object-set-attr-string
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)
                 name
                 (clamp-value-to-cpython value))))
    (if (= result 0)
        |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error (format nil "CPython setattr failed for ~S" name))
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             (format nil "CPython setattr failed for ~S" name))))))

(defun cpython-del-attr-bridge (obj name)
  (let ((result (py-object-set-attr-string
                 (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER obj)
                 name
                 nil)))
    (if (= result 0)
        |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*
        (if (cpython-error-occurred-p)
            (cpython-raise-current-error (format nil "CPython delattr failed for ~S" name))
            (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
             |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
             (format nil "CPython delattr failed for ~S" name))))))

(defun cpython-keyword-name (keyword)
  (or (get keyword :py-original-keyword-name)
      (string-downcase (symbol-name keyword))))

(defun cpython-split-call-args (args)
  (let ((positional '())
        (keywords '())
        (remaining args)
        (seen-keyword nil))
    (loop while remaining
          do (let ((item (pop remaining)))
               (cond
                 ((keywordp item)
                  (setf seen-keyword t)
                  (unless remaining
                    (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
                     |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-ERROR-TYPE*
                     (format nil "keyword argument ~A has no value" item)))
                  (push (cons (cpython-keyword-name item) (pop remaining)) keywords))
                 (seen-keyword
                  (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
                   |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-ERROR-TYPE*
                   "positional argument follows keyword argument"))
                 (t
                  (push item positional)))))
    (values (nreverse positional) (nreverse keywords))))

(defun cpython-positional-args-to-tuple (args)
  (let ((tuple (py-tuple-new (length args))))
    (loop for arg in args
          for index from 0
          do (let ((result (py-tuple-set-item tuple index (clamp-value-to-cpython-stealable arg))))
               (when (/= result 0)
                 (cpython-raise-current-error "CPython argument tuple conversion failed"))))
    tuple))

(defun cpython-keyword-args-to-dict (keywords)
  (when keywords
    (let ((dict (py-new-dict)))
      (loop for (name . value) in keywords
            do (let ((result (py-dict-set-item dict
                                               (py-unicode-from-string name)
                                               (clamp-value-to-cpython value))))
                 (when (/= result 0)
                   (cpython-raise-current-error "CPython keyword argument conversion failed"))))
      dict)))

(defun cpython-finish-call-result (pointer fallback-message)
  (let ((result (cpython-object-to-clamp pointer)))
    (if result
        result
        (progn
          (if (cpython-error-occurred-p)
              (cpython-raise-current-error fallback-message)
              (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
               |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
               fallback-message))))))

(defun clamp-select-metaclass-for-bases (bases)
  (cond
    ((|CLAMP.__CLAMP_INTERNALS__|::PY-TUPLE-OBJECT-P bases)
     (loop for base in (|CLAMP.__CLAMP_INTERNALS__|::PY-ITERABLE-TO-LIST bases)
           when (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P base)
             do (let ((metaclass (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-TYPE base)))
                  (unless (eq metaclass |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-TYPE*)
                    (return metaclass)))
           finally (return |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-TYPE*)))
    (t |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-TYPE*)))

(defun cpython-prepare-class-bridge (&rest args)
  (let* ((name (first args))
         (bases (if (second args)
                    (second args)
                    (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-TUPLE)))
         (kwds (or (third args)
                   (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS)))
         (metaclass (clamp-select-metaclass-for-bases bases))
         (namespace (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS)))
    (declare (ignore name))
    (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-TUPLE metaclass namespace kwds)))

(defun cpython-types-prepare-class-p (callable)
  (and *py-cpython-types-prepare-class*
       (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P callable)
       (cpython-pointer= (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER callable)
                         *py-cpython-types-prepare-class*)))

(defun cpython-abc-abstractmethod-p (callable)
  (and *py-cpython-abc-abstractmethod*
       (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P callable)
       (cpython-pointer= (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER callable)
                         *py-cpython-abc-abstractmethod*)))

(defun cpython-abstractmethod-bridge (funcobj)
  (|CLAMP.__CLAMP_INTERNALS__|:PY-SETATTR
   funcobj
   "__isabstractmethod__"
   |CLAMP.__CLAMP_INTERNALS__|:*PY-TRUE*)
  funcobj)

(defun cpython-sync-proxy-dict-to-clamp-object (value)
  (when (and (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-P value)
             (not (|CLAMP.__CLAMP_INTERNALS__|::PY-TYPE-P value))
             (not (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P value))
             (not (|CLAMP.__CLAMP_INTERNALS__|::PY-MODULE-OBJECT-P value)))
    (handler-case
        (let ((proxy (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTR value "__cpython_proxy__")))
          (when (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-P proxy)
            (let* ((proxy-pointer (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER proxy))
                   (dict-pointer (py-object-get-attr-string proxy-pointer "__dict__")))
              (when (and dict-pointer (not (null-alien dict-pointer)))
                (let ((dict (cpython-dict-to-clamp dict-pointer)))
                  (clrhash (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTRS value))
                  (let ((keys (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-KEYS dict))
                        (storage (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-STORAGE dict "CPython proxy sync")))
                    (loop for index from 0 below (fill-pointer keys)
                          for key = (aref keys index)
                          when (stringp key)
                            do (setf (gethash key (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-ATTRS value))
                                     (gethash key storage)))))))))
      (error () nil))))

(defun cpython-sync-call-args-to-clamp (positional)
  (dolist (value positional)
    (cpython-sync-proxy-dict-to-clamp-object value)))

(defun cpython-call-bridge (callable &rest args)
  (with-cpython-runtime-boundary
    (multiple-value-bind (positional keywords) (cpython-split-call-args args)
      (when (and (null keywords) (cpython-types-prepare-class-p callable))
        (return-from cpython-call-bridge (apply #'cpython-prepare-class-bridge positional)))
      (when (and (null keywords)
                 (= (length positional) 1)
                 (cpython-abc-abstractmethod-p callable))
        (return-from cpython-call-bridge (cpython-abstractmethod-bridge (first positional))))
      (let* ((tuple (cpython-positional-args-to-tuple positional))
             (kwargs (cpython-keyword-args-to-dict keywords))
             (pointer (py-call
                       (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER callable)
                       tuple
                       kwargs)))
        (when (and pointer
                   (not (null-alien pointer))
                   *py-cpython-object-setattr*
                   (cpython-pointer= (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER callable)
                                     *py-cpython-object-setattr*))
          (cpython-sync-call-args-to-clamp positional))
        (cpython-finish-call-result pointer "CPython callable failed")))))

(defun cpython-call-expanded-bridge (callable positional kwargs)
  (with-cpython-runtime-boundary
    (when (and (or (null kwargs)
                   (eq kwargs |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
                   (and (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-P kwargs)
                        (= (or (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-SIZE kwargs) 0) 0)))
               (cpython-types-prepare-class-p callable))
      (return-from cpython-call-expanded-bridge (apply #'cpython-prepare-class-bridge positional)))
    (when (and (or (null kwargs)
                   (eq kwargs |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)
                   (and (|CLAMP.__CLAMP_INTERNALS__|::PY-DICT-OBJECT-P kwargs)
                        (= (or (|CLAMP.__CLAMP_INTERNALS__|::PY-OBJECT-SIZE kwargs) 0) 0)))
               (= (length positional) 1)
               (cpython-abc-abstractmethod-p callable))
      (return-from cpython-call-expanded-bridge (cpython-abstractmethod-bridge (first positional))))
    (let* ((tuple (cpython-positional-args-to-tuple positional))
           (kwargs-pointer (and kwargs
                                (not (eq kwargs |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))
                                (clamp-dict-to-cpython kwargs)))
           (pointer (py-call
                     (|CLAMP.__CLAMP_INTERNALS__|::PY-CPYTHON-OBJECT-POINTER callable)
                     tuple
                     kwargs-pointer)))
      (cpython-finish-call-result pointer "CPython callable failed"))))

(defun cpython-call-pointer-with-clamp-args (callable fallback-message &rest args)
  (with-cpython-runtime-boundary
    (let* ((tuple (cpython-positional-args-to-tuple args))
           (pointer (py-call callable tuple nil)))
      (if (and pointer (not (null-alien pointer)))
          pointer
          (if (cpython-error-occurred-p)
              (cpython-raise-current-error fallback-message)
              (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
               |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
               fallback-message))))))

(defun cpython-call-pointer-with-pointers (callable fallback-message &rest pointers)
  (with-cpython-runtime-boundary
    (let ((tuple (py-tuple-new (length pointers))))
      (loop for pointer in pointers
            for index from 0
            do (let ((result (py-tuple-set-item tuple index (cpython-incref-if-valid pointer))))
                 (when (/= result 0)
                   (cpython-raise-current-error fallback-message))))
      (let ((result-pointer (py-call callable tuple nil)))
        (if (and result-pointer (not (null-alien result-pointer)))
            result-pointer
            (if (cpython-error-occurred-p)
                (cpython-raise-current-error fallback-message)
                (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
                 |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
                 fallback-message)))))))

(defun cpython-load-extension-module-bridge (name source-path)
  (with-cpython-runtime-boundary
    (cpython-sync-sys-path)
  (unless source-path
    (let ((imported (py-import-module-c name)))
      (when (and imported (not (null-alien imported)))
        (return-from cpython-load-extension-module-bridge
          (wrap-cpython-object imported)))
      (when (cpython-error-occurred-p)
        (py-err-clear))))
  (let* ((util (py-import-module-c "importlib.util"))
         (spec-from-file-location (and util (py-object-get-attr-string util "spec_from_file_location")))
         (module-from-spec (and util (py-object-get-attr-string util "module_from_spec")))
         (spec (and spec-from-file-location
                    (cpython-call-pointer-with-clamp-args
                     spec-from-file-location
                     "CPython extension spec_from_file_location failed"
                     name
                     source-path)))
         (module (and module-from-spec
                      spec
                      (cpython-call-pointer-with-clamp-args
                       module-from-spec
                       "CPython extension module_from_spec failed"
                       (wrap-cpython-object spec))))
         (sys (py-import-module-c "sys"))
         (modules (and sys (py-object-get-attr-string sys "modules")))
         (loader (and spec (py-object-get-attr-string spec "loader")))
         (exec-module (and loader (py-object-get-attr-string loader "exec_module"))))
    (unless (and util spec module sys modules loader exec-module)
      (if (cpython-error-occurred-p)
          (cpython-raise-current-error "CPython extension module loader setup failed")
          (|CLAMP.__CLAMP_INTERNALS__|::PY-RAISE-TYPE
           |CLAMP.__CLAMP_INTERNALS__|:*PY-RUNTIME-ERROR-TYPE*
           "CPython extension module loader setup failed")))
    (let ((set-result (py-dict-set-item modules
                                        (py-unicode-from-string name)
                                        module)))
      (when (/= set-result 0)
        (cpython-raise-current-error "CPython sys.modules registration failed")))
    (cpython-call-pointer-with-pointers
     exec-module
     "CPython extension exec_module failed"
     module)
    (wrap-cpython-object module))))

(defun print-help ()
  (write-line "Usage: clamp [OPTION] [FILE]")
  (write-line "")
  (write-line "Compile Python to Common Lisp and run it.")
  (write-line "")
  (write-line "With no FILE, clamp starts in interactive mode.")
  (write-line "")
  (write-line "Options:")
  (write-line "  -h, --help     Print this help message and exit.")
  (write-line "  -v, --verbose  Print compiler diagnostics and generated Lisp code.")
  (write-line "  -c, --compile-only")
  (write-line "                 Compile Python to Lisp and print the generated code without running it."))

(defun read-code (interactive filename)
  (let ((code nil) (done nil))
    (if interactive
	;; Read input from stdin with a prompt:
	(progn
	  (format t ">>> ")
	  (finish-output)
	  (setf code (read-line *standard-input* nil))
	  (if (or (not code) (string-equal code "quit"))
	      (progn
		(setf done t)
		(setf code nil))))
	;; Read input from the filename on the command line:
	(progn
	  (when *verbose*
	    (write-line (concatenate 'string "Reading code from " filename)))
	  (setf code (uiop:read-file-string filename))
	  (when *verbose*
	    (write-line code))
	  (setf done t)))
    (list code done)))

(defun debug-print-globals-and-locals (py-globals py-locals)
  (write-line "Globals:")
  (write-line (python-to-lisp-string py-globals))
  (write-line "Locals:")
  (write-line (python-to-lisp-string py-locals)))

(defun print-python-exception-line (exception)
  (let ((type-name (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-NAME
                    (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-OF exception)))
        (message (with-output-to-string (stream)
                   (|CLAMP.__CLAMP_INTERNALS__|:PY-DISPLAY exception stream))))
    (princ type-name)
    (unless (string= message "")
      (princ ": ")
      (princ message))
    (terpri)))

(defun handle-interactive-condition (condition)
  (print-python-exception-line
   (|CLAMP.__CLAMP_INTERNALS__|:PY-LISP-ERROR-TO-EXCEPTION condition)))

(defun clamp-compile-source (python-code py-globals py-locals module-name package-name source-path)
  (when *verbose*
    (format t "Preparing to compile: ~A~%" python-code))
  ;; Set local variables to hold the compile request for embedded Python.
  (py-dict-set-item py-locals (py-unicode-from-string "python_source_to_compile") (py-unicode-from-string python-code))
  (py-dict-set-item py-locals (py-unicode-from-string "clamp_module_name") (py-unicode-from-string module-name))
  (py-dict-set-item py-locals (py-unicode-from-string "clamp_package_name") (py-unicode-from-string package-name))
  (py-dict-set-item py-locals
                    (py-unicode-from-string "clamp_source_path")
                    (if source-path
                        (py-unicode-from-string source-path)
                        *py-none*))
  (py-run-string "clamp_compiler(python_source_to_compile, clamp_module_name, clamp_package_name, clamp_source_path)"
                 py-eval-input
                 py-globals
                 py-locals))

(defun eval-generated-lisp (generated-lisp-code interactive)
  (handler-case
      (let ((*package* *package*)
            (result nil)
            (eof (gensym "EOF")))
        (with-input-from-string (stream generated-lisp-code)
          (loop for form = (read stream nil eof)
                until (eq form eof)
                do (progn
                     (when *verbose*
                       (write-line "code-to-run:")
                       (print form)
                       (write-line "")
                       (write-line "")
                       (write-line "running:"))
                     (setf result (eval form)))))
        (when (and interactive (not (eq result |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)))
          (|CLAMP.__CLAMP_INTERNALS__|:PY-DISPLAY result)
          (terpri))
        (when *verbose*
          (write-line "")
          (write-line "Result:")
          (print result)
          (write-line "")))
    (|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION (condition)
      (if interactive
          (print-python-exception-line
           (|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION-VALUE condition))
          (error condition)))
    (error (condition)
      (if interactive
          (handle-interactive-condition condition)
          (error condition)))))

(defun clamp-compile-and-run (python-code py-globals py-locals interactive &optional source-path)
  ;; Invoke the Python code to compile the input Python code to Common Lisp:
  (let ((result (clamp-compile-source python-code py-globals py-locals "__main__" "CLAMP" source-path)))
    (if (cpython-error-occurred-p)
        (py-err-print))
    (when (and result (not (eq *py-none* result)))
      (let ((generated-lisp-code (python-to-lisp-string result)))
        (unless (string= "<NULL>" generated-lisp-code)
          (when *verbose*
            (write-line "Generated Lisp code:")
            (write-line generated-lisp-code))
          (if *compile-only*
              (write-line generated-lisp-code)
              (eval-generated-lisp generated-lisp-code interactive))
          (when *verbose*
            (write-line "")))))))

(defun main ()
  ;; save-lisp-and-die does not save the current package info
  (defpackage :clamp (:use "CLAMP.__builtins__"))
  (in-package :clamp)
  (let* ((interactive t)
	 (done nil)
	 (raw-args (uiop:command-line-arguments))
	 (args (remove-if (lambda (arg)
			    (or (string= arg "-v")
				(string= arg "--verbose")
				(string= arg "-c")
				(string= arg "--compile-only")))
			  raw-args)))
    (setf *verbose* (or (member "-v" raw-args :test #'string=)
			(member "--verbose" raw-args :test #'string=)))
    (setf *compile-only* (or (member "-c" raw-args :test #'string=)
                             (member "--compile-only" raw-args :test #'string=)))
    (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-SYS-ARGV*
          (if (> (length args) 0) args (list "")))
    (cond
      ((member "-h" args :test #'string=)
       (print-help))
      ((member "--help" args :test #'string=)
       (print-help))
      (t
       (when *verbose*
	 (write-line "Startup")
	 (print *package*))

       ;;https://stackoverflow.com/questions/2535478/how-do-i-disable-warnings-in-lisp-sbcl
       (declaim (sb-ext:muffle-conditions cl:warning))

       ;; Demonstration that we can access command line arguments from
       ;; when the Lisp core file is executed. The output changes with
       ;; each invocation.
       (if (> (length args) 0)
	   (progn
	     (setf interactive nil)
	     (when *verbose*
	       (princ "Command line arguments: ")
	       (princ args)
	       (write-line ""))))

       ;; Start up Python inside this process and execute some Python code.
       (py-initialize)
       (setf *cpython-finalizing* nil)
       (unwind-protect
	    (let ((py-globals-and-locals (py-new-dict)))
	      (setf *py-none* (py-run-string "None" py-eval-input py-globals-and-locals py-globals-and-locals))
              (setf *py-cpython-ellipsis*
                    (py-run-string "Ellipsis" py-eval-input py-globals-and-locals py-globals-and-locals))
              (setf |CLAMP.__CLAMP_INTERNALS__|::*PY-ELLIPSIS*
                    (wrap-cpython-object *py-cpython-ellipsis*))
              (setf |CLAMP.__builtins__|:ELLIPSIS
                    |CLAMP.__CLAMP_INTERNALS__|:*PY-ELLIPSIS*)
              (cpython-cache-builtin-types py-globals-and-locals)
              (cpython-install-native-builtins)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-IMPORT-MODULE* #'cpython-import-module-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-LOAD-EXTENSION-MODULE* #'cpython-load-extension-module-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-FIND-EXTENSION-ORIGIN* #'cpython-find-extension-origin-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-FIND-SPEC* #'cpython-find-spec-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-EXEC-PYC-MODULE* #'cpython-exec-pyc-module-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-GENERIC-ALIAS* #'cpython-generic-alias-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-DIR* #'cpython-dir-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-GET-ATTR* #'cpython-get-attr-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-SET-ATTR* #'cpython-set-attr-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-DEL-ATTR* #'cpython-del-attr-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-CALL* #'cpython-call-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-CALL-EXPANDED* #'cpython-call-expanded-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-DISPLAY* #'cpython-object-to-string)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-REPR* #'cpython-object-to-repr)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-FORMAT* #'cpython-format-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-IDENTITY* #'cpython-identity-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-TRUTHY* #'cpython-truthy-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-CALLABLE* #'cpython-callable-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-ISINSTANCE* #'cpython-isinstance-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-ISSUBCLASS* #'cpython-issubclass-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-TYPE-OF* #'cpython-type-of-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-GETITEM* #'cpython-getitem-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-SETITEM* #'cpython-setitem-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-DELITEM* #'cpython-delitem-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-ITER* #'cpython-iter-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-NEXT* #'cpython-next-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-CONTAINS* #'cpython-contains-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-ADD* #'cpython-add-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-SUB* #'cpython-sub-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-MUL* #'cpython-mul-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-TRUEDIV* #'cpython-truediv-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-FLOORDIV* #'cpython-floordiv-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-MOD* #'cpython-mod-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-DIVMOD* #'cpython-divmod-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-POW* #'cpython-pow-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-NEG* #'cpython-neg-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-POS* #'cpython-pos-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-ABS* #'cpython-abs-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-RICHCOMPARE* #'cpython-richcompare-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-HASH* #'cpython-hash-bridge)
              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-CPYTHON-LEN* #'cpython-len-bridge)
	      ;;(write-line (python-to-lisp-string *py-none*))

	      ;; Someday clamp will be self-hosting, but not today, so...
	      ;; Send the compiler code to the Python system to compile the compiler :-P
	      (py-run-string *clamp-compiler-source* py-file-input py-globals-and-locals py-globals-and-locals)
	      (if (cpython-error-occurred-p)
		  (py-err-print))
	      (py-run-string
	       (if *verbose*
		   "CLAMP_VERBOSE = True"
		   "CLAMP_VERBOSE = False")
	       py-file-input
	       py-globals-and-locals
	       py-globals-and-locals)
	      (if (cpython-error-occurred-p)
		  (py-err-print))

              (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-MODULE-LOADER*
                    (lambda (source-path module-name package-name)
                      (let* ((python-code (uiop:read-file-string source-path))
                             (result (clamp-compile-source python-code
                                                           py-globals-and-locals
                                                           py-globals-and-locals
                                                           module-name
                                                           package-name
                                                           source-path)))
                        (if (cpython-error-occurred-p)
                            (py-err-print))
                        (when (and result (not (eq *py-none* result)))
                          (eval-generated-lisp (python-to-lisp-string result) nil)))))

	      (loop while (not done)
		    do (progn
			 (let ((code nil))
			   (destructuring-bind (new-code new-done)
			       (read-code interactive (car args))
			     (progn
			       (setf code new-code)
			       (setf done new-done)))
			   (if code
			       (let ((source-path (and (not interactive) (namestring (truename (car args))))))
                         (when source-path
                           (setf |CLAMP.__CLAMP_INTERNALS__|:*PY-MODULE-SEARCH-PATHS*
                                 (list (namestring (uiop:pathname-directory-pathname source-path))
                                       (namestring (uiop:getcwd)))))
                         (clamp-compile-and-run code
                                                py-globals-and-locals
                                                py-globals-and-locals
                                                interactive
                                                source-path)))))))
	 (progn
           ;; Many CPython extension modules do not tolerate interpreter
           ;; finalization cleanly from an embedded host. The process is about
           ;; to exit, so leave CPython state for the OS to reclaim.
           (setf *cpython-finalizing* t)))))))

;; Save a core file named clamp which, when run, will
;; execute the main function above. It can be run as a
;; normal executable, or more explicitly by running
;; `sbcl --core clamp` and you may want to set the
;; SBCL_HOME environment variable first to make sure that
;; contrib/ packages are available.
(sb-ext:save-lisp-and-die "clamp" :compression t :executable t :toplevel #'main)
