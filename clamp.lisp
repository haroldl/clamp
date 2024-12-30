#!/usr/bin/sbcl --script

(require :uiop)

;; Define the __builtins__ module.
(load "clamp-builtins.lisp")

;; This Lisp package models the top level nameless Python module.
(defpackage "CLAMP"
  (:use "SB-ALIEN" "UIOP" "CLAMP.__builtins__"))

;; https://docs.python.org/3/c-api/veryhigh.html
(load-shared-object "/usr/lib/python3.12/config-3.12-x86_64-linux-gnu/libpython3.12.so")

;; Read in the Python -> Lisp compiler so that it will be in memory, even in the saved lisp core.
(defconstant *clamp-compiler-source* (uiop:read-file-string "clamp_compiler.py"))

;;
;; Map the Python C API into Lisp:
;;

;; The SBCL alien type (* t) is a pointer to anything, and is used in many places here to
;; store a pointer to a PyObject which is an opaque type to us - we just need to store
;; references to PyObject values and pass them back to Python later.

(define-alien-routine ("Py_Initialize" py-initialize) void)
(define-alien-routine ("Py_Finalize" py-finalize) void)

(define-alien-routine ("PyRun_SimpleString" py-run-simple-string) int (str c-string))

(define-alien-routine ("PyDict_New" py-new-dict) (* t))

;; https://docs.python.org/3/c-api/unicode.html
(define-alien-routine ("PyUnicode_FromString" py-unicode-from-string) (* t) (input c-string))

;; The int* is an output parameter for the string length which we don't need;
;; it is safe to just pass in nil.
(define-alien-routine ("PyUnicode_AsUTF8AndSize" py-value-to-utf8) c-string
  (python-value (* t)) (output-size (* int)))

;; Call the str() function on a value to convert it to a Python string value.
(define-alien-routine ("PyObject_Str" py-str) (* t) (python-object (* t)))

(defun python-to-lisp-string (python-value)
  (py-value-to-utf8 (py-str python-value) nil))

;; PyObject *PyRun_String(const char *str, int start, PyObject *globals, PyObject *locals)
;; The `start` param should be one of the constants below:
;; (py-single-input, py-file-input, py-eval-input)
(define-alien-routine ("PyRun_String" py-run-string) (* t)
  (str c-string) (start int) (globals (* t)) (locals (* t)))

;; Constants from the Python.h include file and its friends that we need:
(defconstant py-single-input 256)
(defconstant py-file-input 257)
(defconstant py-eval-input 258)

(define-alien-routine ("PyTuple_New" py-tuple-new) (* t) (arity int))
(define-alien-routine ("PyTuple_SetItem" py-tuple-set-item) int (tuple (* t)) (position int) (value (* t)))

(define-alien-routine ("PyObject_Call" py-call) (* t) (callable (* t)) (args (* t)) (kwargs (* t)))

;; https://docs.python.org/3/c-api/dict.html
(define-alien-routine ("PyDict_SetItem" py-dict-set-item) int (dict (* t)) (key (* t)) (value (* t)))
(define-alien-routine ("PyDict_Merge" py-dict-merge) int (target-dict (* t)) (source-dict (* t)) (override int))

;; Exception handling
;; https://docs.python.org/3/c-api/exceptions.html
(define-alien-routine ("PyErr_Occurred" py-err-occurred) (* t))
(define-alien-routine ("PyErr_Print" py-err-print) void)

(defun read-code (interactive filename)
  (let ((code nil) (done nil))
    (if interactive
	;; Read input from stdin with a prompt:
	(progn
	  (format t ">>> ")
	  (finish-output)
	  (setf code (read-line *standard-input* nil))
	  (if (or (not code) (string-equal code "quit"))
	      (setf done t)))
	;; Read input from the filename on the command line:
	(progn
	  (write-line (concatenate 'string "Reading code from " filename))
	  (setf code (uiop:read-file-string filename))
	  (write-line code)
	  (setf done t)))
    (list code done)))

(defun debug-print-globals-and-locals (py-globals py-locals)
  (write-line "Globals:")
  (write-line (python-to-lisp-string py-globals))
  (write-line "Locals:")
  (write-line (python-to-lisp-string py-locals)))

(defun clamp-compile-and-run (python-code py-globals py-locals)
  ;; Set a local variable to hold the code to be compiled:
  (py-dict-set-item py-locals (py-unicode-from-string "python_source_to_compile") (py-unicode-from-string python-code))

  ;; Invoke the Python code to compile the input Python code to Common Lisp:
  (let ((result (py-run-string "compile(python_source_to_compile)" py-eval-input py-globals py-locals)))
    (if (py-err-occurred)
	(py-err-print))
    (if result
	(let ((generated-lisp-code (python-to-lisp-string result)))
	  (write-line "Generated Lisp code:")
	  (write-line generated-lisp-code)
	  (print
	   (eval
	    (read-from-string generated-lisp-code)))
	  (write-line "")))))

(defun main ()
  (let ((interactive t) (done nil) (args (uiop:command-line-arguments)))
    ;; Demonstration that we can access command line arguments from
    ;; when the Lisp core file is executed. The output changes with
    ;; each invocation.
    (if (> (length args) 0)
	(progn
	  (setf interactive nil)
	  (princ "Command line arguments: ")
	  (princ args)
	  (write-line "")))

    ;; Start up Python inside this process and execute some Python code.
    (py-initialize)
    (unwind-protect
	 (let ((py-globals (py-new-dict))
	       (py-locals (py-new-dict)))

	   ;; Someday clamp will be self-hosting, but not today, so...
	   ;; Send the compiler code to the Python system to compile the compiler :-P
	   (py-run-string *clamp-compiler-source* py-file-input py-globals py-locals)
	   (if (py-err-occurred)
	       (py-err-print))

	   ;; Copy all of the locals into the globals for future use:
	   (py-dict-merge py-globals py-locals 1)
	   (setf py-locals (py-new-dict))

	   (loop while (not done)
		 do (progn
		      (let ((code nil))
			(destructuring-bind (new-code new-done)
			    (read-code interactive (car args))
			  (progn
			    (setf code new-code)
			    (setf done new-done)))
			(if code
			    (clamp-compile-and-run code py-globals py-locals))))))
      (progn
	(py-finalize)))))

;; Save a core file named clamp which, when run, will
;; execute the main function above. It can be run as a
;; normal executable, or more explicitly by running
;; `sbcl --core clamp` and you may want to set the
;; SBCL_HOME environment variable first to make sure that
;; contrib/ packages are available.
(sb-ext:save-lisp-and-die "clamp" :compression t :executable t :toplevel #'main)
