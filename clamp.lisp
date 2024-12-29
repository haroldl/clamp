#!/usr/bin/sbcl --script

(require :uiop)

;; Define the __builtins__ module.
(load "clamp-builtins.lisp")

;; This Lisp package models the top level nameless Python module.
(defpackage "CLAMP"
  (:use "SB-ALIEN" "UIOP" "CLAMP.__builtins__"))

;; https://docs.python.org/3/c-api/veryhigh.html
(load-shared-object "/usr/lib/python3.12/config-3.12-x86_64-linux-gnu/libpython3.12.so")

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

;; PyObject *PyRun_String(const char *str, int start, PyObject *globals, PyObject *locals)
(define-alien-routine ("PyRun_String" py-run-string) (* t)
  (str c-string) (start int) (globals (* t)) (locals (* t)))

;; Read in the Python -> Lisp compiler so that it will be in memory, even in the saved lisp core.
(defvar *clamp-compiler-source* (uiop:read-file-string "clamp_compiler.py"))

(defun main ()
  (let ((interactive t) (done nil) (args (uiop:command-line-arguments)))
    ;; Demonstration that we can access command line arguments from
    ;; when the Lisp core file is executed. The output changes with
    ;; each invocation.
    (if (> (length args) 0)
	(progn
	  (setf done t)
	  (princ "Command line arguments: ")
	  (princ args)
	  (write-line "")))

    ;; TODO: non-interactive mode: use PyRun_File[Ex][Flags] to run files from the command line params.

    ;; Start up Python inside this process and execute some Python code.
    (py-initialize)
    (unwind-protect
	 (progn
	   ;;(write-line "protected")

	   ;; Need to wait until after py-initialize to start making calls:
	   (defvar *py-empty-dict* (py-new-dict))
	   (defun py-eval (code)
	     (py-run-string code 0 *py-empty-dict* *py-empty-dict*))

	   (py-run-simple-string "x = 42")

	   ;; Someday clamp will be self-hosting, but not today, so...
	   ;; Send the compiler code to the Python system to compile the compiler :-P
	   (py-run-simple-string *clamp-compiler-source*)

	   (loop while (not done)
		 do (progn
		      (format t ">>> ")
		      (finish-output)
		      (let ((code (read-line *standard-input* nil)))
			(if (or (not code) (string-equal code "quit"))
			    (setf done t)
			    ;; TODO: switch to using py-eval to invoke the compiler
			    ;;       and get back Common Lisp code, then read/eval it.
			    (py-run-simple-string code))))))
      (progn
	(py-finalize)))))

;; Save a core file named clamp which, when run, will
;; execute the main function above. It can be run as a
;; normal executable, or more explicitly by running
;; `sbcl --core clamp` and you may want to set the
;; SBCL_HOME environment variable first to make sure that
;; contrib/ packages are available.
(sb-ext:save-lisp-and-die "clamp" :compression t :executable t :toplevel #'main)
