#!/usr/bin/sbcl --script

(require :uiop)

(defpackage "CLAMP"
  (:use "SB-ALIEN" "UIOP"))


;; https://docs.python.org/3/c-api/veryhigh.html
(load-shared-object "/usr/lib/python3.12/config-3.12-x86_64-linux-gnu/libpython3.12.so")

(define-alien-routine ("Py_Initialize" py-initialize) void)
(define-alien-routine ("Py_Finalize" py-finalize) void)
(define-alien-routine ("PyRun_SimpleString" py-run-simple-string) int (str c-string))

;; Read in the Python -> Lisp compiler so that it will be in memory, even in the saved lisp core.
(defvar *clamp-compiler-source* (uiop:read-file-string "clamp_compiler.py"))

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

    ;; TODO: non-interactive mode: use PyRun_File[Ex][Flags] to run files from the command line params.

    ;; Start up Python inside this process and execute some Python code.
    (py-initialize)
    (unwind-protect
	 (progn
	   (write-line "protected")

	   ;; Someday clamp will be self-hosting, but not today, so...
	   ;; Send the compiler code to the Python system to compile the compiler :-P
	   (py-run-simple-string *clamp-compiler-source*)

	   (py-run-simple-string "x = 72")
	   (py-run-simple-string "print(x + 5)")
	   (py-run-simple-string "print(3 + 5)")
	   (loop while (not done)
		 do (progn
		      (format t ">>> ")
		      (finish-output)
		      (let ((code (read-line)))
			(if (string-equal code "quit")
			    (setf done t)
			    ;; TODO: switch to PyRun_StringFlags which returns the result and print it out.
			    (py-run-simple-string code))))))
      (progn
	(py-finalize)
	(write-line "cleaned up")))))

;; Save a core file named clamp which, when run, will
;; execute the main function above. It can be run as a
;; normal executable, or more explicitly by running
;; `sbcl --core clamp` and you may want to set the
;; SBCL_HOME environment variable first to make sure that
;; contrib/ packages are available.
(sb-ext:save-lisp-and-die "clamp" :compression t :executable t :toplevel #'main)
