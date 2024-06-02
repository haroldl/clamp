#!/usr/bin/sbcl --script

(defpackage "CLAMP"
  (:use "SB-ALIEN"))
				;
(load-shared-object "/usr/lib/python3.12/config-3.12-x86_64-linux-gnu/libpython3.12.so")

(define-alien-routine ("Py_Initialize" py-initialize) void)
(define-alien-routine ("Py_Finalize" py-finalize) void)
(define-alien-routine ("PyRun_SimpleString" py-run-simple-string) int (str c-string))

(defun main ()
  (py-initialize)
  (unwind-protect
       (progn
	 (write-line "protected")
	 (py-run-simple-string "x = 72")
	 (py-run-simple-string "print(x + 5)")
	 (py-run-simple-string "print(3 + 5)"))
    (progn
      (py-finalize)
      (write-line "cleaned up"))))

;; Save a core file named clamp which, when run, will
;; execute the main function above. It can be run as a
;; normal executable, or more explicitly by running
;; `sbcl --core clamp` and you may want to set the
;; SBCL_HOME environment variable first to make sure that
;; contrib/ packages are available.
(sb-ext:save-lisp-and-die "clamp" :compression t :executable t :toplevel #'main)
