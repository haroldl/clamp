#!/usr/bin/sbcl --script

(defpackage "CLAMP"
  (:use "SB-ALIEN"))
				;
(load-shared-object "/usr/lib/python3.11/config-3.11-x86_64-linux-gnu/libpython3.11.so")

(define-alien-routine ("Py_Initialize" py-initialize) void)
(define-alien-routine ("Py_Finalize" py-finalize) void)
(define-alien-routine ("PyRun_SimpleString" py-run-simple-string) int (str c-string))

(py-initialize)
(unwind-protect
     (progn
       (write-line "protected")
       (py-run-simple-string "x = 72")
       (py-run-simple-string "print(x + 5)")
       (py-run-simple-string "print(3 + 5)"))
  (progn
    (py-finalize)
    (write-line "cleaned up")))
