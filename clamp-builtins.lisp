(defpackage "CLAMP.__builtins__"
  (:use :cl)
  (:export :test :dir :plus :times))

(in-package "CLAMP.__builtins__")

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
