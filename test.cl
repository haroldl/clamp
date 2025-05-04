
;; run via: sbcl --script test.cl

(print 'hello)

(defvar p)
(defvar p-in)
(defvar p-out)

(setq p (run-program "/usr/bin/python" '("test_clamp.py") :wait nil :input :stream :output :stream))

(setq p-in (process-input p))
(setq p-out (process-output p))

(loop
  for i in '(1 2 3 4 5)
  do (progn
	(print (process-status p))
	(sleep 0.5)))

(format p-in "1 + 2~%")

(if (not (eq (process-status p) :exited))
    (sleep 5)) ; seconds

;(loop
;  for char = (read-char-no-hang p-out nil nil)
;  while char
;  do (write-char char))

(print (eval (read p-out)))

(if (not (eq (process-status p) :exited))
    (progn
	(process-kill p 9) ; SIGKILL
	(sleep 5))) ; seconds

(print (process-status p))
