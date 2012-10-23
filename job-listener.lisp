(in-package #:vtools)

(defun cores-in-use ()
  (length (ppcre:split #\newline
		       (shellcmd "ps aux | grep vasp | grep -v 'grep vasp'"))))

(defun cores-free ()
  (- *number-of-cores* (cores-in-use)))

(defun machine-free-p ()
  (= 0 (cores-in-use)))

(defun job-listener-loop (seconds)
  (loop until (machine-free-p)
     do (sleep seconds)))

(defun jobs-done-p ()
  "listens till no jobs are being run, then doublechecks briefly, in case it happened to check in between jobs."
  (job-listener-loop 60)
  (sleep 10)
  (machine-free-p))
  
