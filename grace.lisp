(in-package #:vtools)

(defvar plot (grace:new-plot "-barebones"))
(defvar data-set-num 0)

(defun grace-point-string (setnum x y)
  (format nil "g0.s~d point ~a ~a" setnum x y))
(defmacro with-grace-redraw ((plot) cmd-strings)
  `(progn
     ,@(loop
          for cmd in cmd-strings
          collect (list 'grace:send-command plot cmd))
     (grace:send-command ,plot "redraw")))

(defmacro defplot ((symbol) &rest settings)
  `(progn
     (defparameter ,symbol (grace:new-plot "-barebones"))
     (with-grace-redraw (,symbol) ,@settings)))

(defun grace-plot-energies (parent-dir regex-for-dirs)
  (let ((ens (multiple-read-energies-into-alist
	      parent-dir regex-for-dirs)))
    (with-grace-redraw (plot)
      (apply #'append
	     (loop
		for en in ens
		for setnumber = 0 then (1+ setnumber)
		collect (append (list
				 (format nil "~a on" setnumber)
				 (format nil "~a symbol 1" setnumber)
				 (format nil "~a symbol size 0.3" setnumber)
				 (format nil "~a symbol fill pattern 1" setnumber))
				(loop
				   for pair in (cdr en)
				   collect (format nil "g0.s~d point ~a ~a"
						   setnumber (car pair) (cdr pair)))))))))
