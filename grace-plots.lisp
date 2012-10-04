(in-package #:vtools)

(defparameter *plot* nil
  "Grace plot object.")

(defun new-plot (&optional barebones)
  "Bind *plot* to an empty plot that we can print to."
  (setf *plot* (grace:new-plot (if barebones "-barebones" ""))))
  
(defparameter *data-set-number* 0
  "Data set number. Will start from zero when initialized.")

(defun data-set-string ()
  (conc "s" (write-to-string *data-set-number*)))

(defun grace-point-string (x y)
  "Give me a data set number and two points, and I'll give you a string you can feed to Grace that represents that point."
  (format nil "g0.~a point ~a, ~a" (data-set-string) x y))

;; (defun new-data-set! ()
;;   (if (not *data-set-number*)
;;       (setf *data-set-number* 0)
;;       (incf *data-set-number*))
;;   (grace:send-command *plot* (format nil "~a on" *data-set-number*)))

(defmacro with-grace-redraw (&body strings)
  "Bind *plot* to the plot you want to print before calling me. Give me the strings you want to send to Grace, and I'll send the strings to the plot process, and then redraw the plot for you."
  `(progn ,@(loop for cmd in strings collect (list 'grace:send-command '*plot* cmd))
	  (grace:send-command *plot* "redraw")))

(defun plot-these (points xindex yindex &key (symbol 1) (size 0.3) (fill-pattern 1))
  "Give me a list of lists and two integers. I'll loop through the the lists, and for each list l I'll plot point ( (nth xindex l),(nth yindex l) ) and redraw at the end. Indices start at zero. Bind the special variables *data-set-number* and *plot* before you call me. Use my keyword args or use their defaults."
  (let ((setstr (data-set-string)))
    (with-grace-redraw 
      (format nil "~a on" setstr)
      (format nil "~a symbol ~A" setstr symbol)
      (format nil "~a symbol size ~A" setstr size)
      (format nil "~a symbol fill pattern ~A" setstr fill-pattern))
    (dolist (p points)
      (grace:send-command *plot* (grace-point-string (nth xindex p) (nth yindex p))))
    (grace:send-command *plot* "redraw")))

;; save for nostalgia's sake
;; (defmacro grace-plot-energies (name data)
;;   (with-grace-redraw (plot)
;;     (apply #'append
;; 	   (loop
;; 	      for en in ens
;; 	      for setnumber = 0 then (1+ setnumber)
;; 	      collect (append (list
;; 			       (format nil "~a on" setnumber)
;; 			       (format nil "~a symbol 1" setnumber)
;; 			       (format nil "~a symbol size 0.3" setnumber)
;; 			       (format nil "~a symbol fill pattern 1" setnumber))
;; 			      (loop
;; 				 for pair in (cdr en)
;; 				 collect (format nil "g0.s~d point ~a ~a"
;; 						 setnumber (car pair) (cdr pair))))))))
;; (defmacro defplot ((symbol) &rest settings)
;;   `(progn
;;      (defparameter ,symbol (grace:new-plot "-barebones"))
;;      ,(when settings
;; 	    `(with-grace-redraw (,symbol) ,@settings))))
