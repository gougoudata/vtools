(in-package :vtools)

;; from On Lisp:
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                        (car x)
                        (rec (cdr x) acc))))))
    (rec x nil)))

;; from Let Over Lambda:
(defun |#`-reader| (stream sub-char numarg)
	    (declare (ignore sub-char))
	      (unless numarg (setq numarg 1))
	        `(lambda ,(loop for i from 1 to numarg
			                         collect (symb 'a i))
		        ,(funcall
			          (get-macro-character #\`) stream nil)))
(set-dispatch-macro-character
   #\# #\` #'|#`-reader|)


;; probably should be utilities:
(defvar *pretty-shell* t)
(defun cmd (cmd-str)
  (if *pretty-shell*
      (format t (trivial-shell:shell-command cmd-str))
      (trivial-shell:shell-command cmd-str)))
	      
(defun conc (&rest strings) (apply #'concatenate 'string strings))
(defun conc-with-spaces (&rest strings)
  (loop
     for string in (rest strings)
     with result = (first strings)
     do (setq result (conc result " " string))
     finally (return result)))
(defmacro rmvar (var) `(makunbound ',var))
(defun cube (x) (* x x x))

(defun alist<-datafile (file)
    (flet ((f (str) 
	   (with-input-from-string (str str) 
	     (let ((x (read str))
		   (y (read str)))
	       (cons x y)))))
    (let ((lines (with-open-file (in file :direction :input)
		   (loop for line = (read-line in nil)
		      while line collect line))))
      (mapcar #'f lines))))

