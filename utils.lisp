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


(defvar *pretty-shell* t)

(defun cmd (cmd-str)
  (if *pretty-shell*
      (format t (trivial-shell:shell-command cmd-str))
      (trivial-shell:shell-command cmd-str)))
	      
(defun cat-file (file &optional (out t))
  (with-open-file (in file)
    (loop for line = (read-line in nil)
	 while line do (format out "~a~%" line))))

(defun mkdir (pathname)
  (ensure-directories-exist pathname))

(defun conc (&rest strings) (apply #'concatenate 'string strings))
(defun conc-with-spaces (&rest strings)
  (apply #'conc-with-char #\space strings))
(defun conc-with-char (field-separator &rest strings)
    (loop
     for string in (rest strings)
     with result = (first strings)
     do (setq result (conc result (string field-separator) string))
     finally (return result)))
(defun delimit-string (delimiter string)
  (conc (string delimiter) string (string delimiter)))
(defmacro rmvar (var) `(makunbound ',var))
(defun cube (x) (* x x x))

(defun update-raw-energy-data (parent-dir regex-for-dirs)
    (loop
       for dir in (dirs-matching-regex parent-dir regex-for-dirs)
       do (trivial-shell:shell-command (conc-with-spaces "cd" (namestring dir) "&& sh" *get-energies-script*))))

(defun read-data (file)
  (with-open-file (in file :direction :input)
    (loop for line = (read-line in nil)
       while line
       collect
	 (let ((return-list nil))
	   (with-input-from-string (s line)
	     (loop for word = (read s nil nil)
		while word do (push word return-list)))
	   (nreverse return-list)))))
    
(defun write-data (outfile data-list sigfigs)
  (assert (= (length data-list)
	     (length sigfigs)))
  (with-open-file (out outfile
		       :direction :output
		       :if-exists :supersede)
    (dolist (d data-list)
      (loop
	 for n in d
	 for sf in sigfigs
	 do (format out (conc "~," (write-to-string sf) "f" "    ") n)
	 finally (terpri out)))))

(defun column-transform (data-list transforms)
  (loop for d in data-list collect
    (loop
       for n in d
       for tr in transforms
       collect (funcall tr n))))
  
;; one-off
(defun cubed-en-triplets (energy-list)
  (loop for en in energy-list
     collect (cons (car en)
		   (cons (cube (car en))
			 (cdr en)))))

