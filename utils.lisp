(in-package #:vtools)

;; todo: *data-file* ought to be modifiable -- all the more reason to get rid of the script altogether and use lisp
(defvar *get-energies-script* "~/src/lisp/vtools/get-energies.sh")
(defvar *grand-directory* (pathname "/scr1/mohrland/"))
(defvar *summary-directory* (merge-pathnames
			     (make-pathname
			      :directory '(:relative "summary"))
			     *grand-directory*))
(defvar *energy-summary-file* (merge-pathnames
			       (make-pathname
				:name "energy-summary"
				:type "org")
			       *summary-directory*))
(defvar *data-file* (make-pathname
		     :name "energies"
		     :type "dat"
		     :directory '(:relative "data")))

(defvar *remote-branch* "origin"
  "Git remote branch that you want to push to")

(defvar *local-branch* "master"
  "Git local branch for summary")

(defvar *pretty-shell* t)

(defun cmd (cmd-str)
  (if *pretty-shell*
      (format t (trivial-shell:shell-command cmd-str))
      (trivial-shell:shell-command cmd-str)))

(defun shellcmd (command-string)
  (trivial-shell:shell-command command-string))

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
(defun conc-with (field-sep &rest strings)
  (format nil
	  (concatenate
	   'string
	   "~{~a~^" (if (characterp field-sep)
			(string field-sep)
			field-sep)
	   "~}") strings))


(defun delimit-string (delimiter string)
  (conc (string delimiter) string (string delimiter)))
(defmacro rmvar (var) `(makunbound ',var))
(defun cube (x) (* x x x))
(defun last1 (list) (first (last list)))
(defun member1 (&rest args) (first (apply #'member args)))

(defun directory-regex-match (path regex)
  (let ((dirs (cl-fad:list-directory path)))
    (loop for d in dirs
       when (cl-ppcre:scan regex (namestring d))
       collect d)))

(defun update-raw-energy-data (path regex)
  "for each directory D matching the regex: collect the final total
energies from all immediate subdirectories into a summary file located
in D/data/"
  (dolist (dir (directory-regex-match path regex))
    (shellcmd (conc-with-spaces
	       "cd" (namestring dir) "&& sh" *get-energies-script*))))

(defun collect-data (&key regex ((:grandparent *grand-directory*) *grand-directory*))
  (let ((dirs (directory-regex-match *grand-directory* regex)))
    (loop for dir in dirs
       collect (make-instance 'job-set
			      :path dir
			      :data-list (latparam-vol-energy-triplets (read-data (merge-pathnames *data-file* dir)))))))

(defclass job-set ()
  ((path
    :initarg :path)
   (data-list
    :initarg :data-list)))


(defmethod lowest-energy ((set job-set))
  (let ((data (slot-value set 'data-list)))
    (iter:iter
      (iter:for d iter:in data)
      (iter:finding d iter:minimizing (third d)))))

;; for testing
(defun random-data () (loop repeat 6 collect 
			   (loop repeat 8 collect 
				(+ (random 1000) (random 1.0)))))

(defun print-org-table (data column-headers &key ((:stream s) t))
  (format s "~&|-|~%")
  (format s "~&|~{~A~^|~}|~%" column-headers)
  (format s "~&|-|~%")
  (loop for datum in data do
       (format s "~&|~A|~{~,6F~^|~}|~%" (first datum) (rest datum)))
  (format s "~&|-|~%"))

(defun write-energy-summary-file (job-sets)
  (let ((table-data (loop for set in job-sets collect
			 (let ((setname (last1 (pathname-directory (slot-value set 'path)))))
			   (cons setname (lowest-energy set))))))
    (with-open-file (out *energy-summary-file*
			 :direction :output
			 :if-exists :supersede :if-does-not-exist :create)
      (format out "* Energy Summary~%")
      (print-org-table table-data '("Type" "Lattice Param." "Volume" "Energy") :stream out))))

(defun summarize (regex &optional (gparent *grand-directory*))
  (update-raw-energy-data gparent regex)
  (let ((data (collect-data :regex regex :grandparent gparent)))
    (write-energy-summary-file data)
    (write-r-data-files data))
  t)

(defun push-summary (&optional (path *summary-directory*))
  (git-commit-all-and-push path))

;; useful for me
(defun summarize-and-push (regex)
  (summarize regex)
  (push-summary))

(defun write-r-data-files (job-sets)
  (loop for set in job-sets do
       (write-r-data-file set)))
(defmethod write-r-data-file ((set job-set) &key outfile)
  (let ((outdata (slot-value set 'data-list))
	(outfile (if outfile
		     outfile
		     (merge-pathnames
		      (make-pathname :name (last1 (pathname-directory (slot-value set 'path)))
				     :type "dat")
		      *summary-directory*))))
    (write-data outfile outdata '(3 6 6))))

;; this is no longer needed -- it's taken care of by get-energies.sh
(defun job-finished-nicely-p (job-dir-path)
  (let ((outcar (make-pathname :name "OUTCAR" :defaults job-dir-path)))
    (cl-ppcre:scan "Voluntary"
		   (trivial-shell:shell-command
		    (conc-with-spaces "tail -2" (namestring outcar))))))

;; todo: consider making a regex class
;; (defmethod read-data (regex)
;;   (cond ((pathname-name path)
	 

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
    
(defun write-data (outfile data-list sigfigs &key (colwid 12))
  (assert (= (length (first data-list))
	     (length sigfigs)))
  (with-open-file (out outfile
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (dolist (d data-list)
      (loop
	 for n in d
	 for sf in sigfigs
	 do (format out (conc "~" (write-to-string colwid) "," (write-to-string sf) "f") n)
	 finally (terpri out)))))

(defun column-transform (data-list transforms)
  (loop for datum in data-list collect
    (loop
       for col in datum
       for tran in transforms
       collect (funcall tran col))))
  

(defun latparam-vol-energy-triplets (data-list)
  (loop for datum in data-list
     collect (let* ((latparam (first datum))
		    (vol (cube latparam)))
	       (cons latparam (cons vol (rest datum))))))

(defun git-command (cmd path)
  (let ((worktree (namestring path)))
    (shellcmd
     (conc-with-char #\space
		     "git"
		     (conc "--git-dir=" worktree ".git/")
		     (conc "--work-tree=" worktree)
		     cmd))))

(defun shell-and (cmds)
  (shellcmd (apply #'conc-with " && " cmds)))
		       
(defun git-commit-all (path)
  (let ((worktree (namestring path)))
    (git-command "add ." worktree)
    (git-command "commit --allow-empty-message -m ''" worktree)))

(defun git-push (path)
  (let ((worktree (namestring path)))
    (git-command (conc-with-char #\space
				 "push"
				 *remote-branch*
				 *local-branch*)
		 worktree)))

(defun git-commit-all-and-push (path)
  (let ((worktree (namestring path)))
    (git-commit-all worktree)
    (git-push worktree)))
