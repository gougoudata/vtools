;;;; vtools.lisp

(in-package #:vtools)

(defvar *job-root-directory* "/scr1/mohrland/")
(defvar *energy-stream*)
(defvar *get-energies-script* "/homes/mohrland/system/psr/get-energies.sh")
(defun dirs-matching-regex (parent-path regex)
  (flet ((pred (x) (cl-ppcre:scan regex (namestring x))))
    (remove-if-not #'pred (cl-fad:list-directory parent-path))))

(defun git-update (parent-dir regex-for-dirs)
  (let ((dirstrings (mapcar #'namestring (dirs-matching-regex parent-dir regex-for-dirs))))
    (loop
       for dirstring in dirstrings
       do (trivial-shell:shell-command (conc "cd " dirstring " && sh " *get-energies-script*)))))

(defmacro with-git-update ((parent-dir regex-for-dirs) &rest forms)
  `(progn
     (git-update ,parent-dir ,regex-for-dirs)
     ,@forms))

(defun read-energies-into-alist (file)
  (flet ((f (str) 
	   (with-input-from-string (str str) 
	     (let ((x (read str))
		   (y (read str)))
	       (cons x y)))))
    (let ((lines (with-open-file (in file :direction :input)
		   (loop for line = (read-line in nil)
		      while line collect line))))
      (remove-duplicates (mapcar #'f lines) :test #'= :key #'car))))

(defun multiple-read-energies-into-alist (parent-dir regex-for-dirs)
  (git-update parent-dir regex-for-dirs)
  (let ((dir-strings (mapcar #'namestring (dirs-matching-regex parent-dir regex-for-dirs))))
    (loop
       for dir-string in dir-strings
       collect (cons dir-string (read-energies-into-alist (conc dir-string "data/energies.dat"))))))

(defun min-energy-of-energy-alist (energy-alist)
  (format *energy-stream* "~a minimum energy:~%" (car energy-alist))
  (cond ((cdr energy-alist)
	 (let* ((energies (cdr energy-alist))
		(min (loop
			for pair in energies
			minimize (cdr pair)))
		(latparam (car (rassoc min energies))))
	   (format *energy-stream* "~,6f A (~,6f A^3)     ~,6f eV~%~%" latparam (cube latparam) min)))
	(t (format *energy-stream* "NO ENERGIES~%~%"))))

(defun scale-energies (energy-alist factor)
  (flet ((f (pair) (cons (car pair) (* factor (cdr pair)))))
  (cons (car energy-alist)
	(mapcar #'f (cdr energy-alist)))))

(defun remove-dups-of-energies (energy-alist)
  (remove-duplicates (cdr energy-alist) :test #'= :key #'car))


(defun double-bcc-energies (energies-list)
  (flet ((pred (x) (cl-ppcre:scan "bcc" (car x)))
	 (double-energies (alist) (scale-energies alist 2.0)))
    (mapcar #'(lambda (x) (if (pred x) (double-energies x) x)) energies-list )))

(defun quarter-hcp-energies (energies-list)
  (flet ((pred (x) (cl-ppcre:scan "hcp" (car x)))
	 (quarter-energies (alist) (scale-energies alist .25)))
    (mapcar #'(lambda (x) (if (pred x) (quarter-energies x) x)) energies-list )))

(defun halve-hcp-energies (energies-list)
  (flet ((pred (x) (cl-ppcre:scan "hcp" (car x)))
	 (halve-energies (alist) (scale-energies alist .5)))
    (mapcar #'(lambda (x) (if (pred x) (halve-energies x) x)) energies-list )))


(defun assess-energies (parent-dir regex-for-dirs &optional (stream t stream-supplied-p))
  (progn
    (git-update parent-dir regex-for-dirs)
    (let ((ens (multiple-read-energies-into-alist parent-dir regex-for-dirs))
	  (*energy-stream* (if stream-supplied-p stream t)))
      (mapcar #'min-energy-of-energy-alist (double-bcc-energies (halve-hcp-energies ens))))))
  
(defun adjust-energies (parent-dir regex-for-dirs)
  (progn
    (git-update parent-dir regex-for-dirs)
    (let* ((ens (multiple-read-energies-into-alist parent-dir regex-for-dirs))
	   (adj-ens (double-bcc-energies (halve-hcp-energies ens))))
      (loop
	 for alist in adj-ens
	 do (with-open-file (outfile (conc (car alist) "data/" (car (last (ppcre:split "/" (car alist)))) ".energies.adjusted.dat")
				     :direction :output :if-exists :supersede)
	      (loop
		 for datum in (cdr alist)
		 do (format outfile "~,6f ~,6f~%" (car datum) (cdr datum))))))))

(defun adjust-energies-and-volumes (parent-dir regex-for-dirs)
  (progn
    (git-update parent-dir regex-for-dirs)
    (flet ((cube (x) (* x x x)))
      (let* ((ens (multiple-read-energies-into-alist parent-dir regex-for-dirs))
	     (adj-ens (double-bcc-energies (halve-hcp-energies ens))))
	(loop
	   for alist in adj-ens
	   do (with-open-file
		  (outfile (conc (car alist) "data/" (car (last (ppcre:split "/" (car alist)))) ".energies.vol.adjusted.dat")
			   :direction :output :if-exists :supersede)
		(loop
		   for datum in (cdr alist)
		   do (format outfile "~,6f ~,6f~%" (cube (car datum)) (cdr datum)))))))))

		   
(defun summarize-energies (parent-dir regex-for-dirs &key noisyp)
  (let* ((ens (multiple-read-energies-into-alist parent-dir regex-for-dirs))
	 (adjusted-ens (double-bcc-energies (halve-hcp-energies ens)))
	 (summary-dir (conc *job-root-directory* "summary/")))
    (let ((file (conc summary-dir "energies-summary.dat")))
      (with-open-file (s file :direction :output :if-exists :supersede)
	(let ((*energy-stream* s))
	  (mapcar #'min-energy-of-energy-alist adjusted-ens))))
    (loop
       for alist in ens do
       (let* ((file-string (car alist))
	      (data (cdr alist))
	      (outfile (conc *job-root-directory* "summary/" (car (last (ppcre:split "/" file-string)))
			     ".energies-vs-volume.unadjusted.dat")))
	 (datafile<-alist (cdr alist) outfile :transform1 #'cube)))
    (loop
       for alist in adjusted-ens do
       (let* ((file-string (car alist))
	      (data (cdr alist))
	      (outfile (conc *job-root-directory* "summary/" (car (last (ppcre:split "/" file-string)))
			     ".energies-vs-volume.adjusted.dat")))
	 (datafile<-alist (cdr alist) outfile :transform1 #'cube))))
  (git-pushall (conc *job-root-directory* "summary/"))
  (when noisyp (cat-file (conc *job-root-directory* "summary/energies-summary.dat"))))

(defun datafile<-alist (alist outfile &key (transform1 #'identity) (transform2 #'identity))
	   (with-open-file (out outfile :direction :output :if-exists :supersede)
	     (loop
		for datum in alist do
		(format out "~,6f ~,6f~%"
			(funcall transform1 (car datum))
			(funcall transform2 (cdr datum))))))
		   
(defun file-tranform (infile col1trans col2trans outfile)
  (let ((data (alist<-datafile infile)))
    (with-open-file (out outfile :direction :output :if-exists :supersede)
      (loop
	 for datum in data
	 do (format out "~,6f ~,6f~%"
		    (funcall col1trans (car datum))
		    (funcall col2trans (cdr datum)))))))
      
(defun git-command (cmd-string worktree)
  (cmd (conc-with-spaces
	"git" (conc "--git-dir=" worktree ".git/")
	(conc "--work-tree=" worktree) cmd-string)))

(defun git-ls (worktree)
  (git-command "ls" worktree))
		     
(defun git-commit-all (worktree)
  (git-command "add ." worktree)
  (git-command "commit --allow-empty-message -m ''" worktree))

(defun git-push (worktree)
  (git-command "push origin master" worktree))

(defun git-pushall (worktree)
  (git-commit-all worktree)
  (git-push worktree))

(defun job-finished-nicely-p (job-dir-path)
  (let ((outcar (make-pathname :name "OUTCAR" :defaults job-dir-path)))
    (cl-ppcre:scan "Voluntary"
		   (trivial-shell:shell-command
		    (conc-with-spaces "tail -2" (namestring outcar))))))
