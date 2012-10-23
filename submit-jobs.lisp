;;;; submit-jobs.lisp

(in-package #:vtools)

(defvar *after-jobs-hooks* nil
  "Holds functions to be run after jobs are done. functions take zero arguments. (I may end up paying for this later.) Make wise use of dynamic variables.")

(defvar *mpirun* (pathname "/local/openmpi-gcc/bin/mpirun")
  "the path to your mpirun binary.")
(defvar *vasp* (pathname "/homes/mohrland/bin/vasp")
  "the path to your vasp binary.")

(defvar *number-of-cores* 12
  "how many cores are you going to use to run the jobs?")

(defvar *jobs-running-p* nil)

;; util
(defun ../ (path) (make-pathname :directory (butlast (pathname-directory path))))

(defun file-path (dirpath filestring &optional type)
  (make-pathname :directory (pathname-directory dirpath)
				   :name filestring
				   :type type))

(defun dojobs (&key path low high step includes excludes (kpoints-list '(11 11 11)))
  "DOJOBS takes keyword arguments: a path, a range, a step size, a
list of numbers to exclude, (a man, a plan, a canal, Panama!), a list
of numbers to include, and a list of kpoints dimensions (with 11x11x11
as the default). It then assumes each number corresponds to a lattice
parameter and loops across the numbers, creating directories for each
job and submitting that job to Vasp. You must have a template INCAR,
KPOINTS, POSCAR, and POTCAR in the job parent directory.  WARNING:
This overwrites directories with the same names in job parent
directory!"
  (assert (null (intersection excludes includes :test #'=)))
  (let* ((low (rationalize low))
	 (high (rationalize high))
	 (step (rationalize step))
	 (params (nconc (set-difference (loop for p from low to high by step
					   collect (float p))
					excludes
					:test #'=)
			includes)))
    (loop for p in params do
	 (let ((jobpath (merge-pathnames (make-pathname :directory
							(list :relative (format nil "~,3f" p)))
					 path)))
	   (prepare-vasp-files path p kpoints-list)
	   (format t "Job ~a submitted... " jobpath)
	   (submit-single-job jobpath)
	   (format t "Job stopped.~%")
	   )))
  (format t "~&All jobs stopped.~%"))

(defun submit-single-job (pathname)
  (trivial-shell:shell-command
   (conc-with-spaces
    (namestring *mpirun*)
    "-wdir"
    (namestring pathname)
    "-np"
    (write-to-string *number-of-cores*)
    (namestring *vasp*))))

(defun replace-line (&key infile outfile replace-line string if-exists)
  "Pipes :infile to :outfile with line :replace-line replaced with :string.
:replace-line may be nil if there is to be no replacement and the file
is to merely be copied. Line numbers start at 0."
  (with-open-file (out outfile :direction :output :if-exists if-exists)
    (with-open-file (in infile :direction :input)
      (when replace-line
	(loop
	   repeat replace-line
	   for line = (read-line in)
	   while line do (format out "~a~%" line))
	(format out "~a~%" string)
	(read-line in nil nil))
      (loop
	 for line = (read-line in nil nil)
	 while line do (format out "~a~%" line)))))


(defun prepare-vasp-files (path latparam &optional kpoints-list)
  (let ((jobpath (merge-pathnames (make-pathname :directory (list :relative (format nil "~,3f" latparam)))
				  path)))
    (ensure-directories-exist jobpath)
    (cl-fad:copy-file (file-path path "INCAR") (file-path jobpath "INCAR") :overwrite t)
    (cl-fad:copy-file (file-path path "POTCAR") (file-path jobpath "POTCAR") :overwrite t)
    (replace-line :infile (file-path path "KPOINTS")
		  :outfile (file-path jobpath "KPOINTS")
		  :replace-line (when kpoints-list 3) ;otherwise send nil to replace-line (just copy file)
		  :string (format nil "~{~d ~d ~d~}" kpoints-list)
		  :if-exists :supersede)
    (replace-line :infile (file-path path "POSCAR")
		  :outfile (file-path jobpath "POSCAR")
		  :replace-line 1
		  :string (format nil "~,3f" latparam)
		  :if-exists :supersede)))

