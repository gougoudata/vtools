;;;; submit-jobs.lisp

(in-package #:vtools)

(defvar *job-relative-pathstring* "tc-ni-fcc-3-1-kpoints=11")
(defvar *job-parent-directory*
  (merge-pathnames (make-pathname :directory (list :relative *job-relative-pathstring*))
		   *job-root-directory*)
  "*JOB-PARENT-DIRECTORY* is the directory in which the job directories will be created.
It should have template files for INCAR, KPOINTS, POSCAR, and POTCAR.")
(defvar *mpirun* (pathname "/local/openmpi-gcc/bin/mpirun")
  "*mpirun* is the path your mpirun binary.")
(defvar *vasp* (pathname "/homes/mohrland/bin/vasp")
  "*vasp* is the path your vasp binary.")

(defvar *number-of-cores* 12)
(defvar *jobs-running-p* nil)

(defun submit-jobs (&key low high step excludes-list additionals-list kpoints-list)
  "SUMBIT-JOBS takes keyword arguments: A range, a step size, a
list of numbers to exclude, a list of numbers to include, and a
list of kpoints dimensions (with 11x11x11 as the default). It
then assumes each number corresponds to a lattice parameter and
loops across the numbers, creating directories for each job and
submitting that job to Vasp. You must have a template INCAR,
KPOINTS, POSCAR, and POTCAR in the job parent directory.

WARNING: This overwrites directories with the same names in
job parent directory!"

  (let ((params (append (loop for y from low to high by step collecting y) additionals-list)))
    (loop for x in params do
	 (unless (member x excludes-list :test #'=)
	   (let ((job-pathname (merge-pathnames
				(make-pathname :directory (list :relative (format nil "~,3f" x)))
				*job-parent-directory*)))
	     (prepare-files job-pathname kpoints-list x)
	     (format t "Job ~a being submitted now...~%" (namestring job-pathname))
	     (submit-single-job job-pathname)
	     (format t "Job ~a stopped.~%" job-pathname)
	     )))))

(defun submit-single-job (pathname)
  (trivial-shell:shell-command
   (conc-with-spaces
    *mpirun-binary*
    "-wdir"
    (namestring pathname)
    "-np"
    *number-of-cores*
    *vasp-binary*)))

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

(defun prepare-files (job-pathname kpoints-list x)
  (ensure-directories-exist job-pathname)
  (cl-fad:copy-file (make-pathname :directory (pathname-directory *job-parent-directory*)
				   :name "INCAR")
		    (make-pathname :directory (pathname-directory job-pathname)
				   :name "INCAR"))
  (cl-fad:copy-file (make-pathname :directory (pathname-directory *job-parent-directory*)
				   :name "POTCAR")
		    (make-pathname :directory (pathname-directory job-pathname)
				   :name "POTCAR"))
  (replace-line :infile (make-pathname :directory (pathname-directory *job-parent-directory*)
				       :name "KPOINTS")
		:outfile (make-pathname :directory (pathname-directory job-pathname)
					:name "KPOINTS")
		:replace-line (when kpoints-list 3)
		:string (format nil "~{~d ~d ~d~}" kpoints-list)
		:if-exists :supersede)
  (replace-line :infile (make-pathname :directory (pathname-directory *job-parent-directory*)
				       :name "POSCAR")
		:outfile (make-pathname :directory (pathname-directory job-pathname)
					:name "POSCAR")
		:replace-line 1
		:string (format nil "~,3f" x)
		:if-exists :supersede))
