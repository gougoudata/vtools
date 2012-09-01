;;;; submit-jobs.el

(defvar *mpirun-binary* "/local/openmpi-gcc/bin/mpirun")
(defvar *vasp-binary* "/homes/mohrland/bin/vasp")
(defvar *vasp-parent-directory* "/scr1/mohrland/tc-ni-fcc-3-1-kpoints=11/")

(defun submit-jobs (low high step &optional excludes-list additionals-list (kpoints-list '(11 11 11))
  "SUMBIT-JOBS takes a range and a step size and (optionally) a
list of numbers to exclude, a list of numbers to include, and a
list of kpoints dimensions (with 11x11x11 as the default). It
then assumes each number corresponds to a lattice parameter and
loops across the numbers, creating directories for each job and
submitting that job to Vasp. You must have a template INCAR,
KPOINTS, POSCAR, and POTCAR in the *VASP-PARENT-DIRECTORY*.

Warning: This with overwrite directories with the same names in *VASP-PARENT-DIRECTORY*!
"
  (loop
   for x in (append (loop for y from low to high by step collecting y) additionals-list)
   do (unless (member x excludes-list)
        (let ((dir (concat *vasp-parent-directory* (format "%.3f" x) "/")))

	  ;; make the job directory (overwrite, if it exists)
          (if (file-accessible-directory-p dir) (delete-directory dir t))
          (make-directory dir t)

	  ;; copy template files into job directory
          (mapcar (lambda (file) (copy-file
				  (concat *vasp-parent-directory* file)
				  (concat dir file)))
		  '("POSCAR" "POTCAR" "INCAR" "KPOINTS"))

          ;; update POSCAR with new lattice parameter
          (find-file (concat dir "POSCAR"))
          (forward-line)
          (kill-line)
          (insert (format "%.3f" x))
          (save-buffer)
          (kill-buffer)

          ;; update kpoints
          (find-file (concat dir "KPOINTS"))
          (forward-line 3)
          (kill-line)
          (insert (format "%d %d %d"
			  (first kpoints-list)
			  (second kpoints-list)
			  (third kpoints-list)))
          (save-buffer)
          (kill-buffer)

	  ;; tell Unix to submit the job
          (shell-command
           (safe-concat
            (make-spaced-string
             "/local/openmpi-gcc/bin/mpirun -wdir"
             dir
             "-np 12 /homes/mohrland/bin/vasp")))
          ))))
