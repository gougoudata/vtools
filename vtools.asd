;;;; vtools.asd

(asdf:defsystem #:vtools
  :serial t
  :description "vtools is [description here]"
  :author "Andrew Mohrland <amohrland@gmail.com>"
  :license "MIT/X11"
  :depends-on (#:trivial-shell
               #:cl-ppcre
               #:cl-fad
	       #:iterate
	       #:bordeaux-threads
	       #:rcl
               #:cl-grace)
  :components ((:file "package")
	       (:file "utils")
               (:file "submit-jobs")
	       (:file "job-listener")
	       (:file "r-plots")
	       (:file "grace-plots")
	       ))
