;;;; package.lisp

(defpackage #:vtools
  (:use #:cl)
  (:shadowing-import-from #:trivial-shell
			  #:shell-command))

