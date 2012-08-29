;;;; vtools.asd

(asdf:defsystem #:vtools
  :serial t
  :description "vtools is [description here]"
  :author "Andrew Mohrland <amohrland@gmail.com>"
  :license "MIT/X11"
  :depends-on (#:trivial-shell
               #:cl-ppcre
               #:cl-fad
               #:cl-grace)
  :components ((:file "package")
               (:file "vtools")))

