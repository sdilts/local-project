;;;; gdep.asd
;;
;;;; Copyright (c) 2018 Stuart Dilts

(asdf:register-system-packages "local-project" '(:lpro))

(asdf:defsystem #:lpro
  :name "local-project"
  :description "A command line utilty for managing locally compiled programs"
  :author "Your Name <your.name@example.com>"
  :license  "GPL"
  :version "0.0.1"
  :pathname "src"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (#:inferior-shell #:cl-store #:uiop #:unix-options
				#:cl-ansi-text #:lambdalite #:terminfo
				#:local-time
				#:make-hash
				#:iterate #:cl-ppcre  ; #:closer-mop
				#:alexandria #:lpro/all))
