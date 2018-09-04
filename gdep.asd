;;;; gdep.asd
;;
;;;; Copyright (c) 2018 Stuart Dilts


(asdf:defsystem #:gdep
  :name "gdep"
  :description "Describe gdep here"
  :author "Your Name <your.name@example.com>"
  :license  "GPL"
  :version "0.0.1"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (#:inferior-shell #:cl-store #:uiop #:unix-options
				#:cl-ansi-text #:lambdalite #:terminfo
				#:make-hash
				#:iterate #:cl-ppcre  ; #:closer-mop
				#:alexandria #:gdep/all))
