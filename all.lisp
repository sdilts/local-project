;;;; gdep.lisp
;;
;;;; Copyright (c) 2018 Stuart Dilts

;; (uiop:define-package #:gdep
;;     (:use-reexport #:gdep/data-dir))

(defpackage #:gdep/all
  (:use :cl #:gdep/data-init #:gdep/init-project
	#:gdep/update-commands #:gdep/command))

;; (in-package #:gdep)



;; (setf (getf make-hash:*hash-factory-defaults* :test) #'equal)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (make-hash:install-hash-reader ()))

;; (defun foo ())
