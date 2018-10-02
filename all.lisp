;;;; lpro.lisp
;;
;;;; Copyright (c) 2018 Stuart Dilts

;; (uiop:define-package #:lpro
;;     (:use-reexport #:lpro/data-dir))

(defpackage #:lpro/all
  (:use :cl #:lpro/data-init #:lpro/project-management
	#:lpro/update-commands #:lpro/command))

;; (in-package #:lpro)



;; (setf (getf make-hash:*hash-factory-defaults* :test) #'equal)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (make-hash:install-hash-reader ()))

;; (defun foo ())
