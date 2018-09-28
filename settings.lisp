(defpackage #:gdep/settings
  (:use :cl))


(in-package #:gdep/settings)

(export '(data-dir-changed
	  new-directory
	  *data-directory*
	  change-data-directory))

;; TODO: change this directory:
(defvar *data-directory* (uiop:ensure-directory-pathname #p"/home/stuart/Programs/git-install/data"))

;; TODO: change this:
(defvar *pkg-database* "rpm")
