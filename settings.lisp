(defpackage #:gdep/settings
  (:use :cl))


(in-package #:gdep/settings)

(export '(data-dir-changed
	  new-directory
	  *data-directory*
	  change-data-directory))

(defvar *data-directory* (uiop:ensure-directory-pathname (merge-pathnames (user-homedir-pathname)
									  #p".gdep")))

(defun change-data-directory (new-path)
  (assert (pathnamep new-path))
  (setf *data-directory* new-path)
  (format t "Data directory changed to ~A. To make the change permanent, add
(setf gdep/settings:*data-directory ~S)~%to ~A.~%"
	  *data-directory* *data-directory*
	  (merge-pathnames (user-homedir-pathname) #p".gdeprc")))
