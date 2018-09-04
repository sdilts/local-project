(defpackage #:gdep/dependency
  (:use :cl :gdep/dependency-protocol))

(in-package #:gdep/dependency)

(export '(dependency
	  dependency-name
	  dependency-version))

(defclass dependency ()
  ((name :initarg :name :reader dependency-name
	 :type 'string)
  (version :initarg :version
	   :accessor dependency-version)
   (id :initarg id
       :accessor dependency-id
       :type #'numberp)))
