(defpackage #:gdep/project
  (:use #:cl
	#:gdep/dependency-protocol
	#:gdep/dependency
	#:gdep/project-properties))


(in-package #:gdep/project)

(export '(project
	  build-type
	  remote-type
	  project-location
	  project-dependencies))


(defclass project (dependency)
  ((location :initarg :location :accessor project-location
	     :type 'pathname)
   (version-control-type :initarg :version-control-type
		:accessor version-control-type)
   (build-tool :initarg :build-type
	       :type 'cons
	       :accessor build-type)
   (compilation-location :initarg :compilation-type
		     :accessor compilation-location)
   (dependencies :initarg :dependencies
		 :accessor project-dependencies))
  (:default-initargs
   :version-control-type :git))
