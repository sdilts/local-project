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
  ((location :initarg :location
	     :accessor project-location
	     :type 'pathname)
   (version :initarg :version
	    :accessor project-version
	    :type 'number)
   (version-control-type :initarg :version-control-type
			 :accessor version-control-type)
   (build-type :initarg :build-type
		      :reader :build-type
		      :type 'build-instruction)
   (compilation-location :initarg :compilation-type
			 :accessor compilation-location)
   (dependencies :initarg :dependencies
		 :accessor project-dependencies))
  (:default-initargs
   :version-control-type :git))


#|
Project updating order:
1. Check if there is something to pull from version control. If
   there is something, update it.
2. Check if any dependencies were updated since the project
   was last compiled
3. Check all of the projects that depend on the project and
   update those too.
|#
