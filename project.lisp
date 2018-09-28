(defpackage #:gdep/project
  (:use #:cl
	#:gdep/dependency-protocol
	#:gdep/dependency
	#:gdep/project-properties)
  (:import-from #:gdep/version-control
		#:run-update
		#:get-version-control-instructions)
  (:import-from #:gdep/build-types
		#:get-build-instructions
		#:run-build
		#:run-install))


(in-package #:gdep/project)

(export '(project
	  build-type
	  remote-type
	  compilation-location
	  project-location
	  project-dependencies
	  project-url
	  project-update-source
	  project-compile
	  version-control-type))

(defclass project (dependency)
  ((location :initarg :location
	     :accessor project-location
	     :initform (error "Must specify location")
	     :type 'pathname)
   (version-control-type :initarg :version-control-type
			 :accessor version-control-type)
   (build-type :initarg :build-type
	       :reader build-type
	       :initform (error "Must specify build-type")
	       :type 'build-type)
  (compilation-location :initarg :compilation-location
			:initform (error "MUst specify comipation location")
			 :accessor compilation-location)
   (url :initarg :url
	:accessor project-url
	:type #'stringp)
   (dependencies :initarg :dependencies
		 :accessor project-dependencies))
  (:default-initargs
   :version-control-type :git
    :url ""
    :version 0))

(defmethod project-update-source ((project project))
  "Updates the source code of the project"
  (run-update (get-version-control-instructions (version-control-type project))
	      (project-location project)))

(defun  get-build-location (project)
  (case (compilation-location project)
    (:in-source (project-location project))
    (:out-of-source (merge-pathnames (make-pathname :directory '(:relative "build"))
		     (project-location project)))))

(defmethod project-compile ((project project))
  (let ((build-instruction (get-build-instructions (build-type project)))
	(build-location (get-build-location project)))
    (run-build build-instruction build-location)))

;; (defmethod print-object ((object project) stream)
;;   (print-unreadable-object (object stream :type t)
;;     (with-slots (name id location version-control-type build-type compilation-location
;; 		      url)
;; 	object
;;       (format stream "name: ~A id: location: ~A ~A ~A ~A ~A ~A" name id location
;; 	      version-control-type build-type compilation-location url))))

#|
Project updating order:
1. Check if there is something to pull from version control. If
   there is something, update it.
2. Check if any dependencies were updated since the project
   was last compiled
3. Check all of the projects that depend on the project and
   update those too.
|#
