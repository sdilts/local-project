(defpackage #:lpro/project
  (:use #:cl
	#:lpro/dependency-protocol
	#:lpro/dependency)
  (:import-from #:lpro/version-control
		#:run-update
		#:get-version-control-instructions)
  (:import-from #:lpro/build-types
		#:get-build-instructions
		#:run-build
		#:run-install))


(in-package #:lpro/project)

(export '(project
	  build-type
	  compilation-location
	  project-location
	  project-dependencies
	  project-url
	  project-update-source
	  project-compile
	  project-root-install-p
	  version-control-type
	  compilation-error
	  update-source-error
	  project-error-project
	  project-install
	  installation-error))

(defclass project (dependency)
  ((location :initarg :location
	     :accessor project-location
	     :initform (error "Must specify location")
	     :type 'pathname)
   (version-control-type :initarg :version-control-type
			 :accessor version-control-type)
   (build-type :initarg :build-type
	       :accessor build-type
	       :initform (error "Must specify build-type")
	       :type 'build-type)
  (compilation-location :initarg :compilation-location
			:initform (error "MUst specify comipation location")
			:accessor compilation-location)
   (root-install-p :initarg :root-install
		   :initform :yes
		   :accessor project-root-install-p)
   (url :initarg :url
	:accessor project-url
	:type #'stringp)
   (dependencies :initarg :dependencies
		 :accessor project-dependencies))
  (:default-initargs
   :version-control-type :git
    :url ""
    :version 0))

(define-condition project-error ()
  ((project :initarg :project
	    :reader project-error-project))
  (:default-initargs
   :project (error "You must supply a project")))

(define-condition compilation-error (project-error uiop:subprocess-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Error compiling project~% ~A.~%Process exited with error code ~A"
		     (project-error-project condition)
		     (uiop:subprocess-error-code condition)))))

(define-condition update-source-error (project-error uiop:subprocess-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Error updating source of project~% ~A.~%Process exited with error code ~A"
		     (project-error-project condition)
		     (uiop:subprocess-error-code condition)))))

(define-condition installation-error (project-error uiop:subprocess-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Error installing project~% ~A.~%Process exited with error code ~A"
		     (project-error-project condition)
		     (uiop:subprocess-error-code condition)))))

(defmethod project-update-source ((project project))
  "Updates the source code of the project"
  (handler-case  (run-update (get-version-control-instructions (version-control-type project))
			     (project-location project))
    (uiop:subprocess-error (c)
      (cerror "Ignore update error"
	      'update-source-error
	      :project project
	      :code (uiop:subprocess-error-code c)
	      :command (uiop:subprocess-error-command c)
	      :process (uiop:subprocess-error-process c)))))

(defun  get-build-location (project)
  (case (compilation-location project)
    (:in-source (project-location project))
    (:out-of-source (merge-pathnames (make-pathname :directory '(:relative "build"))
		     (project-location project)))))

(defmethod project-compile ((project project))
  (let ((build-instruction (get-build-instructions (build-type project)))
	(build-location (get-build-location project)))
    (handler-case (run-build build-instruction build-location)
      (uiop:subprocess-error (c)
	(cerror "Ignore compilation error"
		'compilation-error :project project
		:code (uiop:subprocess-error-code c)
		:command (uiop:subprocess-error-command c)
		:process (uiop:subprocess-error-process c))))))

(defmethod project-install ((project project))
  (let ((build-instruction (get-build-instructions (build-type project)))
	(build-location (get-build-location project)))
    (handler-case (run-install build-instruction build-location)
      (uiop:subprocess-error (c)
	(cerror "Ignore compilation error"
		'installation-error :project project
		:code (uiop:subprocess-error-code c)
		:command (uiop:subprocess-error-command c)
		:process (uiop:subprocess-error-process c))))))


(defmethod print-object ((object project) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((name dependency-name)
		     (id dependency-id)
		     (location project-location))
	object
      (format stream "name: ~A id: ~A location: ~A" name id location))))

#|
Project updating order:
1. Check if there is something to pull from version control. If
   there is something, update it.
2. Check if any dependencies were updated since the project
   was last compiled
3. Check all of the projects that depend on the project and
   update those too.
|#
