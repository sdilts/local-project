(defpackage #:lpro/build-types
  (:use :cl :lambdalite)
  (:import-from :lpro/util
		:create-dir
		:scan-types
		:directory-dne-error
		:run-script))

(in-package #:lpro/build-types)

(export '(get-build-instructions
	  get-build-types
	  add-build-type
	  build-instruction
	  build-type
	  run-build
	  run-install))

;; column names for items in the build-types table:
(defattributes
  :/build-type #'keywordp
  :/script-build-path #'pathnamep
  :/script-install-path #'pathnamep)

(defun get-build-types ()
  (remove-duplicates (mapcar (lambda (x) (:/build-type x))
			     (select :build-types))))


(defun get-build-instructions (name)
  (declare (type keyword name))
  (let* ((rows (select :build-types (where (equal :/build-type name))))
	(item (first rows)))
    (assert (= (length rows) 1))
    (make-instance 'build-instruction
		   :type (:/build-type item)
		   :build-script (:/script-build-path item)
		   :install-script (:/script-install-path item))))

(defun add-build-type (type install-script build-script)
  (declare (type keyword type)
	   (type pathname install-script build-script))
  (if (select :build-types (where (equal :/build-type type)))
      (warn (format nil "Build type ~A already present. Not adding." type))
      (insert :build-types
	      (list :/build-type type :/script-install-path install-script
		    :/script-build-path build-script))))

(defclass build-instruction ()
  ((type :initarg :type
	 :type 'keyword
	 :reader build-type)
   (build-script :initarg :build-script
		 :reader build-script
		 :type 'pathname)
   (install-script :initarg :install-script
		   :reader install-script
		   :type 'pathname))
  (:default-initargs
   :type (error "You must supply a type")
    :build-script (error "You must supply a build script")
    :install-script (error "You must supply an install script")))

(defmethod run-build ((instruction build-instruction) location)
  (let ((script (build-script instruction)))
    (handler-bind ((directory-dne-error
		    #'(lambda (c)
			(declare (ignore c))
			(invoke-restart 'create-directory))))
      (run-script script location))))

(defmethod run-install ((instruction build-instruction) location)
  (let ((script (install-script instruction)))
    (handler-bind ((directory-dne-error
		    #'(lambda (c)
			(declare (ignore c))
			(invoke-restart 'create-directory))))
    (run-script script location))))

(defun scan-build-types (data-dir)
  (scan-types (data-dir "build-types")
      (dir type-name)
    (format *debug-io* "Adding build type ~S~%" type-name)
    ;; assume the files we need exist:
    (add-build-type type-name
		    (merge-pathnames dir (make-pathname :name "install"
							:type "sh"))
		    (merge-pathnames dir (make-pathname :name "build"
							:type "sh")))))
