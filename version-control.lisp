(defpackage #:gdep/version-control
  (:use :cl :lambdalite)
  (:import-from :gdep/util
		:scan-types
		:run-script))

(in-package #:gdep/version-control)

(export '(scan-version-control
	  get-version-control-instructions
	  run-update
	  get-version-control-types))

(lambdalite:defattributes
  :/pull-script #'pathnamep)

(defclass version-control-instruction ()
  ((type :initarg :type
	 :type 'keyword
	 :reader version-control-type)
   (pull-script :initarg :pull-script
		:reader pull-script
		:type 'pathname))
  (:default-initargs
   :type (error "You must supply a type")
    :pull-script (error "You must supply a pull-script")))

(defun get-version-control-types ()
  (remove-duplicates (mapcar (lambda (x) (:/version-control-type x))
			     (select :version-control))))

(defun add-version-control-type (name pull-script)
  (declare (type keyword name)
	   (type pathname pull-script))
  (if (select :version-control (where (equal :/version-control-type name)))
      (warn (format nil "Build type ~A already present. Not adding." name))
      (lambdalite:insert :version-control
			 (list :/version-control-type name
			       :/pull-script pull-script))))

(defun get-version-control-instructions (name)
  (declare (type keyword name))
  (let* ((rows (select :version-control (where (equal :/version-control-type name))))
	(item (first rows)))
    (assert (= (length rows) 1))
    (make-instance 'version-control-instruction
		   :type (:/version-control-type item)
		   :pull-script (:/pull-script item))))

(defun run-update (version-control-instruction location)
  (run-script (pull-script version-control-instruction)
	      location))

(defun scan-version-control (data-dir)
  (scan-types (data-dir "version-control")
      (dir name)
    (format *debug-io* "Adding version control type ~S~%" name)
    (add-version-control-type name
			      (merge-pathnames dir (make-pathname :name "update"
								  :type "sh")))))
