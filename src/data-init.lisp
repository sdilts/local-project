;;(in-package #:lpro/data-dir)
(defpackage #:lpro/data-init
  (:use #:cl #:alexandria #:lpro/util)
  (:import-from #:lpro/command
		#:defcommand)
  (:import-from #:lpro/settings
		:*data-directory*
	        :*db-directory*)
  (:import-from #:lambdalite
		:load-db)
  (:import-from #:lpro/build-types
		:scan-build-types)
  (:import-from #:lpro/version-control
		:scan-version-control))

(in-package #:lpro/data-init)

(export '(data-dir-init))

(defun scan-scripts ()
  (scan-build-types *data-directory*)
  (scan-version-control *data-directory*))

(defcommand scan-scripts-cmd (current-directory command-line-args)
    ("Scan the data directory for more scripts."
     :command-used "scan-scripts"
     :more-info '("Usage: lpro scan-scripts"
		 " See the README in the data directory for more information"
		  " about lpro scripts."))
  (declare (ignore current-directory command-line-args))
  (scan-scripts))

(defun data-dir-init ()
  ;; make sure the directory is present:
  (let ((first-run (not (uiop:directory-exists-p *data-directory*)))
	(db-created nil))
    (when first-run
      (warn (format nil "Data directory ~A not found. Some functionality will not work."
		    *data-directory*))
      (format t "Creating data dir at ~A"
	      *data-directory*)
      (create-dir *data-directory*))
    (handler-bind ((simple-warning
		    (lambda (c)
		      (format *debug-io* "~&Database not initialized~%")
		      (setf db-created t)
		      (muffle-warning c))))
      (load-db :path (uiop:ensure-directory-pathname (merge-pathnames #p"db"
								      *db-directory*))))
    (when db-created
      (format *debug-io* "~&Initializing database...~%")
      (scan-scripts)
      (format *debug-io* "~&Database initialized~%~%"))))
