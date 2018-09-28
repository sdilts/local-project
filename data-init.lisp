;;(in-package #:gdep/data-dir)
(defpackage #:gdep/data-init
  (:use #:cl #:alexandria #:gdep/util)
  (:import-from #:gdep/settings
		:*data-directory*)
  (:import-from #:lambdalite
		:load-db)
  (:import-from #:gdep/build-types
		:scan-build-types)
  (:import-from #:gdep/version-control
		:scan-version-control))

(in-package #:gdep/data-init)

(export '(data-dir-init))

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
								      *data-directory*))))
    (when db-created
      (format *debug-io* "~&Initializing database...~%")
      (scan-build-types *data-directory*)
      (scan-version-control *data-directory*)
      (format *debug-io* "~&Database initialized~%~%"))))
