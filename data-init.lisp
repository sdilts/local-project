;;(in-package #:gdep/data-dir)
(defpackage #:gdep/data-init
  (:use #:cl #:alexandria #:gdep/util)
  (:import-from #:gdep/settings
		:*data-directory*
		:change-data-directory)
  (:import-from #:lambdalite
		:load-db)
  (:import-from #:gdep/build-types
		:scan-build-types)
  (:import-from #:gdep/version-control
		:scan-version-control))

(in-package #:gdep/data-init)

(export '(data-dir-init))

(define-condition data-dir-changed ()
  ((new-directory :initarg :new-directory
		 :reader new-directory)))

(defun data-dir-menu ()
  (run-menu ()
      ("~&Data Directory doesn't exist at \"~A\". Create it?" *data-directory*)
    ("y" "Yes, agree to create the directory"
	 (if (create-dir *data-directory*)
	     (return-from run-menu)
	     (format t "Could not create directory ~A" *data-directory*)))
    ("n" "No, cancel the operation"
	 (format t "Aborting")
	 (signal 'abort-operation))
    ("c" "Specify data directory location"
	 (format t "Directory to use > ")
	 (signal 'data-dir-changed :new-directory (uiop:ensure-directory-pathname
						   (parse-namestring (read-line)))))))

(defun ensure-data-directory-exists ()
  (when (not (uiop:directory-exists-p *data-directory*))
    (data-dir-menu)))

(defun data-dir-init ()
  ;; make sure the directory is present:
  (let ((first-run (not (uiop:directory-exists-p *data-directory*)))
	(db-created nil))
    (when first-run
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
