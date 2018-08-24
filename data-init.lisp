;;(in-package #:gdep/data-dir)
(defpackage #:gdep/data-init
  (:use #:cl #:alexandria #:gdep/util)
  (:import-from #:gdep/settings
		:*data-directory*
		:change-data-directory)
  (:import-from #:lambdalite
		:load-db))


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
  (tagbody ensure-data-directory
    (handler-case (ensure-data-directory-exists)
      (data-dir-changed (c)
	(change-data-directory (new-directory c))
	(go ensure-data-directory))))
  (load-db :path (uiop:ensure-directory-pathname (merge-pathnames *data-directory*
								  #p"db"))))
