(defpackage #:gdep/init-project
  (:use :cl :gdep/create-project :gdep/project :gdep/dependency
	:gdep/database)
  (:import-from :gdep/command
		:defcommand))

(in-package #:gdep/init-project)

(export '(init-project))

(defun check-project (new-project)
  (format t "Checking for name conflicts..~%")
  (alexandria:when-let (conflicting-project (get-dependency (dependency-name new-project)))
    (format t (concatenate 'string
			   (cl-ansi-text:red "Error: ")
			   "Dependency with name \"~A\" already exists.~%")
	    (dependency-name new-project))
    ;; invalid argument return
    (uiop:quit 22)))

(defcommand init-project (project-location command-line-args)
    ("Create a new project in the current directory"
     :command-used "init"
     :more-info "Usage: gdep init")
  ;; ignore the command line args for now, we can use them if we need them:
  (declare (ignore command-line-args))
  ;; don't waste the user's time: check if we are already in a project:
  ;; (already-exists-p
  (let ((new-project (create-project-interactively project-location)))
    (check-project new-project)
    (add-project new-project)
    (format t "Project ~A added.~%" (dependency-name new-project))))
