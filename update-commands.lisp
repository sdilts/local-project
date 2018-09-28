(defpackage #:gdep/update-commands
  (:use :cl :alexandria)
  (:import-from #:gdep/command
		#:defcommand)
  (:import-from #:gdep/project
		#:project-update-source
		#:project-compile)
  (:import-from #:gdep/database
		#:all-projects
		#:get-project))

(in-package #:gdep/update-commands)

(defcommand update (current-directory command-line-args)
    ("Download and compile the updated source for the listed projects"
     :more-info '("Usage: gdep update [projects...]"
		  " Download and compile, but do not install, the listed projects."
		  " If you want to update all projects at once, use the update-all command."))
  (let ((update-errors ()))
    (dolist (project-name command-line-args)
      (handler-case
	  (if-let ((project (get-project project-name)))
	    (progn
	      (project-update-source project)
	      (project-compile project))
	    (progn
	      (warn (format nil "Project ~A not found" project-name))
	      (push (format nil "Project ~A not found" project-name) update-errors)))
	;; TODO: better condition useage, something more specific than uiop:subprocess-error
	(uiop:subprocess-error (c)
	  (declare (ignore c))
	  (warn (format nil "Unable to update ~A" project-name))
	  (push c update-errors))))
    ;; TODO: better error reporting
    (when update-errors
      (format t "~A Some projects could not be updated."
	      (cl-ansi-text:red "Error:"))
      (uiop:quit 1))))

(defcommand update-all (current-directory command-line-args)
    ("Download and compile the updated source for all projects"))
