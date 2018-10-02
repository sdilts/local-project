(defpackage #:lpro/update-commands
  (:use :cl :alexandria)
  (:import-from #:lpro/command
		#:defcommand
		#:print-info-text)
  (:import-from #:lpro/project
		#:project-update-source
		#:project-compile
		#:compilation-error
		#:project-error-project
		#:update-source-error)
  (:import-from #:lpro/dependency
		#:dependency-name)
  (:import-from #:lpro/database
		#:all-projects
		#:get-project))

(in-package #:lpro/update-commands)

;; (defun update-project (project)
;;   (handler-case (project-update-source project)
;;     (uiop:subprocess-error (c)

(defstruct update-errors
  compile
  update-source)

(defun print-error-summary (update-errors)
  (let ((update-source-failed (update-errors-update-source update-errors))
	(compilation-failed (update-errors-compile update-errors)))
    (when update-source-failed
      (format t "~A These projects failed to update their source code:~%"
	      (cl-ansi-text:red "Error:" :effect :bright))
      (dolist (project update-source-failed)
	(format t "~4T~A~%" (dependency-name project))))
    (when compilation-failed
      (format t "~A These projects failed to compile:~%"
	      (cl-ansi-text:red "Error:" :effect :bright))
      (dolist (project compilation-failed)
	(format t "~4T~A~%" (dependency-name project)))
      (format t "~%"))))

(defun update-projects (project-list)
  (let ((compilation-errors ())
	(update-source-errors ()))
    (dolist (project project-list)
      (let ((error-p nil))
	(format t "~&Updating project ~A~%" (dependency-name project))
	(finish-output)
	(handler-case
	    (progn
	      (project-update-source project)
	      (project-compile project))
	  (compilation-error (c)
	    (setf error-p T)
	    (format t "~A~A~%" (cl-ansi-text:red "Error: " :effect :bright) c)
	    (push (project-error-project c) compilation-errors))
	  (update-source-error (c)
	    (setf error-p T)
	    (format t "~A~A~%"  (cl-ansi-text:red "Error: " :effect :bright) c)
	    (push (project-error-project c) update-source-errors)))
	(if error-p
	    (format t "~&~A Could not fully update project~%~%"
		    (cl-ansi-text:yellow "Error:" :effect :bright))
	    (format t "~&~A Project updated~%~%"
		    (cl-ansi-text:green "Success:" :effect :bright)))))
    (when (or compilation-errors
	    update-source-errors)
      (make-update-errors :compile compilation-errors
			      :update-source update-source-errors))))

(defcommand update (current-directory command-line-args)
    ("Download and compile the updated source for the listed projects"
     :more-info '("Usage: lpro update [projects...]"
		  " Download and compile, but do not install, the listed projects."
		  " If you want to update all projects at once, use the update-all command."))
  (when (not command-line-args)
      (format *debug-io* "~A At least one project name must be given~%~%"
	      (cl-ansi-text:red "Error:" :effect :bright))
      (print-info-text "update" *debug-io*)
      (uiop:quit 22))
  (let ((not-found ())
	(projects ()))
    (dolist (project-name command-line-args)
      (if-let ((project (get-project project-name)))
	(push project projects)
	(push project-name not-found)))
    (let ((results (update-projects projects)))
      ;; if we recieve a return value, something went wrong:
      (when (or not-found results)
	(when not-found
	  (format t "~A These projects could not be found:"
		  (cl-ansi-text:red "Error:" :effect :bright))
	  (dolist (project not-found)
	    (format t "~&~4T~A~%" project)))
	(when results
	  (format t "~%")
	  (print-error-summary results))
	(uiop:quit 1)))))

(defcommand update-all (current-directory command-line-args)
    ("Download and compile the updated source for all projects")
  (let ((results (update-projects (all-projects))))
    (when results
      (print-error-summary results)
      (uiop:quit 1))))
