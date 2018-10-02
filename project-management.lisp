(defpackage #:lpro/project-management
  (:use :cl :lpro/project :lpro/dependency
	:lpro/database)
  (:import-from :lpro/util
		:print-error
		:run-menu)
  (:import-from :lpro/build-types
		:get-build-types)
  (:import-from :lpro/version-control
		:get-version-control-types)
  (:import-from :lpro/command
		:defcommand)
  (:import-from :alexandria
		:if-let))

(in-package #:lpro/project-management)

(export '(init-project
	  edit-project))

(defun read-keyword-and-verify (valid-keywords)
  (loop
     (let ((input (intern (string-upcase (read-line)) "KEYWORD")))
       (if (member input valid-keywords)
	   (return-from read-keyword-and-verify input)
	   (format t "Invalide input. Please enter one of ~{~A~^ ~}~%> "
		   (mapcar #'string valid-keywords))))))

(defun check-project-name (name)
  (alexandria:if-let (conflicting-project (get-dependency name))
    (progn
      (format t (concatenate 'string
			     (cl-ansi-text:red "Error: ")
			     "Dependency with name \"~A\" already exists.~%")
	      name)
      nil)
    t))

(defun query-name ()
  (format t "~&Project Name?~%> ")
  (do ((name (read-line) (read-line)))
      ((check-project-name name) name)
    (format t "~&Project Name?~%> ")))

(defmacro query-variable (to-set prompt valid-answers)
  `(progn
     (format t ,(concatenate 'string prompt " (~{~a~^ ~})~%> ") ,valid-answers)
     (finish-output)
     (setf ,to-set (read-keyword-and-verify ,valid-answers))))

(defun edit-project-interactively (project)
  (let ((version-control-types (get-version-control-types))
	(build-types (get-build-types)))
    (lpro/util:run-menu (:repeat-prompt t :always-show-help t :num-options-shown 0)
	"Enter a number to change an option, or (y) to continue"
      ("y" "Continue. Everything is correct."
	   (return-from run-menu))
      ("0" ("Project name: ~A" (dependency-name project))
	   (setf (dependency-name project) (query-name)))
      ("1" ("Build tool: ~A" (build-type project))
	   (query-variable (build-type project)
			   "Build tool?" build-types))
      ("2" ("In source: ~A" (compilation-location project))
	   (query-variable (compilation-location project)
			   "In source build?" *compilation-location-members*))
      ("3" ("Version control type: ~A" (version-control-type project))
	   (query-variable (version-control-type project)
			   "Version control type?" version-control-types))
      ("4" ("Project location: ~A" (project-location project))
	   (format t " > ")
	   (finish-output)
	   (setf (project-location project) (make-pathname :directory (read-line))))
      ("5" ("Root installation Requried? ~A" (project-root-install-p project))
	   (query-variable (project-root-install-p project)
			   "Root installation required?" '(:yes :no)))
      ("6" ("Project url: ~A" (project-url project))
	   (format t "Project url? > ")
	   (finish-output)
	   (setf (project-url project) (read-line))))
    project))

(defun create-project-interactively (project-dir)
  (declare (type pathname project-dir))
  (ensure-directories-exist (uiop:ensure-directory-pathname project-dir))
  (let ((project-name)
	(version-control-type :git)
	(compilation-location)
	(build-type)
	(root-install-p :yes)
	(project-url "")
	(build-types (get-build-types))
	(version-control-types (get-version-control-types)))

    ;; the project ID gets assigned when the project is inserted into the database
    ;; That's why it isn't mentinoned here
    (format t "Please enter the following information. You will be able to go back
and change the values once everything is entered.~%")
    (setf project-name (query-name))
    (query-variable build-type "Build tool?" build-types)
    (query-variable compilation-location "In source build?" lpro/project-properties:*compilation-location-members*)
    (let ((new-project (make-instance 'project
				      :name project-name
				      :location project-dir
				      :version-control-type version-control-type
				      :build-type build-type
				      :url project-url
				      :root-install root-install-p
				      :version (local-time:timestamp-to-unix (local-time:now))
				      :compilation-location compilation-location)))
      (format t "Is everything correct? ")
      (edit-project-interactively new-project)
      new-project)))

(defun check-project (new-project)
  T)

(defcommand init-project (project-location command-line-args)
    ("Create a new project in the current directory"
     :command-used "init"
     :more-info "Usage: lpro init")
  ;; ignore the command line args for now, we can use them if we need them:
  (declare (ignore command-line-args))
  ;; don't waste the user's time: check if we are already in a project:
  ;; (already-exists-p
  (let ((new-project (create-project-interactively project-location)))
    (when (not (check-project new-project))
      ;; invalid arguement:
      (uiop:quit 22))
    (add-project new-project)
    (format t "Project ~A added.~%" (dependency-name new-project))))

(defcommand edit-project (project-location command-line-args)
    ("Edit the properties of a project"
     :command-used "edit"
     :more-info "Usage: lpro edit [project-name]")
  (if-let (project (get-project (first command-line-args)))
    (progn
      (format t "Updating project ~A" (cl-ansi-text:magenta (first command-line-args)
							    :effect :bright))
      (inspect project)
      (edit-project-interactively project)
      (update-project project)
      (format t (cl-ansi-text:green "Project Updated~%" :effect :bright)))
    (progn
      (print-error t "Project ~A not found" (first command-line-args))
      (uiop:quit 22))))
