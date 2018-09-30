(defpackage #:gdep/create-project
  (:use #:cl #:gdep/project-properties
	#:gdep/project)
  (:import-from #:gdep/dependency
		#:dependency-name)
  (:import-from #:gdep/database
		#:get-dependency)
  (:import-from #:gdep/util
		:run-menu)
  (:import-from #:gdep/build-types
		:get-build-types)
  (:import-from #:gdep/version-control
		:get-version-control-types))

(in-package #:gdep/create-project)

(export '(create-project-interactively
	  edit-project-interactively))

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
    (gdep/util:run-menu (:repeat-prompt t :always-show-help t :num-options-shown 0)
	"Enter a number to change an option, or (y) to continue"
      ("y" "Continue. Everything is correct."
	   (return-from run-menu))
      ("0" ("Project name: ~S" (dependency-name project))
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

    (format t "Please enter the following information. You will be able to go back
and change the values once everything is entered.~%")
    (setf project-name (query-name))
    (query-variable build-type "Build tool?" build-types)
    (query-variable compilation-location "In source build?" gdep/project-properties:*compilation-location-members*)
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
