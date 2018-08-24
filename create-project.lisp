(defpackage #:gdep/create-project
  (:use #:cl #:gdep/project-properties)
  (:import-from #:gdep/project
		:project
		:build-type
		:remote-type
		:project-location
		:project-dependencies)
  (:import-from #:gdep/util
		:run-menu)
  (:import-from #:gdep/build-types
		:get-build-types))

(in-package #:gdep/create-project)

(export '(create-project-interactively))

(defun read-keyword-and-verify (valid-keywords)
  (loop
     (let ((input (intern (string-upcase (read-line)) "KEYWORD")))
       (if (member input valid-keywords)
	   (return-from read-keyword-and-verify input)
	   (format t "Invalide input. Please enter one of ~{~A~^ ~}~%> "
		   (mapcar #'string valid-keywords))))))

(defmacro query-variable (to-set prompt valid-answers)
  `(progn
     (format t ,(concatenate 'string prompt " (~{~A~^ ~})~%> ") ,valid-answers)
     (setf ,to-set (read-keyword-and-verify ,valid-answers))))

(defun create-project-interactively (project-dir)
  (declare (type pathname project-dir))
  (ensure-directories-exist (uiop:ensure-directory-pathname project-dir))
  (let ((project-name)
	(version-control-type :git)
	(compilation-location)
	(build-type)
	(build-types (get-build-types)))

    (format t "Please enter the following information. You will be able to go back
and change the values once everything is entered.~%")
    (format t "Project Name?~%> ")
    (setf project-name (read-line))
    (query-variable build-type "Build tool?" build-types)
    (query-variable compilation-location "In source build?" +compilation-location-members+)
    (gdep/util:run-menu (:repeat-prompt t :always-show-help t :num-options-shown 0)
	"Is this correct? Enter a number to change an option, or (y) to continue"
       ("y" "Continue. Everything is correct."
	   (return-from run-menu))
      ("0" ("Project name is ~S" project-name)
	   (setf project-name (read-line)))
      ("1" ("Build tool is: ~A" build-type)
	   (query-variable build-type "Build tool?" build-types))
      ("2" ("In source: ~A" compilation-location)
	   (query-variable compilation-location "In source build?" +compilation-location-members+))
      ("3" ("Project location: ~A" project-dir)
	   (setf project-dir (make-pathname :directory (read-line)))))
    (make-instance 'project
		   :location project-dir
		   :version-control-type version-control-type
		   :build-type build-type
		   :compilation-type compilation-location)))
