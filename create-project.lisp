(defpackage #:gdep/create-project
  (:use #:cl #:gdep/project-properties)
  (:import-from #:gdep/project
		:project
		:build-type
		:remote-type
		:project-location
		:project-dependencies))

(in-package #:gdep/create-project)

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
	(version-control-type)
	(compilation-location)
	(build-tool))
    (format t "Project Name?~%> ")
    (setf project-name (read-line))
    (query-variable version-control-type "Version Control type?" +version-control-type-members+)
    (query-variable build-tool "Build tool?" +build-tool-members+)
    (query-variable compilation-location "In source build?" +compilation-location-members+)
