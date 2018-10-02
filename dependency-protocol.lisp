(defpackage #:lpro/dependency-protocol
  (:use :cl))

(in-package #:lpro/dependency-protocol)

(export '(dependency
	  system-package
	  dependency-name
	  last-updated
	  was-updated-p
	  dependencies-updated-p
	  needs-recompiling-p
	  update-available-p
	  dependency-update))

(defclass system-package (dependency)
  ((version :initarg :version :accessor system-package-version)))

(defgeneric was-updated-p (dependency)
  (:documentation "Checks if the dependency was updated since the last time the database
was updated"))

(defgeneric dependencies-updated-p (dependency)
  (:documentation "Checks if any of the project's dependencies were updated since the last time
the database was updated"))

(defgeneric needs-recompiling-p (dependency)
  (:documentation "Checks if the project hasn't been compiled since the last pull from the remote repository"))

(defgeneric update-available-p (dependency)
  (:documentation "Checks if the the dependency's source code can be updated"))

(defgeneric dependency-update (dependency)
  (:documentation "Updates the dependency and installs it"))

(defgeneric get-dependencies (dependency)
  (:documentation "Returns the dependencies of the project as a list of ids"))
