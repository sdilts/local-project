(defpackage #:lpro/dependency
  (:use :cl :lpro/dependency-protocol))

(in-package #:lpro/dependency)

(export '(dependency
	  dependency-name
	  dependency-id
	  dependency-type
	  dependency-version))

(defclass dependency ()
  ((name :initarg :name :accessor dependency-name
	 :type 'string)
  (version :initarg :version
	   :accessor dependency-version)
   (id :initarg :id
       :accessor dependency-id
       :type #'numberp)
   (type :initarg :type
	 :reader dependency-type
	 :type keyword)))

;; (defmethod print-object
