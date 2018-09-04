(defpackage :gdep/command
  (:use :cl :alexandria :iterate))

(in-package :gdep/command)

(export '(defcommand
	  get-command-func
	  print-available-commands
	  print-info-text))

(defclass gdep-command ()
  ((info :initarg :info
	:reader command-info
	:type 'string
	:initform (error "You must supply an info string"))
   (more-info :initarg :more-info
	      :reader command-more-info)
   (command :initarg :command
	    :reader command-function
	    :type 'function)
   (name :initarg :name
	 :reader command-name
	 :type 'string)))


(defvar *gdep-commands* (make-hash-table :test #'equal))

(defmacro defcommand (name (&rest args) (info &key command-used more-info)
		      &body body)
  (multiple-value-bind (body decls docstring) (parse-body body :documentation t)
    `(progn
      (defun ,name ,args
	,@(when docstring
	    (list docstring))
	,@decls
	,@body)
      ,(let ((cmd-name (or command-used (string-downcase (string name)))))
	 `(setf (gethash ,cmd-name *gdep-commands*) (make-instance 'gdep-command
								     :name ,cmd-name
								     :command (function ,name)
								     :info ,info
								     :more-info ,more-info))))))
(declaim (inline get-command))
(defun get-command (command-name)
  (gethash command-name *gdep-commands*))

(defun get-command-func (command-name)
  (when-let ((cmd-object (get-command command-name)))
    (command-function cmd-object)))

(defun print-available-commands (&optional (stream *standard-output*))
  (format stream "Available commands:~%")
  (iter (for (cmd-name command) in-hashtable *gdep-commands*)
	(let ((doc (command-info command)))
	  (cl-ansi-text:with-color (:white :effect :bright :stream stream)
	    (format stream "~&~4T~A" cmd-name))
	  (format stream "~15T~A" doc)))
  (format stream "~%"))

(defun print-info-text (command-name &optional (stream *standard-output*))
  (when-let ((cmd (get-command command-name)))
    (cl-ansi-text:with-color (:white :effect :bright :stream stream)
      (format stream "~A:" (command-name cmd)))
    (format stream "~15T~A~%" (command-info cmd))
    (when-let ((more-info (command-more-info cmd)))
      (if (listp more-info)
	  (dolist (line more-info)
	    (format stream "~4T")
	    (format stream line))
	  (progn
	    (format stream "~4T")
	    (format stream more-info)))
      (format stream "~%"))
    t))
