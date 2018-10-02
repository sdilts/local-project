(defpackage :lpro/command
  (:use :cl :alexandria :iterate))

(in-package :lpro/command)

(export '(defcommand
	  get-command-func
	  print-available-commands
	  print-info-text))

(defclass lpro-command ()
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

(defvar *lpro-commands* (make-hash-table :test #'equal))
(defvar *longest-command-length* 0)

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
	 `(add-command (make-instance 'lpro-command
				      :name ,cmd-name
				      :command (function ,name)
				      :info ,info
				      :more-info ,more-info))))))
(defun add-command (command)
  (declare (type lpro-command command))
  (let ((name (command-name command)))
    (setf (gethash name *lpro-commands*) command)
    ;; check to see how long the command is for pretty printing:
    (when (> (length name) *longest-command-length*)
      (setf *longest-command-length* (length name))))
  command)

(declaim (inline get-command))
(defun get-command (command-name)
  (gethash command-name *lpro-commands*))

(defun get-command-func (command-name)
  (when-let ((cmd-object (get-command command-name)))
    (command-function cmd-object)))

(defun print-available-commands (&optional (stream *standard-output*))
  (format stream "Available commands:~%")
  (iter (for (cmd-name command) in-hashtable *lpro-commands*)
	(let ((doc (command-info command))
	      ;; the base indent, plus the length of the ansi color directive if present
	      (description-indent (if cl-ansi-text:*enabled*
				      (+ 4 5)
				      5)))
	  (cl-ansi-text:with-color (:white :effect :bright :stream stream)
	    (format stream "~&~4T~A" cmd-name))
	  (format stream "~vT~A" (+ description-indent *longest-command-length*)
		  doc)))
  (format stream "~%"))

(defun print-info-text (command-name &optional (stream *standard-output*))
  (when-let ((cmd (get-command command-name)))
    (cl-ansi-text:with-color (:white :effect :bright :stream stream)
      (format stream "~A:" (command-name cmd)))
    (format stream "~10T~A~%" (command-info cmd))
    (when-let ((more-info (command-more-info cmd)))
      (if (listp more-info)
	  (dolist (line more-info)
	    (format stream "~4T")
	    (format stream line)
	    (format stream "~%"))
	  (progn
	    (format stream "~4T")
	    (format stream more-info)))
      (format stream "~&"))
    t))
