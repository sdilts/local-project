(defpackage #:gdep/util
  (:use :cl :alexandria))

(in-package :gdep/util)

(export '(run-menu
	  directory-already-exists-error
	  create-dir))

(define-condition directory-already-exists-error (error)
  ((location :initarg :location :reader location))
  (:default-initargs
   :location (error "You must supply a location"))
  (:report (lambda (condition stream)
	     (format stream "Directory \"~S\" already exists" (location condition)))))

(define-condition abort-operation (error)
  ((operation :initarg :operation
	      :reader operation))
  (:report (lambda (condition stream)
	     (format stream "Operation ~S aborted" (operation condition)))))

(defun create-dir (dir &key (error-already-exists nil))
  (assert (uiop:directory-pathname-p dir))
  (restart-case
      (cond
	((not (uiop:directory-exists-p dir))
	 ;; try to create the directories:
	 (ensure-directories-exist dir)
	 ;; check if they were actually created:
	 (uiop:directory-exists-p dir))
	(already-exists-p (cerror (format nil "Use already existing \"~A\"" dir)
				  'directory-already-exists-error :location dir)
			  t))
    (clear-directory ()
      :report (lambda (stream)
		(format stream "Erase everything in \"~A\"" dir))
      (uiop:delete-directory-tree dir)
      (ensure-directories-exist dir))))

(defun build-help-text (forms)
  `(progn
     (format t "~&Available options:~%")
     ,@(loop for option in forms
	  collect
	    (let ((input-char (first option))
		  (help-form (second option)))
       	      `(progn
       		 (format t "~&~3T~A - " ,input-char)
		 ,(if (listp help-form)
       		     `(format t ,(car help-form) ,@(cdr help-form))
       		     `(format t ,help-form)))))
     (format t "~&")))

(defun build-prompt-text (forms &key (options-shown 2) (default (caar forms)))
  (with-output-to-string (stream)
    (format stream "~~&[~A" default)
    (loop for option in (cdr forms) repeat (- options-shown 1) do
      (let ((input-char (first option)))
	(format stream "/~A" input-char)))
    (when (> (length forms) options-shown)
      (format stream "/..."))
    (format stream "/ ? Shows all options] (~A): " default)))

;; future versions of this macro could use a hashtable for larger menus
(defmacro run-menu ((&key (default-choice t) (options-shown 2) (repeat-prompt nil)
			  (always-show-help nil))
		       prompt &body options)
  "(run-menu (menu-options) prompt options), where options is of form (choice-string option-info &body forms).
The menu wil only exit when (return-from run-menu) is called"
  (declare (ignore default-choice))
  (let ((print-help (build-help-text-function options))
	(prompt-text (build-prompt-text options :options-shown options-shown)))
    (with-gensyms (answer)
      `(block run-menu
	 ,(if (listp prompt)
	      `(format t ,(car prompt) ,@(cdr prompt))
	      `(format t ,prompt))
	 (loop
	    ;; use a progn so we can have nil in the code:
	    (progn
	      ,(when always-show-help
		 print-help)
	      ,(when repeat-prompt
		 (if (listp prompt)
		     `(format t ,(car prompt) ,@(cdr prompt))
		     `(format t ,prompt))))
	    (cl-ansi-text:with-color (:white :effect :bright)
	      (format t ,prompt-text))
	    (finish-output)
	    (setf ,answer (read-line))
	    (switch (,answer :test #'string-equal)
	      ,@(append
		 ;; build user-supplied options
		 (loop for opt in options collect
		      (let ((values ()))
			(push (first opt) values)
			(append values (cddr opt))))
		 ;; build the help text entry:
		 `(("?" ,print-help))
		 ;; default option
		 `(("" ,@(cddr (first options))))
		 ;; invalid option
		 `((t (format t (cl-ansi-text:red "~&Invalid answer ~S.") ,answer)))))
	    (clear-input))))))
