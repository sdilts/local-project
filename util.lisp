;; a file for functions that are shared across files

(defpackage #:gdep/util
  (:use :cl :alexandria))

(in-package :gdep/util)

(export '(run-menu
	  run-script
	  directory-already-exists-error
	  directory-dne-error
	  create-dir
	  scan-types))

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

(define-condition directory-creation-error (error)
  ((location :initarg :location :reader location))
  (:default-initargs
   :location (error "You must supply a location"))
  (:report (lambda (condition stream)
	     (format stream "Directory \"~S\" could not be created" (location condition)))))

(define-condition directory-dne-error (error)
  ((location :initarg :location :reader location))
  (:default-initargs
   :location (error "You must supply a location"))
  (:report (lambda (condition stream)
	     (format stream "Directory \"~S\" does not exist." (location condition)))))

(defun create-dir (dir &key (error-already-exists nil))
  (assert (uiop:directory-pathname-p dir))
  (restart-case
      (cond
	((not (uiop:directory-exists-p dir))
	 ;; try to create the directories:
	 (ensure-directories-exist dir)
	 ;; check if they were actually created:
	 (unless (uiop:directory-exists-p dir)
	   (error 'directory-creation-error :location dir))
	 t)
	(error-already-exists (cerror (format nil "Directory \"~A\" already exists." dir)
				  'directory-already-exists-error :location dir)
			  t))
    (clear-directory ()
      :report (lambda (stream)
		(format stream "Erase everything in \"~A\"" dir))
      (uiop:delete-directory-tree dir)
      (ensure-directories-exist dir)
      (unless (uiop:directory-exists-p dir)
	(error 'directory-creation-error :location dir))
      t)))

(defun run-script (script location)
  (declare (type pathname script location))
  (assert (uiop:file-exists-p script))
  (restart-case
      (when (not (uiop:directory-exists-p location))
	(error 'directory-dne-error :location location))
    (create-directory ()
      :report (lambda (stream)
		(format stream "Create directory ~A" location))
      (create-dir location)))
  (let ((command (concatenate 'string (namestring script) " " (namestring location))))
    (format *debug-io* "Running shell script: ~S~%" command)
    (inferior-shell:run/interactive command)))

(defmacro scan-types ((data-dir type-dir) (pathname type-name) &body body)
  "Iterate the subdirectories in the path DATA-DIR/TYPE-DIR with PATHNAME bound to the
path of the subdirectory and TYPENAME bound to subdirectory name translated into a keyword."
  (let ((dir-list (gensym "dir-list"))
	(build-keyword-func (gensym "build-keyword-func")))
    `(let ((,dir-list (uiop:subdirectories (merge-pathnames (make-pathname :directory
									   '(:relative
									     ,type-dir))
							    ,data-dir))))
       (flet ((,build-keyword-func (string)
		(ecase (readtable-case *readtable*)
		  (:upcase (intern (string-upcase string) "KEYWORD"))
		  (:downcase (intern (string-downcase string) "KEYWORD"))
		  (:preserve (intern string "KEYWORD")))))
	 (dolist (,pathname ,dir-list)
	   (let ((,type-name (,build-keyword-func (alexandria:last-elt
						   (pathname-directory ,pathname)))))
	     ,@body))))))


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
(defmacro run-menu ((&key (default-choice t) (num-options-shown 2) (repeat-prompt nil)
			  (always-show-help nil))
		       prompt &body options)
  "(run-menu (menu-options) prompt options), where options is of form (choice-string option-info &body forms).
The menu wil only exit when (return-from run-menu) is called"
  (declare (ignore default-choice))
  (let ((print-help (build-help-text options))
	(prompt-text (build-prompt-text options :options-shown num-options-shown)))
    (with-gensyms (answer)
      `(block run-menu
	 ,(when (not repeat-prompt)
	    (if (listp prompt)
		`(format t ,(car prompt) ,@(cdr prompt))
		`(format t ,prompt)))
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
	    (let ((,answer (read-line)))
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
	      (clear-input)))))))
