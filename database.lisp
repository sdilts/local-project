(defpackage #:gdep/database
  (:use :cl :lambdalite :alexandria :gdep/project :gdep/dependency))

(in-package #:gdep/database)

(export '(all-projects
	  all-dependencies
	  get-project
	  get-dependency
	  add-project))

;; for the dependency table:
(defattributes
  :/dependency-id #'numberp
  :/dependency-name #'stringp
  :/dependency-version #'numberp
  :/dependency-type #'keywordp)

;; for the project table:
(defattributes
  ;; denpendency id already declared
  :/location #'pathnamep
  :/version-control-type #'keywordp
  :/build-type #'keywordp
  :/compilation-location #'keywordp
  :/version-date #'numberp
  :/url #'stringp)

;; for the requires table
(defattributes
  :/project #'numberp
  :/depends-on #'numberp
  ;; version-date already declared
  )

;; for  the project counter:
(defattributes
  :/project-counter #'numberp)

(defun get-new-dependency-id ()
  ;; A hack: store a counter and increment it every time we need a new id
  (if-let ((id (:/project-counter (select1 :counter-db))))
    (progn
      (lambdalite:update :counter-db (lambdalite:where t) (lambdalite:keyset
							   :/project-counter
							   (+ 1 id)))
      (return-from get-new-dependency-id id))
    (progn
      (lambdalite:insert :counter-db '(:/project-counter 1))
      (return-from get-new-dependency-id 0))))

(defun get-dependency (dependency-name)
  "Retrives a dependency from the database"
  (if-let ((dep-part (lambdalite:select1 :dependency-db
					 (where (equal :/dependency-name dependency-name)))))
    (make-instance 'dependency
		   :name (:/dependency-name dep-part)
		   :version (:/dependency-version dep-part)
		   :id (:/dependency-id dep-part)
		   :type (:/dependency-type dep-part))))

(defun get-project (project-name)
  "Retrieve a project from the databse"
  (declare (type string project-name))
  (if-let ((dep (get-dependency project-name)))
    (if (equal (dependency-type dep) :project)
	(let ((project-part (lambdalite:select1 :project-db
						(where (equal :/dependency-id
							      (dependency-id dep))))))
	  (change-class dep 'project
			:location (:/location project-part)
			:version-control-type (:/version-control-type project-part)
			:build-type (:/build-type project-part)
			:compilation-location (:/compilation-location project-part)
			:url (:/url project-part))))))

(defun add-project (project)
  "Stores the project in the database"
  (let ((id (get-new-dependency-id)))
    (lambdalite:with-tx
      ;; store the dependency part first:
      (insert :dependency-db (list :/dependency-id id
				   :/dependency-name (dependency-name project)
				   :/dependency-type :project
				   :/dependency-version (dependency-version project)))
      (insert :project-db (list :/dependency-id id
				:/location (project-location project)
				:/version-control-type (version-control-type project)
				:/compilation-location (compilation-location project)
				:/build-type (build-type project)
				:/url (project-url project))))))

(defun join (table1 table2 attribute &key (test #'equal))
  (let ((tbl (make-hash-table))
	(merge-table ()))
     ;; add one of the tables to a hashtable:
     (dolist (entry table1)
       (setf (gethash (funcall attribute entry) tbl) entry))
     (dolist (entry table2)
       (let ((to-merge (copy-list (gethash (funcall attribute entry) tbl))))
	 (remf to-merge attribute)
	 (push (append entry to-merge) merge-table)))
     merge-table))

(defun update-project (project)
  "Update's the given project in the database.")

(defun search-project (&key project-name project-location build-type version-control-type)
  "Searches for the project with the given attributes")

(defun all-projects ()
  "Return a list of all projects in the database"
  (let ((all-proj-rows (join (select :dependency-db)
			     (select :project-db)
			     :/dependency-id)))
    (mapcar (lambda (row)
	     (make-instance 'project
			:location (:/location row)
			:version-control-type (:/version-control-type row)
			:build-type (:/build-type row)
			:compilation-location (:/compilation-location row)
			:url (:/url row)
			:name (:/dependency-name row)
			:version (:/dependency-version row)
			:id (:/dependency-id row)
			:type (:/dependency-type row)))
	    all-proj-rows)))

(defun all-dependencies ()
  "return a list of all projects and dependencies in the database")
