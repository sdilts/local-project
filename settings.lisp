(defpackage #:lpro/settings
  (:use :cl))


(in-package #:lpro/settings)

(export '(*data-directory*))

;; TODO: change this directory:
(defvar *data-directory* (uiop:ensure-directory-pathname #p"/home/stuart/Programs/git-install/data"))

;; TODO: change this:
(defvar *pkg-database* "rpm"
  "The system database type")

(defvar *pkg-manager* "zypper"
  "The system package manager")
