;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: XLE; Base: 10 -*-

;; paul.meurer@uib.no
;; https://clarino.uib.no/

;; PostgreSQL
;;  pg_dump -U gnc gnc > gnc-2013-08-22.sql
;;  (pw: kartuli)
;; import: 
;; psql -U gnc -d gnc < gnc-2013-01-15.sql
;;
;; paradigm completion in paradigm-completion.lisp

(in-package :xle)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (shadowing-import '(status) :clsql)
  (use-package :clsql))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf clsql-sys::*original-readtable* nil)
  (enable-sql-reader-syntax))

#.(locally-enable-sql-reader-syntax)

(set-macro-character clsql-sys::*sql-macro-open-char* #'clsql-sys::sql-reader-open)
(set-macro-character clsql-sys::*sql-macro-close-char* (get-macro-character #\)))

#+test
(setf cl-user::*readtable* clsql-sys::*original-readtable*)

(def-view-class noun-features ()
  ((stem :initform nil :initarg :root :reader root :type string :db-kind :key)
   (code :initform nil :initarg :code :reader code :type string :db-kind :key)
   (pos :initform nil :initarg :pos :reader part-of-speech :type string :db-kind :key)
   (sub-id :initform nil :initarg :unique-id :reader unique-id :type integer :db-kind :key)
   (features :initform nil :initarg :features :reader features :type string)
   (template :initform nil :initarg :template :reader template :type string)
   (comment :initform nil :initarg :comment :reader comment :type string :db-type "mediumtext")
   (author :initform nil :initarg :author :reader author :type string)
   (translation :initform "" :initarg :translation :reader translation :type "mediumtext")
   (date :initform nil :initarg :date :reader date :type integer :db-type "bigint"))
  (:base-table noun-features))

:eof
