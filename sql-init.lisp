;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: XLE; Base: 10 -*-

;; The contents of this file are subject to a License (LFG Parsebanker License;
;; see the file "license.txt" in this directory);
;; you may not use this file except in compliance with the License.
;; Software distributed under the License is distributed on an "AS IS"
;; basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied.
;; See the License for the specific language governing rights and limitations under the License.
;; This Source Code was developed by the University of Bergen and Unifob AS.
;; Portions created by University of Bergen and Unifob AS are Copyright ©
;; of the University of Bergen and Unifob AS. All Rights Reserved.

;; paul.meurer@aksis.uib.no
;; Aksis, Unifob, University of Bergen.
;; http://www.aksis.uib.no/

(in-package :xle)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (shadowing-import '(status date) :clsql)
  (use-package :clsql))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf clsql-sys::*original-readtable* nil)
  (enable-sql-reader-syntax))

#.(locally-enable-sql-reader-syntax)

#+test
(print (execute-command "set names UTF8;"))

#+test
(start-sql-recording :type :commands); :both)
#+test
(stop-sql-recording :type :both)

;; patch for UTF-8
#+allegro
(defmethod execute-command :around ((sql-expression string) &rest rest &key (database *default-database*))
  #+debug(print sql-expression)
  (if (eq (slot-value (excl::locale-external-format excl::*locale*) 'excl::name) :utf8-base)
      (let ((excl::*locale* (excl::find-locale "C")))
	(call-next-method));; (utf-8-encode sql-expression) :database database))
      (call-next-method)))

#+(and :allegro :mysql)
(defun clsql-mysql::%mysql-real-query (mysql-ptr query-native exp-length)
  (if (eq (slot-value (excl::locale-external-format excl::*locale*) 'excl::name) :utf8-base)
      (let ((excl::*locale* (excl::find-locale "C")))
	(clsql-mysql::mysql-real-query mysql-ptr query-native exp-length))
      (clsql-mysql::mysql-real-query mysql-ptr query-native exp-length)))

#+(and :allegro :mysql)
(defmethod clsql-sys::database-output-sql :around ((str string) database)
  (if (eq (slot-value (excl::locale-external-format excl::*locale*) 'excl::name) :utf8-base)
      (let ((excl::*locale* (excl::find-locale "C")))
	(call-next-method (utf-8-encode str) database))
      (call-next-method)))

#+(and :allegro :mysql)
(defmethod clsql-mysql::database-query-result-set :around ((query-expression string)
							   (database clsql-mysql::mysql-database)
							   &key full-set result-types)
  (let* ((encoding (slot-value (excl::locale-external-format excl::*locale*) 'excl::name)))
    (if (eq encoding :utf8-base)
	(let ((excl::*locale* (excl::find-locale "C"))
	      (query-expression (encoding::utf-8-encode query-expression)))
	  (call-next-method query-expression database :full-set full-set :result-types result-types))
	(call-next-method))))

#+allegro
(defun clsql-uffi::convert-raw-field (char-ptr types index &optional length)
  (declare (optimize (speed 3) (safety 0) (space 0))
 	   (type clsql-uffi::char-ptr-def char-ptr))
  (let ((type (if (consp types)
		  (nth index types)
		  types)))
    (cond
      ((uffi:null-pointer-p char-ptr)
       nil)
      (t
       (case type
	 (:double
	  (clsql-uffi::atof char-ptr))
	 (:int
	  (clsql-uffi::atol char-ptr))
	 (:int32
	  (clsql-uffi::atoi char-ptr))
	 (:uint32
	  (clsql-uffi::strtoul char-ptr))
	 (:uint
	  (clsql-uffi::strtoul char-ptr))
	 ((:int64 :uint64)
	  (uffi:with-foreign-object (high32-ptr :unsigned-int)
	    (let ((low32 (clsql-uffi::atol64 char-ptr high32-ptr))
		  (high32 (uffi:deref-pointer high32-ptr :unsigned-int)))
	      (if (zerop high32)
		  low32
		(clsql-uffi::make-64-bit-integer high32 low32)))))
	 (:blob
	  (if length
	      (uffi:convert-from-foreign-usb8 char-ptr length)
	    (error "Can't return blob since length is not specified.")))
	 (t
	  ;; sb-unicode still broken with converting with length, assume
	  ;; that string is null terminated
	  #+sb-unicode
	  (uffi:convert-from-foreign-string char-ptr :locale :none)
	  #-sb-unicode
          (if length
	      (uffi:convert-from-foreign-string char-ptr ;; :locale :none ;; fixed pm
                                                :null-terminated-p nil
                                                :length length)
            (uffi:convert-from-foreign-string char-ptr :locale :none))))))))

#+test
(insert-records :into [fifi] :values (list "aclåøæff"))

;; bug fix for initialize-instance() in ooddl.lisp; report to Kevin Rosenberg!
(defmethod initialize-instance ((object clsql-sys::standard-db-object)
				&rest all-keys &key &allow-other-keys)
  (declare (ignore all-keys))
  (call-next-method))

(defmethod initialize-instance :around ((object clsql-sys::standard-db-object)
					&rest all-keys &key &allow-other-keys)
  (declare (ignore all-keys))
  (let ((clsql-sys::*db-initializing* t))
    (call-next-method)
    (when (and *db-auto-sync*
	       (not clsql-sys::*db-deserializing*))
      (update-records-from-instance object))))

(clsql-sys::defsql clsql-sys::sql-length (:symbol "length") (&rest rest)
		   (make-instance 'clsql-sys::sql-function-exp
				  :name 'length :args rest))

(clsql-sys::defsql clsql-sys::sql-replace (:symbol "replace") (&rest rest)
		   (make-instance 'clsql-sys::sql-function-exp
				  :name 'replace :args rest))

(clsql-sys::defsql clsql-sys::sql-locate (:symbol "locate") (&rest rest)
		   (make-instance 'clsql-sys::sql-function-exp
				  :name 'locate :args rest))

(clsql-sys::defsql clsql-sys::sql-if (:symbol "if") (&rest rest)
		   (make-instance 'clsql-sys::sql-function-exp
				  :name 'if :args rest))

;;(defmacro when? (cond &rest body) `(if ,cond (progn ,@body) [= 1 1]))

;; for compatibility with acache
(defmethod delete-instance ((instance standard-db-object))
  (when (clsql-sys::view-database instance) 
    (delete-instance-records instance)))

(defparameter *connection-spec*
  #+mysql "localhost/kartuli/treebank/Heebi" ;;"maximos.aksis.uib.no/kartuli/treebank/Heebi"
  #+sqlite
  (list (namestring (translate-logical-pathname "projects:georgian-morph;kartuli.sqlite")))
  )

;;(defparameter *connection-spec* '("localhost" "treebank1" "treebank" "Heebi")) ;; host db user passwd
;;(defparameter *connection-spec* (list "/Users/paul/lolo.db")) ;; host db user passwd

;; this assumes that you have CREATE DATABASE privileges;
;; better do it as root like:
;; mysql> create database kartuli;
;; mysql> grant all on kartuli.* to 'treebank'@'localhost';
#+mysql
(ignore-errors
  (unless (probe-database *connection-spec* :database-type :mysql)
    (create-database *connection-spec* :database-type :mysql)))

;;(create-database *connection-spec* :database-type :sqlite3)

;; move!
(defun make-directory-rec (dir)
  (unless (probe-file (translate-logical-pathname dir))
    (let ((sub-dir (make-pathname :directory (nreverse (cdr (reverse (pathname-directory dir))))
				  :host (pathname-host dir))))
      (make-directory-rec sub-dir)
      #+allegro(sys::make-directory dir)
      #+sbcl(sb-unix:unix-mkdir (namestring (translate-logical-pathname dir)) #o777))))

#+sqlite
(ignore-errors
  (unless (probe-database *connection-spec* :database-type :sqlite3)
    (unless (probe-file "projects:treebank;sqlite;")
      (make-directory-rec "projects:treebank;sqlite;"))
    (create-database *connection-spec* :database-type :sqlite3)))

#+test
(disconnect-pooled)
#+test
(disconnect)

(progn (connect (print *connection-spec*)
		:pool #+sqlite nil #+mysql t
		:if-exists :old
		:database-type #+sqlite :sqlite3 #+mysql :mysql)
       (execute-command (format nil "set wait_timeout=~a;" (* 7 24 60 60))) ;; one week; default is (* 8 60 60)
       (set-autocommit nil))

;; status() is not thread save even inside with-database() !!
#+test
(clsql-sys::status t)

(defmacro with-database-connection (() &body body)
  `(with-database (*default-database* *connection-spec* :pool t :make-default nil :database-type #+mysql :mysql #+sqlite :sqlite3)
    (progn ,@body)))

;; "grant all privileges on treebank.* to 'treebank'@'localhost' identified by 'Heebi';"
;; "grant all privileges on treebank.* to 'treebank'@'%.no' identified by 'Heebi';"
;; "grant all privileges on treebank.* to 'treebank'@'129.177.%' identified by 'Heebi';"

;; ALTER DATABASE db_name [[DEFAULT] CHARACTER SET charset_name] [[DEFAULT] COLLATE collation_name]

;; ALTER DATABASE kartuli CHARACTER SET utf-8;



(setf *db-auto-sync* t)

#+test
(let (#+ignore(excl::*locale* (excl::find-locale "en_EN")))
  (print (select [sentence] :from [sentence] :where [= [id] 6])))

#||

(create-table [fifi] '((text string)))

#+test
(print (execute-command "set names UTF8;"))

(let ((excl::*locale* (excl::find-locale "en_EN")))
  (insert-records :into [fifi] :values (list "åfüföx")))

||#

:eof
