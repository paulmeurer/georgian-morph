;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10 -*-

;; paul.meurer@uni.no

;; paradigm completion in paradigm-completion.lisp

;; add-related-vn-pair()
 

(in-package :fst)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (shadowing-import '(status) :clsql)
  (use-package :clsql))

#-iness2
(defparameter *connection-spec* "localhost/gnc/gnc/kartuli")
#+iness2
(defparameter *connection-spec* "iness.uib.no/gnc/gnc/kartuli")

(setf *default-database-type* :postgresql)

(progn (connect (print *connection-spec*)
		:pool t
		:if-exists :old
		:database-type :postgresql);; :mysql)
       ;;(execute-command (format nil "set wait_timeout=~a;" (* 7 24 60 60))) ;; one week; default is (* 8 60 60)
       (set-autocommit nil))

;; status() is not thread save even inside with-database() !!
#+test
(clsql-sys::status t)

(setf *db-auto-sync* t)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf clsql-sys::*original-readtable* nil)
  (enable-sql-reader-syntax))

#.(locally-enable-sql-reader-syntax)

;;#+test
(set-macro-character clsql-sys::*sql-macro-open-char* #'clsql-sys::sql-reader-open)
;;#+test
(set-macro-character clsql-sys::*sql-macro-close-char* (get-macro-character #\)))

#+test
(setf cl-user::*readtable* clsql-sys::*original-readtable*)


:eof