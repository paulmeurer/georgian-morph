;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: XLE; Base: 10 -*-

;; paul.meurer@uni.no
;; Uni Computing, University of Bergen.
;; http://www.uni.no/

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

;;#+test
(set-macro-character clsql-sys::*sql-macro-open-char* #'clsql-sys::sql-reader-open)
;;#+test
(set-macro-character clsql-sys::*sql-macro-close-char* (get-macro-character #\)))

#+test
(setf cl-user::*readtable* clsql-sys::*original-readtable*)

#+test
(print [a b])

(def-view-class noun-features ()
  ((stem :initform nil :initarg :root :reader root :type string :db-kind :key) ;; collate latin1_bin
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

#+once
(dolist (class '(noun-features))
  ;;(drop-table 'noun-features-xx)
  (unless (table-exists-p (view-table (find-class class)))
    (create-view-from-class class)
    (when (eq class 'noun-features)
      (execute-command "alter table NOUN_FEATURES modify STEM VARCHAR(255) character set utf8 collate utf8_bin;"))))

#+test
(unless (find "TRANSLATION" (list-attributes [noun-features]) :test #'string=)
  (execute-command "alter table NOUN_FEATURES add TRANSLATION MEDIUMTEXT after AUTHOR;"))

#+test
(let ((date (get-universal-time))
      (sub-id 1)
      (prev-stem "")
      (prev-pos "")
      (prev-code nil)
      (stem-pos-code ()))
  (with-transaction ()
    (with-file-lines (line "projects:georgian-morph;georgian-nouns.txt")
      (let ((line (string-trim " " (subseq line 0 (position #\# line)))))
	(unless (string= line "")
	  (destructuring-bind (stem codes) (split line #\:)
	    (destructuring-bind (code &rest readings) (split codes #\space)
	      (dolist (reading readings)
		(destructuring-bind (pos &optional features) (split reading #\+ 2)
		  (print (list (fst::convert stem) code pos features))
		  (unless (find (list stem pos code) stem-pos-code :test #'equal)
		    (cond ((and (string= stem prev-stem)
				(string= pos prev-pos)
				(equal code prev-code))
			   (incf sub-id))
			  (t (setf sub-id 1)))
		    (unless (string= stem prev-stem)
		      (setf stem-pos-code ()))
		    (push (list stem pos code) stem-pos-code)
		    (setf prev-stem stem prev-pos pos prev-code code)
		    (insert-records :into [noun-features]
				    :attributes `([stem] [code] [pos] [sub-id] [features] [template] [author] [date])
				    :values (list (fst::convert stem) code pos sub-id (when features (concat "+" features)) nil "Tsch" date))))))))))))

#+test
(let ((date (get-universal-time)))
  (with-transaction ()
    (with-file-lines (name "projects:georgian-morph;first-names-tmp.txt")
      (when (> (length name) 0)
	(let* ((conjugation (case (char name (1- (length name)))
			      (#\a :K)
			      (#\e :M)
			      (#\o :O)
			      (#\i :X)
			      (#\u :O)
			      (otherwise :Z)))
	       (code (string-upcase conjugation))
	       (sub-id (or (car (select [max [sub-id]]
					:from [noun-features]
					:flatp t
					:where [and [= [stem] name]
						    [= [code] code]
						    [= [pos] "n"]]))
			   0)))
	  (insert-records :into [noun-features]
			  :attributes `([stem] [code] [pos] [sub-id] [features] [template] [author] [date])
			  :values (print (list name code "n" (1+ sub-id) "+N+Prop+Name+FirstName" nil "Opentext" date))))))))


#+test
(with-transaction ()
  (update-records [noun-features]
		  :attributes '([features])
		  :values (list "+Anim")
		  :where [like [stem] "%log"]))

#+test
(let ((tree (dat:make-string-tree)))
  (u:with-file-lines (line "projects:georgian-morph;wordlists;soplebi-list.txt")
    (let* ((paren-start (position #\( line))
	   (word (subseq line 0 paren-start)))
      (setf word (string-trim " .	1234567890" word))
      (cond ((= (length word) 1)
	     nil)
	    (t
	     (setf (dat:string-tree-get tree (nreverse word)) t)))))
  (dat:do-string-tree (lemma ignore tree)
    (setf lemma (reverse lemma))
    (let ((space-pos (position #\space lemma)))
      (when (and space-pos (char= (char lemma (1- space-pos)) #\ი))
	(setf (char lemma (1- space-pos)) #\=)))
    (format t "~a+~a+City~%"
	    lemma
	    (case (char lemma (1- (length lemma)))
	      (#\ი "A")
	      (otherwise "O")))))


:eof
