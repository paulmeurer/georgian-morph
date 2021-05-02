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

(in-package :clsql-mysql)

#+allegro
(defvar *saved-locale*)

#+allegro
(defmethod database-query :around (query-expression (database database) 
						    result-set field-names)
  (let ((*saved-locale* excl::*locale*))
    (if (eq (slot-value (excl::locale-external-format excl::*locale*) 'excl::name) :utf8-base)
	(let ((excl::*locale* (excl::find-locale "C")))
	  (call-next-method))
	(call-next-method))))

#+allegro
(defmethod database-query (query-expression (database mysql-database)
			   result-types field-names)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (let ((mysql-ptr (database-mysql-ptr database)))
    (uffi:with-cstring (query-native query-expression)
      (if (zerop (mysql-real-query mysql-ptr query-native
                                   (expression-length query-expression)))
	  (let ((excl::*locale* *saved-locale*))
	    (let ((res-ptr (mysql-use-result mysql-ptr)))
	      (if res-ptr
		  (unwind-protect
		       (let ((num-fields (mysql-num-fields res-ptr)))
			 (declare (fixnum num-fields))
			 (setq result-types (canonicalize-types
					     result-types num-fields
					     res-ptr))
			 (values
			  (loop for row = (mysql-fetch-row res-ptr)
				for lengths = (mysql-fetch-lengths res-ptr)
				until (uffi:null-pointer-p row)
				collect
				(do* ((rlist (make-list num-fields))
				      (i 0 (1+ i))
				      (pos rlist (cdr pos)))
				     ((= i num-fields) rlist)
				  (declare (fixnum i))
				  (setf (car pos)
					(convert-raw-field
					 (uffi:deref-array row '(:array
								 (* :unsigned-char))
							   i)
					 result-types i
					 (uffi:deref-array lengths '(:array :unsigned-long)
							   i)))))
			  (when field-names
			    (result-field-names num-fields res-ptr))))
		    (mysql-free-result res-ptr))
		  (error 'sql-database-data-error
			 :database database
			 :expression query-expression
			 :error-id (mysql-errno mysql-ptr)
			 :message (mysql-error-string mysql-ptr)))))
	  (error 'sql-database-data-error
		 :database database
		 :expression query-expression
		 :error-id (mysql-errno mysql-ptr)
		 :message (mysql-error-string mysql-ptr))))))

:eof