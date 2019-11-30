;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CLSQL-SYS; Base: 10 -*-

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

(in-package :clsql-sys)

#+sqlite
(defmethod output-sql ((stmt sql-create-view) database)
  (with-slots (name column-list query with-check-option) stmt
    (write-string "CREATE VIEW " *sql-stream*)
    (output-sql name database)
    #+mysql
    (when column-list (write-string " " *sql-stream*)q
          (output-sql (listify column-list) database))
    (write-string " AS " *sql-stream*)
    (with-slots (selections) query
      (when column-list
	(setf selections (mapcar #'cons selections column-list))))
    (output-sql query database)
    (when with-check-option (write-string " WITH CHECK OPTION" *sql-stream*))))

(defmethod database-output-sql ((arg list) database)
  (cond ((null arg)
	 +null-string+)
	((listp (cdr arg))
	 (format nil "(~{~A~^,~})" (mapcar #'(lambda (val)
					       (sql-output val database))
					   arg)))
	(t
	 (format nil "~A ~A" (sql-output (car arg) database) (sql-output (cdr arg) database)))))


;; patch for left join
#+sqlite
(defmethod output-sql ((query sql-query) database)
  (with-slots (distinct selections from where group-by having order-by
                        limit offset inner-join left-join natural-join on all set-operation)
      query
    (when *in-subselect*
      (write-string "(" *sql-stream*))
    (write-string "SELECT " *sql-stream*)
    (when all
      (write-string "ALL " *sql-stream*))
    (when (and distinct (not all))
      (write-string "DISTINCT " *sql-stream*)
      (unless (eql t distinct)
        (write-string "ON " *sql-stream*)
        (output-sql distinct database)
        (write-char #\Space *sql-stream*)))
    (output-sql (apply #'vector selections) database)
    (when from
      (write-string " FROM " *sql-stream*)
      (flet ((ident-table-equal (a b)
               (and (if (and (eql (type-of a) 'sql-ident-table)
                             (eql (type-of b) 'sql-ident-table))
                        (string-equal (slot-value a 'alias)
                                      (slot-value b 'alias))
                        t)
                    (string-equal (sql-escape (slot-value a 'name))
                                  (sql-escape (slot-value b 'name))))))
        (typecase from
          (list (output-sql (apply #'vector
                                   (remove-duplicates from
                                                      :test #'ident-table-equal))
                            database))
          (string (write-string from *sql-stream*))
          (t (let ((*in-subselect* t))
               (output-sql from database))))))
    (when inner-join
      (write-string " INNER JOIN " *sql-stream*)
      (output-sql inner-join database))
    (when left-join
      (dolist (table left-join)
	(write-string " LEFT JOIN " *sql-stream*)
	(output-sql table database)))
    (when natural-join
      (write-string " NATURAL JOIN " *sql-stream*)
      (output-sql natural-join database))
    (when on
      (write-string " ON " *sql-stream*)
      (output-sql on database))
    (when where
      (write-string " WHERE " *sql-stream*)
      (let ((*in-subselect* t))
        (output-sql where database)))
    (when group-by
      (write-string " GROUP BY " *sql-stream*)
      (if (listp group-by)
          (do ((order group-by (cdr order)))
              ((null order))
            (let ((item (car order)))
              (typecase item
                (cons
                 (output-sql (car item) database)
                 (format *sql-stream* " ~A" (cadr item)))
                (t
                 (output-sql item database)))
              (when (cdr order)
                (write-char #\, *sql-stream*))))
          (output-sql group-by database)))
    (when having
      (write-string " HAVING " *sql-stream*)
      (output-sql having database))
    (when order-by
      (write-string " ORDER BY " *sql-stream*)
      (if (listp order-by)
          (do ((order order-by (cdr order)))
              ((null order))
            (let ((item (car order)))
              (typecase item
                (cons
                 (output-sql (car item) database)
                 (format *sql-stream* " ~A" (cadr item)))
                (t
                 (output-sql item database)))
              (when (cdr order)
                (write-char #\, *sql-stream*))))
          (output-sql order-by database)))
    (when limit
      (write-string " LIMIT " *sql-stream*)
      (output-sql limit database))
    (when offset
      (write-string " OFFSET " *sql-stream*)
      (output-sql offset database))
    (when *in-subselect*
      (write-string ")" *sql-stream*))
    (when set-operation
      (write-char #\Space *sql-stream*)
      (output-sql set-operation database)))
  t)

:eof