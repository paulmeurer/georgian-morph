;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10 -*-

;; paul.meurer@uni.no

(in-package :fst)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf clsql-sys::*original-readtable* nil)
  (enable-sql-reader-syntax))

#.(locally-enable-sql-reader-syntax)


(set-macro-character clsql-sys::*sql-macro-open-char* #'clsql-sys::sql-reader-open)

(set-macro-character clsql-sys::*sql-macro-close-char* (get-macro-character #\)))

(defun fst::load-root-table ()
  (clrhash fst::*root-table*)
  (do-query ((root c-root) [select [root] [c-root]
				   :distinct t
				   :from [morph verb-features]
				   :where [<> [c-root] ""]])
    #+debug(print (list root c-root (fst::inv-convert root) (fst::inv-convert c-root)))
    (pushnew (fst::inv-convert c-root)
	     (gethash (fst::inv-convert root) fst::*root-table*)
	     :test #'string=)))

(fst::load-root-table)

;; overrides the original def.
(defmethod fst::match-stems ((dfa fst::georgian-dfa) string pos)
  "Returns a list of pairs of stems matching string starting with pos and stem dgs."
  (collecting
    (loop for i from (1+ pos) to (length string)
       for stem = (subseq string pos i)
       do
       ;;(dolist (features (gethash stem *noun-table*))
       (do-query ((code pos features)
		  [select [code] [pos] [features]
			  :from [morph noun-features]
			  :where [= [stem] ?stem]])
	 ;;(destructuring-bind (lemma conjugation comment . pos-list) features
	 ;;(dolist (pos pos-list)
	 (let* ((pos (intern (string-upcase pos) :fst))
		(pos (cond ((eq pos 'fst::masd)
			    'fst::n)
			   ((consp pos)
			    (car pos))
			   (t
			    pos)))
		(sub-cat (cond ((eq pos 'fst::masd)
				'fst::masd)
			       ((consp pos)
				(cdr pos))
			       (t
				nil)))
		(personp (search "+Name" features))
		(conjugation (intern code :keyword)))
	   #+debug(print (list pos sub-cat))
	   (collect (cons (fst::list-to-dg (fst::conjugation-features
					    conjugation stem stem pos sub-cat nil nil personp))
			  i)))))))

#+test
(do-query ((&rest row)
	   [select [root] [c-root] [id]
	   :distinct t
	   :from [verb-features]
	   :where [= [root] "lag"]
	   ])
  (print row))

#+test
(print (select [root] [c-root] [id]
	       :distinct t
	       :from [verb-features]
	       :where [= [c-root] "lag"]
	       ))

;;(print (gethash "123-3" fst::*paradigm-table*))

#+test
(with-transaction ()
  (let ((rows (select [unique-id] [pf-sfx]
		     :from [verb-features]
		     :where [and [not [null [pf-sfx]]]
				 [not [= [pf-sfx] "-"]]])))
    (dolist (row rows)
      (destructuring-bind (unique-id pf-sfx) row
	(update-records [verb-features]
			:attributes `([pp-sfx])
			:values (list pf-sfx)
			:where [= [unique-id] unique-id])))))

#+test
(with-transaction ()
  (update-records [verb-features]
		  :attributes (list [pv])
		  :values (list [null])
		  :where [= [pv] "-"]))

(defun update-paradigm-table (&key unique-id id)
  (do-query ((root c-root class id sub-id)
	     [select [root] [verb-paradigm c-root] [verb-paradigm tsch-class]
		     [verb-paradigm id] [verb-paradigm sub-id]
		     :distinct t
		     :from [morph verb-paradigm]
		     :left-join [morph verb-features]
		     :on [and [= [verb-features id] [verb-paradigm id]]
			      [= [verb-features sub-id] [verb-paradigm features-sub-id]]]
		     :where (cond (unique-id
				   [and [= [unique-id] ?unique-id]
					[not [= [verb-paradigm c-root] ""]]])
				  (id
				   [and [= [verb-paradigm id] ?id]
					[not [= [verb-paradigm c-root] ""]]])
				  (t
				   [not [= [verb-paradigm c-root] ""]]))])
    #+debug(print (list root c-root class id sub-id))
    (let ((pid (format nil "~a-~a" id sub-id))
	  (root (fst::inv-convert root))
	  (c-root (fst::inv-convert c-root)))
      (if (gethash pid fst::*paradigm-table*)
	  (pushnew root (cddr (gethash pid fst::*paradigm-table*)) :test #'equal)
	  (setf (gethash pid fst::*paradigm-table*) (list class c-root root))))))

#+copy
(unique-id id sub-id root c-root tense pv vn gv sf caus-sf vv
	   tsch-class morph-type relation reduplication red-dir-pv
	   stem-type pr-st-ext part-pfx part-sfx passive-sfx
	   type-aorist type-obj-3-pfx type-aorist-3sg type-optative
	   nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
	   type-subj3-sfx type-subj2-pfx type-ev-sfx
	   style lang type-pr-st-ext paradigm-replacement)

#+test
(with-transaction ()
  (copy-full-paradigm 156 :new-root "ზღარბ"))

#+test
(progn
  (delete-records :from [morph verb-translation]
		  :where [= [id] 3843])
  (delete-records :from [morph verb-features]
		:where [= [id] 3843])
  (delete-records :from [morph verb-paradigm]
		  :where [= [id] 3843]))

(defun copy-full-paradigm (id &key new-root new-c-root)
  (let* ((new-c-root (or new-c-root new-root))
	 (old-features
	  (select [sub-id] [root] [c-root] [tense]
		  [pv] ;; needed??
		  [vn] ;; needed??
		  [gv] [sf] [caus-sf] [vv]
		  [tsch-class] [morph-type] [relation]
		  ;; [reduplication] [red-dir-pv] ;; needed?
		  [stem-type] [pr-st-ext] [part-pfx] [part-sfx] [passive-sfx]
		  [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
		  [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
		  [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
		  [style] [lang] [type-pr-st-ext] [paradigm-replacement]
		  :from [morph verb-features]
		  :order-by [sub-id]
		  :where [= [id] ?id]))
	 (old-paradigm
	  (select [sub-id] [c-root]
		  [vn] [impf-vn] [pf-vn]
		  [tsch-class] [class] [features-sub-id] [link-sub-id] [base-sub-id]
		  [participle-sub-id]
		  [pv] [pf-pv] [impf-pv] [pf-12-pv] ;; [dir-pv-p] [red-dir-pv]
		  [no-preverbless-aor]
		  :from [morph verb-paradigm]
		  :order-by [sub-id]
		  :where [= [id] ?id]))
	 (old-templates (select [sub-id] [template] [comment] [author]
				:from [morph xle-template]
				:order-by [sub-id]
				:where [= [id] ?id]))
	 (new-id (1+ (car (select [max [id]] :flatp t :from [morph verb-paradigm]))))
	 (uid (car (select [max [unique-id]] :from [morph verb-features] :flatp t)))
	 (now (get-universal-time)))
    (dolist (features old-features)
      (destructuring-bind (sub-id root c-root tense ;; pv vn
				  gv sf caus-sf vv
				  tsch-class morph-type relation ;; reduplication red-dir-pv
				  stem-type pr-st-ext part-pfx part-sfx passive-sfx
				  type-aorist type-obj-3-pfx type-aorist-3sg type-optative
				  nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
				  type-subj3-sfx type-subj2-pfx type-ev-sfx
				  style lang type-pr-st-ext paradigm-replacement)
	  features
	(declare (ignore root c-root))
	(let (#+ignore(vn (u:subst-substrings vn (list root new-root))))
	  (make-instance 'verb-features
			 :unique-id (incf uid)
			 :id new-id 
			 :sub-id sub-id
			 :root new-root
			 :c-root new-c-root
			 :tense tense
			 ;; :pv pv :vn vn
			 :gv gv :sf sf :caus-sf caus-sf :vv vv :tsch-class tsch-class
			 :morph-type morph-type :relation relation
			 ;; :reduplication reduplication :red-dir-pv red-dir-pv
			 :stem-type stem-type :pr-st-ext pr-st-ext
			 :part-pfx part-pfx :part-sfx part-sfx :passive-sfx passive-sfx
			 :nasal-infix nasal-infix :type-aorist type-aorist
			 :type-obj-3-pfx type-obj-3-pfx :type-aorist-3sg type-aorist-3sg
			 :type-optative type-optative :nasal-infix nasal-infix
			 :subj-pers subj-pers :subj-num subj-num :obj-pers obj-pers
			 :type-subj12-sfx type-subj12-sfx :type-subj3-sfx type-subj3-sfx
			 :type-subj2-pfx type-subj2-pfx :type-ev-sfx type-ev-sfx :style style
			 :lang lang :type-pr-st-ext type-pr-st-ext 
			 :paradigm-replacement paradigm-replacement
			 :date now))))
    (dolist (paradigm old-paradigm)
      (destructuring-bind (sub-id c-root vn impf-vn pf-vn
			   tsch-class class features-sub-id link-sub-id base-sub-id
			   participle-sub-id
			   pv pf-pv impf-pv pf-12-pv ;; dir-pv-p red-dir-pv
			   no-preverbless-aor)
	  paradigm
	(let (#+ignore(vn (u:subst-substrings vn (list root new-root))))
	  (make-instance 'verb-paradigm
			 :id new-id 
			 :sub-id sub-id
			 :c-root new-c-root
			 :vn vn
			 :impf-vn impf-vn
			 :pf-vn pf-vn
			 :tsch-class tsch-class
			 :class class
			 :features-sub-id features-sub-id
			 :link-sub-id link-sub-id
			 :base-sub-id base-sub-id
			 :participle-sub-id participle-sub-id
			 :pv pv
			 :pf-pv pf-pv
			 :impf-pv impf-pv
			 :date now))
	(make-instance 'verb-translation
		       :id new-id :sub-id sub-id :date now)))
    (dolist (template old-templates)
      (destructuring-bind (sub-id template comment author) template
	(make-instance 'xle-template
		       :id new-id
		       :sub-id sub-id
		       :template template
		       :comment comment
		       :author author
		       :date now)))
    (update-paradigm-table :id new-id)
    (print new-id)))



;; populate *paradigm-table*
(progn
  (clrhash fst::*paradigm-table*)
  (update-paradigm-table))

(defun ce (string)
  (convert-encoding string :amirani :unicode))

#+once
(with-database-connection ("localhost/gnc/gnc/kartuli" :database-type :postgresql)
  (let ((pg *default-database*))
    (with-database-connection ("localhost/kartuli/treebank/Heebi" :database-type :mysql)
      (do-query ((id vn related-vn)
		 [select [id] [vn] [related-vn]
			 :from [related-verbal-nouns]
			 ;;:limit 1
			 ])
	(let ((*default-database* pg))
	  (insert-records
	   :into [morph related-verbal-nouns]
	   :attributes (list [id] [vn] [related-vn])
	   :values (list id
			 (convert-encoding vn :amirani :unicode)
			 (convert-encoding related-vn :amirani :unicode))))))))

#+test
(let ((id+sub-ids (select [id] [sub-id] :distinct t
			  :from [verb-features]
			  :where [or [= [tense] "|masdar|"]
				     [= [tense] "|past-part|"]
				     [= [tense] "|future-part|"]])))
  (with-transaction ()
    (dolist (id+sub-id id+sub-ids)
      (destructuring-bind (id sub-id) id+sub-id
	(update-records [verb-translation]
			:attributes `([participle-sub-id])
			:values (list sub-id)
			:where [and [= [id] id] [= [sub-id] sub-id]]
			)))))



#+test
(with-transaction ()
  (let ((max-uid (car (select [max [unique-id]] :flatp t :from [verb-features]))))
    (dolist (id (select [id] :distinct t :flatp t
			:from [verb-features]
			;;:where [= [id] 2427]
			))
      (let* ((sub-ids (select [sub-id] :distinct t :flatp t
			      :from [verb-features]
			      :where [= [id] id])))
	(let ((max-sub-id (apply #'max sub-ids)))
	  (dolist (sub-id sub-ids)
	    (let ((pvs (select [pv] :distinct t :flatp t
			       :from [verb-features]
			       :where [and [= [id] id]
					   [= [sub-id] sub-id]
					   [not [null [pv]]]
					   [null [subj-pers]]
					   [null [obj-pers]]
					   ]
			       :order-by '([pv]))))
	      (when (and (cdr pvs)
			 (not (and (not (cddr pvs))
				   (or (and (find "mi" pvs :test #'equal)
					    (find "mo" pvs :test #'equal))
				       (string= (cadr pvs) (concat (car pvs) "mo"))
				       )
				   )))
		(print (list id sub-id pvs))
		#-test
		(dolist (pv (cdr pvs))
		  (let ((new-sub-id (incf max-sub-id)))
		    (dolist (uid+pv (select [unique-id] [pv]
					    :from [verb-features]
					    :where [and [= [id] id]
							[= [sub-id] sub-id]]))
		      (cond ((equal pv (cadr uid+pv))
 			     (update-records [verb-features]
					     :attributes `([sub-id])
					     :values (list new-sub-id)
					     :where [= [unique-id] (car uid+pv)]))
			    ((null (cadr uid+pv))
			     (let ((row (car (select [*] :from [verb-features]
						     :where [= [unique-id] (car uid+pv)]))))
			       (insert-records :into [verb-features]
					       :values (print (list* (incf max-uid) id new-sub-id
								     (cdddr row))))))))
		    (let ((row (car (select [*] :from [verb-translation]
					    :where [and [= [id] id]
							[= [sub-id] sub-id]]))))
		      (insert-records :into [verb-translation]
				      :values (print (list* id new-sub-id nil new-sub-id pv (cddr (cdddr row))))))
		    (let ((row (car (select [*] :from [xle-template]
					    :where [and [= [id] id]
							[= [sub-id] sub-id]]))))
		      (insert-records :into [xle-template]
				      :values (list* id new-sub-id  (cddr row))))
		    ))))))))))

(defun add-related-vn-pair (id vn1 vn2)
  (let ((rel-vn1 (select [related-vn]
			 :flatp t
			 :from [morph related-verbal-nouns]
			 :where [and [= [id] ?id]
				     [= [vn] ?vn1]]))
	(rel-vn2 (select [related-vn]
			 :flatp t
			 :from [morph related-verbal-nouns]
			 :where [and [= [id] ?id]
				     [= [vn] ?vn2]])))
    (with-transaction ()
      (cond ((find vn2 rel-vn1 :test #'string=)	;; already added?
	     nil)
	    ((and rel-vn1 rel-vn2) ;; they are disjunct; build union
	     (dolist (vn1 rel-vn1)
	       (dolist (vn2 rel-vn2)
		 (make-instance 'related-verbal-nouns
				:id id
				:vn vn1
				:related-vn vn2)
		 (make-instance 'related-verbal-nouns
				:id id
				:vn vn2
				:related-vn vn1))))
	    (rel-vn1
	     (dolist (vn1 rel-vn1)
	       (make-instance 'related-verbal-nouns
			      :id id
			      :vn vn1
			      :related-vn vn2)
	       (make-instance 'related-verbal-nouns
			      :id id
			      :vn vn2
			      :related-vn vn1)))
	    (rel-vn2
	     (dolist (vn2 rel-vn2)
	       (make-instance 'related-verbal-nouns
			      :id id
			      :vn vn2
			      :related-vn vn1)
	       (make-instance 'related-verbal-nouns
			      :id id
			      :vn vn1
			      :related-vn vn2)))
	    (t
	     (make-instance 'related-verbal-nouns :id id :vn vn2 :related-vn vn1)
	     (make-instance 'related-verbal-nouns :id id :vn vn1 :related-vn vn2))))))

#+test
(add-related-vn-pair 2166 "titini" "titineba")
#+test
(add-related-vn-pair 3268 "cvdena" "cvdoma")
#+test
(add-related-vn-pair 1116 "laparaki" "laparakeba")
#+test
(add-related-vn-pair 128 "bdGvriali" "bdGvrialeba")
#+test
(add-related-vn-pair 2164 "tirili" "tireba")
#+test
(add-related-vn-pair 2163 "tiktiki" "tiktikeba")
#+test
(add-related-vn-pair 2154 "teHa" "tqdoma")
#+test
(add-related-vn-pair 3065 "Cekva" "Cekveba")
#+test
(add-related-vn-pair 1251 "masHaraoba" "masHaraveba")
#+test
(add-related-vn-pair 3277 "cvima" "cvimeba")
#+test
(add-related-vn-pair 729 "TareSoba" "TareSeba")
#+test
(add-related-vn-pair 3613 "Hvdoma" "Hvedreba")
#+test
(add-related-vn-pair 714 "TamaSeba" "TamaSi")
#+test
(add-related-vn-pair 3581 "Hedeba" "Hedva")
#+test
(add-related-vn-pair 3581 "Hedvineba" "Hedva")
#+test
(add-related-vn-pair 3581 "Hedvineba" "Hedeba")

#+test
(add-related-vn-pair 3062 "Cdoma" "Cdena")


#+once
(dolist (class '(super-paradigm verb-translation verb-features xle-template related-verbal-nouns))
  ;;(drop-table 'xle-template)
  (unless (table-exists-p (view-table (find-class class)))
    (create-view-from-class class)))

#+test
(pprint 
 (select [id] [sub-id] [features-sub-id] [base-sub-id]
	 :from [morph verb-paradigm]
	 :where [<> [features-sub-id] [sub-id]]))

#+once
(update-recordsxx [morph verb-paradigm]
		:av-pairs '(([base-sub-id] [sub-id]))
		:where [<> [features-sub-id] [sub-id]])

#+once
(progn
  (unless (index-exists-p [verb-features-id-sub-id])
    (create-index [verb-features-id-sub-id] :on [morph verb-features]
		  :attributes '([id] [sub-id]) 
		  :unique nil))
  (unless (index-exists-p [verb-features-id-sub-id-root])
    (create-index [verb-features-id-sub-id-root] :on [morph verb-features]
		  :attributes '([id] [sub-id] [root]) 
		  :unique nil))
  (unless (index-exists-p [verb-features-root-id-sub-id])
    (create-index [verb-features-root-id-sub-id] :on [morph verb-features]
		  :attributes '([root] [id] [sub-id]) 
		  :unique nil))
  (unless (index-exists-p [verb-translation-id-sub-id])
    (create-index [verb-translation-id-sub-id] :on [morph verb-translation]
		  :attributes '([id] [sub-id]) 
		  :unique t))
  (unless (index-exists-p [xle-template-id-sub-id])
    (create-index [xle-template-id-sub-id] :on [morph xle-template]
		  :attributes '([id] [sub-id]) 
		  :unique nil)))

#+once-only
(let ((date (get-universal-time)))
  (with-transaction ()
    (dat:do-string-tree (key value-list fst::*xle-template-table*)
      (dolist (value (cdr value-list))
	(destructuring-bind (template vn pv class id+subid) value
	  (destructuring-bind (id sub-id) (mapcar #'parse-integer (split (subseq id+subid 1) #\-))
	    (insert-records :into [xle-template]
			    :attributes `([id] [sub-id] [template] [author] [date])
			    :values (list id sub-id template "derived" date))))))))

;;(disconnect)

(defvar *fullform-count-table* (make-hash-table :test #'equal))

(do-query ((word count)
	   [select [word] [corpus-count] :from [morph fullform] :flatp t])
  (setf (gethash word *fullform-count-table*) count))

(defun fullform-count (word)
  (gethash word *fullform-count-table*)
  #+old
  (car (select [corpus-count] :from [morph fullform] :flatp t :where [= [word] ?word])))

#+test
(let ((id-table (dat:make-string-tree)))
  (with-transaction ()
    (with-open-file (stream "projects:georgian-morph;verb-translations.txt")
      (let ((*package* (find-package :fst)))
	(loop for verb-list = (read stream nil nil)
	   while verb-list
	   do (let ((paradigm-id (getf verb-list :id)))
		(if (dat::string-tree-get id-table paradigm-id)
		    (print (list :duplicate-id paradigm-id
				 :old (dat::string-tree-get id-table paradigm-id)
				 :new (getf verb-list :tr)))
		    (destructuring-bind (id sub-id) (mapcar #'parse-integer (split paradigm-id #\-))
		      (setf (dat::string-tree-get id-table paradigm-id) (getf verb-list :tr))
		      (insert-records :into [verb-translation]
				      :av-pairs `(([id] ,id)
						  ([sub-id] ,sub-id)
						  ([translation] ,(getf verb-list :tr))))))))))))

(defun verb-translation (&key id sub-id paradigm-id)
  (if paradigm-id
      (destructuring-bind (id sub-id) (mapcar #'parse-integer (split paradigm-id #\-))
	(car (select [translation] :flatp t :from [morph verb-translation]
		     :where [and [= [id] ?id] [= [sub-id] ?sub-id]])))
      (car (select [translation] :flatp t :from [morph verb-translation]
		   :where [and [= [id] ?id] [= [sub-id] ?sub-id]]))))

#+main
(with-open-file (stream "projects:xle;grammars;georgian;georgian-verb-lex.lfg" :direction :output :if-exists :supersede)
  (write-lfg-verb-lexicon stream))

(defun write-lfg-verb-lexicon (stream)
  (let ((prev-id 0)
	(prev-sub-id 0)
	(prev-tsch-class nil)
	(prev-pv nil)
	(prev-vn nil)
	(templates ()))
    (write-line "STANDARD KARTULI-VERBS LEXICON (1.0)" stream)
    (terpri stream)
    (do-query ((id sub-id pv vn tsch-class template)
	       [select [verb-features id] [verb-features sub-id] [verb-translation pv] [vn] [tsch-class] [template]
		       :distinct t
		       :from [morph xle-template]
		       :distinct t
		       :left-join '([verb-features] [verb-translation])
		       :on [and [= [verb-features id] [xle-template id]]
				[= [verb-features sub-id] [xle-template sub-id]]
				[= [verb-features id] [verb-translation id]]
				[= [verb-features sub-id] [verb-translation base-sub-id]]]
		       :where [and [= [base-sub-id] [verb-features sub-id]]
				   ;;[= [verb-features id] 3261]
				   ]
		       :order-by (list [verb-features id] [verb-features sub-id])])
      (let ((vn (convert-encoding (simple-vn vn) :amirani :unicode))
	    (pv (convert-encoding pv :amirani :unicode))
	    (tsch-class (subseq tsch-class 0 (position #\space tsch-class))))
	#+debug(print (list id sub-id pv vn tsch-class template)) 
	(cond ((and (= prev-id id)
		    (= prev-sub-id sub-id))
	       (push template templates))
	      (t
	       (when templates
		 (if prev-pv
		     (format stream "~a-~a-~a-~a V XLE "
			     prev-pv prev-vn prev-id prev-sub-id)
		     (format stream "~a-~a-~a V XLE "
			     prev-vn prev-id prev-sub-id))
		 (when (cdr templates) (format stream "{ "))
		 (loop for (template . rest) on templates
		      do (if prev-pv
			     (format stream "@(~a ~a-~a ~a ~a V~a-~a)"
				     template prev-pv prev-vn prev-pv prev-tsch-class prev-id prev-sub-id)
			     (format stream "@(~a ~a - ~a V~a-~a)"
				     template prev-vn prev-tsch-class prev-id prev-sub-id))
		      (when rest (format stream "~c~c~c | " #\Linefeed #\Tab #\Tab)))
		 (if (cdr templates)
		     (format stream " };~c" #\Linefeed)
		     (format stream ";~c" #\Linefeed))
		 (format stream "~c~c Vpart XLE " #\Tab #\Tab)
		 (when (cdr templates) (format stream "{ "))
		 (loop for (template . rest) on templates
		      do (if prev-pv
			     (format stream "@(~a ~a-~a ~a ~a V~a-~a)"
				     template prev-pv prev-vn prev-pv prev-tsch-class prev-id prev-sub-id)
			     (format stream "@(~a ~a - ~a V~a-~a)"
				     template prev-vn prev-tsch-class prev-id prev-sub-id))
		      (when rest (format stream "~c~c~c | " #\Linefeed #\Tab #\Tab)))
		 (if (cdr templates)
		     (format stream " }; ETC.~c" #\Linefeed)
		     (format stream "; ETC.~c" #\Linefeed)))
	       (setf prev-vn vn prev-pv pv prev-id id prev-sub-id sub-id prev-tsch-class tsch-class templates (list template))
	       ))))
    (write-line "----" stream)))

(defparameter *id-to-tsch-classes* (make-hash-table :test #'equal))

(defparameter *id-to-roots-table* (make-hash-table))
(defparameter *id-to-c-root-table* (make-hash-table))

#+test
(print (gethash 3117 *id-to-roots-table*))

(defun fetch-id-roots ()
  (clrhash *id-to-roots-table*)
  (do-query ((id root c-root)
	     [select [id] [root] [c-root]
		     :distinct t
		     :from [morph verb-features]])
    (print (list id root))
    (push root (gethash id *id-to-roots-table*))
    (setf (gethash id *id-to-c-root-table*) c-root)))

#+test
(fetch-id-roots)

(defun fetch-tsch-classes ()
  (clrhash *id-to-tsch-classes*)
  (do-query ((id sub-id tsch-class)
	     [select [id] [sub-id] [tsch-class]
		     :distinct t
		     :from [morph verb-features]])
    (let ((tsch-class (subseq tsch-class 0 (position #\space tsch-class))))
      (setf (gethash (list id sub-id) *id-to-tsch-classes*) tsch-class))))

(defun get-tsch-classes (id sub-id-list)
  (let ((classes ()))
    (if (listp sub-id-list)
	(dolist (sub-id sub-id-list)
	  (pushnew (gethash (list id sub-id) *id-to-tsch-classes*) classes :test #'equal))
	(pushnew (gethash (list id sub-id-list) *id-to-tsch-classes*) classes :test #'equal))
    (format nil "~{~a~^/~}" (sort classes #'string<))))

#+test
(print (get-tsch-classes 771 (list 1 2 3 16 17 18 19 20 22 24 25 64 65 66)))

#+test
(fetch-tsch-classes)

#||
(print (select [*] :from [verb-features] :where [= [c-root] "cer"]))

(time (select 'verb-features :where [= [c-root] "TamaS"]))

(defun fst::get-root-override-features (root)
  (print root)
  (print (gethash (fst::inv-convert root) fst::*features-table*)))

||#

#+test
(let ((*package* (find-package :fst))
      (*print-case* :downcase)
      )
  (print (read-from-string (progn ;;fst::convert
			    (format nil "~s" (fst::get-root-override-features "ndom"))))))

(defun inv-convert-num (str)
  str
  #||
  (if nil;;(stringp str)
      (let ((c-str (copy-seq str)))
	(loop for i from 0 for c across str
	   do (setf (char c-str i) (getf '(#\1 #\± #\2 #\² #\3 #\³ #\4 #\´ #\5 #\µ #\6 #\¶ #\7 #\· #\- #\Â #\| #\ª) c c)))
	c-str)
      str)||#)

(defun inv-convert-morph (str)
  (if (equal str "-")
      '-
      str))

#||

hash:

("IV²" "1634-3" - (tense {conj-perfect pluperfect perfect})
  (vn "ãÚäâ×") (c-root "ãÚäâ") (vn "ãÚäâ×") (morph-type passive)
  (relation relative) (sf "ÛØ"))

sql:

("IV²" "1634-3" nil (root "ãÚäâ") (c-root "ãÚäâ")
  (tense {conj-perfect pluperfect perfect}) (vn "ãÚäâ×") (sf "ÛØ")
  (morph-type passive) (relation "relative"))

gv, sf, caus_sf, vv, tsch_class, morph_type, relation, reduplication, red_dir_pv, stem_type, pr_st_ext, pf_sfx, pp_pfx, pp_sfx, passive_sfx, type_aorist, type_obj_3_pfx, type_aorist_3sg, type_optative, nasal_infix, subj_pers, subj_num, obj_pers, type_subj12_sfx, type_subj3_sfx, type_subj2_pfx, type_ev_sfx, type_optative_3pl_en, type_pr_st_ext, paradigm_replacement


||#

#+test
(with-transaction ()
  (update-records [verb-features]
		  :av-pairs '(([style] "dangerous"))
		  :where [= [unique-id] 237320]))

#-(or mysql sqlite)
(defun fst::get-root-override-features (root)
  (print root)
  (gethash (fst::inv-convert root) fst::*features-table*))

(defparameter *feature-cache* (make-hash-table :test #'equal))
(defparameter *feature-cache-base* (make-hash-table :test #'equal))

(defun %int-list (str)
  (when (and str (not (string= str "")))
    (if (find #\| str)
	(parser::make-extended-list
	 :char #\{
	 :form (mapcar #'parse-integer
		       (split str #\| nil nil t)))
	(parse-integer str))))

(defun %int-list-intersect (int-list &rest ints)
  (let ((ints (cond ((null int-list)
		     ints)
		    ((parser::extended-list-p int-list)
		     (intersection (parser::extended-list-form int-list)
				   ints))
		    ((find int-list ints)
		     (list int-list))
		    (t
		     nil))))
    (cond ((null ints)
	   :empty)
	  ((cdr ints)
	   (parser::make-extended-list :char #\{ :form ints))
	  (t
	   (car ints)))))

#+test
(let ((*package* (find-package :fst)))
  (print (fst::get-root-override-features "azat" :all-participles-p t :use-base-sub-id-p t :clear-cache-p t)))

(defun simple-vn (vn)
  (let ((vn (subseq vn 0 (position #\space vn))))
    (string-trim "[]" (subseq vn 0 (position #\- vn)))))

#+test
(set-pv-in-verb-translation-table)

;; sets a representative pv for the whole paradigm; used in paradigm key
(defun set-pv-in-verb-translation-table ()
  (with-database-connection ()
    (dolist (id+sub-id (select [id] [sub-id] :from [morph verb-translation]))
      (destructuring-bind (id sub-id) id+sub-id
	(let ((gv-list (select [pv]
			       :from [morph verb-features]
			       :flatp t
			       :distinct t
			       :where [and [= [id] ?id]
					   [= [sub-id] ?sub-id]
					   [not [null [pv]]]
					   [not [or [like [tense] "%|masdar|%"]
						    [like [tense] "%|past-part|%"]
						    [like [tense] "%|present-part|%"]
						    [like [tense] "%|future-part|%"]
						    [like [tense] "%|negative-part|%"]]]])))
	  (with-transaction ()
	    (update-records [morph verb-translation]
			    :av-pairs `(([pv] ,(car (sort gv-list #'string<))))
			    :where [and [= [id] id] [= [sub-id] ?sub-id] [not [null [pv]]]])))))))


;; postgresql
(defun build-alternation-table ()
  (let ((table (make-hash-table :test #'equal)))
    (do-query ((id sub-id class base-class)
	       [select [verb-translation id] [verb-translation sub-id] [vf1 tsch-class] [vf2 tsch-class]
		       :from `([morph verb-translation] [morph verb-features :as vf1]
			       [morph verb-features :as vf2])
		       :distinct t
		       :where [and [= [verb-translation id] [vf1 id]]
				   [= [verb-translation sub-id] [vf1 sub-id]]
				   [= [verb-translation id] [vf2 id]]
				   [= [verb-translation base-sub-id] [vf2 sub-id]]
				   [<> [verb-translation sub-id] [base-sub-id]]]])
      (setf (gethash (cons id sub-id) table)
	    (let ((c (char class 0))
		  (bc (char base-class 0)))
	      (cond ((char= c bc)
		     'base)
		    ((and (char= c #\P) (char= bc #\T))
		     'passive)
		    ((and (char= c #\P) (char= bc #\M))
		     'passive)
		    ((and (char= c #\R) (char= bc #\T))
		     'passive)
		    ((and (char= c #\I) (char= bc #\R))
		     'passive)
		    ((and (char= c #\K) (char= bc #\T))
		     'causative)
		    ((and (char= c #\K) (char= bc #\M))
		     'causative)
		    (t
		     (warn "Not allowed: ~d ~d ~s ~s" id sub-id class base-class)
		     'passive)))))
    table))

(defparameter *alternation-table* (build-alternation-table))

#+orig
(defun expand-tense (tense-list &key (add-og-tenses t))
  (let ((tenses (if (find #\| tense-list)
		    (mapcar (lambda (str) (intern (string-upcase str) :fst))
			    (split tense-list #\| nil nil t))
		    (list (intern (string-upcase tense-list) :fst)))))
    (when add-og-tenses
      (when (find 'present tenses)
	(push 'iter-present tenses))
      (when (find 'future tenses)
	(pushnew 'iter-present tenses))
      (when (find 'imperfect tenses)
	(setf tenses (list* 'iter-imperfect 'imperative-present tenses)))
      (when (find 'conditional tenses)
	(pushnew 'iter-imperfect tenses)
	(Pushnew 'imperative-present tenses))
      (when (find 'aorist tenses)
	(pushnew 'imperative-aorist tenses))
      (when (find 'optative tenses)
	(pushnew 'iter-aorist tenses))
      (when (find 'pluperfect tenses)
	(push 'iter-perfect tenses))
      (when (find 'perfect tenses)
	(push 'iter-perfect1 tenses)))
    (if (cdr tenses)
	(parser::make-extended-list :char #\{ :form tenses)
	(car tenses))))

(defun expand-tense (tense-list &key (add-og-tenses t))
  (let ((tenses (if (find #\| tense-list)
		    (mapcar (lambda (str) (intern (string-upcase str) :fst))
			    (split tense-list #\| nil nil t))
		    (list (intern (string-upcase tense-list) :fst))))
	(impf-tenses ())
	(pf-tenses ()))
    (dolist (tense tenses)
      (if (find tense '(present imperfect conj-present))
	  (push tense impf-tenses)
	  (push tense pf-tenses)))
    (when add-og-tenses
      (when (find 'present tenses)
	(push 'iter-present impf-tenses))
      (when (find 'future tenses)
	(pushnew 'iter-present impf-tenses))
      (when (find 'imperfect tenses)
	(setf impf-tenses (list* 'iter-imperfect 'imperative-present impf-tenses)))
      (when (find 'conditional tenses)
	(pushnew 'iter-imperfect impf-tenses)
	(Pushnew 'imperative-present impf-tenses))
      (when (find 'aorist tenses)
	(pushnew 'imperative-aorist pf-tenses))
      (when (find 'optative tenses)
	(pushnew 'iter-aorist pf-tenses))
      (when (find 'pluperfect tenses)
	(push 'iter-perfect pf-tenses))
      (when (find 'perfect tenses)
	(push 'iter-perfect1 pf-tenses)))
    (list impf-tenses pf-tenses)))

#+test
(clrhash *feature-cache*)

;; has to be reinitialized when changes are made
;; collect verbs that can have preverb-less imperfective aorists
(defparameter *preverbless-aorists*
  (let ((table (make-hash-table :test #'equal)))
    (do-query ((id sub-id)
	       [select [id] [sub-id]
		       :distinct t
		       :from [morph verb-paradigm]
		       :where [and [= [impf-pv] "-"]
				   [<> [pf-pv] "-"]
				   [or [like [tsch-class] "T%"]
				       [like [tsch-class] "K%"]
				       [like [tsch-class] "P%"]
				       [like [tsch-class] "R%"]]
				   [or [null [no-preverbless-aor]]
				       [not [no-preverbless-aor]]]]])
      (setf (gethash (list id sub-id) table) t))
    table))

#+test
(masdar-full
 (case masdar-code
   ((:A :A1 :A2 :B :P :U)
    (u:concat masdar "ი"))
   (otherwise
    masdar)))

#+test
(print (gethash (list 6 "*ადვილება") *vn-pv-table*))
#+test
(print (gethash (list 2318 "*ფასება") *vn-pv-table*))

(defparameter *vn-pv-table* (make-hash-table :test #'equal))

;; collect all possible perfective pvs for a given vn
(progn
  (clrhash *vn-pv-table*)
  (do-query ((id vn pv pf-pv pf-12-pv impf-pv tense)
	     [select [verb-paradigm id] [stem] [verb-paradigm pv] [pf-pv] [pf-12-pv] [impf-pv] [tense]
		     :from [morph verb-paradigm]
		     :left-join [morph participle :as masdar]
		     :on [and [= [masdar id] [verb-paradigm id]]
			      [= [masdar sub-id] [verb-paradigm features-sub-id]]
			      [= [masdar type] :masdar]
			      [= [masdar main-form] t]]
		     :left-join [morph verb-features]
		     :on [and [= [verb-features id] [verb-paradigm id]]
			      [= [verb-features sub-id] [verb-paradigm features-sub-id]]]
		     :where [and [not [like [verb-features tsch-class] "I%"]]
				 [not [like [verb-features tsch-class] "%M%"]]
				 [or [like [tense] "%|future|%"]
				     [like [tense] "%|aorist|%"]]]])
    (when tense
      #+test
      (when (and vn (char= (char vn 0) #\*))
	(setf vn (subseq vn 1)))
      (when (equal impf-pv "-")
	(pushnew pf-pv (gethash (list id vn) *vn-pv-table*) :test #'equal)
	(when pf-12-pv
	  (pushnew pf-12-pv (gethash (list id vn) *vn-pv-table*) :test #'equal))))))

(defparameter *vn-template-pv-table* (make-hash-table :test #'equal))

;; collect all possible perfective pvs for a given vn and class
(progn
  (clrhash *vn-template-pv-table*)
  (do-query ((id vn pv pf-pv pf-12-pv impf-pv tense template)
	     [select [verb-paradigm id] [stem] [verb-paradigm pv] [pf-pv] [pf-12-pv] [impf-pv] [tense]
		     [template]
		     :from [morph verb-paradigm]
		     :left-join [morph participle :as masdar]
		     :on [and [= [masdar id] [verb-paradigm id]]
			      [= [masdar sub-id] [verb-paradigm features-sub-id]]
			      [= [masdar type] :masdar]
			      [= [masdar main-form] t]]
		     :left-join [morph verb-features]
		     :on [and [= [verb-features id] [verb-paradigm id]]
			      [= [verb-features sub-id] [verb-paradigm features-sub-id]]]
		     :left-join [morph xle-template]
		     :on [and [= [xle-template id] [verb-paradigm id]]
			      [= [xle-template sub-id] [verb-paradigm sub-id]]]
		     :where [and [not [like [verb-features tsch-class] "I%"]]
				 [not [like [verb-features tsch-class] "%M%"]]
				 [or [like [tense] "%|future|%"]
				     [like [tense] "%|aorist|%"]]]])
    (when tense
      #+test
      (when (and vn (char= (char vn 0) #\*))
	(setf vn (subseq vn 1)))
      (when (equal impf-pv "-")
	(pushnew pf-pv (gethash (list id vn template) *vn-template-pv-table*) :test #'equal)
	(when pf-12-pv
	  (pushnew pf-12-pv (gethash (list id vn template) *vn-template-pv-table*) :test #'equal))))))

#+test
(pprint (fst::get-root-override-features "ჭამ"))

#+test
(fst::get-root-override-features "კეთ")

#+test
(update-records [morph verb-paradigm]
		:av-pairs '(([features-sub-id] [sub-id]))
		:where [null [features-sub-id]])

;; new June 2013; todo: add vv info (doesn't work)
(defun tsch-class-to-version (tsch-class vv)
  ;; (print vv)
  (let ((class (intern (subseq tsch-class 0 (position #\space tsch-class)) :keyword)))
    (case class
      (:t2 "SV")
      ((:t3 :rp3 :rp6 :rm2 :iv2 :zp3) "OV")
      ((:t4 :rp4 :rp7 :rm3 :iv3 :zp2) "LV"))))


#||
RP3 OV
RP4 LV
RP6 OV
RP7 LV
RM2 OV
RM3 LV
IV2 OV
IV3 LV
ZP3 OV
ZP2 LV
||#

#+test
(print (select [count [*]]
	       :from [morph verb-paradigm]
	       :where [or [null [disabled]] [<> [disabled] t]]
	       :flatp t))

#+test
(print (select [count [*]]
	       :from [morph verb-paradigm]
	       :where [= [disabled] :false]
	       :flatp t))

(def-view-class pv-variant ()
  ((pv :initform nil :initarg :pv :reader pv :type string :db-kind :key)
   (pv-variant :initform nil :initarg :pv-variant :reader pv-variant :type string :db-kind :key)
   )
  (:base-table [morph pv-variant]))

#+test
(pprint (fst::get-root-override-features "ცოდნ"))
#+test
(pprint (fst::get-root-override-features "ალერს"))

;; used in generate-paradigm() etc.
(defun fst::get-root-override-features (root &key use-base-sub-id-p (clear-cache-p t) 
					clear-all-caches-p all-participles-p
					(preverbless-aorists t) skip-disabled
					;; paradigm-replacement-p
					)
  #+debug(print (list :use-base-sub-id-p use-base-sub-id-p))
  (cond (clear-cache-p
	 (clrhash *feature-cache-base*))
	(clear-all-caches-p
	 (clrhash *feature-cache*)
	 (clrhash *feature-cache-base*)))
  (let ((cache (if use-base-sub-id-p *feature-cache-base* *feature-cache*))
	(tense nil)
	(version nil))
    (or #+disabled(gethash (fst::convert root) cache)
	(with-database-connection ()
	  (setf (gethash (print (fst::convert root)) cache)
		(u:collecting
		  ;;(print (list :querying (fst::convert root)))
		  (do-query ((unique-id id sub-id base-sub-id
					c-root tense-str
					masdar masdar-aspect masdar-code masdar-lang
					impf-pv pf-pv pf-12-pv c-pv
					;; vn impf-vn pf-vn
					gv sf caus-sf vv
					tsch-class class morph-type relation ;; reduplication
					;; red-dir-pv
					stem-type pr-st-ext part-pfx part-sfx passive-sfx
					type-aorist type-obj-3-pfx type-aorist-3sg type-optative
					nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
					type-subj3-sfx type-subj2-pfx type-ev-sfx
					style lang type-pr-st-ext paradigm-replacement
					template)
			     [select [unique-id] [verb-paradigm id] [verb-paradigm sub-id]
				     [base-sub-id]
				     [verb-paradigm c-root] [tense]
				     [masdar stem]
				     [masdar aspect]
				     [masdar code]
				     [masdar variety]
				     [verb-paradigm impf-pv]
				     [verb-paradigm pf-pv]
				     [verb-paradigm pf-12-pv]
				     [verb-paradigm pv]
				     ;; [verb-paradigm vn]
				     ;;[verb-paradigm impf-vn]
				     ;;[verb-paradigm pf-vn]
				     [gv] [sf] [caus-sf] [vv] ;; ****
				     [verb-paradigm tsch-class]
				     [verb-paradigm class]
				     [morph-type] [relation]
				     ;;[reduplication]
				     ;; [verb-paradigm red-dir-pv]
				     [stem-type] [pr-st-ext] [part-pfx] [part-sfx] [passive-sfx]
				     [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
				     [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
				     [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
				     [style] [lang] [type-pr-st-ext] [paradigm-replacement]
				     [template]
				     :from [morph verb-paradigm]
				     :left-join [morph verb-features]
				     :on [and [= [verb-features id] [verb-paradigm id]]
					      [= [verb-features sub-id]
						 [verb-paradigm features-sub-id]]]
				     :left-join [morph participle :as masdar]
				     :on [and [= [masdar id] [verb-paradigm id]]
					      [= [masdar sub-id] [verb-paradigm features-sub-id]]
					      [= [masdar type] "MASDAR"]
					      [= [masdar main-form] t]]
				     :left-join [morph xle-template]
				     :on [and [= [xle-template id] [verb-paradigm id]]
					      [= [xle-template sub-id] [verb-paradigm sub-id]]]
				     :where [and [= [verb-features root] (fst::convert root)]
						 (when skip-disabled
						   [or [null [disabled]] [<> [disabled] t]])
						 (when skip-disabled
						   [or [null [style]] [<> [style] "disabled"]])
						 [null [verb-paradigm red-dir-pv]] ;; new April 2016
						 [or [null [lang]] [= [lang] ""]
						     [null [variety]] [= [variety] ""]
						     [= [lower [lang]] [lower [variety]]]]]])
		    ;;(print (list id sub-id base-sub-id c-root tense))
		    #+debug
		    (print (list :unique-id unique-id :id id :sub-id sub-id base-sub-id
				 c-root tense-str c-pv
				 gv sf caus-sf vv
				 tsch-class morph-type relation ;; red-dir-pv
				 stem-type pr-st-ext part-pfx part-sfx passive-sfx
				 :type-aorist type-aorist type-obj-3-pfx
				 :type-aorist-3sg type-aorist-3sg type-optative
				 nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
				 type-subj3-sfx type-subj2-pfx type-ev-sfx
				 style lang type-pr-st-ext paradigm-replacement))
		    
		    (when (and ;; paradigm-replacement-p 
			   paradigm-replacement) ;; pick the correct paradigm-replacement
		      (let ((replacements (select [paradigm-replacement]
						  :flatp t
						  :distinct t
						  :from [morph verb-features]
						  :where [and [= [verb-features id] ?id]
							      [= [verb-features sub-id] ?sub-id]
							      [= [verb-features tense] ?tense-str]])))
			#+debug
			(print (list :id id :sub-id sub-id :rep replacements
				     :tense tense-str))
			#+test
			(assert (null (cdr replacements)))
			;; new Nov. 2014
			(when replacements
			  (setf paradigm-replacement (car replacements)))))
		    (setf #+orig(expand-tense tense-str)
			  subj-pers (%int-list subj-pers)
			  obj-pers (%int-list obj-pers)
			  root (inv-convert root)
			  c-root (inv-convert c-root)
			  c-pv (inv-convert-morph c-pv)
			  ;; vn (inv-convert-morph (simple-vn vn))
			  sf (inv-convert-morph sf)
			  caus-sf (inv-convert-morph caus-sf)
			  vv (inv-convert-morph vv)
			  ;; red-dir-pv (inv-convert-morph red-dir-pv)
			  pr-st-ext (inv-convert-morph pr-st-ext)
			  part-pfx (inv-convert-morph part-pfx)
			  part-sfx (inv-convert-morph part-sfx)
			  passive-sfx (inv-convert-morph passive-sfx)
			  nasal-infix (inv-convert-morph nasal-infix)
			  type-obj-3-pfx
			  (case (intern (string-upcase type-obj-3-pfx) :fst)
			    (- nil)
			    (+ nil)
			    (-- '--)
			    (++ '++)
			    (x 'x))
			  #+orig
			  (intern (string-upcase type-obj-3-pfx) :fst)
			  type-subj12-sfx (inv-convert-morph type-subj12-sfx)
			  type-subj3-sfx (inv-convert-morph type-subj3-sfx)
			  type-subj2-pfx (intern (string-upcase type-subj2-pfx) :fst)
			  vv (inv-convert-morph vv)
			  relation (intern (string-upcase relation) :fst)
			  morph-type (intern (string-upcase morph-type) :fst)
			  version (tsch-class-to-version tsch-class vv)
			  stem-type (intern (string-upcase stem-type) :fst)
			  subj-num (intern (string-upcase subj-num) :fst)
			  type-pr-st-ext (inv-convert-morph type-pr-st-ext)
			  type-aorist (intern (string-upcase type-aorist) :fst)
			  type-optative (inv-convert-morph type-optative)
			  type-aorist-3sg (inv-convert-morph type-aorist-3sg)
			  type-ev-sfx (intern (string-upcase type-ev-sfx) :fst)
			  ;; reduplication (intern (string-upcase reduplication) :fst)
			  style (intern (string-upcase style) :fst)
			  lang (unless (equal lang "") lang)
			  masdar-lang (unless (equal masdar-lang "") masdar-lang)
			  lang (intern (string-upcase (or lang masdar-lang)) :fst)
			  )
		    (loop for tense-list in (expand-tense tense-str)
		       for impfp in (list t nil)
		       when tense-list
		       do
		       (setf tense (if (cdr tense-list)
				       (parser::make-extended-list :char #\{ :form tense-list)
				       (car tense-list)))
		       (let* ((aor (search "|aorist|" tense-str))
			      (opt (search "|optative|" tense-str))
			      (imperfective-tense-p impfp
				#+orig
				(or (search "|present|" tense-str)
				    (search "|imperfect|" tense-str)
				    (search "|conj-present|" tense-str)))
			      (perfect-group-p
			       (find-if (lambda (tense) (find tense '(perfect pluperfect conj-perfect)))
					tense-list)
				#+orig
				(or (search "|perfect|" tense-str)
				    (search "|pluperfect|" tense-str)
				    (search "|conj-perfect|" tense-str)))
			      (pv (if imperfective-tense-p impf-pv pf-pv))
			      (12-pv (cond ((null pf-12-pv)
					    nil)
					   (imperfective-tense-p
					    (unless (equal impf-pv "-")
					      pf-12-pv))
					   (t
					    (unless (equal pf-pv "-")
					      pf-12-pv))))
			      (12-arg (cond ((null 12-pv)
					     :none)
					    ((or (char= (char tsch-class 0) #\P) ;; absolute passive
						 (and perfect-group-p
						      (or (char= (char tsch-class 0) #\M)
							  (and (char= (char tsch-class 0) #\T)
							       (find (char tsch-class 1) "12")))))
					     :subj)
					    ((and perfect-group-p
						  (char= (char tsch-class 0) #\T)
						  (find (char tsch-class 1) "345"))
					     :subj-obj)
					    ((char= (char tsch-class 0) #\R)
					     :obj)
					    ((and (char= (char tsch-class 0) #\T)
						  (find (char tsch-class 1) "345"))
					     :obj123)))
			      (masdar-code (when masdar-code (intern masdar-code :keyword)))
			      (masdar-full
			       (marked-nom-form masdar masdar-code :masdar t))
			      (masdar1
			       (case masdar-code
				 ((:A :A1 :A2 :B :P :U)
				  (u:concat masdar "ი"))
				 (otherwise
				  masdar)))
			      ;; TODO: use * where no unambiguous preverb can be found
			      ;; but search only in preverbs for same frame/class etc.
			      ;; *vn-template-pv-table*
			      (impf-vn (let ((pv-list
					      ;;(gethash (list id masdar) *vn-pv-table*)
					      ;; new Dec. 2014
					      (gethash (list id masdar template) *vn-template-pv-table*)))
					 (cond ((null masdar)
						(warn "No masdar found for ~a (~a-~a)." root id sub-id)
						"-")
					       ((equal masdar "-")
						"-")
					       ((char/= (char masdar 0) #\*)
						masdar-full)
					       ((not (equal impf-pv "-"))
						(u:concat impf-pv "·" (subseq masdar-full 1)))
					       ((and (equal pf-pv "-") ;; pf form is preverbless
						     (not (cdr pv-list)) ;; new Dec. 2014
						     (find "-" pv-list :test #'equal))
						(subseq masdar-full 1))
					       ((and (equal pf-pv "-") ;; pf form is preverbless
						     (not (cdr pv-list)) ;; new Dec. 2014
						     (find "-" pv-list :test #'equal))
						(subseq masdar-full 1))
					       ((not (cdr pv-list)) ;; new Dec. 2014
						(if (car pv-list)
						    (u:concat (car pv-list) "·" (subseq masdar-full 1))
						    (subseq masdar-full 1)))
					       (t
						(u:concat "*·" (subseq masdar-full 1))
						#+old
						masdar-full))))
			      (pf-vn (cond ((null masdar)
					    (warn "No masdar found for ~a (~a-~a)." root id sub-id)
					    "-")
					   ((equal masdar "-")
					    "-")
					   ((char/= (char masdar 0) #\*)
					    masdar-full)
					   ((equal pf-pv "-")
					    (subseq masdar-full 1))
					   (t
					    (u:concat pf-pv "·" (subseq masdar-full 1)))))
			      (vn (if imperfective-tense-p
				      impf-vn pf-vn)))
			 ;;(debug vn) (debug masdar) (debug masdar-code) (debug masdar-aspect)
			 (when (or (null masdar-aspect) (equal masdar-aspect "")
				   (and imperfective-tense-p
					(equal masdar-aspect "IMPERF"))
				   (and (not imperfective-tense-p)
					(equal masdar-aspect "PERF")))
			   (dolist (pv-subj-obj-pers
				     (case 12-arg
				       (:subj
					(list (list pv
						    (%int-list-intersect subj-pers 3)
						    obj-pers)
					      (list 12-pv
						    (%int-list-intersect subj-pers 1 2)
						    obj-pers)))
				       (:subj-obj
					(list (list pv subj-pers obj-pers)
					      (list 12-pv subj-pers obj-pers)))
				       (:obj
					(list (list pv
						    subj-pers
						    (%int-list-intersect obj-pers 3))
					      (list 12-pv
						    subj-pers
						    (%int-list-intersect obj-pers 1 2))))
				       (:obj123
					(list (list pv subj-pers obj-pers)
					      (list 12-pv
						    subj-pers
						    (%int-list-intersect obj-pers 1 2))))
				       (otherwise
					(list (list pv subj-pers obj-pers)))))
			     (destructuring-bind (pv subj-pers obj-pers) pv-subj-obj-pers
			       (unless (or (eq subj-pers :none) (eq obj-pers :none))
				 ;;(print (list* sub-id 12-arg tense pv-subj-obj-pers))
				 (let ((pv+impf+vn-list (if (and preverbless-aorists
								 ;; aor ;; opt has no preverbless/imperfective forms <- wrong!
								 (or aor opt)
								 (gethash (list id sub-id) *preverbless-aorists*))
							    `((- t ,impf-vn) ;; preverbless imperf aorist
							      (,(inv-convert-morph pv) nil ,vn))
							    `((,(inv-convert-morph pv) nil ,vn)))))
				   #+debug(print (list* id sub-id pv+impf+vn-list))
				   (dolist (pv+impf+vn pv+impf+vn-list)
				     (u:collect (list* (inv-convert-num tsch-class)
						       (format nil "~a-~a" id
							       (if (and use-base-sub-id-p base-sub-id)
								   base-sub-id
								   sub-id))
						       c-pv
						       `(,@(when root `((fst::root ,root)))
							   ,@(when c-root `((fst::c-root ,c-root)))
							   ,@(cond ((cadr pv+impf+vn) ;; aorist and optative only
								    #+old'((fst::tense aorist))
								    (cond ((and aor opt)
									   `((fst::tense ,(parser::make-extended-list
											   :char #\{
											   :form (list 'aorist 'optative)))))
									  (aor '((fst::tense aorist)))
									  (opt '((fst::tense optative)))))
								   (tense
								    `((fst::tense ,tense)))
								   (t
								    nil))
							   ,@(unless (or (null base-sub-id)
									 ;; morphosyntactic alternation, used in PASS template
									 (= sub-id base-sub-id))
								     `((fst::alternation
									,(gethash (cons id sub-id) *alternation-table*))))
							   ,@(when (car pv+impf+vn) `((fst::pv ,(car pv+impf+vn))))
							   ,@(when (cadr pv+impf+vn) '((fst::aspect "imperf")))
							   ,@(when vn `((fst::vn ,(caddr pv+impf+vn))))
							   ,@(when gv `((fst::gv ,gv)))
							   ,@(when sf `((fst::sf ,sf)))
							   ,@(when caus-sf `((fst::caus-sf ,caus-sf)))
							   ,@(when vv `((fst::vv ,vv)))
							   ,@(when morph-type `((fst::morph-type ,morph-type)))
							   ,@(when version `((fst::version ,version)))
							   ,@(when class `((fst::genus ,class)))
							   ,@(when relation `((fst::relation ,relation)))
							   ;; ,@(when red-dir-pv `((fst::reduplication +)))
							   ;; ,@(when red-dir-pv `((fst::red-dir-pv ,red-dir-pv)))
							   ,@(when stem-type `((fst::stem-type ,stem-type)))
							   ,@(when pr-st-ext `((fst::pr-st-ext ,pr-st-ext)))
							   ,@(when part-pfx `((fst::part-pfx ,part-pfx)))
							   ,@(when part-sfx `((fst::part-sfx ,part-sfx)))
							   ,@(when passive-sfx `((fst::passive-sfx ,passive-sfx)))
							   ,@(when type-aorist `(((fst::type fst::aorist) ,type-aorist)))
							   ,@(when type-obj-3-pfx `(((fst::type fst::obj-3-pfx) ,type-obj-3-pfx)))
							   ,@(when type-aorist-3sg `(((fst::type fst::aorist-3sg) ,type-aorist-3sg)))
							   ,@(when type-optative `(((fst::type fst::optative) ,type-optative)))
							   ,@(when nasal-infix `((fst::nasal-infix ,nasal-infix)))
							   ,@(when subj-pers `(((fst::subj fst::pers) ,subj-pers)))
							   ,@(when subj-num `(((fst::subj fst::num) ,subj-num)))
							   ,@(when obj-pers `(((fst::obj fst::pers) ,obj-pers)))
							   ,@(when type-subj12-sfx `(((fst::type fst::subj12-sfx) ,type-subj12-sfx)))
							   ,@(when type-subj3-sfx `(((fst::type fst::subj3-sfx) ,type-subj3-sfx)))
							   ,@(when type-subj2-pfx `(((fst::type fst::subj2-pfx) ,type-subj2-pfx)))
							   ,@(when type-ev-sfx `(((fst::type fst::ev-sfx) ,type-ev-sfx)))
							   ,@(when style `((fst::style ,style)))
							   ,@(when lang `((fst::lang ,lang)))
							   ,@(when type-pr-st-ext `(((fst::type fst::pr-st-ext) ,type-pr-st-ext)))
							   ;;,@(when disabled '((fst::disabled t)))
							   ,@(when paradigm-replacement 
								   `((fst::paradigm-replacement ,paradigm-replacement)))))))))))))))))))))

(defun tense< (tense-list1 tense-list2)
  (labels ((2-has-tense (tense-list)
	     (find-if (lambda (tense) (sfind tense tense-list2)) tense-list))
	   (sfind (a b)
	     (find a b :test #'string=)))
    (cond ((sfind "masdar" tense-list1)
	   t)
	  ((sfind "past-part" tense-list1)
	   (not (sfind "masdar" tense-list2)))
	  ((sfind "present-part" tense-list1)
	   (not (2-has-tense '("masdar" "past-part"))))
	  ((sfind "future-part" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part"))))
	  ((sfind "negative-part" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part"))))
	  ((sfind "present" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part" "negative-part"))))

	  ((sfind "imperfect" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part" "negative-part"
			       "present"))))
	  ((sfind "conj-present" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part" "negative-part"
			       "present" "imperfect"))))
	  ((sfind "future" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part" "negative-part"
			       "present" "imperfect" "conj-present"))))
	  ((sfind "conditional" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part" "negative-part"
			       "present" "imperfect" "conj-present"
			       "future"))))
	  ((sfind "conj-future" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part" "negative-part"
			       "present" "imperfect" "conj-present"
			       "future" "conditional"))))
	  ((sfind "aorist" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part" "negative-part"
			       "present" "imperfect" "conj-present"
			       "future" "conditional" "conj-future"))))
	  ((sfind "optative" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part" "negative-part"
			       "present" "imperfect" "conj-present"
			       "future" "conditional" "conj-future"
			       "aorist"))))
	  ((sfind "perfect" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part" "negative-part"
			       "present" "imperfect" "conj-present"
			       "future" "conditional" "conj-future"
			       "aorist" "optative"))))
	  ((sfind "pluperfect" tense-list1)
	   (not (2-has-tense '("masdar" "past-part" "present-part" "future-part" "negative-part"
			       "present" "imperfect" "conj-present"
			       "future" "conditional" "conj-future"
			       "aorist" "optative"
			       "perfect"))))
	  (t
	   nil))))

;;(print (sort '(("imperfect") ("optative" "aorist") ("future") ("conditional")) #'tense<))

(defun replace-root (new-root root)
  (if new-root
      (u::subst-substrings new-root (list "*" root))
      root))

(defparameter *author* "paul")

(defun normalize-bar (value) (when value (substitute #\/ #\| value)))
(defun denormalize-bar (value) (when value (substitute #\| #\/ value)))

(defun get-feature-choices (sfeature)
  (select (sql-expression :attribute sfeature)
	  :distinct t :flatp t
	  :from [morph verb-features]
	  :order-by (sql-expression :attribute sfeature)))

(defun get-paradigm-feature-choices (sfeature)
  (select (sql-expression :attribute sfeature)
	  :distinct t :flatp t
	  :from [morph verb-paradigm]
	  :order-by (sql-expression :attribute sfeature)))

#+test
(delete-records :from [morph verb-translation]
		:where [and [= [id] 3623]
			    [= [sub-id] 9]])

(defun get-paradigm-features (p-id p-sub-id &key
			      unique-id
			      tenses pv
			      sort-key
			      copy-p ;; copy an existing paradigm, to be edited:
			      new-id ;; new-id should be an existing id if the new paradigm is to be added to an existing id (= super-paradigm)
			      new-pv ;; use this pv
			      new-vn ;; use this vn
			      new-root ;; use this root
			      new-c-root) ;; use this c-root
  #-debug(print (list :p-id p-id :p-sub-id p-sub-id :new-id new-id))
  (let* ((where (cond (unique-id
		       (error "obsolete")
		       [= [unique-id] ?unique-id])
		      (tenses
		       [and [= [id] ?p-id]
			    [or (mapcar (lambda (tense)
					  [like [tense] (concatenate 'string "%|" tense "|%")])
					tenses)]])
		      (pv
		       [and [= [id] ?p-id]
			    [= [pv] ?pv]])
		      (t
		       [and [= [id] ?p-id] [= [sub-id] ?p-sub-id]])))
	 (new-id-exists-p (and new-id (select [id] :flatp t :distinct t
					      :from [morph verb-paradigm]
					      :where [= [id] ?new-id])))
	 (new-p-sub-id (cond ((null copy-p)
			      p-sub-id)
			     ;; new-id exists; have to generate new sub-id
			     (new-id-exists-p
			      (1+ (car (select [max [sub-id]]
					       :flatp t
					       :from [morph verb-paradigm]
					       :where [= [id] ?new-id]))))
			     ;; new id; start with sub-id = 1
			     (t
			      1)))
	 (new-p-id (if new-id-exists-p
		       new-id
		       (1+ (car (select [max [id]] :flatp t :from [morph verb-features]))))))
    #+debug(print (list :copy-p copy-p :sort-key sort-key
			:new-id-exists-p new-id-exists-p
			:p-id p-id
			:p-sub-id p-sub-id))
    ;;(debug new-p-sub-id)
    (let* ((rows (collecting
		   (with-database-connection ()
		     (do-query ((unique-id id sub-id root c-root tense
					   ;; pv vn ;; needed??
					   gv sf caus-sf vv
					   tsch-class morph-type relation 
					   ;; reduplication red-dir-pv
					   stem-type pr-st-ext part-pfx part-sfx passive-sfx
					   type-aorist type-obj-3-pfx type-aorist-3sg type-optative
					   nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
					   type-subj3-sfx type-subj2-pfx type-ev-sfx
					   style lang type-pr-st-ext paradigm-replacement)
				[select [unique-id] [id] [sub-id] [root] [c-root] [tense]
					;;[pv] ;; needed??
					;;[vn] ;; needed??
					[gv] [sf] [caus-sf] [vv]
					[tsch-class] [morph-type] [relation]
					;; [reduplication] [red-dir-pv]
					[stem-type] [pr-st-ext] [part-pfx] [part-sfx] [passive-sfx]
					[type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
					[nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
					[type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
					[style] [lang] [type-pr-st-ext] [paradigm-replacement]
					:from [morph verb-features]
					:order-by (when sort-key
						    (case sort-key
						      (:paradigm '([sub-id]))
						      (otherwise
						       (print (list (sql-expression :attribute sort-key))))))
					:where ?where])
		       (setf tense (nreverse (split tense #\| nil nil t)))
		       (collect `(("unique-id" ,unique-id)
				  ("paradigm" ,(format nil "~a-~a"
						       (if copy-p new-p-id id)
						       (if copy-p new-p-sub-id sub-id)))
				  ("tense" ,tense)
				  ("root" ,(replace-root new-root root))
				  ("c-root" ,(replace-root new-c-root c-root))
				  ;;("pv" ,(or new-pv pv))
				  ;;("vn" ,(or new-vn vn))
				  ("gv" ,gv)
				  ("sf" ,sf)
				  ("caus-sf" ,caus-sf)
				  ("vv" ,vv)
				  ("tsch-class" ,tsch-class)
				  ("morph-type" ,(normalize-bar morph-type))
				  ("relation" ,relation)
				  ;; ("reduplication" ,reduplication) ("red-dir-pv" ,red-dir-pv)
				  ("stem-type" ,stem-type) ("pr-st-ext" ,pr-st-ext) ;; ("pf-sfx" ,pf-sfx)
				  ("part-pfx" ,part-pfx) ("part-sfx" ,part-sfx) ("passive-sfx" ,passive-sfx)
				  ("type-aorist" ,type-aorist)
				  ("type-obj-3-pfx" ,Type-Obj-3-pfx)
				  ("type-aorist-3sg" ,type-aorist-3sg) ("type-optative" ,type-optative)
				  ("nasal-infix" ,nasal-infix) ("subj-pers" ,(normalize-bar subj-pers))
				  ("subj-num" ,subj-num)
				  ("obj-pers" ,(normalize-bar obj-pers))
				  ("type-subj12-sfx" ,type-subj12-sfx) ("type-subj3-sfx" ,type-subj3-sfx)
				  ("type-subj2-pfx" ,type-subj2-pfx) ("type-ev-sfx" ,type-ev-sfx)
				  ("style" ,style)
				  ("lang" ,lang)
				  ("type-pr-st-ext" ,type-pr-st-ext)
				  ("paradigm-replacement" ,paradigm-replacement)))))))
	   (rows (if sort-key rows (sort rows #'tense< :key (lambda (list) (cadr (caddr list))))))
	   
	   (paradigm-rows
	    (select 'verb-paradigm :refresh t :flatp t
		    :where (if nil ;; pv
			       [and [= [id] ?p-id] [or [= [pf-pv] ?pv] [= [impf-pv] ?pv]]]
			       [and [= [id] ?p-id] [= [sub-id] ?p-sub-id]])))
	   (participles
	    (select 'verb-participle :refresh t :flatp t
		    :where [and [= [id] ?p-id] [= [sub-id] ?p-sub-id]]))
	   (templates
	    (select [template] :flatp t
		    :from [morph xle-template]
		    :where [and [= [id] ?p-id] [= [sub-id] ?p-sub-id]]))
	   ;; counts number of non-NULL fields for each column; only those with non-NULL fields are displayed
	   (field-count-list
	    (car (collecting
		   (with-database-connection ()
		     (do-query ((unique-id root c-root tense
					   ;; pv vn
					   gv sf caus-sf vv
					   tsch-class morph-type relation 
					   ;; reduplication red-dir-pv
					   stem-type pr-st-ext part-pfx part-sfx passive-sfx
					   type-aorist type-obj-3-pfx type-aorist-3sg type-optative
					   nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
					   type-subj3-sfx type-subj2-pfx type-ev-sfx
					   style lang type-pr-st-ext paradigm-replacement)
				[select [count [unique-id]] [count [root]] [count [c-root]] [count [tense]]
					;; [count [pv]] [count [vn]]
					[count [gv]] [count [sf]] [count [caus-sf]] [count [vv]] [count [tsch-class]] [count [morph-type]] [count [relation]] 
					;; [count [reduplication]] [count [red-dir-pv]]
					[count [stem-type]] [count [pr-st-ext]] [count [part-pfx]] [count [part-sfx]] [count [passive-sfx]] [count [type-aorist]] [count [type-obj-3-pfx]] [count [type-aorist-3sg]] [count [type-optative]] [count [nasal-infix]] [count [subj-pers]] [count [subj-num]] [count [obj-pers]] [count [type-subj12-sfx]] [count [type-subj3-sfx]] [count [type-subj2-pfx]] [count [type-ev-sfx]] [count [style]] [count [lang]] [count [type-pr-st-ext]] [count [paradigm-replacement]]
					:from [morph verb-features]
					:where ?where])
		       (collect `( ;;("copy" ,unique-id)
				  ("delete" ,unique-id)
				  ("paradigm" ,unique-id)
				  ("tense" ,tense)
				  ("root" ,root) ("c-root" ,c-root)
				  ;; ("pv" ,pv) ("vn" ,vn)
				  ("gv" ,gv) ("sf" ,sf)
				  ("caus-sf" ,caus-sf) ("vv" ,vv) ("tsch-class" ,tsch-class)
				  ("morph-type" ,morph-type) ("relation" ,relation)
				  ;; ("reduplication" ,reduplication) ("red-dir-pv" ,red-dir-pv)
				  ("stem-type" ,stem-type) ("pr-st-ext" ,pr-st-ext) ;; ("pf-sfx" ,pf-sfx)
				  ("part-pfx" ,part-pfx) ("part-sfx" ,part-sfx) ("passive-sfx" ,passive-sfx)
				  ("type-aorist" ,type-aorist) ("type-obj-3-pfx" ,type-obj-3-pfx)
				  ("type-aorist-3sg" ,type-aorist-3sg) ("type-optative" ,type-optative)
				  ("nasal-infix" ,nasal-infix) ("subj-pers" ,subj-pers)
				  ("subj-num" ,subj-num) ("obj-pers" ,obj-pers)
				  ("type-subj12-sfx" ,type-subj12-sfx) ("type-subj3-sfx" ,type-subj3-sfx)
				  ("type-subj2-pfx" ,type-subj2-pfx) ("type-ev-sfx" ,type-ev-sfx)
				  ("style" ,style)
				  ("lang" ,lang)
				  ("type-pr-st-ext" ,type-pr-st-ext)
				  ("paradigm-replacement" ,paradigm-replacement)))))))))
      ;; store copied paradigm
      (when copy-p
	(let ((date (get-universal-time))
	      (first-p t))
	  (with-transaction ()
	    (let ((unique-id (car (select [max [unique-id]] :flatp t :from [morph verb-features])))
		  (vpar (car paradigm-rows)))
	      (when vpar
		(make-instance 'verb-paradigm
			       :id new-p-id 
			       :sub-id new-p-sub-id
			       :c-root (or new-c-root (c-root vpar))
			       :vn (vn vpar)
			       :impf-vn (impf-vn vpar)
			       :pf-vn (pf-vn vpar)
			       :tsch-class (tsch-class vpar)
			       :class (verb-class vpar)
			       :features-sub-id (if (= (features-sub-id vpar) (sub-id vpar))
						    new-p-sub-id
						    (features-sub-id vpar))
			       :link-sub-id new-p-sub-id ;; (link-sub-id vpar)
			       :base-sub-id new-p-sub-id ;; (base-sub-id vpar)
			       :participle-sub-id new-p-sub-id ;; (participle-sub-id vpar)
			       ;;:pv (pv vpar)
			       :pf-pv (pf-pv vpar)
			       :impf-pv (impf-pv vpar)
			       :date (get-universal-time)))
	      (dolist (part participles)
		(make-instance 'verb-participle
			       :id new-p-id
			       :sub-id new-p-sub-id
			       :type (participle-type part)
			       :stem (participle-stem part)
			       :code (conjugation-code part)
			       :variety (variety part)
			       :aspect (aspect part)
			       :main-form (main-form part)
			       :date (get-universal-time)))
	      (dolist (template templates)
		(insert-records
		 :into [morph xle-template]
		 :attributes (list [id] [sub-id] [template])
		 :values (list new-p-id new-p-sub-id template)))
	      (dolist (row rows)
		(let ((row (mapcar #'cadr row)))
		  (destructuring-bind (root c-root  ;; pv vn
					    gv sf caus-sf vv
					    tsch-class morph-type relation ;; reduplication red-dir-pv
					    stem-type pr-st-ext part-pfx part-sfx passive-sfx
					    type-aorist type-obj-3-pfx type-aorist-3sg type-optative
					    nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
					    type-subj3-sfx type-subj2-pfx type-ev-sfx
					    style lang type-pr-st-ext paradigm-replacement) (cdddr row)
		    (print :inserting)
		    (when (and copy-p new-id-exists-p)
		      (remhash root *feature-cache*))
		    (insert-records :into [morph verb-features]
				    :attributes (list [unique-id] [id] [sub-id] [tense]
						      [date] [author]
						      [root] [c-root] ;; [pv] [vn]
						      [gv] [sf] [caus-sf] [vv]
						      [tsch-class] [morph-type] [relation]
						      ;; [reduplication] [red-dir-pv]
						      [stem-type] [pr-st-ext] [part-pfx] [part-sfx] [passive-sfx]
						      [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
						      [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
						      [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
						      [style] [lang] [type-pr-st-ext] [paradigm-replacement]
						      )
				    :values (destructuring-bind (p-id p-sub-id) (split (cadr row) #\-)
					      (print (list
						      (incf unique-id)
						      p-id
						      p-sub-id
						      (format nil "|~(~{~a~^|~}~)|" (reverse (caddr row)))
						      date *author*
						      root c-root ;; pv vn
						      gv sf caus-sf vv
						      tsch-class morph-type relation
						      ;; reduplication red-dir-pv
						      stem-type pr-st-ext part-pfx part-sfx passive-sfx
						      type-aorist type-obj-3-pfx type-aorist-3sg type-optative
						      nasal-infix (denormalize-bar subj-pers) subj-num (denormalize-bar obj-pers)
						      type-subj12-sfx type-subj3-sfx type-subj2-pfx type-ev-sfx
						      style lang type-pr-st-ext paradigm-replacement
						      #+old(cdddr row)))))
		    (when first-p
		      (insert-records :into [morph verb-translation]
				      :attributes (list [id] [sub-id] [link-sub-id] [base-sub-id] [pv] [translation])
				      :values (list new-p-id new-p-sub-id nil p-sub-id pv nil))
		      (setf first-p nil))
		    (pushnew (fst::inv-convert c-root)
			     (gethash (fst::inv-convert root) fst::*root-table*)
			     :test #'string=))))
	      (update-paradigm-table :id new-p-id)))))
      (values rows field-count-list paradigm-rows))))

#+test
(update-records ;;[morph verb-features]
 ;;[morph verb-translation]
 [morph verb-paradigm]
 :av-pairs '(([id] 1936)
	     ([sub-id] 1))
 :where [and [= [id] 377] [= [sub-id] 4]])
#+test
(delete-records
 :from [morph verb-translation]
 :where [and [= [id] 377] [= [sub-id] 4]])


(defun get-paradigm-comments (id sub-id)
  (values nil
	  (car (select [comment] :flatp t :from [morph verb-translation]
		       :where [and [= [id] ?id] [= [sub-id] ?sub-id]]))))

;;(print (select [c-root] [root] :distinct t :from [verb-features] :where [= [unique-id] 47698]))

#+test
(delete-recordsxx :from [morph verb-participle] :where [and [= [id] 2424] [= [sub-id] 13]])

(defun store-paradigm-changes (&key id sub-id delete copy changed-features id-comment
			       sub-id-comment delete-paradigm add-template delete-template)
  #-debug(print (list :updating :id id :sub-id sub-id :sub-id-comment sub-id-comment
		      :copy copy :delete delete :changed-features changed-features
		      :add-template add-template))
  (when (or changed-features id-comment sub-id-comment delete copy delete-paradigm add-template delete-template)
    (let ((now (get-universal-time)))
      (with-database-connection ()
	(with-transaction ()
	  (when sub-id-comment
	    (update-records [morph verb-translation]
			    :attributes `([comment])
			    :values (list sub-id-comment)
			    :where [and [= [id] ?id] [= [sub-id] ?sub-id]]))
	  (when id-comment
	    (if (select [id] :flatp t
			:from [morph super-paradigm]
			:where [= [id] ?id])
		(update-records [morph super-paradigm]
				:attributes `([comment])
				:values (list id-comment)
				:where [= [id] ?id])
		(insert-records :into [morph super-paradigm]
				:attributes `([id] [comment])
				:values (list id id-comment))))
	  (dolist (pair changed-features)
	    (destructuring-bind (unique-id feature) (split (car pair) #\@)
	      (cond ((find-if (lambda (c) (find c "0123456789")) unique-id)
		     (dolist (fv-pair (if (string= feature "paradigm")
					  (destructuring-bind (id sub-id) (split (cdr pair) #\-)
					    (list (cons 'id id)
						  (cons 'sub-id sub-id)))
					  (list (cons (intern (string-upcase feature) :fst) (cdr pair)))))
		       (destructuring-bind (feature . value) fv-pair
			 (let ((value (case feature
					(tense
					 (format nil "|~{~a|~}" (mapcar (lambda (v) (string-trim " " v)) (split value #\,))))
					((subj-pers obj-pers)
					 (substitute #\| #\/ value))
					(otherwise
					 value))))
			   (update-records [morph verb-features]
					   :attributes (list (sql-expression :attribute feature)
							     [date]
							     [author])
					   :values (list (if (equal value "") nil value)
							 now
							 *author*)
					   :where [= [unique-id] ?unique-id]))))
		     (update-paradigm-table :unique-id unique-id)
		     (let ((root-list (select [c-root] [root] :distinct t :from [morph verb-features]
					      :where [= [unique-id] ?unique-id])))
		       (dolist (c-root+root root-list)
			 (destructuring-bind (c-root root) c-root+root
			   (pushnew (fst::inv-convert c-root)
				    (gethash (fst::inv-convert root) fst::*root-table*)
				    :test #'string=)
			   (remhash root *feature-cache*)))))
		    (t ;; paradigm features
		     (destructuring-bind (impf-pv pf-pv) (split unique-id #\$)
		       (let ((feature (intern (string-upcase feature) :fst))
			     (value (cdr pair)))
			 (when (and (equal value "-")
				    (not (find feature '(:impf-pv :pf-pv))))
			   (setf value nil))
			 (update-records [morph verb-paradigm]
					 :attributes (list (sql-expression :attribute feature)
							   [date]
							   [author])
					 :values (list (if (equal value "") nil value)
						       now
						       *author*)
					 :where [and [= [impf-pv] ?impf-pv]
						     [= [pf-pv] ?pf-pv]
						     [= [id] ?id]
						     [= [sub-id] ?sub-id]]))
		       (update-paradigm-table :id id)
		       (let ((root-list (select [c-root] [root] :distinct t :from [morph verb-features]
						:where [= [id] ?id])))
			 (dolist (c-root+root root-list)
			   (destructuring-bind (c-root root) c-root+root
			     (pushnew (fst::inv-convert c-root)
				      (gethash (fst::inv-convert root) fst::*root-table*)
				      :test #'string=)
			     (remhash root *feature-cache*)))))))))
	  (dolist (unique-id copy)
	    (let ((row (car (select [*] :from [morph verb-features] :where [= [unique-id] ?unique-id])))
		  (new-unique-id (1+ (car (select [max [unique-id]] :from [morph verb-features] :flatp t)))))
	      (print (list :inserting (cons new-unique-id (cdr row))))
	      (insert-records :into [morph verb-features] :values (cons new-unique-id (cdr row)))
	      (update-records [morph verb-features]
			      :attributes (list [date] [author])
			      :values (list now *author*)
			      :where [= [unique-id] ?new-unique-id])))
	  (when delete
	    (let ((roots (select [root]
				 :distinct t :flatp t
				 :from [morph verb-features]
				 :where [in [unique-id] ?delete])))
	      (delete-records :from [morph verb-features] :where [in [unique-id] ?delete])
	      (dolist (root roots)
		(remhash root *feature-cache*))))
	  (when delete-paradigm
	    (let ((roots (select [root]
				 :distinct t :flatp t :from [morph verb-features]
				 :where [and [= [id] (parse-integer id)] [= [sub-id] (parse-integer sub-id)]])))
	      (dolist (root roots)
		(remhash root *feature-cache*)))
	    (destructuring-bind (id sub-id) (mapcar #'parse-integer (split delete-paradigm #\-))
	      (print (list :delete id sub-id))
	      (delete-records :from [morph verb-features] :where [and [= [id] ?id] [= [sub-id] ?sub-id]])
	      (delete-records :from [morph verb-paradigm] :where [and [= [id] ?id] [= [sub-id] ?sub-id]])
	      (delete-records :from [morph participle] :where [and [= [id] ?id] [= [sub-id] ?sub-id]])
	      (delete-records :from [morph verb-translation] :where [and [= [id] ?id] [= [sub-id] ?sub-id]])
	      (delete-records :from [morph xle-template] :where [and [= [id] ?id] [= [sub-id] ?sub-id]])
	      ))
	  (when add-template
	    (setf *xle-template-list* ())
	    (insert-records :into [morph xle-template]
			    :attributes `([id] [sub-id] [template])
			    :values (list id sub-id (string-trim '(#\space #\tab #\newline) add-template))))
	  (when delete-template
	    (setf *xle-template-list* ())
	    (delete-records :from [morph xle-template]
			    :where [and [= [id] ?id]
					[= [sub-id] ?sub-id]
					[ = [template] ?delete-template]])))))))

(defun store-translation-changes (translation &key paradigm-id)
  ;;(print (list translation paradigm-id))
  (destructuring-bind (id sub-id) (mapcar #'parse-integer (split paradigm-id #\-))
    ;;(execute-command "set names UTF8")
    ;;#+mysql(execute-command "set names LATIN1")
    (with-transaction ()
      (if (select [id] :flatp t
		  :from [morph verb-translation]
		  :where [and [= [id] ?id] [= [sub-id] ?sub-id]])
	  (update-records [morph verb-translation]
			  :attributes `([translation] [author] [date])
			  :values (list translation *author* (get-universal-time))
			  :where [and [= [id] ?id] [= [sub-id] ?sub-id]])
	  (insert-records :into [morph verb-translation]
			  :attributes `([id] [sub-id] [base-sub-id] [translation] [author] [date])
			  :values (list id sub-id sub-id translation *author* (get-universal-time)))))))

#+test
(print (gethash (fst::inv-convert "lolosdg") fst::*root-table*))

#+test
(print (select [vn] :distinct t :from [verb-features] :where [and [like [vn] "% (%"]
								  [not [like [vn] "%||%"]]]))
#+test
(dolist (uid+vn (select [unique-id] [vn] :from [verb-features] :where [like [vn] "% (Dieses%"]))
  (with-transaction ()
    (destructuring-bind (uid vn) (print uid+vn)
      (update-records [verb-features]
		      :attributes `([vn] [comment])
		      :values (list (subseq vn 0 (position #\space vn))
				    (subseq vn (1+ (position #\space vn))))
		      :where [= [unique-id] uid]))))

#+test
(print (paradigm-base-sub-id+source "904-30"))

(defun paradigm-base-sub-id (id sub-id &key recursive-p)
  (let ((base-sub-id (car (select [base-sub-id]
				  :from [morph verb-translation] :flatp t
				  :where [and [= [id] ?id]
					      [= [sub-id] ?sub-id]
					      [not [= [base-sub-id] 0]]]))))
    (or (and recursive-p
	     base-sub-id
	     (paradigm-base-sub-id-rec id base-sub-id :recursive-p recursive-p))
	base-sub-id)))

#+test
(with-database-connection ()
  (print (paradigm-base/participle-sub-id+source 2361 2)))

(defun paradigm-base/participle-sub-id+source (id sub-id)
  (debug sub-id)
  (debug (car (select [base-sub-id] [participle-sub-id] [source]
	       :from [morph verb-paradigm]
	       :where [and [= [id] ?id] [= [sub-id] ?sub-id]]))))

(defun paradigm-xle-templates (id sub-id)
  (select [template] :flatp t :from [morph xle-template] :where [and [= [id] ?id] [= [sub-id] ?sub-id]]))

#+test
(print (paradigm-xle-templates 1266 67))
#+test
(print (xle-template-to-frame (car (paradigm-xle-templates 1266 67))))

(defun get-available-xle-templates ()
  (select [template] :flatp t :distinct t :from [morph xle-template] :order-by `([template])))

(defun update-base-sub-ids ()
  (with-transaction ()
    (dolist (id+link (select [id] [sub-id] [link-sub-id]
			     :from [morph verb-translation]
			     :where [and [link-sub-id]
					 [not [= [link-sub-id] 0]]]))
      (destructuring-bind (id sub-id link) id+link
	(labels ((get-base-sub-id (link)
		   (let ((base-sub-id (car (select [link-sub-id]
						   :from [morph verb-translation] :flatp t
						   :where [and [= [id] ?id]
							       [= [sub-id] ?link]
							       [not [= [link-sub-id] 0]]]))))
		     (or (and base-sub-id
			      (get-base-sub-id base-sub-id))
			 link))))
	  (update-records [morph verb-translation]
			  :attributes `([base-sub-id])
			  :values (list (get-base-sub-id link))
			  :where [and [= [id] ?id]
				      [= [sub-id] ?sub-id]]))))))

#+test
(defun update-base-sub-ids ()
  (with-transaction ()
    (dolist (id+link (select [id] [sub-id]
			     :from [verb-translation]
			     :where [null [link-sub-id]]))
      (destructuring-bind (id sub-id) id+link
	(update-records [verb-translation]
			:attributes `([base-sub-id])
			:values (list sub-id)
			:where [and [= [id] id]
				    [= [sub-id] sub-id]])))))


(defun delete-nonfinite-forms (id sub-id)
  (print (list :delete-nonfinite id sub-id))
  (let ((id (typecase id (string (parse-integer id)) (t id)))
	(sub-id (typecase sub-id (string (parse-integer sub-id)) (t sub-id))))
    (with-transaction ()
      (delete-records :from [morph verb-features]
		      :where [and [= [id] ?id]
				  [= [sub-id] ?sub-id]
				  [in [tense] (list "|masdar|" "|past-part|" "|present-part|" "|future-part|" "|negative-part|")]])
      (update-records [morph verb-translation]
		      :attributes `([participle-sub-id])
		      :values (list nil)
		      :where [and [= [id] ?id]
				  [= [sub-id] ?sub-id]
				  [= [sub-id] [participle-sub-id]]])))
  (let ((roots (select [root]
		       :distinct t :flatp t :from [morph verb-features]
		       :where [and [= [id] ?id] [= [sub-id] ?sub-id]])))
    (dolist (root roots)
      (print (list :remhash root))
      ;;(remhash root *feature-cache*)
      (clrhash *feature-cache*))))

;; not working any more
(defun copy-participles (id &key from-subid to-subid)
  (print (list :copy id from-subid to-subid))
  (with-transaction ()
    (let* ((uid (car (select [max [unique-id]] :from [morph verb-features] :flatp t)))
	   (pv-tsch-list (select [pv] [tsch-class]
				 :from [morph verb-features]
				 :where [and [= [id] ?id]
					     [= [sub-id] ?to-subid]
					     [or [like [tense] "%|future|%"]
						 [like [tense] "%|present|%"]
						 [like [tense] "%|aorist|%"]]]))
	   (pv-list (sort (delete-if #'null (mapcar #'car pv-tsch-list)) #'string>))
	   (participle-rows
	    (select [root] [c-root] [tense] [vn] [gv] [sf]
		    ;;[tsch-class] [morph-type] [relation]
		    [reduplication] [red-dir-pv] [stem-type]
		    [pr-st-ext] [part-pfx] [part-sfx]
		    :from [morph verb-features]
		    :where [and [= [id] ?id]
				[= [sub-id] ?from-subid]
				[or [like [tense] "%masdar%"]
				    [like [tense] "%-part%"]]])))
      (dolist (row participle-rows)
	(destructuring-bind (root c-root tense vn gv sf reduplication
				  red-dir-pv stem-type pr-st-ext part-pfx part-sfx) row
	  (unless (select [id] :flatp t
			  :from [morph verb-features]
			  :where [and [= [id] ?id]
				      [= [sub-id] ?to-subid]
				      [= [tense] ?tense]
				      [= [root] ?root]
				      [= [c-root] ?c-root]
				      (if gv
					  [= [gv] ?gv]
					  [null [gv]])
				      (if sf
					  [= [sf] ?sf]
					  [null [sf]])
				      [= [pv] (car pv-list)]
				      (if part-pfx
					  [= [part-pfx] ?part-pfx]
					  [null [part-pfx]])
				      (if part-sfx
					  [= [part-sfx] ?part-sfx]
					  [null [part-sfx]])])
	    #+debug
	    (print (list :not-found
			 id to-subid
			 root c-root tense (car pv-list)
			 (cadar pv-tsch-list)
			 :vn vn :gv gv :sf sf :part-pfx part-pfx :part-sfx part-sfx))
	    (insert-records :into [morph verb-features]
			    :attributes '([unique-id] [id] [sub-id]
					  [root] [c-root] [tense] [pv] [tsch-class] [vn] [gv] [sf]
					  ;;[tsch-class] [morph-type] [relation]
					  [reduplication] [red-dir-pv] [stem-type]
					  [pr-st-ext] [part-pfx] [part-sfx]
					  [author] [date])
			    :values (list (incf uid) id to-subid
					  root c-root tense (car pv-list)
					  (cadar pv-tsch-list)
					  vn gv sf reduplication
					  red-dir-pv stem-type pr-st-ext part-pfx part-sfx
					  "paul"
					  (get-universal-time))))))))
  (update-paradigm-table :id id)
  (let ((root-list (select [c-root] [root] :distinct t :from [morph verb-features]
			   :where [= [id] ?id])))
    (dolist (c-root+root root-list)
      (destructuring-bind (c-root root) c-root+root
	(pushnew (fst::inv-convert c-root)
		 (gethash (fst::inv-convert root) fst::*root-table*)
		 :test #'string=)
	(remhash root *feature-cache*)))))

;; OBS: these should modify [morph verb-paradigm] !!!
(defun add-morphosyntactic-link (id sub-id link-sub-id)
  (print (list :add-morphosyntactic-link id sub-id link-sub-id))
  (let* ((id (typecase id (string (parse-integer id)) (t id)))
	 (sub-id (typecase sub-id (string (parse-integer sub-id)) (t sub-id)))
	 (link-sub-id (typecase link-sub-id (string (parse-integer link-sub-id)) (t link-sub-id)))
	 (base-sub-id (car (select [base-sub-id] :flatp t
				   :from [morph verb-translation]
				   :where [and [= [id] ?id]
					       [= [sub-id] ?link-sub-id]]))))
    (when (and link-sub-id base-sub-id)
      (with-transaction ()
	#-debug(print (list :id id :sub-id sub-id :link-sub-id link-sub-id :base-sub-id base-sub-id))
	(update-records [morph verb-translation]
			:attributes `([link-sub-id] [base-sub-id])
			:values (list link-sub-id base-sub-id)
			:where [and [= [id] ?id]
				    [= [sub-id] ?sub-id]])))))

(defun remove-morphosyntactic-link (id sub-id)
  (print (list :remove-morphosyntactic-link id sub-id))
  (let* ((id (parse-integer id))
	 (sub-id (parse-integer sub-id)))
    (with-transaction ()
      (update-records [morph verb-paradigm]
		      :attributes `([link-sub-id] [base-sub-id])
		      :values (list nil sub-id)
		      :where [and [= [id] ?id]
				  [= [sub-id] ?sub-id]]))))

(defun remove-participle-link (id sub-id)
  (print (list :remove-participle-link id sub-id))
  (let* ((id (parse-integer id))
	 (sub-id (parse-integer sub-id)))
    (with-transaction ()
      (update-records [morph verb-paradigm]
		      :attributes `([participle-sub-id])
		      :values (list nil)
		      :where [and [= [id] ?id]
				  [= [sub-id] ?sub-id]]))))

(defun add-participle-link (id sub-id participle-sub-id)
  (print (list :add-participle-link id sub-id participle-sub-id))
  (let* ((id (typecase id (string (parse-integer id)) (t id)))
	 (sub-id (typecase sub-id (string (parse-integer sub-id)) (t sub-id)))
	 (participle-sub-id (typecase participle-sub-id (string (parse-integer participle-sub-id)) (t participle-sub-id))))
    (with-transaction ()
      #+debug(print (list :id id :sub-id sub-id :participle-sub-id participle-sub-id))
      (update-records [morph verb-paradigm]
		      :attributes `([participle-sub-id])
		      :values (list participle-sub-id)
		      :where [and [= [id] ?id]
				  [= [sub-id] ?sub-id]])))
  (delete-nonfinite-forms id sub-id))

#+test
(dolist (id+pv (select [root] [id] [sub-id] [count [distinct [pv]]]
		       :distinct t
		       :from [verb-features]
		       ;;:where [> [count [distinct [pv]]] 2]
		       :group-by `(,[id] ,[sub-id])))
  (print id+pv))

#+test
(with-transactionx ()
  (dolist (id+pv (select [id] [sub-id] [pv]
			 :distinct t
			 :from [verb-features]
			 :where [not [null [pv]]]
			 :order-by (list [id] [sub-id] [- [length [pv]]])))
    (destructuring-bind (id sub-id pv) id+pv
      (update-records [verb-translation]
		      :attributes `([pv])
		      :values (list pv)
		      :where [and [= [id] id]
				  [= [sub-id] sub-id]]))))

#+test
(update-base-sub-ids)

;; populate fullform list from corpus counts

#+test
(with-transaction ()
  (let ((counter 0))
    (with-file-lines (line "projects:georgian-morph;opentext-sorted-freq-words-2007-11-10.txt")
      (destructuring-bind (count word) (split (string-trim " " line) #\space)
	(let ((count (parse-integer count)))
	  ;;(print (list word count))
	  (when (zerop (mod (incf counter) 10000))
	    (commit)
	    (print counter))
	  (insert-records :into [fullform] :values (list word count)))))))

;; link sub-ids to base-sub-ids

#+test
(with-transaction ()
  (let ((kt-list (select [id] [sub-id] [pv] [vn] :distinct t
			 :from [verb-features]
			 :where [and [= [tsch-class] "KT"]
				     [like [tense] "%aorist%"]]
			 )))
    (dolist (kt kt-list)
      (destructuring-bind (id sub-id pv vn) kt
	(let* ((vn (subseq vn 0 (position #\space vn)))
	       (vn (string-trim "[]" (subseq vn 0 (position #\- vn)))))
	  (let ((link-sub-id
		 (car (select [sub-id] :distinct t
			      :from [verb-features]
			      :where [and [= [id] id]
					  [= [vn] vn]
					  [= [pv] pv]
					  [= [tsch-class] "T1"]
					  [like [tense] "%aorist%"]]))))
	    (when link-sub-id
	      (update-records [verb-translation]
			      :attributes `([link-sub-id])
			      :values (list link-sub-id)
			      :where [and [= [id] id]
					  [= [sub-id] sub-id]]))))))))

#+test
(with-transaction ()
  (let ((kt-list (select [id] [sub-id] [pv] [vn] :distinct t
			 :from [verb-features]
			 :where [and [= [tsch-class] "KT"]
				     [like [tense] "%aorist%"]]
			 )))
    (dolist (kt kt-list)
      (destructuring-bind (id sub-id pv vn) kt
	(let* ((vn (subseq vn 0 (position #\space vn)))
	       (vn (string-trim "[]" (subseq vn 0 (position #\- vn)))))
	  (let* ((link-sub-id1
		  (car (select [sub-id] :distinct t
			       :from [verb-features]
			       :where [and [= [id] id]
					   [= [vn] vn]
					   [= [pv] pv]
					   [= [tsch-class] "T1"]
					   [like [tense] "%aorist%"]])))
		 (link-sub-id5
		  (unless link-sub-id1
		    (car (select [sub-id] :distinct t
				 :from [verb-features]
				 :where [and [= [id] id]
					     [= [vn] vn]
					     [= [pv] pv]
					     [= [tsch-class] "T5"]
					     [like [tense] "%aorist%"]]))))
		 (link-sub-id2
		  (unless (or link-sub-id1 link-sub-id5)
		    (car (select [sub-id] :distinct t
				 :from [verb-features]
				 :where [and [= [id] id]
					     [= [vn] vn]
					     [= [pv] pv]
					     [= [tsch-class] "T2"]
					     [like [tense] "%aorist%"]])))))
	    (when link-sub-id2
	      (update-records [verb-translation]
			      :attributes `([link-sub-id])
			      :values (list link-sub-id2)
			      :where [and [= [id] id]
					  [= [sub-id] sub-id]]))))))))


#+test
(let ((kt-list (select [id] [sub-id] [pv] [vn] :distinct t
		       :from [verb-features]
		       :where [and [= [tsch-class] "KT"]
				   [like [tense] "%aorist%"]])))
  (dolist (kt kt-list)
    (destructuring-bind (id sub-id pv vn) kt
      (let* ((vn (subseq vn 0 (position #\space vn)))
	     (vn (string-trim "[]" (subseq vn 0 (position #\- vn)))))
	(let* ((link-sub-id1
		(car (select [sub-id] :distinct t
			     :from [verb-features]
			     :where [and [= [id] id]
					 [= [vn] vn]
					 [= [pv] pv]
					 [= [tsch-class] "T1"]
					 [like [tense] "%aorist%"]])))
	       (link-sub-id5
		(unless link-sub-id1
		  (car (select [sub-id] :distinct t
			       :from [verb-features]
			       :where [and [= [id] id]
					   [= [vn] vn]
					   [= [pv] pv]
					   [= [tsch-class] "T5"]
					   [like [tense] "%aorist%"]])))))
	  (when (and (null link-sub-id1)
		     (null link-sub-id5))
	    (print kt)
	    ))))))


;; P1 -> T1
;; RP1 -> T3

#+test
(with-transaction ()
  (let ((pass-list (select [id] [sub-id] [pv] [vn] :distinct t
			   :from [verb-features]
			   :where [and [= [tsch-class] "P1"]
				       [like [tense] "%aorist%"]])))
    (dolist (pass pass-list)
      (destructuring-bind (id sub-id pv vn) pass
	(let ((link-sub-id
	       (car (select [sub-id] :distinct t
			    :from [verb-features]
			    :where [and [= [id] id]
					[= [vn] vn]
					[= [pv] pv]
					[= [tsch-class] "T1"]
					[like [tense] "%aorist%"]]))))
	  (when link-sub-id
	    (update-records [verb-translation]
			    :attributes `([link-sub-id])
			    :values (list link-sub-id)
			    :where [and [= [id] id]
					[= [sub-id] sub-id]])))))))

#+test
(with-transaction ()
  (let ((pass-list (select [id] [sub-id] [pv] [vn] :distinct t
			   :from [verb-features]
			   :where [and [= [tsch-class] "RP1"]
				       [like [tense] "%aorist%"]])))
    (dolist (pass pass-list)
      (destructuring-bind (id sub-id pv vn) pass
	(let ((link-sub-id
	       (car (select [sub-id] :distinct t
			    :from [verb-features]
			    :where [and [= [id] id]
					[= [vn] vn]
					[= [pv] pv]
					[= [tsch-class] "T3"]
					[like [tense] "%aorist%"]]))))
	  (when link-sub-id
	    (update-records [verb-translation]
			    :attributes `([link-sub-id])
			    :values (list link-sub-id)
			    :where [and [= [id] id]
					[= [sub-id] sub-id]])))))))


#+test
(print (select [*] :from [verb-features] :where [= [c-root] "cer"]))

#+test
(let ((table (make-hash-table :test #'equal)))
  (with-file-lines (line "projects:georgian-morph;Levan;SORTFORM.txt")
    (destructuring-bind (root masdar form) (split line #\|)
      (pushnew masdar (gethash root table) :test #'string=)))
  (maphash (lambda (key val)
	     (format t "~a:~{ ~a~}~%" key val))
	   table)
  (print (hash-table-count table)))

#+test
(with-open-file (stream "projects:georgian-morph;Levan;root-masdar-form.txt" :direction :output :if-exists :supersede)
  (with-file-lines (line "projects:georgian-morph;Levan;SORTFORM.txt")
    (write-line (convert-encoding line :acad :amirani) stream)
    ))

#+test
(with-open-file (stream "projects:georgian-morph;Levan;verb-list.txt" :direction :output :if-exists :supersede)
  (with-file-lines (line "projects:georgian-morph;Levan;SORTFORM.txt")
    (destructuring-bind (root masdar form) (split line #\|)
      (write-line (convert-encoding form :acad :amirani) stream)
      )))

;; lookup ../regex/kartuli-morph.fst < verb-list.txt > verb-list-tagged.txt
;; 873251/2368948 missing

#+test
(add-t3)

#+test
(defun add-t3 ()
  (with-transaction ()
    (let ((t1-list (select [id] [sub-id] [pv] [vn] :distinct t
			   :from [verb-features]
			   :distinct t
			   :where [and [= [tsch-class] "T1"]
				       ;;[= [vn] "abrageba"]
				       [= [morph-type] "active"]
				       [like [tense] "%aorist%"]
				       [< [id] 3824]]
			   :order-by `([id])))
	  (date (get-universal-time)))
      (dolist (t1 t1-list)
	#+test
	(print t1)
	(destructuring-bind (id sub-id t1-pv vn) t1
	  (let* ((t3 (car (select [sub-id] :distinct t
				  :from [verb-features]
				  :where [and [= [id] id]
					      [= [vn] vn]
					      [= [pv] t1-pv]
					      [= [tsch-class] "T3"]
					      [= [morph-type] "active"]
					      [like [tense] "%aorist%"]])))
		 (new-sub-id (1+ (car (select [max [sub-id]] :from [verb-translation] :where [= [id] id] :flatp t)))))
	    (unless t3
	      (format t "~d-~d-~a-~a~%"  id sub-id (or t1-pv "") vn)
	      (let ((uid (car (select [max [unique-id]] :from [verb-features] :flatp t)))
		    (t3-list (select [id] [tense]
				     [root] [c-root] [pv] [vn] [gv] [sf] [caus-sf] [vv]
				     [tsch-class] [morph-type] [relation] [reduplication] [red-dir-pv]
				     [stem-type] [pr-st-ext] [part-pfx] [part-sfx] [passive-sfx]
				     [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
				     [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
				     [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
				     [style] [type-pr-st-ext] [paradigm-replacement]
				     :distinct t
				     :from [verb-features]
				     :where [and [= [id] id]
						 [= [sub-id] sub-id]
						 ;;[= [pv] t1-pv]
						 ;;[= [vn] vn]
						 [not [like [tense] "%part%"]]
						 [not [like [tense] "%masdar%"]]]
				     )))
		(dolist (t3 t3-list) ;; (print (list :t3 t3))
		  (destructuring-bind (id tense
					  root c-root pv vn gv sf caus-sf vv
					  tsch-class morph-type relation reduplication red-dir-pv
					  stem-type pr-st-ext part-pfx part-sfx passive-sfx
					  type-aorist type-obj-3-pfx type-aorist-3sg type-optative
					  nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
					  type-subj3-sfx type-subj2-pfx type-ev-sfx
					  style type-pr-st-ext paradigm-replacement) t3
		    (insert-records :into [verb-features]
				    :attributes (list [unique-id] [id] [sub-id] [tense]
						      [root] [c-root] [pv] [vn] [gv] [sf] [caus-sf] ;; [vv]
						      [tsch-class] [morph-type] [relation] [reduplication] [red-dir-pv]
						      [stem-type] [pr-st-ext] [part-pfx] [part-sfx] [passive-sfx]
						      [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
						      [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
						      [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
						      [style] [type-pr-st-ext] [paradigm-replacement]
						      [source] [author] [date])
				    :values (list (incf uid) id new-sub-id tense
						  root c-root pv vn gv sf caus-sf ;;vv
						  "T3" morph-type relation reduplication red-dir-pv
						  stem-type pr-st-ext part-pfx part-sfx passive-sfx
						  type-aorist type-obj-3-pfx type-aorist-3sg type-optative
						  nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
						  type-subj3-sfx type-subj2-pfx type-ev-sfx
						  style type-pr-st-ext paradigm-replacement "derived" "paul" date))
		    (when (eq t3 (car t3-list))
		      (insert-records :into [verb-translation]
				      :attributes `([id] [sub-id] [base-sub-id] [pv] [source] [author] [date])
				      :values (list id new-sub-id new-sub-id  t1-pv "derived" "paul" date))
		      (insert-records :into [xle-template]
				      :attributes `([id] [sub-id] [template] [author] [date])
				      :values (list id new-sub-id "V-TRANS-O-SUBJ-OBJ3" "derived" date)))))))))))))


#+test
(defun add-rp3 ()
  (with-transaction ()
    (let ((t1-list (select [id] [sub-id] [pv] [vn] :distinct t
			   :from [verb-features]
			   :distinct t
			   :where [and [= [tsch-class] "T1"]
				       ;;[= [vn] "abrageba"]
				       [= [morph-type] "active"]
				       [like [tense] "%aorist%"]
				       [< [id] 3824]]
			   :order-by `([id])))
	  (date (get-universal-time)))
      (dolist (t1 t1-list)
	#+test
	(print t1)
	(destructuring-bind (id sub-id t1-pv vn) t1
	  (let* ((t3 (car (select [sub-id] :distinct t
				  :from [verb-features]
				  :where [and [= [id] id]
					      [= [vn] vn]
					      [= [pv] t1-pv]
					      [= [tsch-class] "T3"]
					      [= [morph-type] "active"]
					      [like [tense] "%aorist%"]])))
		 (new-sub-id (1+ (car (select [max [sub-id]] :from [verb-translation] :where [= [id] id] :flatp t)))))
	    (unless t3
	      (format t "~d-~d-~a-~a~%"  id sub-id (or t1-pv "") vn)
	      (let ((uid (car (select [max [unique-id]] :from [verb-features] :flatp t)))
		    (t3-list (select [id] [tense]
				     [root] [c-root] [pv] [vn] [gv] [sf] [caus-sf] [vv]
				     [tsch-class] [morph-type] [relation] [reduplication] [red-dir-pv]
				     [stem-type] [pr-st-ext] [part-pfx] [part-sfx] [passive-sfx]
				     [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
				     [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
				     [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
				     [style] [type-pr-st-ext] [paradigm-replacement]
				     :distinct t
				     :from [verb-features]
				     :where [and [= [id] id]
						 [= [sub-id] sub-id]
						 ;;[= [pv] t1-pv]
						 ;;[= [vn] vn]
						 [not [like [tense] "%part%"]]
						 [not [like [tense] "%masdar%"]]]
				     )))
		(dolist (t3 t3-list) ;; (print (list :t3 t3))
		  (destructuring-bind (id tense
					  root c-root pv vn gv sf caus-sf vv
					  tsch-class morph-type relation reduplication red-dir-pv
					  stem-type pr-st-ext part-pfx part-sfx passive-sfx
					  type-aorist type-obj-3-pfx type-aorist-3sg type-optative
					  nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
					  type-subj3-sfx type-subj2-pfx type-ev-sfx
					  style type-pr-st-ext paradigm-replacement) t3
		    (insert-records :into [verb-features]
				    :attributes (list [unique-id] [id] [sub-id] [tense]
						      [root] [c-root] [pv] [vn] [gv] [sf] [caus-sf] ;; [vv]
						      [tsch-class] [morph-type] [relation] [reduplication] [red-dir-pv]
						      [stem-type] [pr-st-ext] [part-pfx] [part-sfx] [passive-sfx]
						      [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
						      [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
						      [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
						      [style] [type-pr-st-ext] [paradigm-replacement]
						      [source] [author] [date])
				    :values (list (incf uid) id new-sub-id tense
						  root c-root pv vn gv sf caus-sf ;;vv
						  "T3" morph-type relation reduplication red-dir-pv
						  stem-type pr-st-ext part-pfx part-sfx passive-sfx
						  type-aorist type-obj-3-pfx type-aorist-3sg type-optative
						  nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
						  type-subj3-sfx type-subj2-pfx type-ev-sfx
						  style type-pr-st-ext paradigm-replacement "derived" "paul" date))
		    (when (eq t3 (car t3-list))
		      (insert-records :into [verb-translation]
				      :attributes `([id] [sub-id] [base-sub-id] [pv] [source] [author] [date])
				      :values (list id new-sub-id new-sub-id  t1-pv "derived" "paul" date))
		      (insert-records :into [xle-template]
				      :attributes `([id] [sub-id] [template] [author] [date])
				      :values (list id new-sub-id "V-TRANS-O-SUBJ-OBJ3" "derived" date)))))))))))))

#+test
(clrhash *feature-cache*)
#+(or mysql sqlite)
(progn
  (clrhash fst::*paradigm-table*)
  (update-paradigm-table))

#+test
(with-transaction ()
  (dolist (table (list [xle-template]  [verb-translation]  [verb-features]))
    (delete-records :from table
		    :where [and [= [id] 3265]
				[= [sub-id] 63]])))

#+copy
(with-transaction ()
  (let ((kt-list (select [id] [sub-id] [pv] [vn] :distinct t
			 :from [verb-features]
			 :where [and [= [tsch-class] "KT"]
				     [like [tense] "%aorist%"]]
			 )))
    (dolist (kt kt-list)
      (destructuring-bind (id sub-id pv vn) kt
	(let* ((vn (subseq vn 0 (position #\space vn)))
	       (vn (string-trim "[]" (subseq vn 0 (position #\- vn)))))
	  (let* ((link-sub-id1
		  (car (select [sub-id] :distinct t
			       :from [verb-features]
			       :where [and [= [id] id]
					   [= [vn] vn]
					   [= [pv] pv]
					   [= [tsch-class] "T1"]
					   [like [tense] "%aorist%"]])))
		 (link-sub-id5
		  (unless link-sub-id1
		    (car (select [sub-id] :distinct t
				 :from [verb-features]
				 :where [and [= [id] id]
					     [= [vn] vn]
					     [= [pv] pv]
					     [= [tsch-class] "T5"]
					     [like [tense] "%aorist%"]]))))
		 (link-sub-id2
		  (unless (or link-sub-id1 link-sub-id5)
		    (car (select [sub-id] :distinct t
				 :from [verb-features]
				 :where [and [= [id] id]
					     [= [vn] vn]
					     [= [pv] pv]
					     [= [tsch-class] "T2"]
					     [like [tense] "%aorist%"]])))))
	    (when link-sub-id2
	      (update-records [verb-translation]
			      :attributes `([link-sub-id])
			      :values (list link-sub-id2)
			      :where [and [= [id] id]
					  [= [sub-id] sub-id]]))))))))

#+test
(print (select [id] [sub-id] [pv] [vn]
	       :from [verb-features]
	       :where [and [= [id] 3386]
			   [like [tsch-class] "T%"]
			   [like [tense] "%aorist%"]
			   [= [subj-pers] "3"]]
	       ))

#+test
(with-transaction ()
  (let ((uid (car (select [max [unique-id]] :from [verb-features] :flatp t)))
	(t3-list (select [id] [sub-id] [tense]
			 [root] [c-root] [pv] [vn] [gv] [sf] [caus-sf] [vv]
			 [tsch-class] [morph-type] [relation] [reduplication] [red-dir-pv]
			 [stem-type] [pr-st-ext] [part-pfx] [part-sfx] [passive-sfx]
			 [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
			 [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
			 [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
			 [style] [type-pr-st-ext] [paradigm-replacement] [source] [author] [date]
			 :distinct t
			 :from [verb-features]
			 :where [and [= [id] 3386]
				     [like [tsch-class] "T%"]
				     [like [tense] "%aorist%"]
				     [= [subj-pers] "3"]]
			 )))
    (dolist (t3 t3-list) ;; (print (list :t3 t3))
      (destructuring-bind (id sub-id tense
			      root c-root pv vn gv sf caus-sf vv
			      tsch-class morph-type relation reduplication red-dir-pv
			      stem-type pr-st-ext part-pfx part-sfx passive-sfx
			      type-aorist type-obj-3-pfx type-aorist-3sg type-optative
			      nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
			      type-subj3-sfx type-subj2-pfx type-ev-sfx
			      style type-pr-st-ext paradigm-replacement source author date) t3
	(insert-records :into [verb-features]
			:attributes (list [unique-id] [id] [sub-id] [tense]
					  [root] [c-root] [pv] [vn] [gv] [sf] [caus-sf]	[vv]
					  [tsch-class] [morph-type] [relation] [reduplication] [red-dir-pv]
					  [stem-type] [pr-st-ext] [part-pfx] [part-sfx] [passive-sfx]
					  [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
					  [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
					  [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
					  [style] [type-pr-st-ext] [paradigm-replacement]
					  [source] [author] [date])
			:values (list (incf uid) id sub-id tense
				      "cqv" c-root pv vn gv sf caus-sf vv
				      tsch-class morph-type relation reduplication red-dir-pv
				      stem-type pr-st-ext part-pfx part-sfx passive-sfx
				      type-aorist type-obj-3-pfx type-aorist-3sg type-optative
				      nasal-infix "3" "pl" obj-pers type-subj12-sfx
				      type-subj3-sfx type-subj2-pfx type-ev-sfx
				      style type-pr-st-ext paradigm-replacement source author date))))))

#+test
(with-transaction ()
  (update-records [verb-features]
		  :attributes '([subj-num])
		  :values (list "sg")
		  :where [and [= [id] 3386]
			      [like [tsch-class] "T%"]
			      [like [tense] "%aorist%"]
			      [= [subj-pers] "3"]]
		  ))

#+test
(with-transaction ()
  (update-records [morph participle]
		  :attributes '([aspect])
		  :values (list "PERF")
		  :where [= [aspect] "PF"]
		  ))

#+test
(with-transaction ()
  (update-records [verb-features]
		  :attributes '([style])
		  :values (list "bracket")
		  :where [and [= [unique-id] 25106]]
		  ))

#+test
(with-transaction ()
  (update-records [morph verb-features]
		  :attributes '([root])
		  :values (list "ჴმ")
		  :where [and [= [id] 3659]]
		  ))

#+test
(with-transaction ()
  (update-records [morph verb-features]
		  :attributes '([root])
		  :values (list "ყuარ")
		  :where [and [= [id] 2752]]
		  ))

;; u -> ვ/უ for verbs; still to be done
#+test
(let ((count 0))
  (do-query ((root)
	     [select [root] :from [morph verb-features]
		     :flatp t
		     :distinct t
		     :where [like [root] "%ვ%"]])
    (let ((v-pos (position #\ვ root)))
      (when (and (> v-pos 0)
		 (not (find (char root (1- v-pos)) "აეიოუ"))
		 (or (= v-pos (1- (length root)))
		     (not (find (char root (1+ v-pos)) "ი"))))
	(incf count)
	(write-line root))))
  (print count))
  
#+test
(pprint (select [verb-paradigm id] [verb-paradigm sub-id] [c-root] [tsch-class] [template]
		:from [morph verb-paradigm]
		:left-join [morph xle-template]
		:on [and [= [verb-paradigm id] [xle-template id]]
			 [= [verb-paradigm sub-id] [xle-template sub-id]]]
		:where [like [tsch-class] "RP7%"]))

#+test
(with-database-connection ()
  (pprint (select [verb-paradigm id] ;; [verb-paradigm sub-id]
		  [verb-paradigm c-root] [verb-paradigm tsch-class] [pres morph-type] [fut morph-type]
		  :from [morph verb-paradigm]
		  :distinct t
		  :left-join [morph verb-features :as pres]
		  :on [and [= [verb-paradigm id] [pres id]]
			   [= [verb-paradigm features-sub-id] [pres sub-id]]
			   [like [pres tense] "%|present|%"]]
		  :left-join [morph verb-features :as fut]
		  :on [and [= [verb-paradigm id] [fut id]]
			   [= [verb-paradigm features-sub-id] [fut sub-id]]
			   [like [fut tense] "%|future|%"]]
		  :where [<> [pres morph-type] [fut morph-type]]
		  :order-by [verb-paradigm id])))

;; do the same for RM, IV
#+test
(with-database-connection ()
  (pprint (select [verb-paradigm id] ;; [verb-paradigm sub-id]
		  [verb-paradigm c-root]
		  [pf-pv]
		  [verb-paradigm tsch-class] [pres morph-type] [fut morph-type]
		  :from [morph verb-paradigm]
		  :distinct t
		  :left-join [morph verb-features :as pres]
		  :on [and [= [verb-paradigm id] [pres id]]
			   [= [verb-paradigm features-sub-id] [pres sub-id]]
			   [like [pres tense] "%|present|%"]]
		  :left-join [morph verb-features :as fut]
		  :on [and [= [verb-paradigm id] [fut id]]
			   [= [verb-paradigm features-sub-id] [fut sub-id]]
			   [like [fut tense] "%|future|%"]
			   [= [fut passive-sfx] "დ+ებ"]]
		  :where [and [<> [pres morph-type] [fut morph-type]]
			      [= [impf-pv] "-"]]
		  :order-by [verb-paradigm id])))

#+test
(with-database-connection ()
  (do-query ((id c-root impf-pv pf-pv tsch pres-morph fut-morph)
	     [select [verb-paradigm id] ;; [verb-paradigm sub-id]
		     [verb-paradigm c-root]
		     [impf-pv] [pf-pv]
		     [verb-paradigm tsch-class] [pres morph-type] [fut morph-type]
		     :from [morph verb-paradigm]
		     :distinct t
		     :left-join [morph verb-features :as pres]
		     :on [and [= [verb-paradigm id] [pres id]]
			      [= [verb-paradigm features-sub-id] [pres sub-id]]
			      [like [pres tense] "%|present|%"]]
		     :left-join [morph verb-features :as fut]
		     :on [and [= [verb-paradigm id] [fut id]]
			      [= [verb-paradigm features-sub-id] [fut sub-id]]
			      [like [fut tense] "%|future|%"]]
		     ;; :where
		     #+ignore[and [<> [pres morph-type] [fut morph-type]]
				  ;; [= [impf-pv] "-"]
				  ]
		     :order-by [verb-paradigm id]])
    (print (list  c-root impf-pv pf-pv tsch pres-morph fut-morph
		  (calculate-class tsch pres-morph fut-morph)
		  ))))

#+test
(with-database-connection ()
  (let ((rows (select [verb-paradigm id]
		      [verb-paradigm sub-id]
		      [verb-paradigm c-root]
		      [impf-pv] [pf-pv]
		      [verb-paradigm tsch-class]
		      [pres morph-type]
		      [fut morph-type]
		      [fut passive-sfx]
		      :from [morph verb-paradigm]
		      :distinct t
		      :left-join [morph verb-features :as pres]
		      :on [and [= [verb-paradigm id] [pres id]]
			       [= [verb-paradigm features-sub-id] [pres sub-id]]
			       [like [pres tense] "%|present|%"]]
		      :left-join [morph verb-features :as fut]
		      :on [and [= [verb-paradigm id] [fut id]]
			       [= [verb-paradigm features-sub-id] [fut sub-id]]
			       [like [fut tense] "%|future|%"]]
		      ;; :where
		      #+ignore[and [<> [pres morph-type] [fut morph-type]] ;; [= [impf-pv] "-"]
				   ]
		      :order-by [verb-paradigm id])))
    (with-transaction ()
      (loop for (id sub-id c-root impf-pv pf-pv tsch pres-morph fut-morph pass-sfx) in rows
	 do
	   #+debug
	   (print (list id sub-id c-root impf-pv pf-pv tsch pres-morph fut-morph pass-sfx
			(calculate-class tsch pres-morph fut-morph pass-sfx)))
	   (update-records [morph verb-paradigm]
			    :av-pairs `(([class] ,(calculate-class tsch pres-morph fut-morph pass-sfx)))
			    :where [and [= [id] ?id]
					[= [sub-id] ?sub-id]]
			    )))))


(defun calculate-class (tsch-class pres-morph fut-morph pass-sfx)
  (labels ((medium ()
	     (cond ((equal fut-morph "active")
		    "MedAct")
		   ((equal fut-morph "passive")
		    "MedPass")
		   ((equal pres-morph "active")
		    "MedAct")
		   ((equal pres-morph "passive")
		    "MedPass"))))
    (cond ((equal pass-sfx "დ+ებ")
	   "Pass")
	  (t
	   (ecase (char tsch-class 0)
	     (#\T "Act")
	     (#\K "Caus") ;; perhaps not for all?
	     (#\P "Pass")
	     (#\R (if (eq (char tsch-class 1) #\P)
		      "Pass"
		      (medium)))
	     (#\M (medium))
	     (#\I (medium))
	     (#\Z "StatPass"))))))

#+test
(with-database-connection ()
  (let ((rows (select [verb-paradigm id] [verb-paradigm sub-id]
		      :from [morph verb-paradigm]
		      :distinct t
		      :left-join [morph verb-features :as pres]
		      :on [and [= [verb-paradigm id] [pres id]]
			       [= [verb-paradigm features-sub-id] [pres sub-id]]
			       [like [pres tense] "%|present|%"]]
		      :left-join [morph verb-features :as fut]
		      :on [and [= [verb-paradigm id] [fut id]]
			       [= [verb-paradigm features-sub-id] [fut sub-id]]
			       [like [fut tense] "%|future|%"]
			       [= [fut passive-sfx] "დ+ებ"]]
		      :where [and [<> [pres morph-type] [fut morph-type]]
				  [= [impf-pv] "-"]]
		      :order-by [verb-paradigm id])))
    (with-transaction ()
      (loop for (id sub-id) in rows
	   do (delete-records :from [morph verb-features]
			      :where [and [= [id] ?id]
					  [= [sub-id] ?sub-id]
					  [like [tense] "%|present|%"]])
	   ))))

;; check VN/stem consistency
#+test
(defparameter *vn-lemma-table* (dat:make-string-tree))

#+test
(u:with-file-lines (vn "projects:georgian-morph;regex;vn-lemma-list.txt")
  (destructuring-bind (lemma root) (u:split vn #\/)
    (let ((pv-pos (position #\- lemma :end (- (length lemma) 3))))
      (when pv-pos (setf lemma (subseq lemma (1+ pv-pos)))))
    (pushnew lemma (dat:string-tree-get *vn-lemma-table* root) :test #'string=)))

#+test
(dat:do-string-tree (root lemma-list *vn-lemma-table*)
  (when (find-if (lambda (lemma) (not (search root lemma))) lemma-list)
    (format t "~a: ~{~a~^, ~}~%" root lemma-list)))
		    

;; participle decl.

#+test ;; has to be corrected
(with-database-connection ()
  (print (select [code] [participle stem]
		 :from [morph participle]
		 :where [and [= [type] "NEGATIVE-PART"]
			     [like [stem] "%აველ"]] )))

#+test ;; has to be corrected
(with-database-connection ()
  (do-query ((code stem)
	     [select [code] [stem]
		     :from [morph participle]
		     :where [= [type] "NEGATIVE-PART"]
		     ;;:limit 40
		     ])
    (let ((codes (select [code] :distinct t :flatp t
			 :from [morph noun-features]
			 :where [like [stem] (u:concat "%" (string-trim "*" stem))])))
      (when (or (> (length codes) 1)
		(not (find code codes :test #'string=)))
	(print (list code stem codes)))
      ))) 

#+test
(with-database-connection ()
  (do-query ((root)
	     [select [root]
		     :from [morph verb-features]
		     :where [like [root] "%ვ%"]
		     :distinct t
		     :order-by [root]
		     ])
    (unless (or (char= (char root 0) #\ვ)
		(search "ოვ" root)
		(search "ავ" root)
		(search "ევ" root)
		(search "უვ" root)
		(search "ივ" root)
		(search "-ვ" root)
		(equal (search "ვრ" root) (- (length root) 2))
		(equal (search "ვლ" root) (- (length root) 2))
		(equal (search "ვნ" root) (- (length root) 2)))
      (print root))))

;; ვ -> u/უ
#+test
(with-transactionxx ()
  (do-query ((id sub-id vn impf-vn pf-vn root)
	     [select [verbal-noun id] [verbal-noun sub-id]
		     [verbal-noun vn] [verbal-noun impf-vn] [verbal-noun pf-vn]
		     [root]
		     :from [morph verbal-noun]
		     :left-join [morph verb-features]
		     :on [and [= [verb-features id] [verbal-noun id]]
			      [= [verb-features sub-id] [verbal-noun sub-id]]]
		     :where [like [verb-features root] "%ვ%"]
		     :distinct t
		     :order-by [root]
		     ])
    (unless (or (char= (char root 0) #\ვ)
		(search "ოვ" root)
		(search "ავ" root)
		(search "ევ" root)
		(search "უვ" root)
		(search "ივ" root)
		(search "-ვ" root)
		(equal (search "ვრ" root) (- (length root) 2))
		(equal (search "ვლ" root) (- (length root) 2))
		(equal (search "ვნ" root) (- (length root) 2)))
      (print (list id root vn (search root vn)))
      (let ((u-root (substitute #\u #\ვ root)))
	(update-records [morph verb-features]
			:av-pairs `(([root] ,u-root))
			:where [and [= [verb-features id] ?id]
				    [= [verb-features sub-id] ?sub-id]
				    [= [root] ?root]])
	(when (equal (search root vn) 0)
	  (let ((u-vn (u:concat u-root (subseq vn (length root)))))
	    (print (list root u-vn))
	    (update-records [morph verbal-noun]
			    :av-pairs `(([vn] ,u-vn))
			    :where [and [= [verbal-noun id] ?id]
					[= [verbal-noun sub-id] ?sub-id]
					[= [vn] ?vn]])))
	(when (and impf-vn (equal (search root impf-vn) 0)
	  (let ((u-vn (u:concat u-root (subseq impf-vn (length root)))))
	    (print (list root u-vn))
	    (update-records [morph verbal-noun]
			    :av-pairs `(([impf-vn] ,u-vn))
			    :where [and [= [verbal-noun id] ?id]
					[= [verbal-noun sub-id] ?sub-id]
					[= [impf-vn] ?impf-vn]]))))
	(when (and pf-vn (equal (search root pf-vn) 0)
	  (let ((u-vn (u:concat u-root (subseq pf-vn (length root)))))
	    (print (list root u-vn))
	    (update-records [morph verbal-noun]
			    :av-pairs `(([pf-vn] ,u-vn))
			    :where [and [= [verbal-noun id] ?id]
					[= [verbal-noun sub-id] ?sub-id]
					[= [pf-vn] ?pf-vn]]))))))))

#+test
(with-transactionxx ()
  (do-query ((id sub-id root p-root stem)
	     [select [verb-features id] [verb-features sub-id]
		     [verb-features root] [participle root] [stem]
		     :from [morph participle]
		     :left-join [morph verb-features]
		     :on [and [= [verb-features id] [participle id]]
			      [= [verb-features sub-id] [participle sub-id]]]
		     :where [like [verb-features root] "%u%"]
		     :distinct t
		     :order-by [verb-features root]
		     ])
    (let* ((v-root (substitute #\ვ #\u root))
	   (pos (search v-root stem)))
      ;;(when p-root (print p-root))
      ;;(print (list id sub-id root p-root stem (search v-root stem)  (search v-root p-root)))
      (when pos
	(let ((u-stem (u:concat (subseq stem 0 pos)
				root
				(subseq stem (+ pos (length root)))))
	      (u-root (when (equal v-root p-root) root)))
	  (unless (and (char= (char root (1- (length root))) #\u)
		       (> (length stem) (+ pos (length root)))
		       (find (char stem (+ pos (length root))) "უო"))
	    (print (list id sub-id root stem u-stem))
	    (update-records [morph participle]
			    :av-pairs `(([stem] ,u-stem))
			    :where [and [= [id] ?id]
					[= [sub-id] ?sub-id]
					[= [stem] ?stem]])))))))

#+test
(with-open-file (stream "/ssd/data/verb-features-examples.tsv"
                        :direction :output :if-exists :supersede)
  (format stream "~{~a~^	~}~%"
          (list "unique_id" "id" "sub_id" "root" "c_root" "tense" "pv" "vn" "gv" "sf" "caus_sf" "vv"
                "tsch_class" "morph_type" "relation" "reduplication" "red_dir_pv" "stem_type" "pr_st_ext"
                "part_pfx" "part_sfx" "passive_sfx" "nasal_infix" "type_aorist" "type_obj_3_pfx" 
                "type_aorist_3sg" "type_optative" "subj_pers" "subj_num" "obj_pers" "type_subj12_sfx" 
                "type_subj3_sfx" "type_subj2_pfx" "type_ev_sfx" "style"
                "type_pr_st_ext" "paradigm_replacement" "deleted" "lang"))
  (do-query ((unique-id id sub-id root c-root tense pv vn gv sf caus-sf vv
                        tsch-class morph-type relation reduplication red-dir-pv stem-type pr-st-ext
                        part-pfx part-sfx passive-sfx nasal-infix type-aorist type-obj-3-pfx
                        type-aorist-3sg type-optative subj-pers subj-num obj-pers type-subj12-sfx
                        type-subj3-sfx type-subj2-pfx type-ev-sfx style
                        type-pr-st-ext paradigm-replacement lang)
             [select [unique_id] [id] [sub_id] [root] [c_root] [tense] [pv] [vn] [gv] [sf] [caus_sf] [vv]
                     [tsch_class] [morph_type] [relation] [reduplication] [red_dir_pv] [stem_type] [pr_st_ext]
                     [part_pfx] [part_sfx] [passive_sfx] [nasal_infix] [type_aorist] [type_obj_3_pfx]
                     [type_aorist_3sg] [type_optative] [subj_pers] [subj_num] [obj_pers] [type_subj12_sfx]
                     [type_subj3_sfx] [type_subj2_pfx] [type_ev_sfx] [style]
                     [type_pr_st_ext] [paradigm_replacement] [lang]
                     :from [morph verb-features]
                     :where [in [id] '(3260 904 2073 2120 2764 1116 1628 3581)]
                     :order-by '([id] [sub-id])])
    (format stream "~{~a~^	~}~%"
            (mapcar (lambda (val)
                      (or val ""))
                    (list unique-id id sub-id root c-root tense pv vn gv sf caus-sf vv
                          tsch-class morph-type relation reduplication red-dir-pv stem-type pr-st-ext
                          part-pfx part-sfx passive-sfx nasal-infix type-aorist type-obj-3-pfx
                          type-aorist-3sg type-optative subj-pers subj-num obj-pers type-subj12-sfx
                          type-subj3-sfx type-subj2-pfx type-ev-sfx style
                          type-pr-st-ext paradigm-replacement lang)))))

("id" "sub_id" "c_root" "vn" "impf_vn" "pf_vn" "tsch_class" "features_sub_id" "link_sub_id" "base_sub_id" "participle_sub_id" "pv" "pf_pv" "impf_pv" "dir_pv_p" "red_dir_pv" "comment" "author" "source" "derived_type" "date" "accepted" "pf_12_pv" "no_preverbless_aor" "class" "disabled")

#+test
(with-open-file (stream "/centos6/data/verb-paradigm-examples.tsv"
                        :direction :output :if-exists :supersede)
  (format stream "~{~a~^	~}~%"
          '("id" "sub_id" "c_root" "vn" "impf_vn" "pf_vn" "tsch_class" "features_sub_id" "link_sub_id" "base_sub_id" "participle_sub_id" "pv" "pf_pv" "impf_pv" "dir_pv_p" "red_dir_pv" "comment" "author" "source" "derived_type" "date" "accepted" "pf_12_pv" "no_preverbless_aor" "class" "disabled"))
  (do-query ((&rest rest)
             [select [*]
                     :from [morph verb-paradigm]
                     :where [in [id] '(3260 904 2073 2120 2764 1116 1628 3581)]
                     :order-by '([id] [sub-id])])
    (format stream "~{~a~^	~}~%"
            (mapcar (lambda (val)
                      (or val ""))
                    rest))))
#+test
(debug (select [*] :from [morph verb-features]
               :where [= [id] 3260]
               :limit 1))

#+test
(debug (select [*] :from [morph verb-paradigm]
               :where [= [id] 3260]
               :limit 1))

#+test
(debug (select [*] :from [morph verbal-noun]
               :where [= [id] 3260]
               :limit 1))

;; ("id" "sub_id" "vn" "impf_vn" "pf_vn" "variety" "date")

#+test
(debug (select [*] :from [morph participle]
               :where [= [id] 3260]
               :limit 1))

;; ("id" "sub_id" "type" "stem" "code" "variety" "aspect" "date" "attested" "accepted" "main_form" "root" "wrong" "restriction")

:eof
