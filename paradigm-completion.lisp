;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: XLE; Base: 10 -*-

;; paul.meurer@aksis.uib.no
;; Aksis, Unifob, University of Bergen.
;; http://www.aksis.uib.no/

(in-package :fst)


#+test
(remhash "demoraliz" *feature-cache*)

#+test
(with-transaction ()
  (delete-recordsz :from [verb-features] :where [and [= [id] 471] [= [sub-id] 2]])
  (delete-recordsz :from [verb-translation] :where [and [= [id] 471] [= [sub-id] 2]]))

#+test
(with-transaction ()
  (delete-records :from [verb-features] :where [= [derived-type] "i-passive"])
  (delete-records :from [verb-translation] :where [= [derived-type] "i-passive"]))

;;(update-paradigm-table :id "471-2")

(defun complete-paradigm (&key filter root-filter aor-filter missing-filter id sub-id testp derived-type)
  "A paradigm is added to all super-paradigms matching filter and not having a paradigm matching missing-filter. The new paradigm is modeled according to id+sub-id."
  (let ((replacement-features (select [tense] [pv] [gv] [sf] [caus-sf] [vv]
				      [tsch-class] [morph-type] [relation]
				      [stem-type] [pr-st-ext] [pf-sfx] [pp-pfx] [pp-sfx] [passive-sfx]
				      [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
				      [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
				      [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
				      [type-optative-3pl-en] [type-pr-st-ext]
				      :from [verb-features]
				      :where [and [= [id] id]
						  [= [sub-id] sub-id]
						  [not [= [c-root] ""]]]))
	(count 0)
	(date (get-universal-time))
	(feature-list (select [id] [sub-id] [pv] [root] [c-root] [vn] [reduplication] [red-dir-pv]
			      :distinct t
			      :from [verb-features]
			      :where filter))
	(unique-id (car (select [max [unique-id]] :flatp t :from [verb-features]))))
    (dolist (features feature-list)
      (destructuring-bind (id sub-id pv root c-root vn reduplication red-dir-pv) features
	(when (and (null (select [id]
				 :from [verb-features]
				 :where (funcall missing-filter id pv vn)))
		   (or (null root-filter)
		       (funcall root-filter root)))
	  (let ((aor-roots (when aor-filter (select [root] nil ;;[subj-pers]
						    :flatp nil
						    :distinct t
						    :from [verb-features]
						    :where (funcall aor-filter id sub-id)))))
	    (incf count)
	    (print (list id pv vn root aor-roots ))
	    (unless nil	;;testp
	      (let ((max-sub-id (max (car (select [max [sub-id]]
						  :from [verb-features]
						  :flatp t
						  :where [= [id] id]))
				     (car (select [max [sub-id]]
						  :from [verb-translation]
						  :flatp t
						  :where [= [id] id])))))
		;;(print (list :id id :new-sub-id (1+ max-sub-id)))
		(with-transaction ()
		  (unless testp
		    (insert-records :into [verb-translation]
				    :attributes (list [id] [sub-id] [source] [derived-type] [author] [date])
				    :values (list id (1+ max-sub-id)  "derived" derived-type "paul" date)))
		  (dolist (r-feature-list replacement-features)
		    (destructuring-bind (tense r-pv gv sf caus-sf vv
					       tsch-class morph-type relation
					       stem-type pr-st-ext pf-sfx pp-pfx pp-sfx passive-sfx
					       type-aorist type-obj-3-pfx type-aorist-3sg type-optative
					       nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
					       type-subj3-sfx type-subj2-pfx type-ev-sfx
					       type-optative-3pl-en type-pr-st-ext) r-feature-list
		      (when (find root '("uSno" "uTo" "lenJo" "Tu" "sazogado" "bubko" "ru") :test #'string=)
			(setf root (concat root "v"))) 
		      (dolist (root+pers (if (and aor-roots (or (search "|aorist|" tense)
								(search "|optative|" tense)))
					     aor-roots
					     (list (list root nil)))) 
			(incf unique-id)
			(destructuring-bind (root subj-pers) root+pers
			  (remhash root *feature-cache*)
			  (let* ((new-features (list unique-id
						     id
						     (1+ max-sub-id)
						     root c-root
						     tense (when (and r-pv (string/= r-pv "")) pv) vn gv sf caus-sf vv
						     tsch-class morph-type relation reduplication red-dir-pv
						     stem-type pr-st-ext pf-sfx pp-pfx pp-sfx passive-sfx
						     type-aorist type-obj-3-pfx type-aorist-3sg type-optative
						     nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
						     type-subj3-sfx type-subj2-pfx type-ev-sfx
						     type-optative-3pl-en type-pr-st-ext "derived" derived-type "paul" date)))
			    (if testp
				nil;(print new-features)
				(insert-records :into [verb-features]
						:attributes (list [unique-id] [id] [sub-id] [root] [c-root]
								  [tense] [pv] [vn] [gv] [sf] [caus-sf] [vv]
								  [tsch-class] [morph-type] [relation] [reduplication] [red-dir-pv]
								  [stem-type] [pr-st-ext] [pf-sfx] [pp-pfx] [pp-sfx] [passive-sfx]
								  [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
								  [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
								  [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
								  [type-optative-3pl-en] [type-pr-st-ext] [source] [derived-type] [author] [date])
						:values (print new-features))))))
		      )))
		(unless testp
		  (update-paradigm-table :id (format nil "~d-~d" id (1+ max-sub-id))))))))))
    (print count)))

;; 669
;; Passives of transitives with a-eb
;; should only be done for vocalic root?
#+test
(let ((filter [and [= [tsch-class] "T1"]
		   [like [tense] "%|future|%"]
		   [= [sf] "eb"]
		   [= [vv] "a"]])
      (root-filter (lambda (root) (and (not (find root '("pir" "Par" "qen" "tak" "ubr" "pirob") :test #'equal))
				       (find-if (lambda (v) (find v root)) "aeiou"))))
      (missing-filter (lambda (id pv vn)
			(if pv
			    [and [= [id] id] ;; missing-filter
				 [or [and [in [tsch-class] '("P1" "P2")]
					  [= [pv] pv]
					  [= [vn] vn]]
				     [and [in [tsch-class] '("T1")]
					  ;;[= [pv] pv]
					  [null [vv]]
					  [like [tense] "%|future|%"]
					  [= [vn] vn]]
				     [in [tsch-class] '("MV")]]]
			    [and [= [id] id] ;; missing-filter
				 [or [and [in [tsch-class] '("P1" "P2")]
					  [null [pv]]
					  [= [vn] vn]]
				     [and [in [tsch-class] '("T1")]
					  ;;[= [pv] pv]
					  [null [vv]]
					  [like [tense] "%|future|%"]
					  [= [vn] vn]]
				     [in [tsch-class] '("MV")]]])))
      (id 904)
      (sub-id 18))
  (complete-paradigm :filter filter :root-filter root-filter :missing-filter missing-filter :id id :sub-id sub-id :testp t
		     :derived-type "a-eb-passive"))

(defun complete-i-paradigm (&key filter root-filter aor-filter missing-filter id sub-id testp derived-type)
  "A paradigm is added to all super-paradigms matching filter and not having a paradigm matching missing-filter. The new paradigm is modeled according to id+sub-id."
  (let ((replacement-features (select [root] [tense] [pv] [gv] [sf] [caus-sf] [vv]
				      [tsch-class] [morph-type] [relation]
				      [stem-type] [pr-st-ext] [pf-sfx] [pp-pfx] [pp-sfx] [passive-sfx]
				      [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
				      [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
				      [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
				      [type-optative-3pl-en] [type-pr-st-ext]
				      :from [verb-features]
				      :where [and [= [id] id]
						  [= [sub-id] sub-id]
						  [not [like [tense] "%|aorist|%"]]
						  [not [= [c-root] ""]]]))
	(count 0)
	(date (get-universal-time))
	(feature-list (select [id] [sub-id] [pv] [root] [c-root] [vn] [reduplication] [red-dir-pv]
			      :distinct t
			      :from [verb-features]
			      :where filter))
	(unique-id (car (select [max [unique-id]] :flatp t :from [verb-features]))))
    (dolist (features feature-list)
      (destructuring-bind (id sub-id pv root c-root vn reduplication red-dir-pv) features
	(when (and (null (select [id]
				 :from [verb-features]
				 :where (funcall missing-filter id pv vn)))
		   (or (null root-filter)
		       (funcall root-filter root)))
	  (let ((aor/opt-features (when aor-filter (select [root] [tense] [pv] [gv] [sf] [caus-sf] [vv]
							   [tsch-class] [morph-type] [relation]
							   [stem-type] [pr-st-ext] [pf-sfx] [pp-pfx] [pp-sfx] [passive-sfx]
							   [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
							   [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
							   [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
							   [type-optative-3pl-en] [type-pr-st-ext]
							   :flatp nil
							   :distinct t
							   :from [verb-features]
							   :where (funcall aor-filter id sub-id)))))
	    (incf count)
	    ;;(print (list id pv vn root aor-roots ))
	    (unless nil	;;testp
	      (let ((max-sub-id (max (car (select [max [sub-id]]
						  :from [verb-features]
						  :flatp t
						  :where [= [id] id]))
				     (car (select [max [sub-id]]
						  :from [verb-translation]
						  :flatp t
						  :where [= [id] id])))))
		;;(print (list :id id :new-sub-id (1+ max-sub-id)))
		(with-transaction ()
		  (unless testp
		    (insert-records :into [verb-translation]
				    :attributes (list [id] [sub-id] [source] [derived-type] [author] [date])
				    :values (list id (1+ max-sub-id)  "derived" derived-type "paul" date)))
		  (dolist (r-feature-list (append replacement-features aor/opt-features))
		    (destructuring-bind (a-root tense r-pv gv sf caus-sf vv
						tsch-class morph-type relation
						stem-type pr-st-ext pf-sfx pp-pfx pp-sfx passive-sfx
						type-aorist type-obj-3-pfx type-aorist-3sg type-optative
						nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
						type-subj3-sfx type-subj2-pfx type-ev-sfx
						type-optative-3pl-en type-pr-st-ext) r-feature-list
		      ;;(Print (split tense #\| nil nil t))
		      (if (find r-feature-list replacement-features)
			  (setf tense (format nil "|~{~a|~}" (remove-if (lambda (tns)
									  (find tns '("aorist" "optative")
										:test #'string=))
									(split tense #\| nil nil t))))
			  (setf tense (format nil "|~{~a|~}" (remove-if (lambda (tns)
									  (not (find tns '("aorist" "optative")
										     :test #'string=)))
									(split tense #\| nil nil t)))))
		      (incf unique-id)
		      ;;(print tense)
		      (unless testp (remhash root *feature-cache*))
		      (let* ((new-features (list unique-id
						 id
						 (1+ max-sub-id)
						 (if (or (search "aorist" tense)
							 (search "optative" tense))
						     a-root root)
						 c-root
						 tense (when (and r-pv (string/= r-pv "")) pv) vn gv sf caus-sf vv
						 "P1" "passive" relation reduplication red-dir-pv
						 stem-type pr-st-ext pf-sfx pp-pfx pp-sfx
						 (if (or (search "aorist" tense)
							 (search "optative" tense))
						     "-" passive-sfx)
						 type-aorist type-obj-3-pfx type-aorist-3sg type-optative
						 nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
						 type-subj3-sfx type-subj2-pfx type-ev-sfx
						 type-optative-3pl-en type-pr-st-ext "derived" derived-type "paul" date)))
			(if testp
			    (print new-features)
			    (insert-records
			     :into [verb-features]
			     :attributes (list [unique-id] [id] [sub-id] [root] [c-root]
					       [tense] [pv] [vn] [gv] [sf] [caus-sf] [vv]
					       [tsch-class] [morph-type] [relation] [reduplication] [red-dir-pv]
					       [stem-type] [pr-st-ext] [pf-sfx] [pp-pfx] [pp-sfx] [passive-sfx]
					       [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
					       [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
					       [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
					       [type-optative-3pl-en] [type-pr-st-ext] [source]
					       [derived-type] [author] [date])
			     :values (print new-features))))
		      )))
		(unless testp
		  (update-paradigm-table :id (format nil "~d-~d" id (1+ max-sub-id))))))))))
    (print count)))

;; 259
;; Passives of transitives with i-
#+test
(let ((filter [and [= [tsch-class] "T1"]
		   [like [tense] "%|future|%"]
		   [= [sf] "i"]
		   [null [vv]]
		   ])
      (aor-filter (lambda (id sub-id)
		    [and [= [id] id]
			 [= [sub-id] sub-id]
			 [or [like [tense] "%|aorist|%"]
			     [like [tense] "%|optative|%"]]
			 ]))
      (missing-filter (lambda (id pv vn)
			(if pv
			    [and [= [id] id]
				 [or [and [in [tsch-class] '("P1" "P2" "P3")]
					  [= [pv] pv]
					  [= [vn] vn]]
				     [in [tsch-class] '("MV")]]]
			    [and [= [id] id]
				 [or [and [in [tsch-class] '("P1" "P2" "P3")]
					  [null [pv]]
					  [= [vn] vn]]
				     [in [tsch-class] '("MV")]]])))
      (id 3103)
      (sub-id 34))
  (complete-i-paradigm :filter filter
		       ;;:root-filter root-filter
		       :aor-filter aor-filter
		       :missing-filter missing-filter
		       :id id
		       :sub-id sub-id
		       :testp nil
		       :derived-type "i-passive")
  )

(defun complete-caus-paradigm (&key filter root-filter missing-filter id sub-id testp derived-type)
  "A paradigm is added to all super-paradigms matching filter and not having a paradigm matching missing-filter. The new paradigm is modeled according to id+sub-id."
  (let ((replacement-features (select [tense] [pv] [gv] [caus-sf] [vv]
				      [tsch-class] [morph-type] [relation]
				      [stem-type] [pr-st-ext] [pf-sfx] [pp-pfx] [pp-sfx] [passive-sfx]
				      [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
				      [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
				      [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
				      [type-optative-3pl-en] [type-pr-st-ext]
				      :from [verb-features]
				      :where [and [= [id] id]
						  [= [sub-id] sub-id]
						  [not [= [c-root] ""]]]))
	(count 0)
	(date (get-universal-time))
	(feature-list (select [id] [pv] [root] [c-root] [vn] [sf] [reduplication] [red-dir-pv]
			      :distinct t
			      :from [verb-features]
			      :where filter))
	(unique-id (car (select [max [unique-id]] :flatp t :from [verb-features]))))
    (dolist (features feature-list)
      (destructuring-bind (id pv root c-root vn sf reduplication red-dir-pv) features
	(when (and (null (select [id]
				 :from [verb-features]
				 :where (funcall missing-filter id pv vn)))
		   (or (null root-filter)
		       (funcall root-filter root)))
	  (incf count)
	  (print (list id pv vn root))
	  (let ((max-sub-id (max (car (select [max [sub-id]]
					      :from [verb-features]
					      :flatp t
					      :where [= [id] id]))
				 (car (select [max [sub-id]]
					      :from [verb-translation]
					      :flatp t
					      :where [= [id] id])))))
	    (print (list :id id :new-sub-id (1+ max-sub-id)))
	    (with-transaction ()
	      (unless testp
		(insert-records :into [verb-translation]
				:attributes (list [id] [sub-id] [source] [derived-type] [author] [date])
				:values (list id (1+ max-sub-id)  "derived" derived-type "paul" date)))
	      (dolist (r-feature-list replacement-features)
		(destructuring-bind (tense r-pv gv caus-sf vv
					   tsch-class morph-type relation
					   stem-type pr-st-ext pf-sfx pp-pfx pp-sfx passive-sfx
					   type-aorist type-obj-3-pfx type-aorist-3sg type-optative
					   nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
					   type-subj3-sfx type-subj2-pfx type-ev-sfx
					   type-optative-3pl-en type-pr-st-ext) r-feature-list
		  (incf unique-id)
		  (remhash root *feature-cache*)
		  (let* ((new-features (list unique-id
					     id
					     (1+ max-sub-id)
					     (Print (cond ((and (equal sf "av")
								(not (find-if (lambda (v) (find v root)) "aeiou"))
								(find (char root (1- (length root))) "rln"))
							   (concat (subseq root 0 (1- (length root))) "v" (string (char root (1- (length root))))))
							  (t
							   root)))
					     c-root
					     tense
					     (when (and r-pv (string/= r-pv "")) pv)
					     (concat vn "-KT") gv
					     (Print (cond ((equal sf "i")
						    nil)
						   ((equal sf "av")
						    (cond ((find-if (lambda (v) (find v root)) "aeiou")
							   "v")
							  ((char= (char root (1- (length root))) #\v)
							   nil)
							  ((find (char root (1- (length root))) "rln")
							   nil)
							  (t
							   "v")))
						   ((equal sf "am")
						    "m")
						   (t
						    sf)))
					     (cond ((and (equal sf "i")
							 (not (find-if (lambda (v) (find v root)) "aeiou")))
						    "evin")
						   ((and (equal sf "av")
							 (not (find-if (lambda (v) (find v root)) "aeiou")))
						    "evin")
						   ((equal sf "am")
						    "evin")
						   (t
						    nil))
					     vv
					     tsch-class morph-type relation reduplication red-dir-pv
					     stem-type pr-st-ext pf-sfx pp-pfx pp-sfx passive-sfx
					     type-aorist type-obj-3-pfx type-aorist-3sg type-optative
					     nasal-infix subj-pers subj-num obj-pers type-subj12-sfx
					     type-subj3-sfx type-subj2-pfx type-ev-sfx
					     type-optative-3pl-en type-pr-st-ext "derived" derived-type "paul" date)))
		    (if testp
			(print new-features)
			(insert-records :into [verb-features]
					:attributes (list [unique-id] [id] [sub-id] [root] [c-root]
							  [tense] [pv] [vn] [gv] [sf] [caus-sf] [vv]
							  [tsch-class] [morph-type] [relation] [reduplication] [red-dir-pv]
							  [stem-type] [pr-st-ext] [pf-sfx] [pp-pfx] [pp-sfx] [passive-sfx]
							  [type-aorist] [type-obj-3-pfx] [type-aorist-3sg] [type-optative]
							  [nasal-infix] [subj-pers] [subj-num] [obj-pers] [type-subj12-sfx]
							  [type-subj3-sfx] [type-subj2-pfx] [type-ev-sfx]
							  [type-optative-3pl-en] [type-pr-st-ext] [source] [derived-type] [author] [date])
					:values (print new-features))))
		  )))
	    (unless testp
	      (update-paradigm-table :id (format nil "~d-~d" id (1+ max-sub-id))))))))
    (print count)))

;; Causatives of transitives
;; 5464
#+test
(let ((filter [and [like [tsch-class] "T%"]
		   [like [tense] "%|future|%"]
		   ;;[or [null [sf]][in [sf] '("i" "en" "ev" "eb" "ob")]
		       ])
      (missing-filter (lambda (id pv vn)
			(if pv
			    [and [= [id] id]
				 [or [and [in [tsch-class] '("KT")]
					  [= [pv] pv]]
				     [in [tsch-class] '("MV")]]]
			    [and [= [id] id]
				 [or [and [in [tsch-class] '("KT")]
					  [null [pv]]]
				     [in [tsch-class] '("MV")]]])))
      (id 3260)
      (sub-id 52))
  (complete-caus-paradigm :filter filter
		     ;;:root-filter root-filter
		     :missing-filter missing-filter
		     :id id
		     :sub-id sub-id
		     :testp nil
		     :derived-type "trans-causative")
  )

#|| bugs

paradigms of vpatiZeb

c-root GrGen

check: -adgens/-adginebs: -dgindeba
       -avlens/-avlinebs

cavixeri

vHatav

;; populate *paradigm-table*
#+(or mysql sqlite)
(progn
  (clrhash fst::*paradigm-table*)
  (update-paradigm-table)
  (clrhash *feature-cache*))

(clrhash *feature-cache*)
||#


(with-transaction ()
  (letx ((corrected-features (select [*] :from [verb-features]
				    :where [and [= [id] 2787]
						[= [sub-id] 74]
						[= [tsch-class] "RP5"]
						[or [like [tense] "%|aorist|%"]
						    [like [tense] "%|optative|%"]]]))
	(unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t))))
    (print corrected-features)
  
    (loop for (pv sub-id) in (select [pv] [sub-id] :from [verb-features]
				     :where [and [= [id] 2787]
						 [= [tsch-class] "RP5"]
						 [like [tense] "%|aorist|%"]])
       do (delete-records :from [verb-features]
			  :where [and [= [id] 2787]
				      [= [sub-id] sub-id]
				      [= [tsch-class] "RP5"]
				      [like [tense] "%|aorist|%"]])
       (dolist (cf corrected-features)
	 (destructuring-bind (ui id sid root c-root tense prv . rest) cf
	   (insert-records :into [verb-features]
			   :values (list* (incf unique-id)
					  id
					  sub-id
					  root
					  c-root
					  tense
					  pv
					  rest)))))))


(with-transaction ()
  (letx ((corrected-features (select [*] :from [verb-features]
				    :where [and [= [id] 2787]
						[= [sub-id] 74]
						[= [tsch-class] "RP5"]
						[or [like [tense] "%|aorist|%"]
						    [like [tense] "%|optative|%"]]]))
	(unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t))))
    (print corrected-features)
  
    (loop for (pv sub-id) in (select [pv] [sub-id] :from [verb-features]
				     :where [and [= [id] 2787]
						 [= [tsch-class] "RP5"]
						 [like [tense] "%|future|%"]])
       unless (select [pv] [sub-id] :from [verb-features]
			  :where [and [= [id] 2787]
				      [= [sub-id] sub-id]
				      [like [tense] "%|aorist|%"]])
       do
	 (dolist (cf corrected-features)
	   (destructuring-bind (ui id sid root c-root tense prv . rest) cf
	     (insert-records :into [verb-features]
			     :values (list* (incf unique-id)
					    id
					    sub-id
					    root
					    c-root
					    tense
					    pv
					    rest)))))))

(with-transaction ()
  (letx ((corrected-features (select [*] :from [verb-features]
				    :where [and [= [id] 2787]
						[= [sub-id] 89]
						[= [tsch-class] "RP6"]
						[or [like [tense] "%|aorist|%"]
						    [like [tense] "%|optative|%"]]]))
	(unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t))))
    (print (list :corrected-features corrected-features))
    
    (loop for (pv sub-id) in (select [pv] [sub-id] :from [verb-features]
				     :where [and [= [id] 2787]
						 ;;[= [sub-id] 89]
						 [= [tsch-class] "RP6"]
						 [like [tense] "%|future|%"]])
       unless (select [pv] [sub-id] :from [verb-features]
			  :where [and [= [id] 2787]
				      [= [sub-id] sub-id]
				      [like [tense] "%|aorist|%"]])
       do (dolist (cf corrected-features)
	    (destructuring-bind (ui id sid root c-root tense prv . rest) cf
	      (print (list* (incf unique-id)
			    id
			    sub-id
			    root
			    c-root
			    tense
			    pv
			    rest))
	      #-test
	      (insert-records :into [verb-features]
			      :values (list* (incf unique-id)
					     id
					     sub-id
					     root
					     c-root
					     tense
					     pv
					     rest)))))))

(with-transaction ()
  (let ((corrected-features (select [*] :from [verb-features]
				    :where [and [= [id] 2787]
						[= [sub-id] 55]
						[= [tsch-class] "RP5"]
						[or [like [tense] "%|present|%"]
						    [like [tense] "%|future|%"]]]))
	(unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t))))
    (print (list :corrected-features corrected-features))
    
    (loop for (pv sub-id) in (select [pv] [sub-id] :from [verb-features]
				     :where [and [= [id] 2787]
						 ;;[= [sub-id] 75]
						 [= [tsch-class] "RP5"]
						 [like [tense] "%|aorist|%"]])
       unless (select [pv] [sub-id] :from [verb-features]
			  :where [and [= [id] 2787]
				      [= [sub-id] sub-id]
				      [like [tense] "%|present|%"]])
       do (dolist (cf corrected-features)
	    (destructuring-bind (ui id sid root c-root tense prv . rest) cf
	      (print (list* (incf unique-id)
			    id
			    sub-id
			    root
			    c-root
			    tense
			    pv
			    rest))
	      #-test
	      (insert-records :into [verb-features]
			      :values (list* (incf unique-id)
					     id
					     sub-id
					     root
					     c-root
					     tense
					     (if (search "|present|" tense) "-" pv)
					     rest)))))))

(with-transaction ()
  (let ((corrected-features (select [*] :from [verb-features]
				    :where [and [= [id] 2793]
						[= [sub-id] 63]
						[= [pv] "ga"]
						[or [like [tense] "%|aorist|%"]
						    [like [tense] "%|optative|%"]]]))
	(unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t))))
    (print (list :corrected-features corrected-features))
    
    (loop for (pv sub-id tsch-class gv) in (select [pv] [sub-id] [tsch-class] :from [verb-features]
				     :where [and [= [id] 2793]
						 ;;[= [sub-id] 75]
						 [= [tsch-class] "RP1 (OR)"]
						 [like [tense] "%|future|%"]])
       unless (select [pv] [sub-id] :from [verb-features]
			  :where [and [= [id] 2793]
				      [= [sub-id] sub-id]
				      [like [tense] "%|aorist|%"]])
       do (dolist (cf corrected-features)
	    (destructuring-bind (ui id sid root c-root tense prv vn g-v sf caus-sf vv tsch . rest) cf
	      (print (list* (incf unique-id)
			    id
			    sub-id
			    root
			    c-root
			    tense
			    pv
			    vn
			    gv sf caus-sf vv tsch-class
			    rest))
	      #+test
	      (insert-records :into [verb-features]
			      :values (list* (incf unique-id)
					     id
					     sub-id
					     root
					     c-root
					     tense
					     (if (search "|present|" tense) "-" pv)
					     vn
					     gv sf caus-sf vv tsch-class
					     rest)))))))


(with-transaction ()
  (let ((corrected-features (select [*] :from [verb-features]
				    :where [and [= [id] 3576]
						[= [sub-id] 4]
						;;[= [pv] "ga"]
						[or [like [tense] "%|aorist|%"]
						    [like [tense] "%|optative|%"]
						    [like [tense] "%|perfect|%"]
						    [like [tense] "%|pluperfect|%"]]]))
	(unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t))))
    (print (list :corrected-features corrected-features))
    
    (loop for (pv sub-id tsch-class gv) in (select [pv] [sub-id] [tsch-class] :from [verb-features]
						   :where [and [= [id] 2793]
							       [in [sub-id] '(3 9)]
							       ;;[= [tsch-class] "RP1 (OR)"]
							       [like [tense] "%|future|%"]])
       unless (= sub-id 20)
       do
	 (delete-records :from [verb-features]
			 :where [and [= [id] 2793]
				     [= [sub-id] sub-id]
				     [or [like [tense] "%|aorist|%"]
					 [like [tense] "%|optative|%"]
					 [like [tense] "%|perfect|%"]
					 [like [tense] "%|pluperfect|%"]]])
	 (dolist (cf corrected-features)
	   (destructuring-bind (ui id sid root c-root tense prv vn g-v sf caus-sf vv tsch . rest) cf
	     (print (list* (incf unique-id)
			   id
			   sub-id
			   root
			   c-root
			   tense
			   pv
			   vn
			   gv sf caus-sf vv tsch-class
			   rest))
	     #-test
	     (insert-records :into [verb-features]
			     :values (list* (incf unique-id)
					    id
					    sub-id
					    root
					    c-root
					    tense
					    (if (search "|present|" tense) "-" pv)
					    vn
					    gv sf caus-sf vv tsch-class
					    rest)))))))


(with-transaction ()
  (let ((corrected-features (select [*] :from [verb-features]
				    :where [and [= [id] 432]
						[= [sub-id] 1]
						[or [like [tense] "%|future-part|%"]
						    [like [tense] "%|aorist|%"]
						    [like [tense] "%|optative|%"]
						    ;;[like [tense] "%|perfect|%"]
						    [like [tense] "%|pluperfect|%"]]]))
	(unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t))))
    (print (list :corrected-features corrected-features))
    
    (loop for (pv sub-id tsch-class gv) in (select [pv] [sub-id] [tsch-class] :from [verb-features]
						   :where [and [= [id] 432]
							       [like [tsch-class] "T%"]
							       [like [tense] "%|future|%"]])
       unless (= sub-id 1)
       do
	 #-test
	 (delete-records :from [verb-features]
			 :where [and [= [id] 432]
				     [= [sub-id] sub-id]
				     [or [like [tense] "%|future-part|%"]
					 [like [tense] "%|aorist|%"]
					 [like [tense] "%|optative|%"]
					 ;;[like [tense] "%|perfect|%"]
					 [like [tense] "%|pluperfect|%"]]])
	 (dolist (cf corrected-features)
	   (destructuring-bind (ui id sid root c-root tense prv vn g-v sf caus-sf vv tsch . rest) cf
	     (print (list* (incf unique-id)
			   id
			   sub-id
			   root
			   c-root
			   tense
			   pv
			   vn
			   gv sf caus-sf vv tsch-class
			   rest))
	     #-test
	     (insert-records :into [verb-features]
			     :values (list* (incf unique-id)
					    id
					    sub-id
					    root
					    c-root
					    tense
					    pv
					    vn
					    gv sf caus-sf vv tsch-class
					    rest)))))))

(with-transaction ()
  (let ((corrected-features (select [*] :from [verb-features]
				    :where [and [= [id] 432]
						[= [sub-id] 36]
						[or ;;[like [tense] "%|future-part|%"]
						    [like [tense] "%|aorist|%"]
						    [like [tense] "%|optative|%"]
						    ;[like [tense] "%|perfect|%"]
						    ;[like [tense] "%|pluperfect|%"]
						    ]]))
	(unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t))))
    (print (list :corrected-features corrected-features))
    
    (loop for (pv sub-id tsch-class gv morph-type) in (select [pv] [sub-id] [tsch-class] [gv] [morph-type]
							      :from [verb-features]
							      :where [and [= [id] 432]
									  [like [tsch-class] "MV"]
									  [like [tense] "%|future|%"]])
       unless (= sub-id 1)
       do
	 #-test
	 (delete-records :from [verb-features]
			 :where [and [= [id] 432]
				     [= [sub-id] sub-id]
				     [or ;;[like [tense] "%|future-part|%"]
					 [like [tense] "%|aorist|%"]
					 [like [tense] "%|optative|%"]
					 ;[like [tense] "%|perfect|%"]
					 ;[like [tense] "%|pluperfect|%"]
					 ]])
	 (dolist (cf corrected-features)
	   (destructuring-bind (ui id sid root c-root tense prv vn g-v sf caus-sf vv tsch mtype . rest) cf
	     (print (list* (incf unique-id)
			   id
			   sub-id
			   root
			   c-root
			   tense
			   pv
			   vn
			   gv sf caus-sf vv tsch-class morph-type
			   rest))
	     #-test
	     (insert-records :into [verb-features]
			     :values (list* (incf unique-id)
					    id
					    sub-id
					    root
					    c-root
					    tense
					    pv
					    vn
					    gv sf caus-sf vv tsch-class morph-type
					    rest)))))))



(with-transaction ()
  (loop for (pv sub-id) in (select [pv] [sub-id] :from [verb-features]
				   :where [and [= [id] 3576]
					       [= [tsch-class] "T5"]
					       [like [tense] "%|future|%"]])
     do (dolist (uid (select [unique-id]
			     :from [verb-features]
			     :flatp t
			     :where [and [= [id] 3576]
					 [= [sub-id] sub-id]
					 [= [pv] "-"]
					 [or [like [tense] "%|aorist|%"]
					     [like [tense] "%|optative|%"]
					     [like [tense] "%|perfect|%"]
					     [like [tense] "%|pluperfect|%"]]]))
	  (update-recordsx [verb-features]
			  :attributes `([pv]) :values (list pv)
			  :where [= [unique-id] uid])
	  (print uid))))

(with-transaction ()
  (let ((corrected-features (select [*] :from [verb-features]
				    :where [and [= [id] 432]
						[= [sub-id] 36]]))
	(unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t)))
	(sub-id (car (select [max [sub-id]] :from [verb-features] :where [= [id] 432] :flatp t))))
    (print (list :corrected-features corrected-features))
    
    (loop for (pv) in (select [pv]
			      :distinct t
			      :from [verb-features]
			      :where [and [= [id] 432]
					  [like [tsch-class] "T%"]
					  [like [tense] "%|future|%"]])
       unless (null pv)
       do
	 (incf sub-id)
	 (dolist (cf corrected-features)
	   (destructuring-bind (ui id sid root c-root tense prv vn g-v sf caus-sf vv tsch mtype . rest) cf
	     (print (list* (incf unique-id)
			   id
			   sub-id
			   root
			   c-root
			   tense
			   pv
			   vn
			   nil sf caus-sf vv "P1" "passive"
			   rest))
	     #-test
	     (insert-records :into [verb-features]
			     :values (list* (incf unique-id)
					    id
					    sub-id
					    root
					    c-root
					    tense
					    pv
					    vn
					    nil sf caus-sf vv "P1" "passive"
					    rest)))))))
    
(with-transaction ()
  (dolist (uid (select [unique-id] :from [verb-features]
		       :flatp t
		       :where [and [= [id] 3576]
				   [like [tense] "%|future-part|%"]
				   [= [pf-sfx] "-"]]))
    (Print uid)
    (update-recordsx [verb-features]
		    :attributes `([root]) :values (list "Had")
		    :where [= [unique-id] uid])))

(with-transaction ()
  (letx ((tense (caar (select [tense] :from [verb-features] :where [and [= [id] 2793] [= [sub-id] 4] [like [tense] "%|present|%"]]))))
    (update-records [verb-features]
		    :attributes `([tense]) :values (list tense)
		    :where [and [= [id] 2793] [in [sub-id] '(5 6 7 8 9 10 11 12 13 14 15)] [like [tense] "%|present|%"]])))

(with-transaction ()
  (destructuring-bind (type-aorist type-optative type-ev-sfx type-optative-3pl-en)
      (car (select [type-aorist] [type-optative] [type-ev-sfx] [type-optative-3pl-en]
		   :from [verb-features]
		   :where [and [= [id] 1632]
			       [= [sub-id] 21]
			       [like [tense] "%|aorist|%"]]))
    (let ((unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t))))
      (dolist (uid (select [unique-id] :from [verb-features] :flatp t
			   :where [and [= [id] 1924]
				       [or [like [tsch-class] "P1"]
					   ;;[like [tsch-class] "RP%"]
					   ]
				       [like [tense] "%|aorist|%"]]))
	(print uid)
	(update-records [verb-features]
			 :attributes `([type-aorist] [type-optative] [type-ev-sfx] [type-optative-3pl-en])
			 :values (list type-aorist type-optative type-ev-sfx type-optative-3pl-en)
			 :where [= [unique-id] uid])))))
      )))

;; fix -am verbs

(defun fix-am-verbs (id)
  (with-transaction ()
    (update-records [verb-features]
		    :attributes `([type-aorist] [type-optative])
		    :values (list "strong" "a")
		    :where [and [= [id] id]
				[or [and [like [tsch-class] "T%"]
					 [or [like [tense] "%|aorist|%"]
					     [like [tense] "%|optative|%"]
					     [like [tense] "%|conj-perfect|%"]]
					 ;; [= [type-aorist] "strong"]
					 ]
				    ;;#+test
				    [and [or [like [tsch-class] "P%"] [like [tsch-class] "RP%"]]
					 [or [like [tense] "%|aorist|%"]
					     [like [tense] "%|optative|%"]]]]])))

;;(fix-am-verbs 192)

;; delete 2787-79/81!

;; fix jaH: -(o)d-

#+test
(progn
  (clrhash fst::*paradigm-table*)
  (update-paradigm-table)
  (clrhash *feature-cache*))


#+test
(Print (select [*] :from [verb-features]
	       :where [and [= [sf] "av"]
			   [= [tense] "|future-part|"]]))


#+test
(let ((pr-features (select [*] :from [verb-features]
			   :where [and [= [id] 1844]
				       [= [sub-id] 17]
				       [or [like [tense] "%|present|%"]
					   [like [tense] "%|imperfect|%"]]]))
      (subid-pv-list (select [sub-id] [pv]
			     :distinct t
			     :from [verb-features]
			     :where [and [= [id] 1844]
					 [> [sub-id] 17]
					 [< [sub-id] 33]
					 [like [tense] "%|aorist|%"]]))
      (unique-id (car (select [max [unique-id]] :from [verb-features] :flatp t))))
  (with-transaction ()
    (dolist (subid-pv subid-pv-list)
      (dolist (flist pr-features)
	(destructuring-bind (uid id sid root c-root tense pv . rest) flist
	  (insert-records :into [verb-features]
			  :values (list* (incf unique-id)
					 id
					 (car subid-pv)
					 root
					 c-root
					 tense
					 (cadr subid-pv)
					 rest)))))))


#+test
(let ((pr-features (select [id] [sub-id] [c-root]
			   :from [verb-features]
			   :distinct t
			   :where [and [not [null [c-root]]]
				       [not [= [c-root] ""]]])))
  (with-transaction ()
    (dolist (pr-f pr-features)
      (destructuring-bind (id sub-id c-root) (print pr-f)
	(update-records [verb-features]
			:attributes `([c-root])
			:values (list c-root)
			:where [and [= [id] id]
				    [= [sub-id] sub-id]
				    [or [null [c-root]]
					[= [c-root] ""]]])))))



#+test
(pprint (select [verb-translation sub-id] [link-sub-id] [participle-sub-id] [verb-translation pv] [verb-features tsch-class]
	       :distinct t
	       :from [verb-translation]
	       :left-join '([verb-features])
	       :on [and [= [verb-translation id] [verb-features id]]
			[= [verb-translation sub-id] [verb-features sub-id]]]
	       :where [= [verb-translation id] 943])) 

(print (select [tsch-class] :distinct t :flatp t
	       :from [verb-features]
	       :group-by [tsch-class]
	       :order-by `((,[count [tsch-class]] :desc)))
	       )


("T1" "T3" "MV" "T2" "KT" "P2" "P1" "T5" "T4" "RP1 (OR)" "RP1" "RP3 (OR)" "P3" "RM1" "RM2" "RP1 (ohne i.O.)" "T3 (OR)" "IV1" "IV4" "RM4" "RP3" "RP6" "T1 (OR)" "RM2 (OR)" "RP6 (OR)" "RP5" "RP7" "T3 (nur mit i.O.)" "RP5 (OR)" "IV2" "T5 (nur mit i.O.)" "RP4" "IV3" "RM1 (OR)" "RP2" "T5 (OR)" "RP2 (OR)" "ZP3" "RP7 (OR)" "ZP1" "ZP2" "RM3" "KT (nur mit i.O.)" "RP4 (OR)" "T2 (OR)" "T5 (OR) (nur mit i.O.)" "T4 (nur mit i.O.)" "RP1 (mit" "T4 (OR)" "KT (OR)" "RM3 (OR)" "RM4 (OR)" "RP7 (ohne i.O.)")

(dotimes (id 3834)
  (print id)
  (let* ((paradigms (select [verb-translation sub-id] [link-sub-id] [participle-sub-id] [verb-translation pv] [vn] [tsch-class]
			    :distinct t
			    :from [verb-translation]
4			    :left-join '([verb-features])
			    :on [and [= [verb-translation id] [verb-features id]]
				     [= [verb-translation sub-id] [verb-features sub-id]]]
			    :where [= [verb-translation id] id]
			    :order-by '([tsch-class])))
	 (vn-pv-subid-table (dat:make-string-tree)))
    (dolist (paradigm paradigms)
      (destructuring-bind (sub-id link-sub-id part-sub-id pv vn tsch-class) paradigm
	(let ((vn (subseq vn 0 (position #\space vn))))
	   (when (and tsch-class
		     (or (char= (char tsch-class 0) #\T)
			 (char= (char tsch-class 0) #\M))
		     (null (dat:string-tree-get-list vn-pv-subid-table (list vn pv))))
	    (setf (dat:string-tree-get-list vn-pv-subid-table (list vn pv)) sub-id)))))
    (with-transaction ()
      (dolist (paradigm paradigms)
	(destructuring-bind (sub-id link-sub-id part-sub-id pv vn tsch-class) paradigm
	  (unless (or link-sub-id (and part-sub-id (/= part-sub-id sub-id)))
	    (let* ((vn (subseq vn 0 (position #\space vn)))
		   (part (dat:string-tree-get-list vn-pv-subid-table (list vn pv))))
	      (unless (eql part part-sub-id)
		(add-participle-link id sub-id part)
		(print (cons part paradigm))))))
	))))

(let* ((id 338)
       (paradigms (select [verb-translation sub-id] [link-sub-id] [participle-sub-id] [verb-translation pv] [vn] [tsch-class]
			  :distinct t
			  :from [verb-translation]
			  :left-join '([verb-features])
			  :on [and [= [verb-translation id] [verb-features id]]
				   [= [verb-translation sub-id] [verb-features sub-id]]]
			  :where [= [verb-translation id] id]
			  :order-by '([tsch-class])))
       (vn-pv-subid-table (dat:make-string-tree)))
  (dolist (paradigm paradigms)
    (destructuring-bind (sub-id link-sub-id part-sub-id pv vn tsch-class) paradigm
      (let ((vn (subseq vn 0 (position #\space vn))))
	(when (and tsch-class (not (search "(OR)" tsch-class)))
	  (setf (dat:string-tree-get-list vn-pv-subid-table (list vn pv tsch-class)) sub-id)))))
  (progn ;;with-transaction ()
    (dolist (paradigm paradigms)
      (destructuring-bind (sub-id link-sub-id part-sub-id pv vn tsch-class) paradigm
	(when (and tsch-class (search "(OR)" tsch-class))
	  (let* ((vn (subseq vn 0 (position #\space vn)))
		 (or-start (search " (OR)" tsch-class)))
	    (when or-start
	      (let* ((no-or-class (concat (subseq tsch-class 0 or-start) (subseq tsch-class (+ or-start 5))))
		     (no-or-subid (dat:string-tree-get-list vn-pv-subid-table (list vn pv no-or-class))))
		(debug no-or-class)
		(when no-or-subid
		  (add-morphosyntactic-link id sub-id no-or-subid)
		  (print (cons no-or-subid paradigm))))))
	  )))))

#+test
(clrhash *feature-cache*)

;; controlled: < 468

:eof