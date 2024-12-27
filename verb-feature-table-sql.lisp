;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10 -*-

;; paul.meurer@uib.no

(in-package :fst)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf clsql-sys::*original-readtable* nil)
  (enable-sql-reader-syntax))

;; #.(locally-enable-sql-reader-syntax)


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

(fst::load-root-table)

;; populate *paradigm-table*
(progn
  (clrhash fst::*paradigm-table*)
  (update-paradigm-table))

(defun ce (string)
  (convert-encoding string :amirani :unicode))


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

(defvar *fullform-count-table* (make-hash-table :test #'equal))

(do-query ((word count)
	   [select [word] [corpus-count] :from [morph fullform] :flatp t])
  (setf (gethash word *fullform-count-table*) count))

(defun fullform-count (word)
  (gethash word *fullform-count-table*))

(defun verb-translation (&key id sub-id paradigm-id)
  (if paradigm-id
      (destructuring-bind (id sub-id) (mapcar #'parse-integer (split paradigm-id #\-))
	(car (select [translation] :flatp t :from [morph verb-translation]
		     :where [and [= [id] ?id] [= [sub-id] ?sub-id]])))
      (car (select [translation] :flatp t :from [morph verb-translation]
		   :where [and [= [id] ?id] [= [sub-id] ?sub-id]]))))



(defparameter *id-to-tsch-classes* (make-hash-table :test #'equal))

(defparameter *id-to-roots-table* (make-hash-table))
(defparameter *id-to-c-root-table* (make-hash-table))

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


(defun inv-convert-num (str)
  str)

(defun inv-convert-morph (str)
  (if (equal str "-")
      '-
      str))

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

(defun simple-vn (vn)
  (let ((vn (subseq vn 0 (position #\space vn))))
    (string-trim "[]" (subseq vn 0 (position #\- vn)))))

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
(defparameter *preverbless-aorists* (make-hash-table :test #'equal))

(defun update-preverbless-aorists ()
  (let ((table *preverbless-aorists*))
    (clrhash table)
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

(update-preverbless-aorists)

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

(def-view-class pv-variant ()
  ((pv :initform nil :initarg :pv :reader pv :type string :db-kind :key)
   (pv-variant :initform nil :initarg :pv-variant :reader pv-variant :type string :db-kind :key)
   )
  (:base-table [morph pv-variant]))

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

(defun replace-root (new-root root)
  (if new-root
      (u::subst-substrings new-root (list "*" root))
      root))

(defun replace-participle-stem (stem new-root root)
  (let ((pos (search root stem)))
    (if pos
        (concat (subseq stem 0 pos) new-root (subseq stem (+ pos (length root))))
        stem)))

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
			       :stem (replace-participle-stem (participle-stem part) new-root (c-root vpar))
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
					    style lang type-pr-st-ext paradigm-replacement)
                      (cdddr row)
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

(defun get-paradigm-comments (id sub-id)
  (values nil
	  (car (select [comment] :flatp t :from [morph verb-translation]
		       :where [and [= [id] ?id] [= [sub-id] ?sub-id]]))))

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

(defun paradigm-base/participle-sub-id+source (id sub-id)
  (debug sub-id)
  (debug (car (select [base-sub-id] [participle-sub-id] [source]
	       :from [morph verb-paradigm]
	       :where [and [= [id] ?id] [= [sub-id] ?sub-id]]))))

(defun paradigm-xle-templates (id sub-id)
  (select [template] :flatp t :from [morph xle-template]
 :where [and [= [id] ?id] [= [sub-id] ?sub-id]]))

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

:eof
