;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10 -*-

;; paul.meurer@uni.no

(in-package :fst)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf clsql-sys::*original-readtable* nil)
  (enable-sql-reader-syntax))

(def-view-class noun-features ()
  ((stem :initform nil :initarg :root :reader root :type string :db-kind :key) ;; collate latin1_bin
   (code :initform nil :initarg :code :reader code :type string :db-kind :key)
   (pos :initform nil :initarg :pos :reader part-of-speech :type string :db-kind :key)
   (sub-id :initform nil :initarg :unique-id :reader unique-id :type integer :db-kind :key)
   (features :initform nil :initarg :features :reader features :type string)
   (style-features :initform nil :initarg :style-features :reader style-features :type string)
   (lang :initform nil :initarg :lang :reader language :type string) ;; nil, "ng", "og" etc.
   (template :initform nil :initarg :template :reader template :type string)
   (comment :initform nil :initarg :comment :reader comment :type string);; :db-type "mediumtext")
   (author :initform nil :initarg :author :reader author :type string)
   (translation :initform "" :initarg :translation :reader translation);; :type "mediumtext")
   (date :initform nil :initarg :date :reader date :type integer :db-type "bigint"))
  (:base-table [morph noun-features]))

#+once
(execute-command "alter table morph.noun_features add lang varchar;")

#+test
(drop-table [morph noun-features])
#+test
(create-view-from-class 'noun-features)

(def-view-class verb-translation ()
  ((id :initform nil :initarg :id :reader id :type integer :db-kind :key)
   (sub-id :initform nil :initarg :sub-id :reader sub-id :type integer :db-kind :key)
   ;; move to verb-paradigm
   (link-sub-id :initform nil :initarg :link-sub-id :reader link-sub-id :type integer)
   ;; move to verb-paradigm
   (base-sub-id :initform nil :initarg :base-sub-id :reader base-sub-id :type integer)
   ;; move to verb-paradigm
   (participle-sub-id :initform nil :initarg :participle-sub-id :reader participle-sub-id :type integer)
   ;; move to verb-paradigm
   (pv :initform nil :initarg :pv :reader pv :type string)
   (translation :initform nil :initarg :translation :reader translation :type string)
   (comment :initform nil :initarg :comment :reader comment :type string)
   (source :initform nil :initarg :source :reader source :type string)
   ;; move to verb-paradigm
   (derived-type :initform nil :initarg :derived-type :reader derived-type :type string)
   (author :initform nil :initarg :author :reader author :type string)
   (date :initform nil :initarg :date :reader date :type integer :db-type "bigint")
   )
  (:base-table [morph verb-translation]))

(def-view-class verb-features ()
  ((unique-id :initform nil :initarg :unique-id :reader unique-id :type integer :db-kind :key)
   (id :initform nil :initarg :id :reader id :type integer :db-kind :key)
   (sub-id :initform nil :initarg :sub-id :reader sub-id :type integer)
   (root :initform nil :initarg :root :reader root :type string :db-kind :key)
   ;; make obsolete
   (c-root :initform nil :initarg :c-root :reader c-root :type string :db-kind :key)
   (tense :initform nil :initarg :tense :reader tense :type string)
   ;; make obsolete
   (pv :initform nil :initarg :pv :reader pv :type string)
   ;; make obsolete
   (vn :initform nil :initarg :vn :reader vn :type string)
   ;; make obsolete
   (gv :initform nil :initarg :gv :reader gv :type string)
   (sf :initform nil :initarg :sf :reader sf :type string)
   (caus-sf :initform nil :initarg :caus-sf :reader caus-sf :type string)
   (vv :initform nil :initarg :vv :reader vv :type string)
   ;; make obsolete
   (tsch-class :initform nil :initarg :tsch-class :reader tsch-class :type string)
   (morph-type :initform nil :initarg :morph-type :reader morph-type :type string)
   (relation :initform nil :initarg :relation :reader relation :type string)
   ;; make obsolete
   (reduplication :initform nil :initarg :reduplication :reader reduplication :type string)
   ;; make obsolete
   (red-dir-pv :initform nil :initarg :red-dir-pv :reader red-dir-pv :type string)
   (stem-type :initform nil :initarg :stem-type :reader stem-type :type string)
   (pr-st-ext :initform nil :initarg :pr-st-ext :reader pr-st-ext :type string)
   (part-pfx :initform nil :initarg :part-pfx :reader part-pfx :type string)
   (part-sfx :initform nil :initarg :part-sfx :reader part-sfx :type string)
   (passive-sfx :initform nil :initarg :passive-sfx :reader passive-sfx :type string)
   (nasal-infix :initform nil :initarg :nasal-infix :reader nasal-infix :type string)
   (type-aorist :initform nil :initarg :type-aorist :reader type-aorist :type string)
   (type-obj-3-pfx :initform nil :initarg :type-obj-3-pfx :reader type-obj-3-pfx :type string)
   (type-aorist-3sg :initform nil :initarg :type-aorist-3sg :reader type-aorist-3sg :type string)
   (type-optative :initform nil :initarg :type-optative :reader type-optative :type string)
   (subj-pers :initform nil :initarg :subj-pers :reader subj-pers :type string)
   (subj-num :initform nil :initarg :subj-num :reader subj-num :type string)
   (obj-pers :initform nil :initarg :obj-pers :reader obj-pers :type string)
   (type-subj12-sfx :initform nil :initarg :type-subj12-sfx :reader type-subj12-sfx :type string)
   (type-subj3-sfx :initform nil :initarg :type-subj3-sfx :reader type-subj3-sfx :type string)
   (type-subj2-pfx :initform nil :initarg :type-subj2-pfx :reader type-subj2-pfx :type string)
   (type-ev-sfx :initform nil :initarg :type-ev-sfx :reader type-ev-sfx :type string)
   (style :initform nil :initarg :style :reader style :type string)
   (lang :initform nil :initarg :lang :reader lang :type string)
   (type-pr-st-ext :initform nil :initarg :type-pr-st-ext :reader type-pr-st-ext :type string)
   (paradigm-replacement :initform nil :initarg :paradigm-replacement :reader paradigm-replacement :type string)
   (comment :initform nil :initarg :comment :reader comment :type string) ;; :db-type "mediumtext")
   (author :initform nil :initarg :author :reader author :type string)
   (source :initform nil :initarg :source :reader source :type string)
   (derived-type :initform nil :initarg :derived-type :reader derived-type :type string)
   (date :initform nil :initarg :date :reader date :type integer :db-type "int8")
   (deleted :initform nil :initarg :deleted :reader deleted :type boolean)
   )
  (:base-table [morph verb-features]))

;; new 2013
(def-view-class verb-paradigm ()
  ((id :initform nil :initarg :id :reader id :type integer :db-kind :key)
   (sub-id :initform nil :initarg :sub-id :reader sub-id :type integer :db-kind :key)
   (c-root :initform nil :initarg :c-root :reader c-root :type string)
   ;; move next 3 to verb-participle
   (vn :initform nil :initarg :vn :reader vn :type string)
   (impf-vn :initform nil :initarg :impf-vn :reader impf-vn :type string) ;; if NIL, impf-vn is impf-pv + vn
   (pf-vn :initform nil :initarg :pf-vn :reader pf-vn :type string) ;; if NIL, pf-vn is pf-pv + vn
   (tsch-class :initform nil :initarg :tsch-class :reader tsch-class :type string)
   (class :initform nil :initarg :class :reader verb-class :type string)
   (features-sub-id :initform nil :initarg :features-sub-id :reader features-sub-id :type integer)
   (link-sub-id :initform nil :initarg :link-sub-id :reader link-sub-id :type integer)
   (base-sub-id :initform nil :initarg :base-sub-id :reader base-sub-id :type integer)
   (participle-sub-id :initform nil :initarg :participle-sub-id :reader participle-sub-id :type integer)
   ;;(pv :initform nil :initarg :pv :reader pv :type string) ;; prelim
   (pf-pv :initform nil :initarg :pf-pv :reader pf-pv :type string :db-kind :key)
   (impf-pv :initform nil :initarg :impf-pv :reader impf-pv :type string :db-kind :key)
   (pf-12-pv :initform nil :initarg :pf-12-pv :reader pf-12-pv :type string)
   (dir-pv-p :initform nil :initarg :dir-pv-p :reader dir-pv-p :type boolean)
   (red-dir-pv :initform nil :initarg :red-dir-pv :reader red-dir-pv :type string)
   (no-preverbless-aor :initform nil :initarg :no-preverbless-aor :reader no-preverbless-aor :type boolean)
   (comment :initform nil :initarg :comment :reader comment :type string)
   (author :initform nil :initarg :author :reader author :type string)
   (source :initform nil :initarg :source :reader source :type string)
   (derived-type :initform nil :initarg :derived-type :reader derived-type :type string)
   (date :initform nil :initarg :date :reader date :type integer :db-type "int8")
   (accepted :initform nil :accessor accepted :type boolean)
   (disabled :initform nil :initarg :disabled :reader disabled :type boolean)
   )
  (:base-table [morph verb-paradigm]))

#+test
(execute-command "alter table morph.verb_paradigm add disabled boolean")
#+test
(execute-command "alter table morph.verb_paradigm drop deleted")

(def-view-class verb-participle ()
  ((id :initform nil :initarg :id :reader id :type integer :db-kind :key)
   (sub-id :initform nil :initarg :sub-id :reader sub-id :type integer :db-kind :key)
   (type :initform nil :initarg :type :reader participle-type :type keyword :db-kind :key)
   (stem :initform nil :initarg :stem :reader participle-stem :type string :db-kind :key)
   (root :initform nil :initarg :root :reader participle-root :type string)
   (code :initform nil :initarg :code :reader conjugation-code :type keyword :db-kind :key)
   ;; NIL, :ng or :og
   (variety :initform nil :initarg :variety :reader variety :type keyword)
   ;; NIL, :perf or :imperf
   (aspect :initform nil :initarg :aspect :reader aspect :type keyword)
   (date :initform nil :initarg :date :reader date :type integer :db-type "int8")
   (attested :initform nil :initarg :attested :reader attested :type boolean)
   (accepted :initform nil :initarg :accepted :accessor accepted :type boolean)
   ;; used for masdars; main-form is used to construct lemma
   (main-form :initform nil :initarg :main-form :accessor main-form :type boolean)
   (wrong :initform nil :initarg :wrong :accessor wrongp :type boolean)
   (restriction :initform nil :initarg :restriction :accessor restriction :type keyword)
   )
  (:base-table [morph participle]))

#+once
(execute-commandxx "alter table morph.participle add wrong boolean")
#+once
(execute-commandx "alter table morph.participle add restriction varchar")

;;(execute-command "alter table morph.participle drop constraint verb_participlepk;")
;;(execute-command "alter table morph.participle add PRIMARY KEY (id, sub_id, type, stem, code);"))

;;(execute-command "alter table morph.verb_paradigm add class varchar")
;;(execute-command "alter table morph.participle add root varchar")

;; remove this one
(def-view-class verbal-noun ()
  ((id :initform nil :initarg :id :reader id :type integer :db-kind :key)
   (sub-id :initform nil :initarg :sub-id :reader sub-id :type integer :db-kind :key)
   (vn :initform nil :initarg :vn :reader vn :type string)
   (impf-vn :initform nil :initarg :impf-vn :reader impf-vn :type string) ;; if NIL, impf-vn is impf-pv + vn
   (pf-vn :initform nil :initarg :pf-vn :reader pf-vn :type string)
   (variety :initform nil :initarg :variety :reader variety :type keyword)
   (date :initform nil :initarg :date :reader date :type integer :db-type "int8")
   )
  (:base-table [morph verbal-noun]))

#+test
(create-view-from-class 'verbal-noun)

#+test
(delete-records :from  [morph verbal-noun])

(def-view-class xle-template ()
  ((id :initform nil :initarg :id :reader id :type integer :db-kind :key)
   (sub-id :initform nil :initarg :sub-id :reader sub-id :type integer :db-kind :key)
   (template :initform nil :initarg :template :reader template :type string :db-kind :key)
   (comment :initform nil :initarg :comment :reader comment :type string);; :db-type "mediumtext")
   (author :initform nil :initarg :author :reader author :type string)
   (date :initform nil :initarg :date :reader date :type integer :db-type "bigint")
   )
  (:base-table [morph xle-template]))

#+test
(create-view-from-class 'xle-template)

(def-view-class related-verbal-nouns ()
  ((id :initform nil :initarg :id :reader id :type integer :db-kind :key)
   (vn :initform nil :initarg :vn :reader vn :type string :db-kind :key)
   (related-vn :initform nil :initarg :related-vn :reader related-vn :type string :db-kind :key)
   )
  (:base-table [morph related-verbal-nouns]))

#+test
(drop-table [morph related-verbal-nouns])
#+test
(create-view-from-class 'related-verbal-nouns)

(def-view-class super-paradigm ()
  ((id :initform nil :initarg :id :reader id :type integer :db-kind :key)
   (comment :initform nil :initarg :comment :reader comment :type string);; :db-type "mediumtext")
   )
  (:base-table [morph super-paradigm]))

#+test
(create-view-from-class 'super-paradigm)

(def-view-class paradigm ()
  ((id :initform nil :initarg :id :reader id :type integer :db-kind :key)
   (sub-id :initform nil :initarg :sub-id :reader sub-id :type integer :db-kind :key)
   (tense :initform nil :initarg :tense :reader tense :type string :db-kind :key)
   (subj-pers :initform "-" :initarg :subj-pers :reader subj-pers :type string :db-kind :key)
   (subj-num :initform "-" :initarg :subj-num :reader subj-num :type string :db-kind :key)
   (obj-pers :initform "-" :initarg :obj-pers :reader obj-pers :type string :db-kind :key)
   (obj-num :initform "-" :initarg :obj-num :reader obj-num :type string :db-kind :key)
   (verbform :initform nil :initarg :verbform :reader verbform :type string :db-kind :key)))

(def-view-class fullform ()
  ((word :initform nil :initarg :word :reader word :type string :db-kind :key)
   (corpus-count :initform 0 :initarg :corpus-count :reader corpus-count :type integer))
  (:base-table [morph fullform]))

#+disabled
(dolist (class '(paradigm fullform))
  ;;(drop-table 'paradigm)
  (unless (table-exists-p (view-table (find-class class)))
    (create-view-from-class class)
    (when (eq class 'fullform)
      (execute-command "alter table fullform modify WORD VARCHAR(255) character set utf8 collate utf8_bin;"))))

#+once
(create-view-from-class 'fullform)

#+test
(with-transaction ()
  (let ((vn-table (make-hash-table :test #'equal))
	(rows (select [id] [sub-id] [vn] [impf-vn] [pf-vn]
		      :from [morph verb-paradigm])))
    (dolist (row rows)
      (setf (gethash (list (car row) (cadr row)) vn-table) row))
    (maphash
     (lambda (key row)
       (destructuring-bind (id sub-id vn impf-vn pf-vn) row
	 (insert-records :into [morph verbal-noun]
			 :values (print (list id sub-id vn impf-vn pf-vn nil (get-universal-time))))))
     vn-table)))


#+test
(execute-command "alter table morph.verb_participle rename comment to aspect")
#+test
(execute-command "alter table morph.verb_paradigm add no_preverbless_aor boolean")

#+test
(drop-tablexx [morph verb-participle])
#+test
(create-view-from-class 'verb-participle)

#+oncexx
(create-index [paradigm-id-sub-id] :on [morph verb-paradigm]
	      :attributes '([id] [sub-id]) 
	      :unique nil)

#+test
(execute-command "alter table morph.verb_paradigm add pf_12_pv varchar")

#+test
(drop-table [morph verb-participle])

#+test
(create-view-from-class 'verb-paradigm)

#+test
(delete-records :from [morph verb-paradigm])

;; populate new table

#+test
(defparameter *vn-table* (make-hash-table :test #'equal))

#+test
(let ((pids (select [id] [sub-id]
		   :from [morph verb-features]
		   :distinct t
		   :order-by '([id] [sub-id]))))
  (dolist (pid pids)
    (destructuring-bind (id sub-id) pid
      (insert-paradigm id sub-id))))

#+test
(defun insert-paradigm (id sub-id)
  (let ((pf-pv nil)
	(impf-pv nil)
	(dir-pv nil)
	(red-dir-pv nil)
	(redup nil)
	(tsch-class nil)
	(c-root nil)
	(vn nil))
    (do-query ((%c-root %vn %tsch-class tense pv %red-dir-pv obj-pers reduplication)
	       [select [c-root] [vn] [tsch-class] [tense]
		       [pv] [red-dir-pv] [obj-pers] [reduplication]
		       :from [morph verb-features]
		       :distinct t
		       :where [and [= [id] ?id]
				   [= [sub-id] ?sub-id]]])
      ;;(print (list %c-root %vn %tsch-class tense pv %red-dir-pv obj-pers reduplication))
      (cond ((or (search "|present|" tense)
		 (search "|imperfect|" tense)
		 (search "|conj-present|" tense))
	     (pushnew pv impf-pv :test #'equal))
	    ((or (search "|masdar|" tense)
		 (search "-part|" tense))
	     nil)
	    (t
	     (cond ((equal obj-pers "1|2")
		    (pushnew pv dir-pv))
		   (t
		    (pushnew pv pf-pv :test #'equal)))))
      (when %red-dir-pv (setf red-dir-pv %red-dir-pv))
      (unless redup (setf redup reduplication))
      (when (and redup pv (null red-dir-pv))
	(setf red-dir-pv "მო"))
      (setf c-root %c-root)
      (unless tsch-class (setf tsch-class %tsch-class))
      (unless vn (setf vn %vn)))
    #+test
    (insert-records :into [morph verb-paradigm]
		    :attributes '([id] [sub-id] [c-root] [vn] [tsch-class]
				  [pf-pv] [impf-pv] [dir-pv-p] [red-dir-pv] [date])
		    :values (list id sub-id c-root vn tsch-class
				  (when pf-pv (format nil "~{~a~^|~}" pf-pv))
				  (when impf-pv (format nil "~{~a~^|~}" impf-pv))
				  (when (and dir-pv
					     (not (loop for pv in dir-pv
						     thereis (find pv pf-pv :test #'string=))))
				    t)
				  red-dir-pv
				  (get-universal-time)))
    (when (and (cdr pf-pv) (member nil pf-pv))
      (print (list id sub-id c-root tsch-class impf-pv pf-pv dir-pv red-dir-pv)))))

#+test
(dolist (root+vn
	  '(("ჯილდოვ" "ჯილდო(ვ)ება" "ჯილდოვება") 
	    ("ჯაფა" "ჯაფა#" "-") 
	    ("ხუჭ1" "ხუჭვა-ქთ" "ხუჭვა") 
	    ("ხურ1" "ხურვა-ქთ" "ხურვა") 
	    ("ხუნტრუც" "ხუნტრუცობა" "ხუნტრუცობა || ხუნტრუცი ბზw. -ხუნტრუცება") 
	    ("ხუმრ" "ხუმრობა" "ხუმრობა ბზw. -ხუმრება") 
	    ("ხსენ1" "ხსენება-ქთ" "ხსენება") 
	    ("ხროტინ" "ხროტინი" "ხროტინი ბზw. -ხროტინება") 
	    ("ხრ1" "ხრა-ქთ" "ხრა") 
	    ("ხოკ" "ხოკ(ვ)ა" "ხოკვა") 
	    ("ხლურჩუნ" "ხლუ(რ)ჩუნება" "ხლურჩუნება") 
	    ("ხლეჩ" "ხლეჩ(ვ)ა" "ხლეჩვა") 
	    ("ხვრეპ" "ხვრეპა-ქთ" "ხვრეპა") 
	    ("ხვიხვინ" "ხვიხვინი" "ხვიხვინი ბზw. -ხვიხვინება") 
	    ("ხვეჭ" "ხვეჭა-ქთ" "ხვეჭა") 
	    ("ხვეწ2" "ხვეწ(ვ)ა" "ხვეწა" "ხვეწა (|| ხვეწვა)") 
	    ("ხვეწ1" "ხვეწ(ვ)ა" "ხვეწა" "ხვეწა (|| ხვეწვა)") 
	    ("ხვეტ" "ხვეტ(ვ)ა" "ხვეტა" "ხვეტა (|| ხვეტვა)") 
	    ("ხვევ" "ხვევ(ნ)ა" "ხვევა" "ხვევა (ბისw. ხვევნა)") 
	    ("ხვედრ" "ხვდომა || ხვედრა" "ხვედრება") 
	    ("ხეხ" "ხეხ(ვ)ა" "ხეხვა") 
	    ("ხელმრუდე" "ხელმრუდ(ე)ობა" "ხელმრუდეობა") 
	    ("ხეთქ" "ხეთქ(ვ)ა" "ხეთქვა") 
	    ("ხდენ1" "ხდომა" "ხდომა] ბზw. ხდენა") 
	    ("ხარჯ" "ხარჯვა-ქთ" "ხარჯვა") 
	    ("ხარ1" "ხარება-ქთ" "ხარება") 
	    ("ხამ4" "-" "ხამ#") 
	    ("ხად1" "-" "ხად#")
	    ("ჭყეტ" "ჭყეტა-ქთ" "ჭყეტა") 
	    ("ჭრ" "ჭრა-ქთ" "ჭრა") 
	    ("ჭორიკანავ" "ჭორიკან(ა)ობა" "ჭორიკანაობა") 
	    ("ჭიმ" "ჭიმვა-ქთ" "ჭიმვა") 
	    ("ჭეჭყ" "ჭეჭყ(ვ)ა" "ჭეჭყვა") 
	    ("ჭექ2" "ჭექ(ვ)ა" "ჭექვა") 
	    ("ჭედ"  "ჭედ(ვ)ა" "ჭედვა" "ჭედვა (|| ჭედა)") 
	    ("წყმენდ" "წყმე(ნ)და-ქთ" "წყმენდა") 
	    ("წყმენდ" "წყმე(ნ)და" "წყმენდა") 
	    ("წყვეტ" "წყვეტა-ქთ" "წყვეტა") 
	    ("წურთნ" "წურთ(ვ)ნა" "წურთვნა") 
	    ("წონ2" "წონება-ქთ" "წონება") 
	    ("წოვ" "წოვება-ქთ" "წოვება") 
	    ("წნეხ" "წნეხ(ვ)ა" "წნეხვა") 
	    ("წმენდ" "წმენდა-ქთ" "წმენდა") 
	    ("წკეპვლ" "წკეპვლა-ქთ" "წკეპვლა") 
	    ("წირ" "წირვა-ქთ" "წირვა") 
	    ("წინასწარმეტყველ" "წინასწარმეტყველება (|| წინასწარმეტყველობა)" "წინასწარმეტყველება") 
	    ("წიდნ" "წიდ(ვ)ნა" "წიდვნა") 
	    ("წვეთ" "წვეთ(ვ)ა" "წვეთვა") 
	    ("წეწ" "წეწ(ვ)ა" "წეწვა") 
	    ("წევ1" "წევა-ქთ" "წევა") 
	    ("წანწკარ" "წანწკარი ბზw. -წანწკარება" "წანწკარი") 
	    ("წამ" "წამება-ქთ" "წამება") 
	    ("ძღოლ" "ძღოლ(ი)ება" "ძღოლიება") 
	    ("ძღვანი" "ძღვან(ი)ება" "ძღვანიება") 
	    ("ძრახ" "ძრახვა-ქთ" "ძრახვა") 
	    ("ძოვ" "ძოვება-ქთ" "ძოვება") 
	    ("ძოვ" "ძოვ(ნ)ა" "ძოვნა") 
	    ("ძლევ2" "ძლევა-ქთ" "ძლევა")
	    ("ძიძგილავ" "ძიძგილაობა ბზw. -ძიძგილავება" "ძიძგილაობა") 
	    ("ძახ1" "ძახება-ქთ" "ძახება") 
	    ("ძარცვ" "ძარცვა-ქთ" "ძარცვა") 
	    ("ცხ2" "ცხება-ქთ" "ცხება") 
	    ("ცურავ" "ცურვა ბზw. -ცურება" "ცურვა") 
	    ("ცრ2" "ცრა-ქთ" "ცრა") 
	    ("ცილ1" "ცილობა (|| ცილაობა || ცილება)" "ცილობა") 
	    ("ცივ" "ცი(ვ)ება" "ცივება") 
	    ("ცვლ" "ცვლა-ქთ" "ცვლა") 
	    ("ცვივნ" "ცვივნა || ცვენა" "ცვივნა") 
	    ("ცვარ" "დეს MV -" "ცვარ#") 
	    ("ცვ3" "ცმევა" "ცმა") 
	    ("ცეც" "ცეცება-ქთ" "ცეცება") 
	    ("ჩხრეკ" "ჩხრეკ(ვ)ა" "ჩხრეკვა") 
	    ("ჩხვლეტ" "ჩხვლეტ(ვ)ა" "ჩხვლეტვა") 
	    ("ჩქროლ" "ჩქროლ(ვ)ა" "ჩქროლვა") 
	    ("ჩქარ" "ჩქარება-ქთ" "ჩქარება")
	    ("ჩუჩხურ" "ჩუჩხური ბზw. -ჩუჩხურება" "ჩუჩხური") 
	    ("ჩუჩუნ" "ჩუჩუნი ბზw. -ჩუჩუნება" "ჩუჩუნი") 
	    ("ჩურჩუტ" "ჩურჩუტობა || ჩურჩუტი" "ჩურჩუტობა") 
	    ("ჩურჩულ" "ჩურჩული ბზw. -ჩურჩულება" "ჩურჩული") 
	    ("ჩლექ" "ჩლექა (|| ჩლექვა)" "ჩლექა") 
	    ("ჩეხ" "ჩეხ(ვ)ა" "ჩეხვა") 
	    ("ჩეჩ1" "ჩეჩ(ვ)ა" "ჩეჩვა") 
	    ("ჩერჩეტ" "ჩერჩეტობა || ჩერჩეტი" "ჩერჩეტობა") 
	    ("ჩერ" "ჩერება-ქთ" "ჩერება") 
	    ("ჩენ" "ჩენა" "ჩენა-ქთ") 
	    ("ჩეკ2" "ჩეკ(ვ)ა" "ჩეკვა") 
	    ("ჩეკ1" "ჩეკ(ვ)ა" "ჩეკვა") 
	    ("ჩადი" "ჩადენა" "ჩადენა)") 
	    ("ჩაგრ" "ჩაგვრა" "ჩაგვრა-ქთ") 
	    ("შურ1" "შურება (|| შურვება)" "შურება") 
	    ("შორ" "შორება" "შორება-ქთ") 
	    ("შოვნ" "შოვნა (|| შოვა)" "შოვნა") 
	    ("შვრ1" "ქმნა || ქნა)" "ქმნა") 
	    ("შველ" "შველიება" "შველება") 
	    ("ყრუ" "ყრუ(ვ)ება" "ყრუვება") 
	    ("ყრ" "ყრა" "ყრა-ქთ") 
	    ("ყოფ1" "ყოფა" "ყოფა-ქთ") 
	    ("ყოლ2" "ყოლება" "ყოლება-ქთ") 
	    ("ყლაპ" "ყლაპვა" "ყლაპვა-ქთ") 
	    ("ყიდულ" "ყიდვა (|| -ყიდვა)" "ყიდვა") 
	    ("ყვლეფ" "ყვლეფა" "ყვლეფა-ქთ") 
	    ("ყვინჩილ" "ყვინჩილ(ა)ობა" "ყვინჩილაობა") 
	    ("ყვან1" "ყვანა" "ყვანა-ქთ") 
	    ("ღიარ" "აღიარება (|| აღვიარება)" "აღიარება") 
	    ("ღვწ" "ღვწა || ღწვა" "ღვწა") 
	    ("ღეღ" "ღეღვა (|| ღეღა)" "ღეღვა") 
	    ("ღამ" "ღამება" "ღამება-ქთ") 
	    ("ღ1" "ღება" "ღება-ქთ") 
	    ("ქცევ1" "ქცევა" "ქცევა-ქთ") 
	    ("ქონ1" "ქონა || ქონება" "ქონა") 
	    ("ქმნ" "ქმნა (|| ქნა)" "ქმნა") 
	    ("ქიქინ" "ქიქინი ბზw. ქიქინება" "ქიქინი") 
	    ("ქილიკ" "ქილიკ(ა)ობა" "ქილიკაობა") 
	    ("ქექ" "ქექვა (|| ქექა)" "ქექვა") 
	    ("ქელ1" "ქელვა" "ქელვა-ქთ") 
	    ("ქარვ1" "ქარვება" "ქარვება-ქთ") 
	    ("ქადაგ" "ქადაგება (|| ქადაგობა)" "ქადაგება") 
	    ("ქად" "ქად(ნ)ება" "ქადნება") 
	    ("ფურჩქვნ" "ფურჩქ(ვ)ნა" "ფურჩქვნა") 
	    ("ფუილ" "ფუ(ვ)ება" "ფუვება") 
	    ("ფრენ" "ფრენა (|| ფრინვა)" "ფრენა") 
	    ("ფოფინ" "ფოფინი (|| ფოფინება)" "ფოფინი") 
	    ("ფეთქ1" "ფეთქვა (|| ფეთქა)" "ფეთქვა") 
	    ("ფართქალ" "ფართქალი ბზw. -ფართქალება" "ფართქალი") 
	    ("ფართო" "ფართო(ვ)ება" "ფართოვება") 
	    ("ფარ" "ფარება" "ფარება-ქთ") 
	    ("ფარ" "ფარვა" "ფარვა-ქთ") 
	    ("უთოვ" "უთოება || უთოობა" "უთოება") 
	    ("ტყუ" "ტყუ(ვ)ება" "ტყუვება") 
	    ("ტლინკ" "ტლინკვა || ტლინკაობა" "ტლინკვა") 
	    ("ტკრციალ" "ტკრციალი ბზw. -ტკრციალება" "ტკრციალი") 
	    ("ტკეც" "ტკეცა (|| ტკიცვა)" "ტკეცა") 
	    ("ტირ" "ტირება" "ტირილი") 
	    ("ტეხ" "ტეხა" "ტეხა (|| ტეხვა)") 
	    ("ტევ2" "ტევა" "ტევე") 
	    ("ტბორ" "ტბორება" "ტბორვა") 
	    ("სხეპ" "სხეპა" "სხეპა (|| სხეპვა)") 
	    ("სლუკუნ" "სლუკუნი" "სლუკუნი ბზw. -სლუკუნება") 
	    ("სლოკინ" "სლოკინი" "სლოკინი ბზw. -სლოკინება") 
	    ("საუბრ" "საუბარი" "საუბარი (|| საუბრობა ბზw. -საუბრება)") 
	    ("რიკ-ტაფელავ" "რიკ-ტაფელაობა" "რიკ") 
	    ("რეწ" "რეწა" "რეწა || რეწვა") 
	    ("რეცხ" "რეცხვა" "რეცხვა (|| რეცხა)") 
	    ("რეკ" "რეკ(ვ)ა" "რეკვა") 
	    ("რბ1" "რბენა" "რბენა || სირბილი") 
	    ("პრანჭია" "პრანჭი(ა)ობა" "პრანჭიაობა") 
	    ("პოვნ" "პოვება" "პოვება (|| პოება)") 
	    ("პატიჟ" "პატიჟება" "პატიჟება (|| პატიჟობა)") 
	    ("პატივ" "პატი(ვ)ება" "პატივება") 
	    ("ომ" "ომობა" "ომობა] ბზw. ომი") 
	    ("ნძრევ" "ნძრევა" "ნძრევა-ქთ") 
	    ("ნისიავ" "-" "ნისიავ#") 
	    ("ნერვიულ" "ნერვ(ი)ულება" "ნერვიულება") 
	    ("ნერვიულ" "ნერვ(ი)ულობა" "ნერვიულობა") 
	    ("ნახულ" "ნახვება" "ნახვება-ქთ") 
	    ("ნახულ" "ნახულობა" "ნახულობა ბზw. ნახვა") 
	    ("ნავარდ" "ნავარდობა" "ნავარდობა || ნავარდი") 
	    ("მუსრ" "მუსვრა" "მუსვრა (|| მუსრვა  )") 
	    ("მსახურ" "მსახურობა" "მსახურობა ბზw. მსახურება") 
	    ("მოჯამაგირე" "მოჯამაგირ(ე)ობა" "მოჯამაგირეობა") 
	    ("მოჩინარ" "-" "მოჩინარ#") 
	    ("მოსახლე" "მოსახლეობა" "მოსახლეობა (|| მოსახლობა)") 
	    ("მეწისქვილე" "მეწისქვილ(ე)ობა" "მეწისქვილეობა") 
	    ("მეკობრე" "მეკობრ(ე)ობა" "მეკობრეობა") 
	    ("მდინარე" "მინარე(ო)ბა" "მინარეობა") 
	    ("მბობ" "უბნობა" "უბნობა  ") 
	    ("მარტოვ" "მარტო(ვ)ება" "მარტოვება") 
	    ("მამლაყინწა" "მამლაყინწ(ა)ობა" "მამლაყინწაობა") 
	    ("ლეწ" "ლეწვა" "ლეწვა (|| ლეწა)") 
	    ("ლეს" "ლესვა" "ლესვა (|| ლესა)") 
	    ("ლაციც" "ლაციცი" "ლაციცი (|| ლაციცობა)") 
	    ("ლაქლაქ" "ლაქლაქი" "ლაქლაქი (|| ლაქლაქობა)") 
	    ("ლასლას" "ლასლასი" "ლასლასი (-ლასლასება)") 
	    ("ლაზღანდარ" "ლაზღანდარ(ავ)ება" "ლაზღანდარავება") 
	    ("ლაზღანდარ" "ლაზღანდარ(ა)ობა" "ლაზღანდარაობა") 
	    ("კუნტრუშ" "კუნტრუში" "კუნტრუში (|| კუნტრუშობა)") 
	    ("კუთვნ" "კუთვნება" "კუთვნება (|| კუთვნა  )") 
	    ("კრუხ" "კრუხვა" "კრუხვა (|| კრუხობა)") 
	    ("კრ" "კვრა" "კვრა (|| კრვა  )") 
	    ("კოწიაწ" "კოწიაწი" "კოწიაწი (|| კოწიაწობა)") 
	    ("კლაკნ" "კლაკნა" "კლაკნა (|| კლაკვნა)") 
	    ("კისრულ" "კისრება" "კისრება (|| კისრულობა)") 
	    ("კიაფ" "კიაფობა" "კიაფობა || კიაფი") 
	    ("კვეც" "კვეცა" "კვეცა (|| კვეცვა)") 
	    ("კვეს" "კვესვა" "კვესვა (|| კვესა)") 
	    ("კეც" "კეცვა" "კეცვა (|| კეცა)") 
	    ("კენკ" "კენკვა" "კენკვა (|| კენკა)") 
	    ("კაკუნ" "კაკუნება" "კაკუნება || კაკუნი") 
	    ("კაზმ" "კაზმვა" "კაზმვა (|| კაზმა)") 
	    ("ია-ვარდ" "ია-ვარდობა" "ია") 
	    ("თხოვნ" "თხოვნა" "თხოვნა || თხოვა") 
	    ("თუთი" "თუთი(ავ)ება" "თუთიავება") 
	    ("თნევ" "თნევა" "თნევა (|| თნება)") 
	    ("თეს" "თესვა" "თესვა || თესა") 
	    ("თამაშ" "თამაშება" "თამაში" "თამაში || თამაშობა" "თამაშობა") 
	    ("ზრ" "ზვრა" "ზვრა (|| ზრვა)") 
	    ("ზომ" "ზომება" "ზომვა") 
	    ("ზოზინ" "ზოზინი" "ზოზინი || ზოზინობა") 
	    ("ზვერ" "ზვერვა" "ზვერვა (|| ზვერა)") 
	    ("ზელ" "ზელა" "ზელა (|| ზელვა)") 
	    ("ვლენ" "ვლენა" "ვლენა (|| ვლინება)") 
	    ("ვარდისფერ" "ვარდისფ(ე)რება" "ვარდისფერება") 
	    ("ვალალ" "ვალალება" "ვალალება (|| ვალალობა)") 
	    ("ვაგლახ" "ვაგლახება" "ვაგლახება (|| ვაგლახობა)") 
	    ("დრო" "დრო(ვ)ება" "დროვება") 
	    ("დედ2" "დედვა" "დედ") 
	    ("დედ1" "დედება" "დედ") 
	    ("დგ1" "დგმა" "დგ") 
	    ("გულ" "გულ(ვ)ება" "გულვება") 
	    ("გორ" "გორვა" "გორვა (|| გორაობა)") 
	    ("გმირ2" "გმირობა" "გმირ") 
	    ("გმირ1" "გმირვა" "გმირ") 
	    ("გვარ1" "გვარება" "გვარ") 
	    ("გ" "გება" "გება-ქთ") 
	    ("გდ" "გდება" "გდება-ქთ") 
	    ("ბღლაძუნ" "ბღლაძუნობა" "ბღლაძუნობა || ბღლაძუნი") 
	    ("ბუქნ" "ბუქნა" "ბუქნა || ბუქნაობა") 
	    ("ბუბვნ" "ბუბ(ვ)ნა" "ბუბვნა") 
	    ("ბუ1" "ბუება" "ბუ") 
	    ("ბრწყინ" "ბრწყინ(ვ)ება" "ბრწყინვება") 
	    ("ბრძ2" "ბრძვა" "ბრძ") 
	    ("ბრეგ" "ბრეგ(ვ)ა" "ბრეგვა") 
	    ("ბორძიკ" "ბორძიკი" "ბორძიკი || ბორძიკობა" "ბორძიკობა") 
	    ("ბორძიკ" "ბორძიკი" "ბორძიკი || ბორძიკობა") 
	    ("ბლაგვ" "ბლაგვა" "ბლაგვა (იმერ.)") 
	    ("ბირ1" "ბირება" "ბირ") 
	    ("ბზუ1" "ბზუება" "ბზუ") 
	    ("ბერტყ2" "ბერტყება" "ბერტყ") 
	    ("ბერტყ1" "ბერტყვა" "ბერტყ") 
	    ("ბერ3" "ბერობა" "ბერ") 
	    ("ბერ2" "ბერება" "ბერ") 
	    ("ბერ1" "ბერვა" "ბერ") 
	    ("ბარგან" "ბარგანი" "ბარგანი || ბარგანაობა") 
	    ("ბარგ2" "ბარგვა" "ბარგ") 
	    ("ბარგ1" "ბარგება" "ბარგ") 
	    ("ბარ2" "ბარვა" "ბარ") 
	    ("ბანდ2" "ბანდება" "ბანდ") 
	    ("ბანდ1" "ბანდვა" "ბანდ") 
	    ("ბაას" "ბაასი" "ბაასი (|| ბაასობა)") 
	    ("ახლ1" "ახლება" "ახლ") 
	    ("ანგარიშ" "ანგარიში" "ანგარიში, ანგარიშობა") 
	    ("ალერს" "ალერსი" "ალერსი, ალერსება") 
	    ("აზროვნ" "აზროვნება" "აზროვნება (|| აზროვნობა)")))
  (setf (gethash (car root+vn) *vn-table*)
	(cdr root+vn)))



:eof
