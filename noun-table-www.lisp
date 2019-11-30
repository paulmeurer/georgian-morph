;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10 -*-

;; paul.meurer@uni.no
;; Uni Research
;; http://www.uni.no/

;; Extracted morph:



;; write-fst-participle-stems-sql()
;; write-fst-noun-stems-sql()

(in-package :fst)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf clsql-sys::*original-readtable* nil)
  (enable-sql-reader-syntax))

#.(locally-enable-sql-reader-syntax)

;;#+test
(set-macro-character clsql-sys::*sql-macro-open-char* #'clsql-sys::sql-reader-open)
;;#+test
(set-macro-character clsql-sys::*sql-macro-close-char* (get-macro-character #\)))

;; FIXME: add [corr]!
(def-view-class extracted-morph ()
  ((source :initform nil :initarg :source :reader source :type string :db-kind :key)
   (word :initform nil :initarg :word :reader word :type string :db-kind :key)
   (norm :initform nil :initarg :norm :reader norm :type string)
   (lemma :initform nil :initarg :lemma :reader lemma :type string) ;; :db-kind :key)
   (code :initform nil :initarg :code :reader code :type string)
   (pos :initform nil :initarg :pos :reader part-of-speech :type string)
   (features :initform nil :initarg :features :reader features :type string)
   (inflects-like :initform nil :initarg :features :reader features :type string)
   (typo :initform nil :initarg :typo :reader typo :type boolean)
   (class :initform nil :initarg :class :reader verb-class :type string)
   (present :initform nil :initarg :present :reader features :type string)
   (future :initform nil :initarg :future :reader future :type string)
   (aorist :initform nil :initarg :aorist :reader aorist :type string)
   (perfect :initform nil :initarg :perfect :reader perfect :type string)
   (comment :initform nil :initarg :comment :reader comment :type string)
   (date :initform nil :initarg :date :reader date :type string))
  (:base-table [morph extracted-morph]))

#+once
(execute-command "alter table morph.extracted_morph add norm varchar ;")
#+once
(execute-command "alter table morph.extracted_morph add inflects_like varchar ;")
#+once
(execute-command "alter table morph.extracted_morph add class varchar ;")
#+once
(execute-command "alter table morph.extracted_morph add present varchar ;")
#+once
(execute-command "alter table morph.extracted_morph add future varchar ;")
#+once
(execute-command "alter table morph.extracted_morph add aorist varchar ;")
#+once
(execute-command "alter table morph.extracted_morph add vn varchar ;")
#+once
(execute-command "alter table morph.extracted_morph add typo boolean ;")
#+once
(execute-command "alter table morph.extracted_morph rename vn to comment ;")
#+once
(execute-command "alter table morph.extracted_morph add perfect varchar ;")

#+ignore
(defparameter *extracted-morph*
  (select [*] :from [morph extracted-morph]))

#+test
(print (select [*] :from [morph extracted-morph]))
#+ignore
(with-transaction ()
  (dolist (row *extracted-morph*)
    (unless (select [*] :from [morph extracted-morph]
		    :where [and [= [source] (car row)]
				[= [word] (cadr row)]])
      (insert-records :into [morph extracted-morph]
		      :attributes '([source] [word] [lemma] [code] [pos] [features] [date] [norm])
		      :values row))))

#+once
(with-transactionxx ()
  (u:with-file-fields ((&rest row) "projects:georgian-morph;lists;finance.txt"
		       :separator #\, :empty-to-nil t)
    (Print row)
    (unless (char= (char (car row) 0) #\#)
      (insert-records :into [morph noun-features]
		      :attributes '([stem] [code] [pos] [sub-id] [features] [style-features]
				    [template] [comment] [author] [translation] [date] [lang])
		      :values row))))

#+test
(u:with-file-fields ((stem code pos sub-id features &rest row)
		     "projects:georgian-morph;lists;finance.txt"
		     :separator #\, :empty-to-nil t)
  ;;(print row)
  (unless (char= (char stem 0) #\#)
    (when (equal features "+N+Anim+Qual")
      (write-line stem))
    ))

#+once
(create-view-from-class 'extracted-morph)
#+test
(drop-tablexx [morph extracted-morph])

;;  pg_dump -U gnc gnc > gnc-2014-08-01.sql

;; import:

;; psql -U postgres -d gnc < gnc-2014-08-01.sql

;; run this before import:

#+test
(prognxx
  (drop-table [morph fullform])
  (drop-table [morph noun-features])
  (drop-table [morph participle])
  (drop-table [morph pv-variant])
  (drop-table [morph related-verbal-nouns])
  (drop-table [morph super-paradigm])
  (drop-table [morph verb-features])
  (drop-table [morph verb-translation])
  (drop-table [morph verbal-noun])
  (drop-table [morph xle-template]))

;; paradigm completion in paradigm-completion.lisp


#+once
(with-database-connection ("localhost/gnc/gnc/kartuli" :database-type :postgresql)
  (let ((pg *default-database*))
    (with-database-connection ("localhost/kartuli/treebank/Heebi" :database-type :mysql)
      (do-query ((stem code pos sub-id features style-features template comment author translation date)
		 [select [stem] [code] [pos] [sub-id] [features] [style-features]
			 [template] [comment] [author] [translation] [date]
			 :from [noun-features]
			 ;;:limit 1
			 ])
	(let ((*default-database* pg))
	  (insert-records
	   :into [morph noun-features]
	   :attributes (list [stem] [code] [pos] [sub-id] [features] [style-features] [template] [comment] [author] [translation] [date])
	   :values (list (convert-encoding stem :amirani :unicode)
			 code pos sub-id features style-features template comment author (encoding::utf-8-decode translation) date)))))))

#+test
(print (length (noun-features "გ*")))

(defun noun-features (search-stem &key exact-p max-rows)
  (collecting
    (block query
      (do-query ((&rest row)
		 [select [*]
			 :from [morph noun-features]
			 :where (if exact-p
				    [= [stem] ?search-stem]
				    [like [stem] (substitute #\% #\* search-stem)])
			 :order-by [stem]
			 :limit 100])
	(when (and (not (null max-rows)) (= (decf max-rows) -1))
	  (return-from query))
	;;(debug row)
	(collect (copy-seq row))))))

(define-url-function noun-features-xml
    (request (search-stem)
	     :xsl #'noun-features-xsl
	     :path (concatenate 'string "/kartuli/noun-features.xml"))
  #m((nouns :search-stem #s search-stem)))

#+test ;; strange error
(debug (select [stem] [code] [pos] [features] [style-features] [lang] [template] [comment]
	       :from [morph noun-features]
	       :where [and [like [stem] "სეფე-წულ"]]))

(define-url-function js/noun-features-xml
    (request (search-stem action stem-key stem code pos features style-features lang template comment)
	     :xsl #'js/noun-features-xsl
	     :write-doctype-p nil ;; no HTML doctype
	     :path "/kartuli/js/noun-features.xml")
  (print (request-query request))
  (with-database-connection ()
    (when action
      (when (equal stem "-") (setf stem nil))
      (destructuring-bind (old-stem old-code old-pos sub-id) (split stem-key #\@)
	(print (list old-stem old-code old-pos sub-id))
	(destructuring-bind (old-features old-style-features old-lang old-template old-comment)
	    (car (debug (select [features] [style-features] [lang] [template] [comment]
			 :from [morph noun-features]
			 :where [and [= [stem] ?old-stem]
				     [= [code] ?old-code]
				     [= [pos] ?old-pos]])))
	  (cond ((equal features "-")
		 (setf features old-features))
		((equal features "*")
		 (setf features "")))
	  (cond ((equal style-features "-")
		 (setf style-features old-style-features))
		((equal style-features "*")
		 (setf style-features "")))
	  (cond ((equal template "-")
		 (setf template old-template))
		((equal template "*")
		 (setf template "")))
	  (when (equal code "-") (setf code nil))
	  (when (equal pos "-") (setf pos nil))
	  (when (equal comment "-") (setf comment nil))
	  (when (equal template "-") (setf template nil))
	  (ecase (intern (string-upcase action) :keyword)
	    (:add
	     (let* ((sub-ids (select [sub-id]
				     :from [morph noun-features] :flatp t
				     :where [and [= [stem] (or stem old-stem)]
						 [= [code] (or code old-code)]
						 [= [pos] (or pos old-pos)]]))
		    (new-sub-id (loop for i from 1
				   when (not (find i sub-ids))
				   do (return i))))
	       #+debug
	       (print (list :insert (or stem old-stem) (or code old-code) (or pos old-pos) new-sub-id
			    (substitute #\+ #\space features) template comment
			    "PM" (get-universal-time)))
	       (with-transaction ()
		 (insert-records :into [morph noun-features]
				 :attributes (list [stem] [code] [pos] [sub-id]
						   [features] [template] [comment]
						   [author] [date])
				 :values (list (or stem old-stem) (or code old-code) (or pos old-pos) new-sub-id
					       (substitute #\+ #\space features) template comment
					       "PM" (get-universal-time))))))
	    (:change
	     #+debug
	     (print (list :update old-stem old-code (or pos old-pos) sub-id
			  :sf style-features
			  (substitute #\+ #\space features) template comment
			  "PM" (get-universal-time)))
	     (with-transaction ()
	       (update-records [morph noun-features]
			       :attributes (list [code] [pos] [features] [style-features]
						 [lang] [template] [comment] [date])
			       :values (list (or code old-code) (or pos old-pos)
					     (substitute #\+ #\space features)
					     (when style-features (substitute #\+ #\space style-features))
					     lang
					     template
					     (or comment
						 (cond ((null old-comment)
							"PM")
						       ((search "PM" old-comment)
							old-comment)
						       (t
							(u:concat old-comment ", PM"))))
					     (get-universal-time))
			       :where [and [= [stem] ?old-stem]
					   [= [code] ?old-code]
					   [= [pos] ?old-pos]
					   [= [sub-id] ?sub-id]])))
	    (:delete
	     (with-transaction ()
	       (delete-records :from [morph noun-features]
			       :where [and [= [stem] ?old-stem]
					   [= [code] ?old-code]
					   [= [pos] ?old-pos]
					   [= [sub-id] ?sub-id]] )))))))
      (let ((noun-features-list
	     (if (and (> (length search-stem) 0)
		      (char= (char search-stem (1- (length search-stem))) #\#))
		 (noun-features (substitute #\% #\* (subseq search-stem 0 (1- (length search-stem)))) :max-rows 1000)
		 (noun-features (concat (substitute #\% #\* search-stem) "%") :max-rows 1000))))
	#m((nouns :search-stem #s search-stem :count #s (length noun-features-list))
	   #L(dolist (noun-features noun-features-list)
	       (destructuring-bind (stem code pos sub-id features style-features template
					 comment author translation date lang) noun-features
		 #m((noun :stem #s stem
			  :code #s code
			  :pos #s pos
			  :sub-id #s sub-id
			  :features #s features
			  :style-features #s style-features
			  :lang #s lang
			  :template #s template
			  :comment #s comment
			  :author #s author
			  :translation #s translation
			  :date #s (u::format-universal-time date nil)
			  ))))))))

#+test
(print (select [stem] :from [morph noun-features] :where [= [code] ""]))

#+test
(update-records [noun-features] :attributes '([date]) :values (list (get-universal-time))
		:where [null [date]])

#+test
(update-records [noun-features] :attributes '([features]) :values (list "+Prop")
		:where [and [null [features]] [= [pos] "N"]])

;;(u::now)
#+test
(with-transaction ()
  (insert-records :into [noun-features]
		  :attributes (list [stem] [code] [pos] [sub-id]
				    [features] [template] [comment]
				    [author] [date])
		  :values (list "proPesor" "A" "n" 1
				nil nil nil
				"PM" (get-universal-time))))


(define-javascript-writer js/noun-features (stream)
  #j((defvar chosen-row -1)
     (defvar chosen-row-id nil)
     (defvar chosen-row-class-name nil) ;; class name for restoring
     
     (defun noun-features ()
       (when (and (not event.ctrl-key)
		  (not (= event.key-code 17))
		  (not (= event.key-code 37))
		  (not (= event.key-code 38))
		  (not (= event.key-code 39))
		  (not (= event.key-code 40)))
	 (let* ((doc window.parent.document)
		(noun-features-table (doc.get-element-by-id "noun-features-table"))
		(search-stem (doc.get-element-by-id "search-stem"))
		(req (new (XMLHttpRequest))))
	   (req.open "get" (+ "js/noun-features.xml?search-stem="
			      (encodeURIComponent search-stem.value))
		     false)
	   (req.send "")
	   (when (not (= req.responseText ""))
	     (setf noun-features-table.innerHTML req.responseText)))))
     
     (defun display-choices (stem-key type comment)
       ;;(alert (+ stem-key ":" type ":" comment))
       ;;(alert (+ type "@" stem-key))
       (let* ((doc window.parent.document)
	      (td (doc.get-element-by-id (+ type "@" stem-key)))
	      (edit-td (doc.get-element-by-id (+ "edit@" stem-key)))
	      (req (new (XMLHttpRequest))))
	 (when type
	   (req.open "get" (+ "js/feature-choices.xml?type=" type
			      "&stem-key=" stem-key "&comment=" (encodeURIComponent comment))
		     false)
	   (req.send "")
	   ;;(alert req.responseText)
	   ;;(alert td)
	   (when (not (= req.responseText ""))
	     (setf td.innerHTML req.responseText)
	     (setf td.onclick "")))
	 ;;(alert (+ stem-key type comment))
	 (req.open "get" (+ #L(concatenate 'string "/" *url-base* "/js/edit.xml?stem-key=") stem-key "&type=" type "&comment=" (encodeURIComponent comment))
		   false)
	 (req.send "")
	 ;;(alert req.responseText)
	 (setf edit-td.outerHTML req.responseText)))
     
     (defun change-entry (stem-key action)
       ;;(alert (+ stem-key " " action))
       (let ((doc window.parent.document)
	     (noun-features-table (doc.get-element-by-id "noun-features-table"))
	     (stem-td (doc.get-element-by-id (+ "stem:" stem-key)))
	     (stem "-")
	     (code-td (doc.get-element-by-id (+ "code:" stem-key)))
	     (code "-")
	     (pos-td (doc.get-element-by-id (+ "pos:" stem-key)))
	     (pos "-")
	     (features-td (doc.get-element-by-id (+ "features:" stem-key)))
	     (features "-")
	     (style-features-td (doc.get-element-by-id (+ "style-features:" stem-key)))
	     (style-features "-")
	     (lang-td (doc.get-element-by-id (+ "lang:" stem-key)))
	     (lang "*")
	     (template-td (doc.get-element-by-id (+ "template:" stem-key)))
	     (template "-")
	     (comment-td (doc.get-element-by-id (+ "comment:" stem-key)))
	     (comment "-")
	     (search-stem (doc.get-element-by-id "search-stem"))
	     (req (new (XMLHttpRequest))))
	 ;;(alert style-features-td)
	 (when stem-td
	   (setf stem (encodeURIComponent stem-td.value)))
	 (when features-td
	   (setf features (encodeURIComponent features-td.value)))
	 (when style-features-td
	   (setf style-features (encodeURIComponent style-features-td.value)))
	 (when lang-td
	   (setf lang (encodeURIComponent lang-td.value)))
	 (when code-td
	   (setf code (encodeURIComponent code-td.value)))
	  (when pos-td
	    (setf pos (encodeURIComponent pos-td.value)))
	 (when template-td
	   (setf template (encodeURIComponent template-td.value)))
	 (when comment-td
	   (setf comment (encodeURIComponent comment-td.value)))
	 (req.open "get"
		   (+ "js/noun-features.xml?search-stem="
		      (encodeURIComponent search-stem.value)
		      "&action=" action
		      "&stem-key=" stem-key
		      "&stem=" stem
		      "&code=" code
		      "&pos=" pos
		      "&features=" features
		      "&style-features=" style-features
		      "&lang=" lang
		      "&template=" template
		      "&comment=" comment)
		   false)
	 (req.send "")
	 (when (not (= req.responseText ""))
	   ;;(alert req.responseText)
	   (setf noun-features-table.innerHTML req.responseText))
	 #+ignore
	 (alert (+ features-td.first-child.value " " template-td.first-child.value))))

     (defvar focus-row false)
     
     (defun menu-shortcut (event)
       (let* ((doc window.parent.document)
	      (features-table (doc.get-element-by-id "features-table"))
	      (rows (document.get-elements-by-name "noun-row")))
	 #+ignore-yet
	 (when event.ctrl-key
	   ;;(alert event)
	   (cond ((= event.key-code 40)	;; arrow-down
		  (when (< chosen-row (- rows.length 1))
		    (when (< -1 chosen-row)
		      (let ((row (aref rows chosen-row)))
			(setf row.class-name chosen-row-class-name)))
		    (incf chosen-row)
		    (let ((row (aref rows chosen-row)))
		      (setf chosen-row-class-name row.class-name)
		      (setf row.class-name "row-focus")
		      (when (> (obj-position row) document.body.client-height)
			(row.scroll-into-view false)))))
		 ((= event.key-code 38)	;; arrow-up
		  (when (< -1 chosen-row)
		    (let ((row (aref rows chosen-row)))
		      (setf row.class-name chosen-row-class-name)))
		  (decf chosen-row)
		  (let ((row (aref rows chosen-row)))
		    (setf chosen-row-class-name row.class-name)
		    (setf row.class-name "row-focus")
		    ))
		 ))))))

(defparameter *feature-list* (cons "*" (select [features] :distinct t
					       :flatp t
					       :where [not [null [features]]]
					       :from [morph noun-features] :order-by `([features]))))

(defparameter *style-feature-list* (print (select [style-features] :distinct t
						  :flatp t
						  :where [not [null [style-features]]]
						  :from [morph noun-features] :order-by `([style-features]))))

#+test
(push "+Foreign" *style-feature-list*)

#+test
(push "+Pron+Int" *feature-list*)
#+test
(push "+Prop+Place" *feature-list*)
#+test
(push "+Prop+Title" *feature-list*)
#+test
(push "+POSSint" *feature-list*)
#+test
(push "+Prop+Ignore" *feature-list*)
#+test
(push "+Prop+Name+Zoon" *feature-list*)

#+ignore
(update-recordsxx [morph noun-features]
		:av-pairs '(([pos] "n"))
		:where [= [stem] "%"])

(define-url-function js/feature-choices-xml
    (request (type stem-key comment)
	     :xsl #'js/feature-choices-xsl
	     :write-doctype-p nil ;; no HTML doctype
	     :path (concatenate 'string "/" *url-base* "/js/feature-choices.xml"))
  ;;(print (list :stem-key stem-key))
  ;;(print (request-query request))
  (destructuring-bind  (stem old-code old-pos sub-id) (split stem-key #\@)
    (ecase (intern (string-upcase type) :keyword)
      (:stem
       #m((cell :type "text" :id #s (format nil "stem:~a" stem-key))
	  (value #s stem)))
      (:pos
       (let ((values (list "a" "ad" "n" #+nil "hv" "int" "p.a." "p.f." "p.n." "p.p.")))
	 #m((cell :type "list" :id #s (format nil "pos:~a" stem-key))
	    #s(dolist (value values)
		#m(value #s value)))))
      (:code
       (let ((values '(("A" "კაცი - კაცის - კაცები")
		       ("A1")
		       ("B" "კედელი - კედლის - კედლები")
		       ("C")
		       ("D" "ქვეყანა - ქვეყნის - ქვეყნები")
		       ("D1" "ქვეყანა - ქვეყ(ა)ნის - ქვეყნები")
		       ("F" "მხარე - მხრის - მხრები")
		       ("I" "ძმა - ძმის - ძმები")
		       ("K" "გოგონა - გოგონას - გოგონები")
		       ("M" "ხე - ხის -ხეები")
		       ("M1" "დღე - დღეის")
		       ("O" "სოკო - სოკოს - სოკოები")
		       ("O1" "ღვინო")
		       ("O2" "ღვინო")
		       ("O3" "დრო - დროის")
		       ("P" "ნიორი - ნივრის - ნივრები")
		       ("R" "ხატვა - ხატვის")
		       ("U" "ხუცესი - ხუცის - ხუცები")
		       ("X" "ტრამვაი - ტრამვაის - ტრამვაები")
		       ("Z" "დავით - დავითის")
		       ("H" "აშშ - აშშ-ის")
		       )))
	 #m((cell :type "list" :id #s (format nil "code:~a" stem-key))
	    #s(loop for (value example) in values
		 do #m((value :example #s example)
		       #s value)))))
      (:template
       (let ((values (list "" "NOUN-COMP"))) ;; NOUN-COMP: nouns having a COMP argument
	 #m((cell :type "list" :id #s (format nil "template:~a" stem-key))
	    #s(dolist (value values)
		#m(value #s value)))))
      (:features
       #m((cell :type "list" :id #s (format nil "features:~a" stem-key))
	  #s(dolist (features *feature-list*)
	      #m(value #s (or features "")))))
      (:style-features
       #m((cell :type "list" :id #s (format nil "style-features:~a" stem-key))
	  #s(dolist (style-features *style-feature-list*)
	      #m(value #s (or style-features "")))))
      (:lang
       #m((cell :type "list" :id #s (format nil "lang:~a" stem-key))
	  #s(dolist (lang '("ng" "mg" "og"))
	      #m(value #s lang))))
      (:comment
       #m((cell :type "text" :id #s (format nil "comment:~a" stem-key))
	  (value #s comment))))))

;;(print (select [pos] :distinct t :flatp t :from [noun-features] :order-by `([pos])))
;;(print (select [code] :distinct t :flatp t :from [noun-features] :order-by `([code])))

(defstylesheet js/feature-choices-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")
     ((xsl:template :match "//cell[@type='list']")
      ((xsl:element :name "select")
       ((xsl:attribute :name "name") "frame") ;; ?? (xsl:value-of/ :select "@unique-id") "@" (xsl:value-of/ :select "@feature"))
       ((xsl:attribute :name "id") (xsl:value-of/ :select "@id"))
       (xsl:apply-templates/ :select "value" :mode "list")))
     
     ((xsl:template :match "//cell[@type='text']")
      ((input :type "text" :spellcheck "false" :style "font-size: 12pt")
       ((xsl:attribute :name "id") (xsl:value-of/ :select "@id"))
       ((xsl:attribute :name "name") "stem")
       ((xsl:attribute :name "value") (xsl:value-of/ :select "value"))))
     
     ((xsl:template :match "value" :mode "list")
      ((xsl:element :name "option")
       ((xsl:attribute :name "title") (xsl:value-of/ :select "@example"))
       ((xsl:attribute :name "value") (xsl:value-of/ :select "."))
       #+ignore-yet
       ((xsl:if :test "../@chosen-value=.")
	((xsl:attribute :name "selected") "true"))
       (xsl:value-of/ :select ".")
       ((xsl:if :test "@example")
	" - "  (xsl:value-of/ :select "@example"))
       ))))

(define-url-function js/edit-xml
    (request (stem-key type comment)
	     :xsl #'js/edit-xsl
	     :write-doctype-p nil ;; no HTML doctype
	     :path (concatenate 'string "/" *url-base* "/js/edit.xml"))
  ;;(print (request-query request))
  #m(edit/ :stem-key #s stem-key :comment #s comment))

(defstylesheet js/edit-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")
     ((xsl:template :match "/edit")
      (td ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "button")
	   ((xsl:attribute :name "name") "add")
	   ((xsl:attribute :name "value") "Add")
	   ((xsl:attribute :name "onclick")
	    "changeEntry('" (xsl:value-of/ :select "@stem-key") "', 'add', '"
	    (xsl:value-of/ :select "@comment") "')"))
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "button")
	   ((xsl:attribute :name "name") "change")
	   ((xsl:attribute :name "value") "Change")
	   ((xsl:attribute :name "onclick")
	    "changeEntry('" (xsl:value-of/ :select "@stem-key") "', 'change', '"
	    (xsl:value-of/ :select "@comment") "')"))
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "button")
	   ((xsl:attribute :name "name") "delete")
	   ((xsl:attribute :name "value") "Delete")
	   ((xsl:attribute :name "onclick")
	    "changeEntry('" (xsl:value-of/ :select "@stem-key") "', 'delete', '"
	    (xsl:value-of/ :select "@comment") "')"))))))

(defstylesheet js/noun-features-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")     
     ((xsl:template :match "/nouns")
      (div (xsl:value-of/ :select "@count")
	   (xsl:choose
	    ((xsl:when :test "@count = 1")
	     " entry found.")
	    (xsl:otherwise
	     " entries found (max. 1000).")))
      ((table :id "features-table")
       (tr ((td :class "title")	;; :style "text-align: right")
	    ((xsl:element :name "a")
	     ;;((xsl:attribute :name "class") "stem")
	     "Stem"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "Code"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "POS"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "SubId"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "Features"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "StyleFeatures"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "Lang"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "Template"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "Edit"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "Date"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "Translation"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "Comment"))
	   ((td :class "title")
	    ((xsl:element :name "a")
	     "Author")))
       (xsl :apply-templates/ :select "noun")))
     
     ((xsl:template :match "noun")
      ((xsl:element :name "tr")
       ((xsl:attribute :name "name") "noun-row")
       ((xsl:attribute :name "class")
	(xsl:choose
	 ((xsl:when :test "count(preceding-sibling::*) mod 2 = 0")
	  "row-lightblue")
	 (xsl:otherwise
	  "row")))
       #+ignore
       ((td)
	((xsl:attribute :name "style") "font-family: Amirani; font-size: 12pt") 
	(xsl:value-of/ :select "@stem"))
       
       ((xsl:element :name "td")
	((xsl:attribute :name "style") "font-family: Amirani; font-size: 12pt") 
	((xsl:attribute :name "class")
	 (xsl:choose
	  ((xsl:when :test "count(preceding-sibling::*) mod 2 = 0")
	   "value-lightblue")
	  (xsl:otherwise
	   "value")))
	((xsl:attribute :name "id")
	 "stem@" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id"))
	((xsl:attribute :name "onclick")
	 "displayChoices('" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id") "', 'stem', '" (xsl:value-of/ :select "@comment") "')")
	(xsl:value-of/ :select "@stem"))
       
       ((xsl:element :name "td")
	((xsl:attribute :name "class")
	 (xsl:choose
	  ((xsl:when :test "count(preceding-sibling::*) mod 2 = 0")
	   "value-lightblue")
	  (xsl:otherwise
	   "value")))
	((xsl:attribute :name "id")
	 "code@" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id"))
	((xsl:attribute :name "onclick")
	 "displayChoices('" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id") "', 'code', '" (xsl:value-of/ :select "@comment") "')")
	(xsl:value-of/ :select "@code"))
       
       ((xsl:element :name "td")
	((xsl:attribute :name "class")
	 (xsl:choose
	  ((xsl:when :test "count(preceding-sibling::*) mod 2 = 0")
	   "value-lightblue")
	  (xsl:otherwise
	   "value")))
	((xsl:attribute :name "id")
	 "pos@" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id"))
	((xsl:attribute :name "onclick")
	 "displayChoices('" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id") "', 'pos', '" (xsl:value-of/ :select "@comment") "')")
	(xsl:value-of/ :select "@pos"))
       
       ((td)
	(xsl:value-of/ :select "@sub-id"))
       ((xsl:element :name "td")
	((xsl:attribute :name "class")
	 (xsl:choose
	  ((xsl:when :test "count(preceding-sibling::*) mod 2 = 0")
	   "value-lightblue")
	  (xsl:otherwise
	   "value")))
	((xsl:attribute :name "id")
	 "features@" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id"))
	((xsl:attribute :name "onclick")
	 "displayChoices('" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id") "', 'features', '" (xsl:value-of/ :select "@comment") "')")
	(xsl:value-of/ :select "@features"))
       ((xsl:element :name "td")
	((xsl:attribute :name "class")
	 (xsl:choose
	  ((xsl:when :test "count(preceding-sibling::*) mod 2 = 0")
	   "value-lightblue")
	  (xsl:otherwise
	   "value")))
	((xsl:attribute :name "id")
	 "style-features@" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id"))
	((xsl:attribute :name "onclick")
	 "displayChoices('" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id") "', 'style-features', '" (xsl:value-of/ :select "@comment") "')")
	(xsl:value-of/ :select "@style-features"))
       ((xsl:element :name "td")
	((xsl:attribute :name "class")
	 (xsl:choose
	  ((xsl:when :test "count(preceding-sibling::*) mod 2 = 0")
	   "value-lightblue")
	  (xsl:otherwise
	   "value")))
	((xsl:attribute :name "id")
	 "lang@" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id"))
	((xsl:attribute :name "onclick")
	 "displayChoices('" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id") "', 'lang', '" (xsl:value-of/ :select "@comment") "')")
	(xsl:value-of/ :select "@lang"))
       ((xsl:element :name "td")
	((xsl:attribute :name "class")
	 (xsl:choose
	  ((xsl:when :test "count(preceding-sibling::*) mod 2 = 0")
	   "value-lightblue")
	  (xsl:otherwise
	   "value")))
	((xsl:attribute :name "id")
	 "template@" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id"))
	((xsl:attribute :name "onclick")
	 "displayChoices('" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id") "', 'template', '" (xsl:value-of/ :select "@comment") "')")
	(xsl:value-of/ :select "@template"))
       ((xsl:element :name "td")
	((xsl:attribute :name "class")
	 (xsl:choose
	  ((xsl:when :test "count(preceding-sibling::*) mod 2 = 0")
	   "value-lightblue")
	  (xsl:otherwise
	   "value")))
	((xsl:attribute :name "id")
	 "edit@" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id"))
	((xsl:attribute :name "onclick")
	 "displayChoices('" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id") "', 'edit', '" (xsl:value-of/ :select "@comment") "')")
	"")
       ((td)
	(xsl:value-of/ :select "@date"))
       ((td)
	(xsl:value-of/ :select "@translation"))
       ((xsl:element :name "td")
	((xsl:attribute :name "class")
	 (xsl:choose
	  ((xsl:when :test "count(preceding-sibling::*) mod 2 = 0")
	   "value-lightblue")
	  (xsl:otherwise
	   "value")))
	((xsl:attribute :name "id")
	 "comment@" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id"))
	((xsl:attribute :name "onclick")
	 "displayChoices('" (xsl:value-of/ :select "@stem")
	 "@" (xsl:value-of/ :select "@code")
	 "@" (xsl:value-of/ :select "@pos")
	 "@" (xsl:value-of/ :select "@sub-id") "', 'comment', '" (xsl:value-of/ :select "@comment") "')")
	(xsl:value-of/ :select "@comment"))
       ((td)
	(xsl:value-of/ :select "@author"))
       
       ))))

(defstylesheet noun-features-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")     
     ((xsl:template :match "/")
      (html
       (head
        (title "Kartuli :: Noun Features"
	       ((xsl:if :test "@user")
		" [" (xsl:value-of/ :select "@user") "]"))
        ((style :type "text/css")
	 (!CDATA
	  (CSS-STYLE
	    (div :margin "16"
		 :color #-BW "#004499" #+BW "black"
		 :font-family "Verdana, Tahoma, MS Sans Serif, Arial, Geneva, Helvetica")
	    (div.title :font-size "18" :font-weight "bold" :text-align "center")
	    (div.label :font-size "12")
	    (div.text :font-size "8pt" :color "black" :margin-left "2px" :margin-bottom "2px")
	    (div.link :font-size "8pt" :margin-left "0px")
	    (table :padding "0pt" :font-size "8pt" :border-collapse "collapse" ;; ??
		   :font-family "Verdana, Tahoma, MS Sans Serif, Arial, Geneva, Helvetica")
	    (tr.row)
	    (tr.row-lightblue :background-color "#ebf3ff")
	    (tr.row-focus :border "2px solid blue")
	    (td :margin "10pt" :padding-right "10pt" :border "none" :vertical-align "top")
	    (td.progress-bar :margin "0pt" :padding "0pt"
			     :width "200px"
			     :border "1px solid black"
			     :vertical-align "top")
	    (td.id :text-align "right" :vertical-align "top" :font-weight "bold" :border-top "1px solid lightgray")
	    (td.title :font-style "italic" :color "#004499")
	    (td.title-right :font-style "italic" :color "#004499" :text-align "right" :padding-right "10pt")
	    (td.value :background "white")
	    (td.value\:hover :background "lightgray")
	    (td.value-lightblue :background "#ebf3ff")
	    (td.value-lightblue\:hover :background "lightgray")
	    )))
	((script :type "text/javascript" :language "javascript") 
         (!CDATA #L(js/noun-features stream))))
       ((xsl:element :name "body")
	((xsl:attribute :name "onkeydown") "menuShortcut(event)")
	;;((xsl:attribute :name "onload") "document.form.elements['search-stem'].focus()")
	((xsl:attribute :name "style") "font-family: Verdana, Tahoma, MS Sans Serif, Arial, Geneva, Helvetica")
	
	((div :class "title") "Kartuli :: Noun Features")

	((form :method "post" :id "form" :name "form" :action "/kartuli/noun-features.xml")
	 
	 ((div :class "text")
	  "Stem: " ((input :type "text" :style "font-size: 12pt" :name "search-stem"
			   :spellcheck "false"
			   :id "search-stem")
		    ((xsl:attribute :name "value") (xsl:value-of/ :select "/nouns/@search-stem"))
		    ((xsl:attribute :name "onkeyup") "nounFeatures()")))
	 ((div :id "noun-features-table"))))))))

#+ignore
(noun :stem #s stem
      :code #s code
      :pos #s pos
      :sub-id #s sub-id
      :features #s features
      :comment #s comment
      :author #s author
      :date #s date)


#+main
(progn
  (with-open-file (stream "projects:xle;grammars;georgian;georgian-noun-lex-1.lfg"
			  :direction :output :if-exists :supersede)
    (write-xle-noun-lexicon-sql stream :part 1))
  (with-open-file (stream "projects:xle;grammars;georgian;georgian-noun-lex-2.lfg"
			  :direction :output :if-exists :supersede)
    (write-xle-noun-lexicon-sql stream :part 2)))

(defun add-connecting-backslash (lemma)
  (u::subst-substrings lemma '(" " "` ")))

;; TODO: add style features
(defun write-xle-noun-lexicon-sql (stream &key part masdarp)
  (assert (or (null part) (find part '(1 2))))
  (cond (masdarp
         (format stream "STANDARD GEORGIAN-MASDARS LEXICON (1.0)"))
	(part
	 (format stream "STANDARD GEORGIAN-NOUNS-~a LEXICON (1.0)" part))
        (t
         (format stream "STANDARD GEORGIAN-NOUNS LEXICON (1.0)")))
  (write-char #\newline stream)
  (let ((count 0)
	(firstp t)
	;;(prev-stem "")
	(prev-lemma "")
	(prev-pos "")
	(prev-lex-class "")
	(prev-tpl ""))
    (do-query ((stem conjugation pos features style-features lang template)
	       [select [stem] [code] [pos] [features] [style-features] [lang] [template]
		       :from [morph noun-features]
		       :where [or [null [comment]]
				  [and [not [like [comment] "%wrong%"]]
				       [not [like [comment] "%variant%"]]
				       [not [like [comment] "%verbal-morph%"]]
				       [not [like [comment] "%VN%"]]]]
		       ;;:where [not [like [stem] "% %"]]
		       :order-by `([stem] [pos])])
      (when (or (null part)
		(and (= part 1)
		     (string< stem "g"))
		(and (= part 2)
		     (not (string< stem "g"))))
	(when (or (and masdarp (equal "masd" pos))
		  (and (not masdarp) #+orig(not (equal "masd" pos))))
	  (let* ((conjugation (intern (string-upcase conjugation) :keyword))
		 (nom-ending
		  (ecase conjugation
		    ((:A :A1 :A2 :B :P :U :ZA) "i")
		    ((:C :D :F :H :I :K :M :M1 :O :O1 :O2 :O3 :R :X :Z) "")))
		 (lemma (add-connecting-backslash (concat stem nom-ending))) ;; lemma is nominative form
		 (foundp nil))
	    (unless (or (find #\/ stem)		 ;; Gvino, GmerTi
			(find conjugation '(:O2))) ; fix for íÜßãä 
	      (when (find pos '("n" "a" "masd") :test #'string-equal)
		(setf foundp t)
		(unless firstp
		  (unless (equal lemma prev-lemma)
		    (write-string " ETC." stream)))
		(setf firstp nil)
		;;(unless (equal lemma prev-lemma) (write-char #\Linefeed stream))
		(let ((lex-class (cond ((and features (search "+Prop" features))
					"PROP")
				       ((string-equal pos "n")
					"N")
				       ((string-equal pos "masd")
					"N")
				       ((string-equal pos "a")
					"A")
				       (t
					(error "POS ~s not implemented." pos))))
		      (tpl (cond (template
				  template)
				 ((and features (search "+Prop" features))
				  "PROP")
				 ((string-equal pos "n")
				  "NOUN")
				 ((string-equal pos "masd")
				  "NOUN" #+orig"MASDAR")
				 ((string-equal pos "a")
				  "ADJECTIVE")
				 (t
				  (error "POS ~s not implemented." pos)))))
		  (unless (and (equal lemma prev-lemma)
			       (equal lex-class prev-lex-class)
			       (equal tpl prev-tpl))
		    ;;(unless (equal lemma prev-lemma) (write-char #\Newline stream))
		    ;;(write-char #\Linefeed stream)
		    (format stream "~%~a ~a XLE @(~a ~a);"
			    (if (equal lemma prev-lemma) "    " (convert-encoding lemma :amirani :unicode))
			    ;; XLE lexical class
			    lex-class
			    ;; template
			    tpl
			    (convert-encoding lemma :amirani :unicode)))
		  (setf prev-lemma lemma ;; prev-stem stem
			prev-lex-class lex-class
			prev-tpl tpl
			prev-pos pos))
		(incf count))))))))
  (format stream " ETC.~2%----~%")
  nil)

;; :H is bare hyphenated stem: აშშ, აშშ-ს, აშშ-ის, …
;; from morphology.lisp, adapted to changed char set 
;; see there for more info
(defun alternate-stem (stem conjugation &optional mark)
  (ecase conjugation
    ((:A :O :A1 :A2 :M1 :O1 :O2 :O3 :C :H :X :Z :ZA :-) nil)
    (:B (values :C (fst::syncope-stem stem mark) :BX)) ;; non-syncopated as subnorm
    (:B1 (values :C (fst::syncope-stem stem mark))) ;; syncopated only
    (:P (values :Q (fst::ablaut-stem stem #\ვ 1 mark) :PX)) ;; ნიორი - ნივრის/ნიორის
    (:U (values :V (fst::truncate-stem stem 2 mark) :UX)) ;; ამბავი - ამბის/ამბავის
    (:D (values :E (fst::truncate-syncope-stem stem mark)))
    (:D1 (values :E (fst::truncate-syncope-stem stem mark) :DX)) ;; non-syncopated as subnorm (ქუეყანის OG)
    (:F (values :G (fst::truncate-syncope-stem stem mark))) ;; ??
    (:I (values :J (fst::truncate-stem stem 1 mark)))
    (:K (values :L (if mark stem (fst::truncate-stem stem 1))))
    (:M (values :N (fst::truncate-stem stem 1 mark)))
    (:R (values :S (fst::truncate-stem stem 1 mark)))

    (:AA (values :AA1 (fst::mwe-stem stem mark))) ;; MWE stem
    (:IA (values :IA1 (fst::mwe-truncate-stem stem t mark))) ;; MWE stem, nom - gen/inst
    (:JA (values :JA1 (fst::mwe-stem stem mark))) ;; MWE stem, dat/adv
    (:OA (values :OA1 (fst::mwe-stem stem mark))) ;; MWE stem
    (:ON (values :O stem :N (fst::truncate-stem stem 1 mark))) ;; truncated as subnorm

    #+ignore
    (:X (values :Y (fst::truncate-stem stem 1 mark)))))

#+test
(write-fst-geo-stems)


;;(pprint (select [style-features] :distinct t :from [noun-features]))

#+main
(let ((max-count 8))
  (dotimes (i max-count)
    (write-fst-noun-stems-sql :count i :max-count max-count)))

#+test
(write-fst-noun-stems-sql)

(defun mark-stem (stem conj)
  (or (nth-value 1 (alternate-stem stem conj t))
      stem))

(defun marked-nom-form (stem conjugation &key lemma prop masdar)
  (when stem
    (let* ((nom-ending
	    (ecase conjugation
	      ((:A :A1 :A2 :B :B1 :P :U :AA :ZA) "·ი") ;; AA: MWE of type დიდი დიღომი
	      ((:C :H :X :Z :-) "")
	      ((:D :D1 :F :I :IA :JA :K :M :M1 :O :O1 :O2 :O3 :ON :R :OA) ;; OA: MWE of type დიდი რაყა
	       (if (or prop masdar) "" "·ჲ"))))
	   (marked-stem (mark-stem stem conjugation))
	   (lemma (or lemma (concat marked-stem nom-ending))))
      lemma)))

#+test
(print (marked-nom-form "წამება" :i))

#+test
(Print (select [stem] [code] [pos] [features] [style-features] [lang]
	       :from [morph noun-features]
	       :where [like [stem] "მოადგილ%"]
	       :where [and [or [null [comment]]
			       [and [not [like [comment] "%wrong%"]]
				    [<> [comment] "verbal-morph"]]]
			   [or [null [features]] [not [like [features] "%+Ignore%"]]]]
	       :order-by `([stem] [pos])))

;; exceptions: see stem = ღვთ/ღმერთ-ი, ღვინ/ღვინო, ხბორ/ხბო

;; needs utp-to-fst.lisp
(defun write-fst-noun-stems-sql (&key count max-count)
  (let ((firstp t)
	(i -1)
	(prev-stem ""))
    (with-open-file (stream (format nil "projects:georgian-morph;regex;noun-list~a.regex" (or count ""))
			    :direction :output :if-exists :supersede)
      (do-query ((stem conjugation pos features style-features lang)
		 [select [stem] [code] [pos] [features] [style-features] [lang]
			 :from [morph noun-features]
			 ;;:where [like [stem] "gia%"]
			 :where [and [or [null [comment]]
					 [and [not [like [comment] "%wrong%"]]
					      [<> [comment] "verbal-morph"]
					      ;; e.g., უდავო / უდაო
					      [not [like [comment] "%variant%"]]
					      ;;[not [like [comment] "%VN%"]]
					      [not [like [comment] "%NVN%"]] ;; VN + N
					      ]]
				     [or [null [features]] [not [like [features] "%+Ignore%"]]]]
			 :order-by `([stem] [pos])])
	(when (equal lang "*")
	  (setf lang nil))
	(unless (string= prev-stem stem)
	  (incf i)
	  (setf prev-stem stem))
	(when (or (null count)
		  (= (mod i max-count) count))
	  (destructuring-bind (stem &optional lemma) (split stem #\/)
	    (let* ((pos (cond ((string= pos "N")
			       "prop")
			      ;; preliminary: interpret all participles as adjectives
			      ((find pos '("p.p." "p.a." "p.f." "p.n.") :test #'equal)
			       "a")
			      (t
			       pos)))
		   (pos (intern (string-upcase pos) :fst))
		   (style (cond ((or (null style-features)
				     (equal style-features "")
				     (search "+Norm" style-features)
				     (search "+Poetic" style-features)
				     (search "+Sov" style-features)
				     (search "+Rus" style-features)
				     )
				 nil)
				((or (search "+OG" style-features)
				     (search "+Saba" style-features)
				     (search "+Obsolete" style-features))
				 "old")
				((or (search "+Colloquial" style-features)
				     (search "+Gr" style-features)
				     (search "+NonStandard" style-features))
				 "nonstandard")
				((or (search "+Abbrev" style-features))
				 "abbrev")
				(t
				 "dialect")))
		   (cats (cond ((eq pos 'fst::masd)
				(list 'fst::n))
			       (features
				(cons pos (split (string-downcase features) #\+ nil nil t)))
			       (t
				(list pos))))
		   (cats (if (find (cadr cats) '(pron n allq q prop) :test #'string-equal)
			     (cdr cats)
			     cats))
		   (pos1 (car cats))
		   (sub-cat (when (cdr cats)
			      (format nil "~{~a~^+~}"
				      (cdr cats))))
		   (conjugation (intern (string-upcase conjugation) :keyword))
		   #+orig
		   (nom-ending
		    (ecase conjugation
		      ((:A :A1 :A2 :B :B1 :P :U :ZA) "·ი")
		      ((:C :H :X :Z) "")
		      ((:D :F :I :K :M :M1 :O :O1 :O2 :O3 :R)
		       (if (string-equal pos1 'prop) "" "·ჲ"))))
		   #+orig
		   (marked-stem (mark-stem stem conjugation))
		   #+orig
		   (lemma (or lemma (concat marked-stem nom-ending)))) ;; lemma is nominative form
	      (write-stem stream :stem stem :lemma lemma
			  :conjugation conjugation :sub-cat sub-cat :style style :lang lang
			  :namep (search "+Name" features) :pos pos1 :firstp firstp)
	      (setf firstp nil)
	      #+orig
	      (multiple-value-bind (alt-conjugation alt-stem subnorm-conj) (alternate-stem stem conjugation)
		(dolist (conjugation+stem
			  `((,conjugation ,stem)
			    ,@(when alt-conjugation (list `(,alt-conjugation ,alt-stem)))
			    ,@(when subnorm-conj (list `(,subnorm-conj ,stem)))))
		  (destructuring-bind (conjugation stem) conjugation+stem
		    (let* ((features (fst::conjugation-features conjugation stem lemma pos1
								sub-cat style lang (search "+Name" features)))
			   (stem (cadr (assoc 'fst::stem features)))
			   ;; = lemma
			   (lex (cadr (assoc 'fst::lex features)))
			   )
		      ;;(debug (assoc 'fst::stem-type features))
		      (unless (or (equal lex "") (equal stem ""))
			(cond (firstp
			       (write-string "read regex [" stream)
			       (setf firstp nil))
			      (t
			       (write-string "|" stream)
			       (write-char #\linefeed stream)))
			(format stream "{~a}:{~a} " lex stem)
			(dolist (att '(fst::cat fst::sub-cat fst::stem-type
				       fst::sync-stem fst::rigid-stem fst::num fst::case fst::style fst::lang))
			  (let ((att-str (fst::format-symbol-fst att))
				(val (cadr
				      (if nil ; (eq att 'cat)
					  (or (assoc 'fst::sub-cat features)
					      (assoc 'fst::cat features))
					  (assoc att features)))))
			    (cond ((null val)
				   nil)
				  ((parser::extended-list-p val)
				   (write-string " [ " stream)
				   (loop for (alt . rest) on (parser::extended-list-form val)
				      do (format stream " \"@U.~a.~a@\" " att-str (fst::convert-val alt))
				      (cond ((string-equal att-str "Cat")
					     (pushnew (fst::convert-val alt) fst::*cat-list* :test #'string-equal))
					    ((string-equal att-str "SubCat")
					     (pushnew (fst::convert-val alt) fst::*subcat-list* :test #'string-equal)))
				      (if rest
					  (write-string " | " stream)
					  (write-string " ] " stream))))
				  (t
				   (let ((val (fst::convert-val val)))
				     (format stream " \"@U.~a.~a@\" " att-str val)
				     (cond ((string-equal att-str "Cat")
					    (pushnew val fst::*cat-list* :test #'string-equal))
					   ((string-equal att-str "SubCat")
					    (pushnew val fst::*subcat-list* :test #'string-equal)))))))))
		      (when nil (write-string "| " stream))))))))))
      (write-string "] ;" stream)
      (write-char #\linefeed stream)
      (format stream "save stack noun-list~a.fst" (or count ""))
      (write-char #\linefeed stream)
      (print (list :noun-stems-written count)))))


;; additional excerpted noun stems

;; import extracted 
#+main
(with-database-connection ();;("localhost/gnc/gnc/kartuli" :database-type :postgresql)
  (delete-records :from [morph extracted-morph])
  (dolist (lex-file (directory "projects:gnc;data;TEI;NG;*;*.lex")) 
    (u:with-file-fields ((token norm lemma code pos features
				class present future aorist
				perfect
				corr comment
				approved
				)
			 lex-file :empty-to-nil t)
      (when (and approved (not (equal approved "false")) (not corr) (not norm))
	(print (list approved lemma (pathname-name lex-file) token pos code features))
	(insert-records :into [morph extracted-morph]
			:av-pairs `(([lemma] ,lemma)
				    ([source] ,(pathname-name lex-file))
				    ([word] ,token)
				    ([pos] ,pos)
				    ([code] ,code)
				    ([features] ,features)
				    ([date] ,(u:now))))))))

;; those are used in noun(-splice).regex and syntax.regex
#+test
(write-fst-extracted-stems)

;; is called in compile-morphology()
(defun write-fst-extracted-stems ()
  (with-open-file (stream "projects:georgian-morph;regex;extracted-noun-adj.regex"
			  :direction :output :if-exists :supersede)
    (with-open-file (adv-stream "projects:georgian-morph;regex;extracted-adv.regex"
				:direction :output :if-exists :supersede)
      (with-open-file (interj-stream "projects:georgian-morph;regex;extracted-interj.regex"
				  :direction :output :if-exists :supersede)
	(let ((firstp t)
	      (adv-firstp t)
	      (interj-firstp t))
	  (with-database-connection ()
	    (do-query ((lemma pos code features)
		       [select [lemma] [pos] [code] [features]
			       :from [morph extracted-morph]
			       :order-by `([lemma] [pos])])
	      (print (list lemma pos code features))
	      (setf lemma (substitute #\- #\– lemma))
	      (cond ((or (null lemma) (equal lemma "-"))
		     nil)
		    ((equal pos "Adv")
		     (cond (adv-firstp
			    (debug lemma)
			    (write-line "define NGExtractedAdv [" adv-stream)
			    (format adv-stream "  {~a} \"+Adv\":0~%" lemma)
			    (setf adv-firstp nil))
			   (t
			    (format adv-stream " | {~a} \"+Adv\":0~%" lemma))))
		    ((equal pos "Interj")
		     (cond (interj-firstp
			    (debug lemma)
			    (write-line "define NGExtractedInterj [" interj-stream)
			    (format interj-stream "  {~a} \"+Interj\":0~%" lemma)
			    (setf interj-firstp nil))
			   (t
			    (format interj-stream " | {~a} \"+Interj\":0~%" lemma))))
		    ((find pos '("N" "A") :test #'string=)
		     (setf features (if (or (null features) (equal features ""))
					nil
					(intern (string-upcase features) :keyword)))
		     (let* ((conjugation (intern code :keyword))
			    (stem (cond ((find conjugation '(:A :B))
					 (subseq lemma 0 (1- (length lemma))))
					((and (eq conjugation :Z)
					      (char= (char lemma (1- (length lemma))) #\ი))
					 (subseq lemma 0 (1- (length lemma))))
					(t
					 lemma)))
			    (pos (cond ((null features)
					(intern (string-upcase pos)))
				       ((find (debug features) '(:hum :hum+qual :qual+hum
								 :qual :temp :foreign :title))
					(intern (string-upcase pos)))
				       ((eq features :qual+hum)
					(intern (string-upcase :hum+qual)))
				       (t
					'prop)))
			    (sub-cat (case features
				       (:FirstName :Name+Firstname)
				       (:LastName :Name+LastName)
				       (:Patronym :Name+Patronym)
				       (:Person :Name+Person)
				       ;;Qual
				       (:Hum :Anim)
				       (:Hum+Qual :Anim+Qual)
				       ;;Temp
				       ;;(Foreign
				       (:Place :geo+city)
				       (:Area :geo+area)
				       (:Hydr :geo+sea)
				       (:Zoon :name+zoon)
				       (:Demin :demin)
				       (:Onom :onom)
				       (otherwise features))))
		       (write-stem stream :stem stem :pos pos :conjugation conjugation
				   :sub-cat sub-cat :firstp firstp
				   :namep (find features '(:firstname :lastname :patronym :person :zoon)))
		       (setf firstp nil)))))))
	(format stream "] ;~%save stack extracted-noun-adj.fst~%")
	(format adv-stream "] ;~%") ;save stack extracted-adj-list.fst~%")
	(format interj-stream "] ;~%"); save stack extracted-interj-list.fst~%")
	(print :extracted-stems-written)))))

(defun write-stem (stream &key stem lemma conjugation sub-cat style lang namep pos firstp)
  (let* ((nom-ending
	  (ecase conjugation
	    ((:A :A1 :A2 :B :B1 :P :U) "·ი")
	    ((:C :H :X :Z :ZA :-) "")
	    ((:D :D1 :F :I :K :M :M1 :O :O1 :O2 :O3 :R)
	     (if (string-equal pos 'prop) "" "·ჲ"))))
	 (marked-stem (mark-stem stem conjugation))
	 (lemma (or lemma (concat marked-stem nom-ending))))
    (multiple-value-bind (alt-conjugation alt-stem subnorm-conj subnorm-stem)
	(alternate-stem stem conjugation)
      (dolist (conjugation+stem
		`((,conjugation ,stem)
		  ,@(when alt-conjugation (list `(,alt-conjugation ,alt-stem)))
		  ,@(when subnorm-conj (list `(,subnorm-conj ,(or subnorm-stem stem))))))
	(destructuring-bind (conjugation stem) conjugation+stem
	  (let* ((features (fst::conjugation-features conjugation stem lemma pos
						      sub-cat style lang
						      namep))
		 (stem (cadr (assoc 'fst::stem features)))
		 ;; = lemma
		 (lex (cadr (assoc 'fst::lex features)))
		 )
	    ;;(debug (assoc 'fst::stem-type features))
	    (unless (or (equal lex "") (equal stem ""))
	      (cond (firstp
		     (write-string "read regex [" stream)
		     (setf firstp nil))
		    (t
		     (write-string "|" stream)
		     (write-char #\linefeed stream)))
	      (format stream "{~a}:{~a} " lex stem)
	      (dolist (att '(fst::cat fst::sub-cat fst::stem-type
			     fst::sync-stem fst::rigid-stem fst::num fst::case fst::style fst::lang))
		(let ((att-str (fst::format-symbol-fst att))
		      (val (cadr
			    (if nil	; (eq att 'cat)
				(or (assoc 'fst::sub-cat features)
				    (assoc 'fst::cat features))
				(assoc att features)))))
		  (cond ((null val)
			 nil)
			((parser::extended-list-p val)
			 (write-string " [ " stream)
			 (loop for (alt . rest) on (parser::extended-list-form val)
			    do (format stream " \"@U.~a.~a@\" " att-str (fst::convert-val alt))
			    (cond ((string-equal att-str "Cat")
				   (pushnew (fst::convert-val alt) fst::*cat-list* :test #'string-equal))
				  ((string-equal att-str "SubCat")
				   (pushnew (fst::convert-val alt) fst::*subcat-list* :test #'string-equal)))
			    (if rest
				(write-string " | " stream)
				(write-string " ] " stream))))
			(t
			 (let ((val (fst::convert-val val)))
			   (format stream " \"@U.~a.~a@\" " att-str val)
			   (cond ((string-equal att-str "Cat")
				  (pushnew val fst::*cat-list* :test #'string-equal))
				 ((string-equal att-str "SubCat")
				  (pushnew val fst::*subcat-list* :test #'string-equal)))))))))
	    (when nil (write-string "| " stream))))))))

#+main
(dolist (type '(:masdar :present-part :past-part :future-part :negative-part))
  (write-fst-participle-stems-sql :type type))

#+main
(dolist (type '(:past-part :future-part :negative-part))
  (write-fst-participle-stems-sql :type type))

#+test
(select [id] [sub-id] [root]
	:from [morph verb-features]
	:where [like [tense] "%|present|%"])

;; fixme: root should always be unique
#+test
(let ((pr-root-table (make-hash-table :test #'equal)))
  (do-query ((id sub-id root)
	     [select [id] [sub-id] [root]
		     :from [morph verb-features]
		     :where [like [tense] "%|present|%"]])
    (pushnew root (gethash (list id sub-id) pr-root-table) :test #'equal))
  (maphash (lambda (id-subid roots)
	     (when (cdr roots)
	       (print (list id-subid roots))))
	   pr-root-table))

#+test
(write-fst-participle-stems-sql :type :masdar)

(defparameter *wrong-vn-root-table* (dat:make-string-tree))

#+test
(dat:do-string-tree (lemma roots *wrong-vn-root-table*)
  (format t "~a: ~{~a~^, ~}~%" lemma roots))

#+test
(write-fst-participle-stems-sql :type :past-part)

#+test
(write-fst-participle-stems-sql :type :masdar)

(defun write-fst-participle-stems-sql (&key type id)
  (let ((firstp t)
	(i -1)
	(prev-stem "")
	(seen-table (make-hash-table :test #'equal))
	(pr-root-table (make-hash-table :test #'equal)))
    ;; collect present roots
    (do-query ((id sub-id root lang)
	       [select [id] [sub-id] [root] [lang]
		       :from [morph verb-features]
		       :where [like [tense] "%|present|%"]])
      (when lang (setf lang (intern (string-upcase lang) :keyword)))
      (pushnew (cons root lang) (gethash (list id sub-id) pr-root-table) :test #'equal))
    (with-open-file (stream (format nil "projects:georgian-morph;regex;~(~a~)-list.regex" type) ;
			    :direction :output :if-exists :supersede)
      (dolist (asp '(:perf :imperf))
	(do-query ((id sub-id stem conjugation c-root features-pr-root
		       root impf-pv pf-pv restriction vn impf-vn pf-vn
		       lang aspect masdar masdar-code masdar-lang masdar-aspect tense)
		   [select [part id]
			   [part sub-id]
			   [part stem] [part code] [verb-paradigm c-root]
			   [verb-features root]
			   [part root]
			   [impf-pv] [pf-pv]
			   [part restriction]
			   [verb-paradigm vn] [impf-vn] [pf-vn]
			   [part variety]
			   [part aspect]
			   [masdar stem]
			   [masdar code]
			   [masdar variety]
			   [masdar aspect]
			   [verb-features tense]
			   :distinct t
			   :from [morph verb-paradigm]
			   :left-join [morph participle :as part]
			   :on [and [= [part id] [verb-paradigm id]]
				    [= [part sub-id] [verb-paradigm features-sub-id]]]
			   :left-join [morph participle :as masdar]
			   :on [and [= [masdar id] [verb-paradigm id]]
				    [= [masdar sub-id] [verb-paradigm features-sub-id]]
				    [= [masdar type] :masdar]
				    [= [masdar main-form] t]
				    ]
			   :left-join [morph verb-features]
			   :on [and [= [verb-features id] [verb-paradigm id]]
				    [= [verb-features sub-id] [verb-paradigm features-sub-id]]]
			   :where [and [= [part type] ?type]
				       ;; new Nov. 2013
				       [or [null [part wrong]]
					   [= [part wrong] :false]]
				       (when id [= [verb-paradigm id] ?id])
				       (if (eq asp :imperf)
					   [and [like [tense] "%|present|%"]
						[or [null [part aspect]]
						    [= [part aspect] ""]
						    [= [part aspect] :imperf]]]
					   [and [or [like [tense] "%|future|%"]
						    [like [tense] "%|aorist|%"]]
						[or [null [part aspect]]
						    [= [part aspect] ""]
						    [= [part aspect] :perf]]])]
			   :order-by `([part id] ;; [verb-paradigm sub-id] 
				       [part stem])])
	  (when (equal aspect "") (setf aspect nil))
	  (when (equal masdar-aspect "") (setf masdar-aspect nil))
	  (when (equal lang "") (setf lang nil))
	  (when (equal masdar-lang "") (setf masdar-lang nil))
	  (when masdar-code (setf masdar-code (intern (string-upcase masdar-code) :keyword)))
	  ;;(print (list type sub-id stem root lang masdar-lang asp aspect masdar-aspect masdar))
	  (when (and (or (null aspect)
			 (null masdar-aspect)
			 (equal aspect masdar-aspect))
		     (or (null lang)
			 (null masdar-lang)
			 (equal lang masdar-lang))
		     (not (equal stem "-"))
		     (not (find #\* stem :start 1)))
	    ;; masdars with star (e.g., "*წერა") are to be combined with a preverb
	    ;;(when lang (print (list :lang lang asp masdar)))
	    (let* ((lang (or lang masdar-lang))
		   (variety (when lang (intern (string-upcase lang) :keyword)))
		   (pr-root (or (car (find-if (lambda (root.lang)
						(or (null (cdr root.lang))
						    (null variety)
						    (eq (cdr root.lang) variety)))
					      (gethash (list id sub-id) pr-root-table)))
				features-pr-root))
		   (masdar-full
		    (case masdar-code
		      ((:A :A1 :A2 :B :B1 :BX :P :PX :U :UX)
		       (u:concat masdar "ი"))
		      (otherwise
		       masdar)))
		   (impf-vn (or ;; impf-vn
			     (let ((pv-list (gethash (list id masdar) *vn-pv-table*)))
			       (cond ((null masdar)
				      (warn "No masdar found for ~a; using ~a." stem vn)
				      vn)
				     ((char/= (char masdar 0) #\*)
				      (marked-nom-form masdar masdar-code :masdar t))
				     ((not (equal impf-pv "-"))
				      (u:concat impf-pv "·"
						(marked-nom-form (subseq masdar 1) masdar-code :masdar t)
						#+ignore(subseq masdar 1)))
				     ((and (equal pf-pv "-") ;; pf form is preverbless
					   (find "-" pv-list :test #'equal))
				      (marked-nom-form (subseq masdar 1) masdar-code :masdar t))
				     (t ;; (fullform-count (subseq masdar-full 1)) ;; impf form is attested
				      (marked-nom-form (subseq masdar 1) masdar-code :masdar t))
				     #+obsolete
				     (t
				      (marked-nom-form masdar masdar-code :masdar t))))))
		   (pf-vn (or ;; pf-vn
			   (cond ((null masdar)
				  (warn "No masdar found for ~a." stem)
				  vn)
				 ((char/= (char masdar 0) #\*)
				  (marked-nom-form masdar masdar-code :masdar t))
				 ((equal pf-pv "-")
				  (marked-nom-form (subseq masdar 1) masdar-code :masdar t))
				 (t
				  (u:concat pf-pv "·"
					    (marked-nom-form (subseq masdar 1) masdar-code :masdar t))))))
		   (vn (if (eq asp :imperf) impf-vn pf-vn))
		   (pv (if (eq asp :imperf) impf-pv pf-pv)))
	      (unless (and (eq type :masdar)
			   (equal restriction "PV-ONLY")
			   (equal pv "-"))
		(when aspect (setf aspect (intern aspect :keyword)))
		(cond ((null lang)
		       nil)
		      ((equal lang "")
		       (setf lang nil))
		      (t
		       (setf lang (string-downcase lang))))
		(let* ((pos :v-part)
		       (sub-cat (string-downcase type))
		       (conj (intern (or conjugation "Z") :keyword))
		       (nom-ending
			(ecase conj
			  ((:A :A1 :A2 :B :B1 :BX :P :PX :U :UX :AZ) "·ი")
			  ((:C :H :R :X :Z) "")
			  ((:D :D1 :DX :F :I :K :M :M1 :O :O1 :O2 :O3) "·ჲ")))
		       (base-stem stem)
		       (lemma (concat stem nom-ending))
		       ;;(stem (cadr (assoc 'fst::stem features)))
		       ;;(lex (cadr (assoc 'fst::lex features)))
		       )
		  (multiple-value-bind (alt-conjugation alt-stem subnorm-conj) (alternate-stem stem conj)
		    ;;(print (list stem conjugation alt-stem subnorm-conj))
		    (dolist (conjugation+stem
			      `((,conj ,stem)
				,@(when alt-conjugation (list `(,alt-conjugation ,alt-stem)))
				,@(when subnorm-conj (list `(,subnorm-conj ,stem)))))
		      (destructuring-bind (conjugation stem) conjugation+stem
			;;(print (list conjugation stem))
			(let ((features (fst::conjugation-features conjugation stem lemma pos sub-cat nil lang nil)))
			  ;;(debug features)
			  (unless (or (and (eq asp :perf)
					   (not (or (search "|future|" tense)
						    (search "|aorist|" tense))))
				      (and (eq asp :imperf)
					   (or (not (search "|present|" tense))
					       (and (eq type :masdar)
						    (find #\* vn)) ;; only forms with existing masdars
					       ))
				      (and (eq asp :perf)
					   (not (equal pf-pv "-"))
					   (char/= (char stem 0) #\*))
				      (and (eq asp :perf)
					   (eq aspect :imperf))
				      (and (eq asp :imperf)
					   (eq aspect :perf)))
			    (when (equal root "-") (setf root nil))
			    (let* ((pv (cond ((char/= (char stem 0) #\*)
					      "")
					     ((eq asp :imperf)
					      (if (equal impf-pv "-") "" impf-pv))
					     (t
					      (if (equal pf-pv "-") "" pf-pv))))
				   (stem (if (char= (char stem 0) #\*)
					     (subseq stem 1)
					     stem))
				   (lemma (ecase type
					    (:masdar vn)
					    ((:present-part :past-part :future-part :negative-part)
					     (let ((marked (string-left-trim "*" (marked-nom-form base-stem conj))))
					       (if (or (equal pv "") (null pv))
						   marked
						   (u:concat pv "·" marked)))
					     ;;(substitute #\- #\* (u:concat pv (marked-nom-form base-stem conj)))
					     )))
				   (lemma-root (or root pr-root))
				   (key (format nil "{~a/~a}:{~a#~a} " lemma lemma-root pv stem)) ;; was c-root
				   (key1 (format nil "{~a/~a}:{~a#~a} "
						 (delete #\- lemma)
						 lemma-root
						 pv stem))
				   (na-pp (and (eq type :past-part)
					       (eql (string< "ნა" stem) 2)
					       (not (eql (string< "ნა" lemma-root) 2)))))
			      (let ((norm-root (substitute #\ხ #\ჴ lemma-root))
				    (norm-lemma (substitute #\ხ #\ჴ lemma)))
				(unless (or (search norm-root norm-lemma)
					    (and (> (length norm-root) 3)
						 (let ((v-root (u:concat (subseq norm-root 0 (1- (length norm-root)))
									 "ვ" (subseq norm-root (1- (length norm-root))))))
						   (search v-root norm-lemma))))
				  (let* ((h-pos (position #\- lemma :end (max 0 (- (length lemma) 2))))
					 (pv-less-vn (subseq lemma (if h-pos (1+ h-pos) 0))))
				    (pushnew lemma-root
					     (dat:string-tree-get *wrong-vn-root-table* pv-less-vn)
					     :test #'string=)))
				#+testing
				(pushnew lang (dat:string-tree-get *vn-test-table* norm-lemma)))
			      (or (gethash (list key lang conjugation) seen-table)
				  (gethash (list key1 lang conjugation) seen-table)
				  (progn
				    (setf (gethash (list key lang conjugation) seen-table) t)
				    (cond (firstp
					   (write-string "read regex [" stream)
					   (setf firstp nil))
					  (t
					   (write-string "|" stream)
					   (write-char #\linefeed stream)))
				    (write-string key stream)
				    (dolist (att '(fst::stem-type
						   fst::sync-stem fst::rigid-stem fst::num fst::case fst::style fst::lang))
				      (let ((att-str (fst::format-symbol-fst att))
					    (val (cadr (assoc att features))))
					(cond ((null val)
					       nil)
					      ((parser::extended-list-p val)
					       (write-string " [ " stream)
					       (loop for (alt . rest) on (parser::extended-list-form val)
						  do (format stream " \"@U.~a.~a@\" " att-str (fst::convert-val alt))
						    (cond ((string-equal att-str "Cat")
							   (pushnew (fst::convert-val alt) fst::*cat-list* :test #'string-equal))
							  ((string-equal att-str "SubCat")
							   (pushnew (fst::convert-val alt) fst::*subcat-list* :test #'string-equal)))
						    (if rest
							(write-string " | " stream)
							(write-string " ] " stream))))
					      (t
					       (let ((val (fst::convert-val val)))
						 (format stream " \"@U.~a.~a@\" " att-str val)
						 (cond ((string-equal att-str "Cat")
							(pushnew val fst::*cat-list* :test #'string-equal))
						       ((string-equal att-str "SubCat")
							(pushnew val fst::*subcat-list* :test #'string-equal))))))))
				    (when (and pv (string/= pv ""))
				      (format stream " \"@U.Pv.+@\" "))
				    (when na-pp
				      (format stream " \"@U.NaPastPart.+@\" ")))))))
			(when nil (write-string "| " stream)))))))))))
      (if (eq type :masdar)
	  (format stream "] \"@U.Cat.masdar@\"  ;~%")
	  (format stream "] \"@U.Cat.v-part@\"  \"@U.SubCat.~(~a~)@\" ;~%" type))
      (format stream "save stack ~(~a~)-list.fst~%" type)
      (format t "~a written.~%" type))))

#+test
(u:with-file-fields ((name class type &optional morph) "projects:georgian-morph;regex;geonames.txt"
		     :separator #\+ :comment #\#)
  (print (list name class type morph)))

#+test
(write-fst-geo-stems)

(defun write-fst-geo-stems ()
  (let ((firstp t)
	(i -1)
	(prev-stem ""))
    (with-open-file (stream "projects:georgian-morph;regex;geonames-list.regex"
			    :direction :output :if-exists :supersede)
      (u:with-file-fields ((name conj name-type &optional morph) "projects:georgian-morph;regex;geonames.txt"
			   :separator #\+ :comment #\#)
	(let* ((conjugation (intern (string-upcase conj) :keyword))
	       (stem (case conjugation
		       ((:A :AA :B) (subseq name 0 (1- (length name))))
		       (otherwise name)))
	       (nom-ending
		(ecase conjugation
		  ((:A :A1 :A2 :AA :B :B1 :P :U :AZ) "·ი")
		  ((:C :H :X :Z) "")
		  ((:D :D1 :F :I :IA :JA :K :M :M1 :O :O1 :O2 :O3 :OA :ON :R)
		   "")))
	       (marked-stem (mark-stem stem conjugation))
	       (lemma (concat marked-stem nom-ending))
	       (name-type (intern (string-upcase name-type) :keyword)))
	  (dolist (conjugation (if (eq conjugation :IA)
				   (list :ia :ja) ;; შავი ზღვა
				   (list conjugation)))
	    (multiple-value-bind (alt-conjugation alt-stem subnorm-conj subnorm-stem)
		(alternate-stem stem conjugation)
	      (dolist (conjugation+stem
			`((,conjugation ,stem)
			  ,@(when alt-conjugation (list `(,alt-conjugation ,alt-stem)))
			  ,@(when subnorm-conj (list `(,subnorm-conj ,(or subnorm-stem stem))))))
		;;(when alt-stem (debug alt-stem))
		(destructuring-bind (conjugation stem) conjugation+stem
		  (let* ((features (fst::conjugation-features conjugation stem lemma 'prop
							      nil nil nil t))
			 (stem (cadr (assoc 'fst::stem features)))
			 ;; = lemma
			 (lex (cadr (assoc 'fst::lex features)))
			 )
		    ;;(debug (assoc 'fst::stem-type features))
		    (unless (or (eq conjugation :ja) (equal lex "") (equal stem ""))
		      (cond (firstp
			     (write-string "read regex [" stream)
			     (setf firstp nil))
			    (t
			     (write-string "|" stream)
			     (write-char #\linefeed stream)))
		      (format stream "{~a}:{~a} " lex (u:subst-substrings stem '("=" "ი")))
		      (format stream " \"@U.SubCat.~a@\" "
			      (ecase name-type
				((:country :area) "geo+area")
				((:road :building) "geo+city")
				(:place "geo+city")
				(:mountain "geo+mount")
				(:water "geo+river")))
		      (dolist (att '(fst::cat fst::stem-type
				     fst::sync-stem fst::rigid-stem fst::num fst::case ))
			(let ((att-str (fst::format-symbol-fst att))
			      (val (cadr (assoc att features))))
			  (cond ((null val)
				 nil)
				((parser::extended-list-p val)
				 (write-string " [ " stream)
				 (loop for (alt . rest) on (parser::extended-list-form val)
				    do (format stream " \"@U.~a.~a@\" " att-str (fst::convert-val alt))
				    
				    (if rest
					(write-string " | " stream)
					(write-string " ] " stream))))
				(t
				 (let ((val (fst::convert-val val)))
				   (format stream " \"@U.~a.~a@\" " att-str val)
				   (cond ((string-equal att-str "Cat")
					  (pushnew val fst::*cat-list* :test #'string-equal))
					 ((string-equal att-str "SubCat")
					  (pushnew val fst::*subcat-list* :test #'string-equal)))))))))
		    (when nil (write-string "| " stream)))))))))
      (write-string "] ;" stream)
      (write-char #\linefeed stream)
      (format stream "save stack geonames-list.fst")
      (write-char #\linefeed stream)
      (print :geonames-written))))

#+test
(with-transaction ()
  (delete-records :from [noun-features] :where [and [= [stem] "gamovlinebaxxxxxx"] [= [sub-id] 2]]))

#+test
(with-transaction ()
  (update-records [noun-features] 
		  :attributes `([features] [date])
		  :values (list "+N+Anim" (get-universal-time))
		  :where [and [null [features]]
			      [like [stem] "me%e"]
			      [= [pos] "n"]]))

#+test
(with-transaction ()
  (update-records [noun-features] 
		  :attributes `([features] [date])
		  :values (list "+N+Prop+Geo" (get-universal-time))
		  :where [and [like [stem] "aPHazeT"]]))

#+test
(print (select [*] :from [noun-features] 
	       :where [and [like [stem] "aPHaz"]]))


#+test
(with-transaction ()
  (with-file-lines (line "~/lisp/projects/georgian-morph/marchev-geographic-names.txt")
    (destructuring-bind (translation stem code features &optional template) (split line #\tab)
      (if (select [stem] :from [noun-features] :where [= [stem] stem])
	  (update-records [noun-features]
			  :attributes `([translation] [stem] [code] [pos] [sub-id] [features] [template] [author] [date])
			  :values (list translation stem code "n" 1 features template "Marchev" (get-universal-time))
			  :where [and [= [stem] stem]
				      [sub-id] 1])
	  (insert-records :into [noun-features]
			  :attributes `([translation] [stem] [code] [pos] [sub-id] [features] [template] [author] [date])
			  :values (list translation stem code "n" 1 features template "Marchev" (get-universal-time)))))))

#+test
(with-transaction ()
  (update-records [noun-features] 
		  :attributes `([pos] [date])
		  :values (list "a" (get-universal-time))
		  :where [and [like [stem] "same%o"]
			      [= [pos] "n"]]))

;;; inclusion of Rayfield


#+test
(let ((count 0)
      (abbr-list ()))
  (with-file-lines (line "~/lisp/projects/web-dictionary/data/rayfield-utf8.txt")
    (let ((word (subseq line 0 (position #\space line)))
	  (adj-p nil)
	  (n-p nil)
	  (adv-p nil)
	  (pp-p nil))
      (labels ((collect-i (pos)
		 (let* ((start (search "<i>" line :start2 pos))
			(end (when start (search "</i>" line :start2 start))))
		   (when end
		     (let ((g-list (string-parse
				    (subseq line (+ start 3) end)
				    :left-separating-chars ",;:…?!){[]}\"–„”‚’"
				    :right-separating-chars "([{}]\"‚’„”"
				    :whitespace #(#\Space))))
		       (dolist (g g-list) (pushnew g abbr-list :test #'string=))
		       (when (find "a" g-list :test #'equal)
			 (setf adj-p t))
		       (when (find "n" g-list :test #'equal)
			 (setf n-p t))
		       (when (find "adv" g-list :test #'equal)
			 (setf adv-p t))
		       (when (find "pp" g-list :test #'equal)
			 (setf pp-p t))
		       (collect-i end))))))
	(collect-i 0)
	(when (or n-p (and adj-p (not pp-p)))
	  (let* ((sync (when (find #\[ word)
			 (subseq word (1+ (position #\[ word)) (position #\] word))))
		 (word (string-right-trim ",ი" (delete #\[ (delete #\] word))))
		 (alt-word (when (find #\( word)
			     (concat (subseq word 0 (position #\( word))
				     (subseq word (1+ (position #\) word))))))
		 (word (string-right-trim ",ი" (delete #\( (delete #\) word)))))
	    (unless (select [stem] :from [noun-features]
			    :flatp t
			    :where [= [stem] (convert-encoding word :unicode :amirani)])
	      (incf count)
	      (format t "~a~@[ ~a~]~:[~; n~]~:[~; a~]~%" word sync n-p (and adj-p (not pp-p))
		      ;;:adv-p adv-p
		      ;;:pp-p pp-p
		      
		      )))
	  ))))
  (print abbr-list)
  (print count))

(progn
  (defparameter *style-features* (make-hash-table :test #'equal))
  (dolist (f '("Aj" "Al" "Av" "Be" "Er" "Fe" "Gd" "Gm" "Gr" "Gu"
	       "Ig" "Im" "Ja" "Ju" "Ka" "Ki" "Kk" "Kv" "LI" "Le" "Lo" "Me" "Ml" 
	       "Mo" "Mt" "OG" "Ps" "Pv" "Ra" "Ru" "Rus" "Saba" "Sov"
	       "Ti" "Tu" "UI" "Up"
	       ("ab" "Abbrev")
	       ("©" "Colloquial")
	       ("†" "Obsolete")
	       ("‡" "OG")
	       ("◊" "NonStandard")
	       ("ﬂ" "Poetic")) )
    (if (consp f)
	(setf (gethash (car f) *style-features*) (cadr f))
	(setf (gethash f *style-features*) f))))

(defun headword-end (line)
  (labels ((find-end (pos end)
	     (let* ((i-pos (search " <i>" line :start2 pos :end2 end))
		    (class-end (when i-pos (position-if (lambda (c) (find c " ,<")) line :start (+ i-pos 4) :end end)))
		    (class (when class-end (subseq line (+ i-pos 4) class-end))))
	       (cond ((null class-end)
		      nil)
		     ((find class '("n" "N" "a" "adv" "pp" "vn" "fp" "pa" "pn" "np" "ab" "cj" "ij" "px") :test #'string=)
		      i-pos)
		     (t
		      (find-end class-end end))))))
    (let* ((end (when (search "; 2" line)
		 (search " 1" line))))
      (or end (find-end 0 nil)))))

#+test
(with-file-lines (line "~/lisp/projects/web-dictionary/data/rayfield-utf8.txt")
  (let* ((line (string-trim " " line))
	 (end (headword-end line)))
    #+test
    (unless end (write-line line))
    (when end
      (let ((words (subseq line 0 end)))
	(when (and end (find #\< words))
	  (print (string-parse
		  words
		  :brace-pairs '(("(" . ")"))
		  :whitespace #(#\,)))
	  #+ignore
	  (format t "~a~%" words))))))
#+test
(with-file-lines (line "~/lisp/projects/web-dictionary/data/rayfield-utf8.txt")
  (loop for i from 1 below (1- (length line))
     until (and (char<= #\a (char line (1- i)) #\z)
		(char= (char line i) #\))
		(char<= #\ა (char line (1+ i)) #\ჰ)
		(format t "~a~%"  (subseq line 0 (min (length line) (+ i 5)))))))

#||

A    k'atsi-k'atsis-k'atsebi
B/C  k'edeli-k'edlis-k'edlebi
P/Q  niori-nivris-nivrebi
U/V  khutsesi-khutsis-khutsebi
D/E  kveqana-kveqnis-kveqnebi
# F    mkhare-mkhris-mkhrebi ;; ?? missing
I/J  dzma-dzmis-dzmebi
K/l  gogona-gogonas-gogonebi
M/N  khe-khis-kheebi
O    sok'o-sok'os-sok'oebi
R/S  khat'va-khat'vis-0 (masdari)
X/Y  t'ramvai-t'ramvais-t'ramvaebi
H    aSS-aSS-s
Z    daviT-daviTis

||#

(defun decl-code (word features)
  (setf features (substitute #\- #\– features))
  (let* ((final-char (char word (1- (length word))))
	 (cons-stem (char= final-char #\ი))
	 (a-stem (char= final-char #\ა))
	 (e-stem (char= final-char #\ე))
	 (o-stem (char= final-char #\ო))
	 (u-stem (char= final-char #\უ))
	 (i-stem (search "(<i>dat</i> -ის)" features))
	 (i-stem (or (and i-stem cons-stem)
		     (and (> (length word) 2)
			  (find (subseq word (- (length word) 2)) '("აი" "ეი" "უი" "ოი") :test #'string=))))
	 (rigid-a (search "(-ას)" features))
	 (rigid-e (search "(-ეს)" features))
	 (reduced-o (and (search "ვნის)" features) (string= word "ონი" :start1 (- (length word) 3))))
	 (sync (or (search "[ა]" word) (search "[ე]" word) (search "[უ]" word) (search "[ო]" word)))
	 (opt-sync (or (search "[[ა]]" word) (search "[[ე]]" word) (search "[[უ]]" word) (search "[[ო]]" word)))
	 (code (cond ((string= word "ხუცესი")
		      :U)
		     (i-stem
		      :X)
		     ((and cons-stem opt-sync)
		      (list :A :B))
		     ((and cons-stem sync)
		      :B)
		     ((and cons-stem (search "ვრის)" features))
		      :P)
		     ((and cons-stem reduced-o)
		      :P)
		     (cons-stem
		      :A)
		     ((and a-stem rigid-a)
		      :K)
		     ((and a-stem opt-sync)
		      (list :D :K))
		     ((and a-stem sync)
		      :D)
		     (a-stem
		      :I)
		     ((and e-stem opt-sync)
		      (list :D :M))
		     ((and e-stem sync)
		      :D)
		     ((and e-stem rigid-e)
		      :O)
		     (e-stem
		      :M)
		     (o-stem
		      :O)
		     (u-stem
		      :O))))
    (when sync (setf word (delete #\[ (delete #\] word))))
    (unless (consp code) (setf code (list code)))
    (values code word)))

#+test
(with-file-lines (line "~/lisp/projects/web-dictionary/data/rayfield-utf8.txt")
  (unless (char= (char line 0) #\#)
    (extract-headwords line)))

(defun analyze-headword (hw)
  (let* ((br-pos (search " (" hw))
	 (brack-pos (search " <" hw))
	 (word-end (if (and br-pos brack-pos)
		       (min br-pos brack-pos)
		       (or br-pos brack-pos)))
	 #+old
	 (word-end (position #\space hw))
	 (word (subseq hw 0 word-end))
	 (par-start (position #\( word))
	 (par-end (position #\) word))
	 (sword (if par-start
		    (concat (subseq word 0 par-start)
			    (subseq word (1+ par-end)))
		    word))
	 (lword (when par-start
		  (concat (subseq word 0 par-start)
			  (subseq word (1+ par-start) par-end)
			  (subseq word (1+ par-end)))))
	 (features (when word-end (subseq hw (1+ word-end))))
	 (s-unorm (search "lst" features))
	 (l-unorm (search "sst" features))
	 (style-features (collect-features features))
	 (s-decl-code nil)
	 (l-decl-code nil))
    ;;(when (or t (find #\space word) (find #\< word)) (print word))
    (when (search "თა)" features) (push "OldPl" style-features))
    (multiple-value-setq (s-decl-code sword) (decl-code sword features))
    (when lword (multiple-value-setq (l-decl-code lword) (decl-code lword features)))
    (collecting
      (collect (list sword s-decl-code (if s-unorm (cons "NonStandard" style-features) style-features)))
      (when l-decl-code
	(collect (list lword s-decl-code (if l-unorm (cons "NonStandard" style-features) style-features)))))))

#+test
(print (select [*] :from [noun-features] :where [like [code] "%O2%"])) 

;;(print (extract-headwords "ოშიყანა რაბა <i>n Ju</i> four species (Sukkot ritual)"))

(defun extract-headwords (line &optional headword-end)
  (let ((headword-end (or headword-end (headword-end line))))
    (when headword-end
      (let ((headwords (mapcar (lambda (str) (string-trim " " str))
			       (string-parse
				(subseq line 0 headword-end)
				:brace-pairs '(("(" . ")"))
				:whitespace #(#\,)))))
	(mapcan #'analyze-headword headwords)))))

(defun collect-features (string &optional (pos 0))
  (let ((class nil)
	(style-features ()))
    (labels ((collect-i (pos)
	       (let* ((start (search "<i>" string :start2 pos))
		      (end (when (and start
				      (not (find-if-not (lambda (c) (find c "( ),")) string :start pos :end start)))
			     (search "</i>" string :start2 start))))
		 (when end
		   (let ((g-list (string-parse
				  (subseq string (+ start 3) end)
				  :left-separating-chars ",;:…?!){[]}\"–-„”‚’"
				  :right-separating-chars "([{}]\"‚’„”"
				  :whitespace #(#\Space))))
		     ;;(dolist (g g-list) (pushnew g abbr-list :test #'string=))
		     ;;(debug g-list)
		     (when (find "a" g-list :test #'equal)
		       (setf class :adj))
		     (when (find "n" g-list :test #'equal)
		       (setf class :n))
		     (when (find "N" g-list :test #'equal)
		       (setf class :name))
		     (when (find "adv" g-list :test #'equal)
		       (setf class :adv))
		     ;; passive part
		     (when (find "pp" g-list :test #'equal)
		       (setf class :pp))
		     ;; active part
		     (when (find "pa" g-list :test #'equal)
		       (setf class :pa))
		     ;; neg part
		     (when (find "np" g-list :test #'equal)
		       (setf class :np))
		     ;; fut part
		     (when (find "fp" g-list :test #'equal)
		       (setf class :fp))
		     ;; masdar
		     (when (find "vn" g-list :test #'equal)
		       (setf class :vn))
		     (dolist (f g-list)
		       (when (gethash f *style-features*)
			 (pushnew (gethash f *style-features*) style-features :test #'equal)))
		     (collect-i end))))))
      (collect-i pos)
      (values style-features class))))


#+test
(print (parse-rayfield-line "მეფე <i>n</i> 1 king; monarch, sovereign; მეფეთ(ა) ~ <i>†</i> suzerain, overlord: მნათობთა ~ <i>ﬂ</i> sun; ყვავილთა ~ <i>ﬂ</i> rose; ბოზბაში – წვნიანების მეფეა, როგორც თართი – თევზებისა Mutton soup is the king of soups as sturgeon is the king of fish: წიგნი პირველი/მეორე მეფეთა Book of Samuel I/II; წიგნი მესამე/მეოთხე მეფეთა Book of Kings I/II; 2 <i>†</i> groom (<i>at wedding</i>); 3 <i>chess</i> king; 4 <i>N</i> Queen (<i>Tamar only</i>)"))

#+test
(print (collect-features "king; monarch, sovereign; მეფეთ(ა) ~ <i>†</i> suzerain, overlord: მნათობთა ~ <i>ﬂ</i> sun; ყვავილთა ~ <i>ﬂ</i> rose; ბოზბაში – წვნიანების მეფეა, როგორც თართი – თევზებისა Mutton soup is the king of soups as sturgeon is the king of fish: წიგნი პირველი/მეორე მეფეთა Book of Samuel I/II; წიგნი მესამე/მეოთხე მეფეთა Book of Kings I/II;"))
#+test
(print (collect-features "<i>†</i> groom (<i>at wedding</i>); 3 <i>chess</i> king; 4 <i>N</i> Queen (<i>Tamar only</i>)"))


;;(execute-command "alter table NOUN_FEATURES add STYLE_FEATURES varchar(255) after FEATURES;")

;; pa np fp vn

(defun parse-rayfield-line (line)
  (let* ((headword-end (headword-end line))
	 (headwords+features (extract-headwords line headword-end)))
    (dolist (hw+f headwords+features)
      ;;(debug hw+f)
      (destructuring-bind (word class features) hw+f
	(when (find (car class) (list :A :B :P :U))
	  (setf word (subseq word 0 (1- (length word)))))
	(let ( ;;(word (subseq line 0 (position #\space line)))
	      (n-p nil)
	      (name-p nil)
	      (adj-p nil)
	      (adv-p nil)
	      (pp-p nil)
	      (c-n-p nil)
	      (c-name-p nil)
	      (c-adj-p nil)
	      (c-adv-p nil)
	      (c-pp-p nil)
	      (sub-adj-p nil)
	      (sub-n-p nil)
	      (sub-name-p nil)
	      (sub-adv-p nil)
	      (sub-pp-p nil)
	      (prev-sub-adj-p nil)
	      (prev-sub-n-p nil)
	      (prev-sub-name-p nil)
	      (prev-sub-adv-p nil)
	      (prev-sub-pp-p nil)
	      (style-features (copy-seq features))
	      (common-style-features ())
	      (n-style-features t)
	      (name-style-features t)
	      (adj-style-features t)
	      (adv-style-features t)
	      (pp-style-features t)
	      (prev-cat nil)
	      (new-features nil)
	      (sub-found-p nil))
	  (labels ((collect-i (string &optional (pos 0))
		     (let* ((start (search " <i>" string :start2 pos))
			    (end (when start (search "</i>" string :start2 start))))
		       (when end
			 (let ((g-list (string-parse
					(subseq string (+ start 4) end)
					:left-separating-chars ",;:…?!){[]}\"–-„”‚’"
					:right-separating-chars "([{}]\"‚’„”"
					:whitespace #(#\Space))))
			   ;;(dolist (g g-list) (pushnew g abbr-list :test #'string=))
			   ;;(debug g-list)
			   ;; np pa fp vn
			   ;; n a N pp
			   #-first-set
			   (progn
			     (when (find "n" g-list :test #'equal)
			       (setf sub-n-p t prev-cat "n"))
			     (when (find "a" g-list :test #'equal)
			       (setf sub-adj-p t prev-cat "a"))
			     (when (find "N" g-list :test #'equal)
			       (setf sub-name-p t prev-cat "N"))
			     (when (find "pp" g-list :test #'equal)
			       (setf sub-pp-p t prev-cat "pp")))
			   #+second-set
			   (progn
			     (when (find "np" g-list :test #'equal)
			       (setf sub-n-p t prev-cat "np"))
			     (when (find "pa" g-list :test #'equal)
			       (setf sub-adj-p t prev-cat "pa"))
			     (when (find "fp" g-list :test #'equal)
			       (setf sub-name-p t prev-cat "fp"))
			     (when (find "vn" g-list :test #'equal)
			       (setf sub-pp-p t prev-cat "vn")))
			   #+ignore
			   (when (find "adv" g-list :test #'equal)
			     (setf sub-adv-p t prev-cat "adv"))
			   ;;(debug g-list)
			   (cond ((find-if (lambda (cat) (find cat g-list :test #'equal))
					   '("pa" "np" "fp" "vn" "a" "n" "N" "adv" "pp" "ij" "ab" "cj" "px" "pn"))
				  (setf sub-found-p t))
				 (t
				  ;;(debug prev-cat)
				  #-first-set
				  (cond ((equal prev-cat "n")
					 (setf sub-n-p t))
					((equal prev-cat "a")
					 (setf sub-adj-p t))
					((equal prev-cat "N")
					 (setf sub-name-p t))
					((equal prev-cat "pp")
					 (setf sub-pp-p t))
					((equal prev-cat "adv")
					 (setf sub-adv-p t)))
				  #+second-set
				  (cond ((equal prev-cat "np")
					 (setf sub-n-p t))
					((equal prev-cat "pa")
					 (setf sub-adj-p t))
					((equal prev-cat "fp")
					 (setf sub-name-p t))
					((equal prev-cat "vn")
					 (setf sub-pp-p t))
					((equal prev-cat "adv")
					 (setf sub-adv-p t)))))
			   (dolist (f g-list)
			     (when (gethash f *style-features*)
			       (setf new-features t)
			       (pushnew (gethash f *style-features*) style-features :test #'equal)))
			   (collect-i string end))))))
	    (cond ((search "; 2 " line)
		   (let* ((common-end (search " 1" line))
			  (common (subseq line 0 common-end))
			  (sub-defs (loop with start = (+ common-end 2)
				       for i from 2 to 16
				       for next = (when start (search (format nil "; ~d" i) line :start2 start))
				       while start
				       collect (subseq line start next)
				       do (setf start (when next (+ next (if (= i 1) 2 3)))))))
		     (collect-i (subseq common 0 (position #\: common)))
		     (setf common-style-features style-features style-features ())
		     (setf c-adj-p sub-adj-p
			   c-n-p sub-n-p
			   c-name-p sub-name-p
			   c-adv-p sub-adv-p
			   c-pp-p sub-pp-p)
		     (dolist (sub-def sub-defs)
		       (setf sub-def (substitute #\- #\– sub-def))
		       (cond ((search "(-ას)" sub-def)
			      (pushnew :K class))
			     ((search "(-ეს)" sub-def)
			      (pushnew :O class)))
		       (setf sub-adj-p nil sub-n-p nil sub-name-p nil sub-adv-p nil sub-pp-p nil
			     style-features (copy-seq common-style-features))
		       (setf new-features nil sub-found-p nil)
		       (collect-i (subseq sub-def 0 (position #\: sub-def)))
		       (when (and (null common-style-features) (null new-features))
			 ;;(print (list :add-norm style-features))
			 (pushnew "Norm" style-features))
		       (setf sub-adj-p (or sub-adj-p c-adj-p)
			     sub-n-p (or sub-n-p c-n-p)
			     sub-name-p (or sub-name-p c-name-p)
			     sub-adv-p (or sub-adv-p c-adv-p)
			     sub-pp-p (or sub-pp-p c-pp-p))
		       (if (or sub-found-p sub-adj-p sub-n-p sub-name-p sub-adv-p sub-pp-p)
			   (setf prev-sub-adj-p sub-adj-p
				 prev-sub-n-p sub-n-p
				 prev-sub-name-p sub-name-p
				 prev-sub-adv-p sub-adv-p
				 prev-sub-pp-p sub-pp-p)
			   (setf sub-adj-p prev-sub-adj-p
				 sub-n-p prev-sub-n-p
				 sub-name-p prev-sub-name-p
				 sub-adv-p prev-sub-adv-p
				 sub-pp-p prev-sub-pp-p))
		       ;;(print (list sub-n-p sub-adj-p sub-adv-p sub-pp-p))
		       (setf n-p (or n-p sub-n-p)
			     name-p (or name-p sub-name-p)
			     adj-p (or adj-p sub-adj-p)
			     adv-p (or adv-p sub-adv-p)
			     pp-p (or pp-p sub-pp-p))
		       (when sub-n-p
			 (cond ((eq n-style-features t)
				(setf n-style-features (or style-features (list "Norm"))))
			       (t
				(dolist (f style-features)
				  (pushnew f n-style-features :test #'equal)))))
		       (when sub-name-p
			 (cond ((eq name-style-features t)
				(setf name-style-features (or style-features (list "Norm"))))
			       (t
				(dolist (f style-features)
				  (pushnew f name-style-features :test #'equal)))))
		       (when sub-adj-p
			 (cond ((eq adj-style-features t)
				(setf adj-style-features (or style-features (list "Norm"))))
			       (t
				(dolist (f style-features)
				  (pushnew f adj-style-features :test #'equal)))))
		       (when sub-pp-p
			 (cond ((eq pp-style-features t)
				(setf pp-style-features (or style-features (list "Norm"))))
			       (t
				(dolist (f style-features)
				  (pushnew f pp-style-features :test #'equal)))))
		       (when sub-adv-p
			 (cond ((eq adv-style-features t)
				(setf adv-style-features (or style-features (list "Norm"))))
			       (t
				(dolist (f style-features)
				  (pushnew f adv-style-features :test #'equal))))))
		     (when (or n-p name-p adv-p adj-p pp-p)
		       (dolist (cl class)
			 (add-rayfield-entry word cl n-p name-p adj-p pp-p n-style-features name-style-features adj-style-features pp-style-features)))))
		  (t
		   (setf style-features (sort (copy-seq features) #'string-lessp))
		   (collect-i (subseq line 0 (position #\: line)))
		   (when (null style-features)
		     (push "Norm" style-features))
		   (setf adj-p sub-adj-p
			 n-p sub-n-p
			 name-p sub-name-p
			 adv-p sub-adv-p
			 pp-p sub-pp-p)
		   (when (or n-p name-p adj-p pp-p)
		     (dolist (cl class)
		       (add-rayfield-entry word cl n-p name-p adj-p pp-p style-features style-features style-features style-features)))))))))))
	   
(defun write-style-features (features)
  (format nil "~{+~a~}" 
	  (if (find "Norm" features :test #'string=)
	      (cons "Norm" (sort (remove "Norm" features :test #'string=) #'string-lessp))
	      (sort (copy-seq features) #'string-lessp))))

(defun add-rayfield-entry (word cl n-p name-p adj-p pp-p n-style-features name-style-features a-style-features pp-style-features)
  (unless (find-if-not (lambda (c) (or (char<= #\ა c #\ჰ) (find c "-–—"))) word) ;; (break)
    (setf word (substitute #\- #\– word))
    #+second-set(when (and pp-p (eq cl :i)) (setf cl :r))
    (print (list word :cl cl
		 :n/pn n-p n-style-features
		 :a/pa adj-p a-style-features
		 :name/pf name-p name-style-features
		 :pp/masd pp-p pp-style-features))
    #-ignore
    (when (and cl (not (find word '("Gvino" "-") :test #'string=)))
      (loop for pos in
	   #-first-set'("n" "a" "N" "p.p.")
	   #+second-set'("p.n." "p.a." "p.f." "masd") 
	 for pos-p in (list n-p adj-p name-p pp-p)
	 for features in (list n-style-features a-style-features name-style-features pp-style-features)
	 when pos-p
	 do (let ((stem+code+pos+subid
		   (select [stem] [code] [pos] [sub-id]
			   :from [morph noun-features] :flatp t
			   :where [and [= [stem] (convert-encoding word :unicode :amirani)]
				       [= [code] ?cl]
				       [= [pos] ?pos]])))
	      (cond ((null stem+code+pos+subid)	;; not found
		     (print :inserted)
		     (insert-records :into [morph noun-features]
				     :attributes '([stem] [code] [pos] [sub-id] [style-features] [author] [comment])
				     :values (list (convert-encoding word :unicode :amirani)
						   cl
						   pos
						   1
						   (write-style-features features)
						   "Rayfield"
						   "Rayfield")))
		    (t
		     (dolist (row stem+code+pos+subid)
		       (destructuring-bind (&optional stem code p-o-s sub-id) row
			 (update-records [morph noun-features]
					 :where  [and [= [sub-id] ?sub-id]
						      [= [stem] ?stem]
						      [= [code] ?code]
						      [= [pos] ?pos]]
					 :attributes '([style-features] [comment])
					 :values (list (write-style-features features) "Rayfield")))))))))))


#+test
(start-sql-recording :type :commands :database *default-database*)
#+test
(start-sql-recording :type :both :database *default-database*)
#+test
(stop-sql-recording :type :both)


(defun mark-wrong-tsch-pos ()
  (let ((rows (select [stem] [code] [pos] [sub-id]
		      :from [noun-features "XX"]
		      :where [and [= [author] "Tsch"]
				  [null [comment]]
				  [in [stem] [select [stem]
						     :from [noun-features]
						     :where [and [= [comment] "Rayfield"]
								[<> [pos] [xx pos]]
								[= [stem] [xx stem]]
								[= [code] [xx code]]]]]])))
    (pprint rows)
    #+ignore
    (with-transaction ()
      (dolist (row rows)
	(destructuring-bind (stem code pos sub-id) row
	  (update-records [noun-features]
			  :where [and [= [stem] stem]
				      [= [code] code]
				      [= [pos] pos]
				      [= [sub-id] sub-id]]
			  :attributes '([comment])
			  :values '("wrong-pos")))))))

#+test
(delete-records :from [noun-features] :where [and [like [stem] "Gvino"] [= [code] "O"]])

#+test
(mark-wrong-tsch-pos)

#|| errors:
ავტოგენური <i>a</i> autogenous
პათოგენური <i>a</i> pathogenic
პასკვილური <i>a</i> libellous, slanderous, scurrilous
პეიზაჟური <i>n a art</i> landscape (<i>painting etc</i>)
რეგიონალური <i>a</i> regional <i>see</i> რეგიონული
სადომაზოხისტური <i>a</i> sadomasochist(ic)
||#


#+test
(update-records [noun-features]
		:where  [and [= [stem] "Gvino"]]
		:attributes '([style-features])
		:values (list "+Norm"))
#+test
(delete-records :from [noun-features] :where [and [like [stem] ""]])

#+test
(insert-records :into [noun-features]
		:attributes '([stem] [code] [pos] [sub-id] [features] [author] [date])
		:values (list "dro" "O3" "n" 1 "+N+Temp" "Tsch" (get-universal-time)))
#+test
(update-records [noun-features]
		:attributes '([date]) :values (list (get-universal-time)) :where [= [date] 2011])
		 

#||

(parse-rayfield-line "შენი 1 <i>2p</i> (<i>sing fam</i>) <i>a</i> your, yours: ~ (სიკეთისა) არ იყოს… <i>©</i> just like you… (<i>in bad circumstances, e.g. forgetting, disliking sb/sth</i>); 2 <i>pp n ‡</i> built; dwelling: შენ არს has been built; 3 <i>ij vu eu</i> Eff you!")

(parse-rayfield-line "ოშიყანა რაბა <i>n Ju</i> four species (Sukkot ritual)")

(parse-rayfield-line "ხისთავა (-ას), ხისთავიანი <i>n a</i> blockhead")

;;(parse-rayfield-line "ჯმუხი 1 <i>a</i> stocky; 2 sullen, grim; 3 <i>n ©</i> bumpkin")

(parse-rayfield-line "აახარისხებს (აახარისხა, აუხარისხებია) <i>math</i> will square (<i>a number</i>)")

(parse-rayfield-line "ჰარალ(ალ)ი, ჰარალ(ალ)ე (–ეს), ჰარალ(ალ)ო <i>n</i> <i>refrain to Georgian folk-song</i>")

(parse-rayfield-line "ჰარიჰარალი, ჰარიჰარალე (–ეს) <i>n</i> <i>refrain to Georgian folk-song</i>")

(parse-rayfield-line "ავალა (-ას) <i>n</i> free space; rising space (<i>in chimney</i>)")

(parse-rayfield-line "ანისონი (<i>†</i>), ანისული, ანისუნი (<i>†</i>) <i>n bot</i> aniseed (<i>Pimpinella anisum</i>)")

(with-transaction ()
  (parse-rayfield-line "ბერა 1 (–ას) <i>a Ps</i> <i>dim</i> (<i>sb</i>) old: ~ თითი thumb; 2 (-ის) <i>n</i> sheep-run/cote (<i>for counting/milking</i>); coral (<i>for deer</i>): ბერაში წველა letting sheep in one-by-one for milking; 3 gauntlet (<i>that prisoners had to run</i>): ბერაში გატარება embarrassing sb publicly; 4 (-ას) goal, marked place (<i>in children’s games to hit ball, to stand with eyes shut etc</i>); 5 <i>Ka</i> sheaves laid criss-cross at top of cart-load of grain; 6 <i>‡ dim</i> monk"))
||#

#+main
(with-transaction ()
  (with-file-lines (line "~/lisp/projects/web-dictionary/data/rayfield-utf8.txt")
    (let ((line (string-trim " " line)))
      (unless (or (zerop (length line))
		  (char= (char line 0) #\#))
	(parse-rayfield-line line)))))

#+test
(print (string-parse
	"ჰუი <i>n</i>: ჰაი ~ (women’s) excited chatter <i>see</i> ჰაი–ჰუი"
	:left-separating-chars ",;:…?!){[]}\"–„”‚’"
	:right-separating-chars "([{}]\"‚’„”"
	:whitespace #(#\Space)))


;;(delete-records :from [noun-features] :where [like [stem] "-%"])

#+test
(print (select [features] :distinct t :from [noun-features]
	       :where [not [null [features]]]))
;;(print (select [count [*]] :from [noun-features]))

;; titles

#+test
(with-transaction ()
  (block title
    (with-file-lines (line "projects:georgian-corpus;titles-reduced.txt")
      (unless (or (string= line "")
		  (char= (char line 0) #\#))
	(destructuring-bind (count stem &optional (type "title")) (split line #\space)
	  (declare (ignore count))
	  (update-records [noun-features]
			  :attributes '([features])
			  :values (list
				   (cond ((equal type "title")
					  "+N+Title")
					 ((equal type "an")
					  "+N+Anim+Title")
					 ((equal type "an?")
					  "+N+Anim?+Title")
					 ((equal type "meas")
					  "+N+Meas")
					 (t
					  (error "Should not happen: type = ~s" type))))
			  :where [and [= [stem] stem] [or [null [comment]] [<> [comment] "wrong-pos"]]])
	  #+ignore
	  (print (cons type (select [stem] [code] [pos] [features]
				    :from [noun-features]
				    :where [and [= [stem] stem] [or [null [comment]] [<> [comment] "wrong-pos"]]])))))
      (when (string= line "33 kreditor an")
	(return-from title)))))

#+test
(cl-user::asdf :cl-fst)

#+test
(defparameter *kartuli-morph*
  (make-instance 'cl-fst:fst-net :file "projects:georgian-morph;regex;kartuli-morph.fst"))
#+test
(dolist (stem+f (select [stem] [style-features]
		      :from [noun-features]
		      :where [= [pos] "p.f."]))
  (destructuring-bind (stem features) stem+f
    (when (and (or (null features) (search "+Norm" features))
	       (search "sa" stem))
      (cl-fst:fst-lookup *kartuli-morph* stem
			 (lambda (word readings)
			   (declare (ignore word))
			   (unless (search "+FutPart" readings)
			     (format t "~a~%" stem)))))))

#+test
(dolist (stem+f (select [stem] [style-features]
		      :from [noun-features]
		      :where [= [pos] "p.n."]))
  (destructuring-bind (stem features) stem+f
    (when (or (null features) (search "+Norm" features))
      (cl-fst:fst-lookup *kartuli-morph* stem
			 (lambda (word readings)
			   (declare (ignore word))
			   (unless (search "+NegPart" readings)
			     (format t "~a~%" stem)))))))

#+test
(dolist (stem+f (select [stem] [style-features]
		      :from [noun-features]
		      :where [= [pos] "p.p."]))
  (destructuring-bind (stem features) stem+f
    (when (or (null features) (search "+Norm" features))
      (cl-fst:fst-lookup *kartuli-morph* stem
			 (lambda (word readings)
			   (declare (ignore word))
			   (unless (search "+PastPart" readings)
			     (format t "~a~%" stem)))))))

#+test
(dolist (stem+f (select [stem] [style-features]
		      :from [noun-features]
		      :where [= [pos] "p.a."]))
  (destructuring-bind (stem features) stem+f
    (when (or (null features) (search "+Norm" features))
      (cl-fst:fst-lookup *kartuli-morph* stem
			 (lambda (word readings)
			   (declare (ignore word))
			   (unless (search "+PresPart" readings)
			     (format t "~a~%" stem)))))))

(with-open-file (stream "projects:georgian-morph;wordlists;oge-deu-noun-code1.tsv"
			  :direction :output :if-exists :supersede)
  (u:with-file-fields ((word code pos features trans) "projects:georgian-morph;wordlists;oge-deu-noun-code.tsv")
    (labels ((ends-in (&rest sfx-list)
	       (loop for sfx in sfx-list
		  thereis (string= word sfx :start1 (max 0 (- (length word) (length sfx)))))))
      (when (and (ends-in "ება-ჲ" "ობა-ჲ" "ვა-ჲ")
		 (string= pos "N"))
	(setf code "R"))
      (format stream "~{~a~^	~}~%" (list word code pos features trans)))))
	
		     
#+main
(with-transaction ()
  (block test
    (u:with-file-fields ((word code pos features trans) "projects:georgian-morph;wordlists;oge-deu-noun-code.tsv")
      (unless (char= (char word 0) #\#)
	(loop for c across pos
	   do (make-instance 'noun-features
			     :root (if (eq (char word (- (length word) 2)) #\-)
				       (subseq word 0 (- (length word) 2))
				       word)
			     :code code
			     :pos (string (char-downcase c))
			     :unique-id 1
			     :features (cond ((string= features "")
					      nil)
					     ((string= features "N")
					      "+Prop+Name")
					     ((string= features "A")
					      "+N+Anim")
					     ((string= features "AT")
					      "’Н+Anim+Title")
					     ((string= features "G")
					      "+Prop+Geo+Area")
					     ((string= features "C")
					      "+Prop+Geo+City")
					     ((string= features "F")
					      "+Prop+Name+FirstName")
					     (t
					      (error "~a not found: ~a" features word)))
			     :lang "og"
			     :author "სარჯველაძე"
			     :translation trans
			     :date (get-universal-time))))
      #+test
      (return-from test))))


#+test
(update-records [morph noun-features]
		:av-pairs `(([features] "+Comp"))
		:where [and [like [stem] "უ%ეს"]
			    [= [pos] "a"]])

#+test
(update-records [morph noun-features]
		:av-pairs `(([features] "+Elat"))
		:where [= [features] "+Comp"])

#+test
(update-records [morph noun-features]
		:av-pairs `(([features] "+N+Anim+Qual"))
		:where [like [stem] "%ოლოგ"])

#+test
(u:with-file-lines (line "/raid/svn/lisp/projects/georgian-morph/regex/ng-adv.regex")
  (unless (or (search "# s" line)
	      (search "# m" line)
	      (search "# t" line)
	      (search "# l" line))
    (write-line line)))

;; words that are optionally syncopated
#+test
(with-database-connection ()
  (let ((count 0))
    (do-query ((stem)
	       [select [x stem] :flatp t
		       :from [morph noun-features :as x]
		       :left-join [morph noun-features :as y]
		       :on [= [x stem] [y stem]]
		       :where [and [= [x code] "A"]
				   [= [y code] "B"]
				   [= [x pos] "n"]
				   [= [y pos] "n"]
				   [or [null [x features]] [not [like [x features] "%Prop%"]]]
				   [or [null [y features]] [not [like [y features] "%Prop%"]]]
				   ]
		       :order-by [stem]])
      (incf count)
      (format t "~a: ~a~%" stem (syncope-stem stem t)))
    (print count)))

;; VN
#+test ;; result see noun-vn-list.txt
(write-vn)

#+test
(defparameter *vn-table* (dat:make-string-tree))

#+test
(with-database-connection ()
  (let ((count 0)
	(table (dat:make-string-tree)))
    (do-query ((stem code)
	       [select [stem] [code]
		       :from [morph noun-features]
		       :where [and [= [pos] "n"]
				   [or [null [comment]]
				       [not [like [comment] "%wrong-pos%"]]]]
		       :distinct t
		       :order-by [stem]])
      (let* ((code (intern code :keyword))
	     (codes (dat:string-tree-get *vn-table* stem)))
	(when (find code codes)
	  (setf (dat:string-tree-get table (reverse stem)) code)
	  ;;(write-line (marked-nom-form stem code))
	  (incf count))))
    (dat:do-string-tree (mets code table)
      (write-line (marked-nom-form (nreverse mets) code)))
    (print count)))

;; update Noun database
#+test
(with-database-connection ()
  (let ((count 0)
	(table (dat:make-string-tree)))
    (do-query ((stem code comment)
	       [select [stem] [code] [comment]
		       :from [morph noun-features]
		       :where [= [pos] "n"]
		       :distinct t
		       :order-by [stem]])
      (let* ((code (intern code :keyword))
	     (codes (dat:string-tree-get *vn-table* stem)))
	(when (find code codes)
	  (setf (dat:string-tree-get table stem) (list code comment))
	  ;;(write-line (marked-nom-form stem code))
	  (incf count))))
    (with-transactionxx ()
      (dat:do-string-tree (stem code+comment table)
	(unless (or (equal stem "ომ")
		    (string= stem "ობა" :start1 (max 0 (- (length stem) 3))))
	  (destructuring-bind (code comment) code+comment
	    (let ((comment (if comment (u:concat comment " VN") "VN"))) 
	      (update-records [morph noun-features]
			      :where [and [= [pos] "n"]
					  [= [code] ?code]
					  [= [stem] ?stem]]
			      :av-pairs `(([comment] ,comment))))))))
    (print count)))

#+test
(defun write-vn ()
  (let ((pr-root-table (make-hash-table :test #'equal)))
    ;; collect present roots
    (do-query ((id sub-id root lang)
	       [select [id] [sub-id] [root] [lang]
		       :from [morph verb-features]
		       :where [like [tense] "%|present|%"]])
      (when lang (setf lang (intern (string-upcase lang) :keyword)))
      (pushnew (cons root lang) (gethash (list id sub-id) pr-root-table) :test #'equal))
    (dolist (asp '(:perf :imperf))
      (do-query ((id sub-id stem conjugation c-root features-pr-root
		     root impf-pv pf-pv vn ;; impf-vn pf-vn
		     lang aspect masdar masdar-code masdar-lang masdar-aspect tense)
		 [select [part id]
			 [part sub-id]
			 [part stem] [part code] [verb-paradigm c-root]
			 [verb-features root]
			 [part root]
			 [impf-pv] [pf-pv]
			 [verb-paradigm vn] ;; [impf-vn] [pf-vn]
			 [part variety]
			 [part aspect]
			 [masdar stem]
			 [masdar code]
			 [masdar variety]
			 [masdar aspect]
			 [verb-features tense]
			 :distinct t
			 :from [morph verb-paradigm]
			 :left-join [morph participle :as part]
			 :on [and [= [part id] [verb-paradigm id]]
				  [= [part sub-id] [verb-paradigm features-sub-id]]]
			 :left-join [morph participle :as masdar]
			 :on [and [= [masdar id] [verb-paradigm id]]
				  [= [masdar sub-id] [verb-paradigm features-sub-id]]
				  [= [masdar type] :masdar]
				  [= [masdar main-form] t]
				  ]
			 :left-join [morph verb-features]
			 :on [and [= [verb-features id] [verb-paradigm id]]
				  [= [verb-features sub-id] [verb-paradigm features-sub-id]]]
			 :where [and [= [part type] :masdar]
				     ;; new Nov. 2013
				     [or [null [part wrong]]
					 [= [part wrong] :false]]
				     (if (eq asp :imperf)
					 [and [like [tense] "%|present|%"]
					      [or [null [part aspect]]
						  [= [part aspect] ""]
						  [= [part aspect] :imperf]]]
					 [and [or [like [tense] "%|future|%"]
						  [like [tense] "%|aorist|%"]]
					      [or [null [part aspect]]
						  [= [part aspect] ""]
						  [= [part aspect] :perf]]])]
			 :order-by `([part id] ;; [verb-paradigm sub-id] 
				     [part stem])])
	;;(print (list type sub-id lang masdar-lang asp masdar))
	(when (equal aspect "") (setf aspect nil))
	(when (equal masdar-aspect "") (setf masdar-aspect nil))
	(when (equal lang "") (setf lang nil))
	(when (equal masdar-lang "") (setf masdar-lang nil))
	(when masdar-code (setf masdar-code (intern (string-upcase masdar-code) :keyword)))
	(when (and (or (null aspect)
		       (null masdar-aspect)
		       (equal aspect masdar-aspect))
		   (or (null lang)
		       (null masdar-lang)
		       (equal lang masdar-lang))
		   (not (find #\* stem :start 1)))
	  ;; masdars with star (e.g., "*წერა") are to be combined with a preverb
	  ;;(when lang (print (list :lang lang asp masdar)))
	  (let* ((lang (or lang masdar-lang))
		 (impf-vn (or
			   (let ((pv-list (gethash (list id masdar) *vn-pv-table*)))
			     (cond ((null masdar)
				    (warn "No masdar found for ~a; using ~a." stem vn)
				    vn)
				   ((char/= (char masdar 0) #\*)
				    masdar ;; (marked-nom-form masdar masdar-code)
				    )
				   ((not (equal impf-pv "-"))
				    (u:concat impf-pv "-"
					      #+ignore(marked-nom-form (subseq masdar 1) masdar-code)
					      (subseq masdar 1)))
				   (t
				    (subseq masdar 1))
				   ((and (equal pf-pv "-") ;; pf form is preverbless
					 (find "-" pv-list :test #'equal))
				    (marked-nom-form (subseq masdar 1) masdar-code))
				   (t
				    (marked-nom-form (subseq masdar 1) masdar-code))))))
		 (pf-vn (or
			 (cond ((null masdar)
				(warn "No masdar found for ~a." stem)
				vn)
			       ((char/= (char masdar 0) #\*)
				masdar
				#+ignore
				(marked-nom-form masdar masdar-code))
			       (t
				(subseq masdar 1))
			       ((equal pf-pv "-")
				(marked-nom-form (subseq masdar 1) masdar-code))
			       (t
				(u:concat pf-pv "-"
					  (marked-nom-form (subseq masdar 1) masdar-code))))))
		 (vn (if (eq asp :imperf) impf-vn pf-vn)))
	    (unless (dat:string-tree-get *vn-table* vn)
	      (print vn))
	    (pushnew masdar-code (dat:string-tree-get *vn-table* vn))
	    ))))))

#+test
(with-database-connection ()
  (let ((count 0))
    (do-query ((stem)
	       [select [stem] :flatp t
		       :from [morph participle]
		       :where [and [= [type] :masdar]
				   ]
		       :distinct t
		       :order-by [stem]])
      (incf count)
      ;;(format t "~a: ~a~%" stem (syncope-stem stem t))
      (print stem)
      )
    (print count)))

;; u -> უ/ვ
#+test
(with-transaction ()
  (u:with-file-fields ((word trans) "projects:georgian-morph;wordlists;oge-deu.tsv")
    (declare (ignore trans))
    (let* ((hypos (position #\- word :start (max 0 (- (length word) 2))))
	   (stem (subseq word 0 hypos))
	   (u-pos (position #\უ stem :from-end t)))
      (when (and u-pos (> u-pos 0))
	(let ((stem-list
	       (labels ((u-pos (stem-list pos)
			  (let ((pos (position #\უ stem :start (1+ pos))))
			    (cond ((null pos)
				   stem-list)
				  (t
				   (let ((stem-list
					  (append stem-list
						  (mapcar (lambda (stem)
							    (let ((stem (copy-seq stem)))
							      (setf (char stem pos) #\u)
							      stem))
							  stem-list))))
				     (u-pos stem-list pos)))))))
		 (u-pos (list stem) 0))))
	  (dolist (u-stem stem-list)
	    (let ((v-stem (substitute #\ვ #\u u-stem)))
	      (when (and (select [stem] :from [morph noun-features]
				 :where [= [stem] ?v-stem])
			 (string/= stem v-stem))
		(update-records [morph noun-features]
				:av-pairs `(([stem] ,u-stem))
				:where [= [stem] ?v-stem])
		(print (list stem u-stem))))))))))


;; useless verbal nouns (ბავშვობა)

#+test
(do-query ((stem) [select [stem]
			  :from [morph participle]
			  :distinct t
			  :where [= [type] :masdar]])
  (print stem))

(defparameter *vn-test-table* (dat:make-string-tree))

(defparameter *vn-inverted-table* (dat:make-string-tree))

#+test
(write-fst-participle-stems-sql :type :masdar)

#+test
(dat:do-string-tree (masdar langs *vn-test-table*)
  (setf (dat:string-tree-get *vn-inverted-table* (reverse masdar))
	masdar))

#+test
(dat:do-string-tree (inv-masdar masdar *vn-inverted-table*)
  (let* ((has-pv (> (count #\- masdar) 1))
	 (masdar (delete-if (lambda (c) (find c "[-]"))
			    (subseq masdar 0 (position #\- masdar :from-end t))))
	 (masdar1 (substitute #\ვ #\u (substitute #\ე #\ჱ (substitute #\ხ #\ჴ  masdar)))))
       (when (and (not has-pv)
		  (select [stem] :from [morph noun-features]
			  :where [and [or [= [stem] ?masdar1]
					  [= [stem] ?masdar]]
				      [= [pos] "n"]
				      [like [comment] "%Rayfield%"]])
		  (not (select [stem] :from [morph noun-features]
			       :where [and [or [= [stem] ?masdar1]
					       [= [stem] ?masdar]]
					   [= [pos] "masd"]
					   [like [comment] "%Rayfield%"]])))
	 (print masdar1))
       ))

#+test ;; VNs that aren't tagged as VN in Rayfield; good candidates to be taken out as VN
(with-database-connection ()
  (with-transaction ()
    (dat:do-string-tree (inv-masdar masdar *vn-inverted-table*)
      (let* ((has-pv (> (count #\- masdar) 1))
	     (masdar (delete-if (lambda (c) (find c "[-]"))
				(subseq masdar 0 (position #\- masdar :from-end t))))
	     (masdar1 (substitute #\ვ #\u (substitute #\ე #\ჱ (substitute #\ხ #\ჴ  masdar))))
	     (a-masdar (char= (char masdar (1- (length masdar))) #\ა))
	     (%masdar (if a-masdar masdar (u:concat masdar "ი")))
	     (%masdar1 (if a-masdar masdar1 (u:concat masdar1 "ი")))
	     )
	(when (and (not has-pv)
		   (select [lemma] :from [dict-entry-kat-eng]
			   :where [and [or [= [lemma] ?%masdar1]
					   [= [lemma] ?%masdar]]
				       [not [like [entry] "%vn%"]]
				       [not [like [entry] "%vi1%"]]
				       [not [like [entry] "%vi2%"]]
				       [not [like [entry] "%vt2%"]]
				       ]))
	  (let ((stem (car (select [stem] :from [morph participle]
				   :flatp t
				   :where [and [or [= [stem] ?masdar]
						   [= [stem] (u:concat "*" masdar)]]
					       [= [type] :masdar]]))))
	    (update-records [morph participle]
			    :av-pairs `(([restriction] :pv-only))
			    :where [and [or [= [stem] ?masdar]
					    [= [stem] (u:concat "*" masdar)]]
					[= [type] :masdar]])
	    (print (list masdar stem))))
	))))

;; remove hyphen duplicates

#+test
(let* ((hy-words (select [stem] :from [morph noun-features]
			 :flatp t
			 :where [and [like [stem] "%-%"]
				     ;;[null [comment]]
				     ;;#+ignore
				     [not [like [comment] "%wrong%"]]]))
       (nohy-words (mapcar (lambda (word)
			     (delete #\- word))
			   hy-words)))
  ;;(print nohy-words)
  (print (length nohy-words))
  ;;#+test
  (with-transaction ()
    (dolist (stem nohy-words)
      (let ((lemma-list (select [stem] [pos] [comment]
				:from [morph noun-features]
				:flatp t
				:where [= [stem] ?stem])))
	(dolist (lemma-p-c lemma-list) 
	  (destructuring-bind (stem pos comment) lemma-p-c
	    (update-records [morph noun-features]
			    :av-pairs `(([comment]
					 ,(if (and comment (search "wrong-pos" comment))
					      "wrong-pos wrong-hyphen"
					      "wrong-hyphen")))
			    :where [and [= [stem] ?stem]
					[= [pos] ?pos]])))))))

#+test
(pprint (length (select [stem] [comment] :from [morph noun-features]
	       ;;:flatp t
	       :where [like [comment] "%wrong-hyphen%"])))

;;(delete-recordsxx :from [morph verb-translation] :where [and [= [id] 29] [= [sub-id] 7]])

;; fst -s masdar-list.fst
;; define vn ;
;; define vnBase vn "@U.SyncStem.-@" .o. [ "#" | "-" -> 0 ] ;
;; save stack vn.fst

#+test
(defparameter *vn-net*
  (make-instance 'cl-fst:fst-net :file "projects:georgian-morph;regex;vn.fst" :name :vn))

#+test
(cl-fst::fst-lookup *vn-net* "დაწერა"
		    (lambda (w lemma net)
		      (declare (ignore net))
		      (unless (equal lemma "")
			(print (list w (u:trim-whitespace lemma))))))

#+test
(with-database-connection ()
  (with-transactionxx ()
    (do-query ((noun comment)
	       [select [stem] [comment]
		       :from [morph noun-features]
		       :where [and [= [pos] "n"]
				   [or [null [comment]]
				       [and [not [like [comment] "%wrong%"]]
					    [not [like [comment] "%VN%"]]]]
				   [or [null [lang]]
				       [not [= [lang] "og"]]]]
		       :order-by [stem]])
      (cl-fst::fst-lookup *vn-net* noun
			  (lambda (w lemma net)
			    (declare (ignore net))
			    (unless (equal lemma "")
			      (print (list w (u:trim-whitespace lemma)))
			      (update-records [morph noun-features]
					      :av-pairs `(([comment]
							   ,(if comment
								(format nil "~a NVN" comment)
								"NVN")))
					      :where [and [= [pos] "n"]
							  [= [stem] ?noun]
							  [or [null [comment]]
							      [and [not [like [comment] "%wrong%"]]
								   [not [like [comment] "%VN%"]]]]
							  [or [null [lang]]
							      [not [= [lang] "og"]]]])
			      )))
      )))

#+test ;; missing ones have to be entered as VN
(with-database-connection ()
  (do-query ((noun) [select [stem] :from [morph noun-features]
			    :where [and [= [pos] "n"]
					[like [comment] "%VN%"]
					[not [like [comment] "%NVN%"]]
					[or [null [lang]]
					    [not [= [lang] "og"]]]]
			    :order-by [stem]])
    (cl-fst::fst-lookup *vn-net* noun
			(lambda (w lemma net)
			  (declare (ignore net))
			  (when (equal lemma "")
			    (print w))))))

#+test
(defparameter *ng-net*
  (make-instance 'cl-fst:fst-net :file "projects:georgian-morph;regex;georgian-morph-ng.fst" :name :ng))

#+test
(dolist (word *vn-cand*)
  (cl-fst::fst-lookup *ng-net*
		      (if (char= (char word (1- (length word))) #\ა)
			  word
			  (u:concat word "ი"))
		      (lambda (w l+f net)
			(declare (ignore net))
			(when (search "+VN" l+f)
			  (print word)))))

;; "ზღვევინება" 
;; "ყვანა" 


;; marking as N should be suppressed for these; but see *.
#+test
(defparameter *vn-list*
  '(("აგება" "ა-გებ[ა]-ჲ/გ") 
    ("ადენა" "ა-დენ[ა]-ჲ/დენ
ა-დენ[ა]-ჲ/დინ
ა-დენ[ა]-ჲ/დი") 
    ("ავდრობა" "ავდრობ[ა]-ჲ/ავდრ") 
    ("ავყიაობა" "ავყიაობ[ა]-ჲ/ავყია") 
    ("აკრძალვა" "ა-კრძალვ[ა]-ჲ/კრძალ") 
    ("ალინთვა" "ა-ლინთვ[ა]-ჲ/ლინთ") 
    ("ამაღლება" "ა-მაღლებ[ა]-ჲ/მაღლ") 
    ("ამოყრევინება" "ამო-ყრევინებ[ა]-ჲ/ყრ") 
    ("ამპარტავნობა" "ამპარტავნობ[ა]-ჲ/ამპარტავნ") 
    ("არევა" "ა-რევ[ა]-ჲ/რ") 
    ("აფეთქება" "ა-ფეთქებ[ა]-ჲ/ფეთქ") 
    ("აღგზნება" "აღ-გზნებ[ა]-ჲ/გზნ") 
    ("აღდგომა" "აღ-დგომ[ა]-ჲ/დგ") 
    ("აღება" "ა-ღებ[ა]-ჲ/ღ") 
    ("აღთქმა" "აღ-თქმ[ა]-ჲ/თქმ") 
    ("აღორძინება" "ა-ღორძინებ[ა]-ჲ/ღორძნ") 
    ("აღორძინება" "ა-ღორძინებ[ა]-ჲ/ღორძნ") 
    ("აღსრულება" "აღ-სრულებ[ა]-ჲ/სრულ") 
    ("აღტაცება" "აღ-ტაცებ[ა]-ჲ/ტაც") 
    ("აღქმა" "აღ-ქმ[ა]-ჲ/ქu") 
    ("აღწერა" "აღ-წერ[ა]-ჲ/წერ") 
    ("აყოლება" "ა-ყოლებ[ა]-ჲ/ყოლ") 
    ("აშრიალ" "ა-შრიალ-ი/შრიალ") 
    ("აშრობა" "ა-შრობ[ა]-ჲ/შVრ
ა-შრობ[ა]-ჲ/შრ") 
    ("აცმა" "ა-ცმ[ა]-ჲ/ცმ
ა-ცმ[ა]-ჲ/ცu") 
    ("აწონა" "ა-წონ[ა]-ჲ/წონ") 
    ("ბიჭობა" "ბიჭობ[ა]-ჲ/ბიჭ") 
    ("ბრჭობა" "ბრჭობ[ა]-ჲ/ბრჭ") 
    ("გაბმა" "გა-ბმ[ა]-ჲ/ბ") 
    ("გაგდებინება" "გა-გდებინებ[ა]-ჲ/გდ") 
    ("გაგება" "გა-გებ[ა]-ჲ/გ") 
    ("გაგებინება" "გა-გებინებ[ა]-ჲ/გ") 
    ("გადაბმა" "გადა-ბმ[ა]-ჲ/ბ") 
    ("გადაბრუნება" "გადა-ბრუნებ[ა]-ჲ/ბრუნ") 
    ("გადათქმევინება" "გადა-თქმევინებ[ა]-ჲ/თქ") 
    ("გადაკარება" "გადა-კარებ[ა]-ჲ/კარ") 
    ("გადაკეტვა" "გადა-კეტვ[ა]-ჲ/კეტ") 
    ("გადაქაჩვა" "გადა-ქაჩვ[ა]-ჲ/ქაჩ") 
    ("გადახდევინება" "გადა-ხდევინებ[ა]-ჲ/ხდ") 
    ("გაერთიანება" "გა-ერთიანებ[ა]-ჲ/ერთიან") 
    ("გათხრა" "გა-თხრ[ა]-ჲ/თხრ") 
    ("გალობა" "გალობ[ა]-ჲ/გალობ") 
    ("გამეხება" "გა-მეხებ[ა]-ჲ/მეხ") 
    ("გამოგება" "გამო-გებ[ა]-ჲ/გ") 
    ("გამოთიშვა" "გამო-თიშვ[ა]-ჲ/თიშ") 
    ("გამოტევება" "გამო-ტევებ[ა]-ჲ/ტევ") 
    ("გამოფენა" "გამო-ფენ[ა]-ჲ/ფინ
გამო-ფენ[ა]-ჲ/ფენ") 
    ("გამოცემა" "გამო-ცემ[ა]-ჲ/ცემ") 
    ("გამოხატვა" "გამო-ხატვ[ა]-ჲ/ხატ") 
    ("განგდება" "გან-გდებ[ა]-ჲ/გდ") 
    ("განგება" "გან-გებ[ა]-ჲ/გ") 
    ("განდგომა" "გან-დგომ[ა]-ჲ/დგ") 
    ("განსაკუთრება" "გან-საკუთრებ[ა]-ჲ/საკუთრ") 
    ("განსხვავება" "გან-სხვავებ[ა]-ჲ/სხვავ") 
    ("განჩინება" "გან-ჩინებ[ა]-ჲ/ჩინ") 
    ("განწყობა" "გან-წყობ[ა]-ჲ/წყ") 
    ("გარბენა" "გა-რბენ[ა]-ჲ/რბენ
გა-რბენ[ა]-ჲ/რბ") 
    ("გარდატეხა" "გარდა-ტეხ[ა]-ჲ/ტეხ") 
    ("გარემოცვა" "გარემო-ცვ[ა]-ჲ/ც") 
    ("გარიჟრაჟ" "გა-რიჟრაჟ-ი/რიჟრაჟ") ;; * 
    ("გასuენება" "გა-სuენებ[ა]-ჲ/სuენ") 
    ("გასამართლება" "გა-სამართლებ[ა]-ჲ/სამართლ") 
    ("გასაუბრება" "გა-საუბრებ[ა]-ჲ/საუბრ") 
    ("გატლეკა" "გა-ტლეკ[ა]-ჲ/ტლეკ") 
    ("გაფიცვა" "გა-ფიცვ[ა]-ჲ/ფიც") 
    ("გაქანება" "გა-ქანებ[ა]-ჲ/ქან") 
    ("გაყრევინება" "გა-ყრევინებ[ა]-ჲ/ყრ") 
    ("გაჩერება" "გა-ჩერებ[ა]-ჲ/ჩერ") 
    ("გაცვლა" "გა-ცვლ[ა]-ჲ/ცვლ") 
    ("გახარება" "გა-ხარებ[ა]-ჲ/ხარ") 
    ("გახრა" "გა-ხრ[ა]-ჲ/ხრ") 
    ("გახტომა" "გა-ხტომ[ა]-ჲ/ხტ") 
    ("გულოვნობა" "გულოვნობ[ა]-ჲ/გულოვან
გულოვნობ[ა]-ჲ/გულოვნ") 
    ("დაავადება" "და-ავადებ[ა]-ჲ/ავად") 
    ("დაბარვინება" "და-ბარვინებ[ა]-ჲ/ბარ") 
    ("დაბოლოება" "და-ბოლოებ[ა]-ჲ/ბოლოვ
და-ბოლოებ[ა]-ჲ/ბოლო") 
    ("დაგდებინება" "და-გდებინებ[ა]-ჲ/გდ") 
    ("დაგრილება" "და-გრილებ[ა]-ჲ/გრილ") 
    ("დაგრძელება" "და-გრძელებ[ა]-ჲ/გრძელ") 
    ("დაგუდლება" "და-გუდლებ[ა]-ჲ/გუდლ") 
    ("დადარება" "და-დარებ[ა]-ჲ/დარ") 
    ("დადევნა" "და-დევნ[ა]-ჲ/დევნ") 
    ("დადუმება" "და-დუმებ[ა]-ჲ/დუმ") 
    ("დაელექტროება" "და-ელექტროებ[ა]-ჲ/ელექტროვ
და-ელექტროებ[ა]-ჲ/ელექტრო") 
    ("დავაკება" "და-ვაკებ[ა]-ჲ/ვაკ") 
    ("დავლა" "და-ვლ[ა]-ჲ/ვლ") 
    ("დაზერგნა" "და-ზერგნ[ა]-ჲ/ზერგნ") 
    ("დაზმორება" "და-ზმორებ[ა]-ჲ/ზმორ") 
    ("დათავთავება" "და-თავთავებ[ა]-ჲ/თავთავ") 
    ("დათბუნვა" "და-თბუნვ[ა]-ჲ/თბუნ") 
    ("დათესვლა" "და-თესვლ[ა]-ჲ/თესლ") 
    ("დაკამკამება" "და-კამკამებ[ა]-ჲ/კამკამ") 
    ("დაკანვა" "და-კანვ[ა]-ჲ/კან") 
    ("დაკვლევინება" "და-კვლევინებ[ა]-ჲ/კვლ") 
    ("დაკლება" "და-კლებ[ა]-ჲ/კლ") 
    ("დაკრთომა" "და-კრთომ[ა]-ჲ/კრთ") 
    ("დალევინება" "და-ლევინებ[ა]-ჲ/ლ") 
    ("დამარაგება" "და-მარაგებ[ა]-ჲ/მარაგ") 
    ("დამართვა" "და-მართვ[ა]-ჲ/მართ") 
    ("დამეხვა" "და-მეხვ[ა]-ჲ/მეხ") 
    ("დამოქლონვა" "და-მოქლონვ[ა]-ჲ/მოქლონ") 
    ("დამსგავსება" "და-მსგავსებ[ა]-ჲ/მსგავს") 
    ("დამქრქალება" "და-მქრქალებ[ა]-ჲ/მქრქალ") 
    ("დამშევა" "და-მშევ[ა]-ჲ/მშ") 
    ("დამწყალობება" "და-მწყალობებ[ა]-ჲ/მწყალობ") 
    ("დანერბვა" "და-ნერბვ[ა]-ჲ/ნერბ") 
    ("დანიხვრა" "და-ნიხვრ[ა]-ჲ/ნიხრ") 
    ("დაპეპვლა" "და-პეპვლ[ა]-ჲ/პეპლა") 
    ("დარდიანობა" "დარდიანობ[ა]-ჲ/დარდიან") 
    ("დარტყმა" "და-რტყმ[ა]-ჲ/რტყ") 
    ("დარუფიავება" "და-რუფიავებ[ა]-ჲ/რუფიავ") 
    ("დარჩოლება" "და-რჩოლებ[ა]-ჲ/რჩოლ") 
    ("დასახლება" "და-სახლებ[ა]-ჲ/სახლ") 
    ("დასვლა" "და-სვლ[ა]-ჲ/სვლ
და-სვლ[ა]-ჲ/ვიდ
და-სვლ[ა]-ჲ/ვAლ
და-სვლ[ა]-ჲ/ვAლ
და-სვლ[ა]-ჲ/ვIდ") 
    ("დასობა" "და-სობ[ა]-ჲ/ს") 
    ("დატაბაკება" "და-ტაბაკებ[ა]-ჲ/ტაბაკ") 
    ("დატბორვა" "და-ტბორვ[ა]-ჲ/ტბორ") 
    ("დატუმბვა" "და-ტუმბვ[ა]-ჲ/ტუმბ") 
    ("დაუნჯება" "და-უნჯებ[ა]-ჲ/უნჯ") 
    ("დაურწყება" "და-ურწყებ[ა]-ჲ/ურწყ") 
    ("დაუქმება" "და-უქმებ[ა]-ჲ/უქმ") 
    ("დაუძლურება" "და-უძლურებ[ა]-ჲ/უძლურ") 
    ("დაფერდება" "და-ფერდებ[ა]-ჲ/ფერდ") 
    ("დაფლობა" "და-ფლობ[ა]-ჲ/ფლ") 
    ("დაქანება" "და-ქანებ[ა]-ჲ/ქან") 
    ("დაღალვა" "და-ღალვ[ა]-ჲ/ღალ") 
    ("დაღებინება" "და-ღებინებ[ა]-ჲ/ღ") 
    ("დაყოლება" "და-ყოლებ[ა]-ჲ/ყოლ") 
    ("დაყრევინება" "და-ყრევინებ[ა]-ჲ/ყრ") 
    ("დაშოშმინება" "და-შოშმინებ[ა]-ჲ/შოშმინ") 
    ("დაშრატება" "და-შრატებ[ა]-ჲ/შრატ") 
    ("დაცდევინება" "და-ცდევინებ[ა]-ჲ/ცდ") 
    ("დაცემინება" "და-ცემინებ[ა]-ჲ/ცემ") 
    ("დაცვა" "და-ცვ[ა]-ჲ/ც") 
    ("დაცხომა" "და-ცხომ[ა]-ჲ/ცხ") 
    ("დაძრობა" "და-ძრობ[ა]-ჲ/ძვრ
და-ძრობ[ა]-ჲ/ძრ") 
    ("დაწებოება" "და-წებოებ[ა]-ჲ/წებო") 
    ("დაწერინება" "და-წერინებ[ა]-ჲ/წერ") 
    ("დაწყებინება" "და-წყებინებ[ა]-ჲ/წყ") 
    ("დაწყობა" "და-წყობ[ა]-ჲ/წყ") 
    ("დახარება" "და-ხარებ[ა]-ჲ/ხარ") 
    ("დახარჯვინება" "და-ხარჯვინებ[ა]-ჲ/ხარჯ") 
    ("დახევინება" "და-ხევინებ[ა]-ჲ/ხ") 
    ("დახრა" "და-ხრ[ა]-ჲ/ხრ") 
    ("დახსოვნება" "და-ხსოვნებ[ა]-ჲ/ჴსოვნ") 
    ("დაჯარვა" "და-ჯარვ[ა]-ჲ/ჯარ") 
    ("დაჯგუფება" "და-ჯგუფებ[ა]-ჲ/ჯგუფ") 
    ("დეზერტირობა" "დეზერტირობ[ა]-ჲ/დეზერტირ") 
    ("დიდკაცობა" "დიდკაცობ[ა]-ჲ/დიდკაც") 
    ("დომინანტობა" "დომინანტობ[ა]-ჲ/დომინანტ") 
    ("დოსტაქრობა" "დოსტაქრობ[ა]-ჲ/დოსტაქრ") 
    ("დურგლობა" "დურგლობ[ა]-ჲ/დურგლ") 
    ("დღეგრძელობა" "დღეგრძელობ[ა]-ჲ/დღეგრძელ") 
    ("ერობა" "ერობ[ა]-ჲ/ერ") 
    ("ექიმობა" "ექიმობ[ა]-ჲ/ექიმ") ;; *
    ("ვაჟკაცობა" "ვაჟკაცობ[ა]-ჲ/ვაჟკაც") ;; * 
    ("ვარდობა" "ვარდობ[ა]-ჲ/ვარდ") ;; *
    ("ვაჭრობა" "ვაჭრობ[ა]-ჲ/ვაჭრ") 
    ("ვრდომა" "ვარდნ[ა]-ჲ/ვარდ") ;; ?
    ("ზრობა" "ზრობ[ა]-ჲ/ზრ") 
    ("თავგასულობა" "თავგასულობ[ა]-ჲ/თავგასულ") 
    ("თავკაცობა" "თავკაცობ[ა]-ჲ/თავკაც") 
    ("თავმჯდომარეობა" "თავმჯდომარეობ[ა]-ჲ/თავმჯდომარე") 
    ("თანაგრძნობა" "თანა-გრძნობ[ა]-ჲ/გრძნ") 
    ("თაოსნობა" "თაოსნობ[ა]-ჲ/თაოსნ") 
    ("თითლიბაზობა" "თითლიბაზობ[ა]-ჲ/თითლიბაზ") 
    ("იავარქმნა" "იავარ-ქმნ[ა]-ჲ/ქმნ") 
    ("კეკელაობა" "კეკელაობ[ა]-ჲ/კეკელა") 
    ("კიკნა" "კიკნ[ა]-ჲ/კიკნ") 
    ("კოპწიაობა" "კოპწიაობ[ა]-ჲ/კოპწიავ
კოპწიაობ[ა]-ჲ/კოპწია") 
    ("კოჭლობა" "კოჭლობ[ა]-ჲ/კოჭლ") 
    ("კრვა" "კრვა-ი/კრ
კრვ[ა]-ჲ/კრ
კრვ[ა]-ჲ/კვრ") 
    ("ლამაზობა" "ლამაზობ[ა]-ჲ/ლამაზ") ;; * 
    ("ლაჩრობა" "ლაჩრობ[ა]-ჲ/ლაჩრ") 
    ("ლეკვა" "ლეკვ[ა]-ჲ/ლეკ") 
    ("მადლობა" "მადლობ[ა]-ჲ/მადლ") 
    ("მამლაყინწობა" "მამლაყინწობ[ა]-ჲ/მამლაყინწ") 
    ("მდგომარეობა" "მდგომარეობ[ა]-ჲ/მდგომარე") 
    ("მედიდურობა" "მედიდურობ[ა]-ჲ/მედიდურ") 
    ("მედუქნეობა" "მედუქნეობ[ა]-ჲ/მედუქნე") 
    ("მევახშეობა" "მევახშეობ[ა]-ჲ/მევახშე") 
    ("მეძავობა" "მეძავობ[ა]-ჲ/მეძავ") 
    ("მეჭურჭლეობა" "მეჭურჭლეობ[ა]-ჲ/მეჭურჭლე") 
    ("მიდევნა" "მი-დევნ[ა]-ჲ/დევნ") 
    ("მიდენა" "მი-დენ[ა]-ჲ/დინ
მი-დენ[ა]-ჲ/დი") 
    ("მიდუღება" "მი-დუღებ[ა]-ჲ/დუღ") 
    ("მიზმანება" "მი-ზმანებ[ა]-ჲ/ზმან") 
    ("მილტოლვა" "მი-ლტოლვ[ა]-ჲ/ლტu") 
    ("მიმოქცევა" "მიმო-ქცევ[ა]-ჲ/ქც") 
    ("მიმოხილვა" "მიმო-ხილვ[ა]-ჲ/ხილ") 
    ("მინიშნება" "მი-ნიშნებ[ა]-ჲ/ნიშნ") 
    ("მირთვა" "მი-რთვ[ა]-ჲ/რთ") 
    ("მიფერება" "მი-ფერებ[ა]-ჲ/ფერ") 
    ("მიჩუმება" "მი-ჩუმებ[ა]-ჲ/ჩუმ") 
    ("მიცვლა" "მი-ცuალებ[ა]-ჲ/ცვლ") 
    ("მიხuევა" "მი-ხuევ[ა]-ჲ/ხu") 
    ("მიხედვა" "მი-ხედვ[ა]-ჲ/ხედ") 
    ("მოგება" "მო-გებ[ა]-ჲ/გ") 
    ("მოდგმა" "მო-დგმ[ა]-ჲ/დგ") 
    ("მოვლენა" "მო-ვლენ[ა]-ჲ/ვლინ
მო-ვლენ[ა]-ჲ/ვლენ") 
    ("მოთავეობა" "მოთავეობ[ა]-ჲ/მოთავე") 
    ("მოთხრობა" "მო-თხრობ[ა]-ჲ/თხრ") 
    ("მოკიდება" "მო-კიდებ[ა]-ჲ/კიდ") 
    ("მოკირიანება" "მო-კირიანებ[ა]-ჲ/კირიან") 
    ("მოლაპარაკება" "მო-ლაპარაკებ[ა]-ჲ/ლაპარაკ") 
    ("მოლოდება" "მო-ლოდებ[ა]-ჲ/ლოდ") 
    ("მომართვა" "მო-მართვ[ა]-ჲ/მართ") 
    ("მომდაბლება" "მო-მდაბლებ[ა]-ჲ/მდაბლ") 
    ("მომზადება" "მო-მზადებ[ა]-ჲ/მზად") 
    ("მომხრობა" "მო-მხრობ[ა]-ჲ/მხრ") 
    ("მონადირობა" "მო-ნადირობ[ა]-ჲ/ნადირ") 
    ("მონათვლინება" "მო-ნათვლინებ[ა]-ჲ/ნათლ") 
    ("მონანება" "მო-ნანებ[ა]-ჲ/ნან") 
    ("მონდომა" "მო-ნდომ[ა]-ჲ/ნდ") 
    ("მონიშვნა" "მო-ნიშვნ[ა]-ჲ/ნიშნ") 
    ("მოსაზრება" "მო-საზრებ[ა]-ჲ/საზრ") 
    ("მოსვლა" "მო-სვლ[ა]-ჲ/სვლ") 
    ("მოსრულება" "მო-სრულებ[ა]-ჲ/სრულ") 
    ("მოტლეკა" "მო-ტლეკ[ა]-ჲ/ტლეკ") 
    ("მოფოლხuება" "მო-ფოლხuებ[ა]-ჲ/ფოლხu") 
    ("მოფოცხვა" "მო-ფოცხვ[ა]-ჲ/ფოცხ") 
    ("მოფრენა" "მო-ფრენ[ა]-ჲ/ფრინ
მო-ფრენ[ა]-ჲ/ფრენ") 
    ("მოყuარება" "მო-ყuარებ[ა]-ჲ/ყuარ") 
    ("მოშურნეობა" "მოშურნეობ[ა]-ჲ/მოშურნე") 
    ("მოჩuენება" "მო-ჩuენებ[ა]-ჲ/ჩuენ") 
    ("მოწილვა" "მო-წილვ[ა]-ჲ/წილ") 
    ("მოწმობა" "მოწმობ[ა]-ჲ/მოწმ") 
    ("მოხოხვა" "მო-ხოხვ[ა]-ჲ/ხოხ") 
    ("მოჴსენება" "მო-ჴსენებ[ა]-ჲ/ჴსენ") 
    ("მოჯამაგირეობა" "მოჯამაგირეობ[ა]-ჲ/მოჯამაგირე") 
    ("მოჯახუნება" "მო-ჯახუნებ[ა]-ჲ/ჯახუნ") 
    ("მოჯრა" "მო-ჯრ[ა]-ჲ/ჯრ") 
    ("მუსიკობა" "მუსიკობ[ა]-ჲ/მუსიკ") 
    ("მშობიარობა" "მშობიარობ[ა]-ჲ/მშობიარ") 
    ("მძლეობა" "მძლეობ[ა]-ჲ/მძლე") 
    ("მხიარულობა" "მხიარულობ[ა]-ჲ/მხიარულ") 
    ("პარაზიტობა" "პარაზიტობ[ა]-ჲ/პარაზიტ") 
    ("პყრობა" "პყრობ[ა]-ჲ/პყრ") 
    ("სარგებლობა" "სარგებლობ[ა]-ჲ/სარგებლ") 
    ("საუბარ" "საუბარ-ი/საუბრ
საუბ[ა]რ-ი/საუბრ") 
    ("საუბარ" "საუბარ-ი/საუბრ
საუბ[ა]რ-ი/საუბრ") 
    ("სიამტკბილობა" "სიამტკბილობ[ა]-ჲ/სიამტკბილ") 
    ("სობა" "სობ[ა]-ჲ/სu
სობ[ა]-ჲ/ს") 
    ("სტუმრობა" "სტუმრობ[ა]-ჲ/სტუმრ") 
    ("სუმა" "სუმ[ა]-ჲ/სუმ") 
    ("ტანა" "ტან[ა]-ჲ/ქu") 
    ("უკადრისობა" "უკადრისობ[ა]-ჲ/უკადრის") 
    ("უკმეხობა" "უკმეხობ[ა]-ჲ/უკმეხ") 
    ("უცილობლობა" "უცილობლობ[ა]-ჲ/უცილობლ") 
    ("ქარაფშუტობა" "ქარაფშუტობ[ა]-ჲ/ქარაფშუტ") 
    ("ქლესა" "ქლეს[ა]-ჲ/ქლეს") 
    ("ქოთობა" "ქოთობ[ა]-ჲ/ქოთ") 
    ("ქონა" "ქონ[ა]-ჲ/ქონი
ქონ[ა]-ჲ/ქვ%") 
    ("ყაზახობა" "ყაზახობ[ა]-ჲ/ყაზახ") 
    ("ყაირათობა" "ყაირათობ[ა]-ჲ/ყაირათ") 
    ("ყალბობა" "ყალბობ[ა]-ჲ/ყალბ") 
    ("ყოჩობა" "ყოჩობ[ა]-ჲ/ყოჩ") 
    ("შეგნება" "შე-გნებ[ა]-ჲ/გნ") 
    ("შევრდომა" "შე-ვარდნ[ა]-ჲ/ვარდ") 
    ("შეიარაღება" "შე-იარაღებ[ა]-ჲ/იარაღ") 
    ("შეკვრა" "შე-კვრა-ი/კრ
შე-კვრ[ა]-ჲ/კრ
შე-კვრ[ა]-ჲ/კვრ") 
    ("შეკუმშვა" "შე-კუმშვ[ა]-ჲ/კუმშ") 
    ("შელოცვა" "შე-ლოცვ[ა]-ჲ/ლოც") 
    ("შემართება" "შე-მართებ[ა]-ჲ/მართ") 
    ("შემაღლება" "შე-მაღლებ[ა]-ჲ/მაღლ") 
    ("შემდურება" "შე-მდურებ[ა]-ჲ/მდურ") 
    ("შემთხuევა" "შე-მთხuევ[ა]-ჲ/მთხu") 
    ("შემოდგომა" "შემო-დგომ[ა]-ჲ/დგ") ;; * 
    ("შემორონინება" "შემო-რონინებ[ა]-ჲ/რონინ") 
    ("შემოწმება" "შე-მოწმებ[ა]-ჲ/მოწმ") 
    ("შემოხედვა" "შემო-ხედვ[ა]-ჲ/ხედ") 
    ("შემჩნევა" "შე-მჩნევ[ა]-ჲ/მჩნ") 
    ("შენდობა" "შე-ნდობ[ა]-ჲ/ნდ") 
    ("შეოჭვა" "შე-ოჭვ[ა]-ჲ/ოჭ") 
    ("შესხლტომა" "შე-სხლტომ[ა]-ჲ/სხლტ") 
    ("შეტაკება" "შე-ტაკებ[ა]-ჲ/ტაკ") 
    ("შეტყობინება" "შე-ტყობინებ[ა]-ჲ/ტყ") 
    ("შეფერდება" "შე-ფერდებ[ა]-ჲ/ფერდ") 
    ("შეფერფვლა" "შე-ფერფვლ[ა]-ჲ/ფერფლ") 
    ("შეფიცვრა" "შე-ფიცვრ[ა]-ჲ/ფიცრ") 
    ("შექცევა" "შე-ქცევ[ა]-ჲ/ქც") 
    ("შეღწევა" "შე-ღწევ[ა]-ჲ/ღწ") 
    ("შეყიდვა" "შე-ყიდვ[ა]-ჲ/ყიდ") 
    ("შეშთობა" "შე-შთობ[ა]-ჲ/შთ") 
    ("შეშფოთება" "შე-შფოთებ[ა]-ჲ/შფოთ") 
    ("შეცდომა" "შე-ცდომ[ა]-ჲ/ცდ") 
    ("შეჭუხვა" "შე-ჭუხვ[ა]-ჲ/ჭუხ") 
    ("შეჭყაპება" "შე-ჭყაპებ[ა]-ჲ/ჭყაპ") 
    ("შეხიზნება" "შე-ხიზნებ[ა]-ჲ/ხიზნ") 
    ("შეხრაკვა" "შე-ხრაკვ[ა]-ჲ/ხრაკ") 
    ("შთობა" "შთობ[ა]-ჲ/შთ") 
    ("შობა" "შობ[ა]-ჲ/შობ") ;; *
    ("შუამდგომლობა" "შუამდგომლობ[ა]-ჲ/შუამდგომლ") 
    ("ჩავარდნა" "ჩა-ვარდნ[ა]-ჲ/ვარდ") 
    ("ჩაკუზვა" "ჩა-კუზვ[ა]-ჲ/კუზ") 
    ("ჩალულვა" "ჩა-ლულვ[ა]-ჲ/ლულ") 
    ("ჩამატება" "ჩა-მატებ[ა]-ჲ/მატ") 
    ("ჩამობრუნება" "ჩამო-ბრუნებ[ა]-ჲ/ბრუნ") 
    ("ჩამორჩომა" "ჩამო-რჩენ[ა]-ჲ/რჩომ") 
    ("ჩამოფასება" "ჩამო-ფასებ[ა]-ჲ/ფას") 
    ("ჩამოყრევინება" "ჩამო-ყრევინებ[ა]-ჲ/ყრ") 
    ("ჩამტერება" "ჩა-მტერებ[ა]-ჲ/მტერ") 
    ("ჩამუხვლა" "ჩა-მუხვლ[ა]-ჲ/მუჴლ") 
    ("ჩამუხლისთავება" "ჩა-მუხლისთავებ[ა]-ჲ/მუჴლისთავ") 
    ("ჩამწნილება" "ჩა-მწნილებ[ა]-ჲ/მწნილ") 
    ("ჩასობა" "ჩა-სობ[ა]-ჲ/ს") 
    ("ჩასოლვა" "ჩა-სოლვ[ა]-ჲ/სოლ") 
    ("ჩატრიალება" "ჩა-ტრიალებ[ა]-ჲ/ტრიალ") 
    ("ჩაფისვა" "ჩა-ფისვ[ა]-ჲ/ფის") 
    ("ჩაფუკვა" "ჩა-ფუკვ[ა]-ჲ/ფუკ") 
    ("ჩაღრმავება" "ჩა-ღრმავებ[ა]-ჲ/ღრმავ") 
    ("ჩაყრევინება" "ჩა-ყრევინებ[ა]-ჲ/ყრ") 
    ("ჩაჩეხვა" "ჩა-ჩეხვ[ა]-ჲ/ჩეხ") 
    ("ჩაჩოქება" "ჩა-ჩოქებ[ა]-ჲ/ჩოქ") 
    ("ჩაციება" "ჩა-ციებ[ა]-ჲ/ცივ") 
    ("ჩაცხრომა" "ჩა-ცხრომ[ა]-ჲ/ცხრ") 
    ("ჩაწერინება" "ჩა-წერინებ[ა]-ჲ/წერ") 
    ("ჩაჭიდება" "ჩა-ჭიდებ[ა]-ჲ/ჭიდ") 
    ("ჩახრინწიანება" "ჩა-ხრინწიანებ[ა]-ჲ/ხრინწიან") 
    ("ძაღლობა" "ძაღლობ[ა]-ჲ/ძაღლ") 
    ("ძმობა" "ძმობ[ა]-ჲ/ძმ") ;; *
    ("წაგდებინება" "წა-გდებინებ[ა]-ჲ/გდ") 
    ("წავლა" "წა-ვლ[ა]-ჲ/ვლ") 
    ("წალულვა" "წა-ლულვ[ა]-ჲ/ლულ") 
    ("წამოგდებინება" "წამო-გდებინებ[ა]-ჲ/გდ") 
    ("წამოლანდება" "წამო-ლანდებ[ა]-ჲ/ლანდ") 
    ("წამოროტვა" "წამო-როტვ[ა]-ჲ/როტ") 
    ("წამოსარჩლება" "წამო-სარჩლებ[ა]-ჲ/სარჩლ") 
    ("წამოქროლვა" "წამო-ქროლვ[ა]-ჲ/ქროლ") 
    ("წამოჩრა" "წამო-ჩრ[ა]-ჲ/ჩრ") 
    ("წამოწყება" "წამო-წყებ[ა]-ჲ/წყ") 
    ("წარდგომა" "წარ-დგომ[ა]-ჲ/დგ") 
    ("წარმატება" "წარ-მატებ[ა]-ჲ/მატ") 
    ("წარმოდგომა" "წარმო-დგომ[ა]-ჲ/დგ") 
    ("წარმოდენა" "წარმო-დენ[ა]-ჲ/დი") 
    ("წარწყმენდა" "წარ-წყმენდ[ა]-ჲ/წყმდ
წარ-წყმენდ[ა]-ჲ/წყმენდ") 
    ("წასარჩლება" "წა-სარჩლებ[ა]-ჲ/სარჩლ") 
    ("წასხმა" "წა-სხმ[ა]-ჲ/სხ") 
    ("წაფრენა" "წა-ფრენ[ა]-ჲ/ფრენ") 
    ("წაშენა" "წა-შენ[ა]-ჲ/შენ") 
    ("წაწყმენდა" "წა-წყმენდ[ა]-ჲ/წყმდ
წა-წყმენდ[ა]-ჲ/წყმინდ
წა-წყმენდ[ა]-ჲ/წყმენდ") 
    ("წახედვა" "წა-ხედვ[ა]-ჲ/ხედ") 
    ("წახუმრება" "წა-ხუმრებ[ა]-ჲ/ხუმრ") 
    ("წინააღმდეგობა" "წინააღმდეგობ[ა]-ჲ/წინააღმდეგ") 
    ("წინაგრძნობა" "წინა-გრძნობ[ა]-ჲ/გრძნ") 
    ("წინამძღოლობა" "წინამძღოლობ[ა]-ჲ/წინამძღოლ") 
    ("წინაძღოლა" "წინა-ძღოლ[ა]-ჲ/ძღu") 
    ("წუწაობა" "წუწაობ[ა]-ჲ/წუწა") 
    ("ჭამა" "ჭამ[ა]-ჲ/ჭმ
ჭამ[ა]-ჲ/ჭამ
ჭამ[ა]-ჲ/ჭამ
ჭამ[ა]-ჲ/ჭამ") 
    ("ჭნობა" "ჭნობ[ა]-ჲ/ჭნ") 
    ("ჭორიკანაობა" "ჭორიკანაობ[ა]-ჲ/ჭორიკანა") 
    ("ხადილობა" "ხადილობ[ა]-ჲ/ხადილ") 
    ("ხასობა" "ხასობ[ა]-ჲ/ხას") 
    ("ხდილობა" "ხდილობ[ა]-ჲ/ჴდილ") 
    ("ხევა" "ხევ[ა]-ჲ/ხ") 
    ("ხელობა" "ხელობ[ა]-ჲ/ხელ") 
    ("ხერხიანობა" "ხერხიანობ[ა]-ჲ/ხერხიან") 
    ("ხტუნაობა" "ხტუნაობ[ა]-ჲ/ხტუნავ
ხტუნაობ[ა]-ჲ/ხტუნა") 
    ("ხუმრობა" "ხუმრობ[ა]-ჲ/ხუმრ") 
    ("ჴშვა" "ჴშვ[ა]-ჲ/ჴშ") 
    ("ჯაგვა" "ჯაგვ[ა]-ჲ/ჯაგ")
    )
  #+ignore
  (("აგება" "ა-გებ[ა]-ჲ/გ") 
   ("ადენა" "ა-დენ[ა]-ჲ/დენ
ა-დენ[ა]-ჲ/დინ
ა-დენ[ა]-ჲ/დი") 
   ("ავდრობა" "ავდრობ[ა]-ჲ/ავდრ") 
   ("ავყიაობა" "ავყიაობ[ა]-ჲ/ავყია") 
   ("აკრძალვა" "ა-კრძალვ[ა]-ჲ/კრძალ") 
   ("ალერს" "ალერსი/ალერს") ;; ?? 
   ("ალინთვა" "ა-ლინთვ[ა]-ჲ/ლინთ") 
   ("ამაღლება" "ა-მაღლებ[ა]-ჲ/მაღლ") 
   ("ამბორება" "ამბორებ[ა]-ჲ/ამბორ") 
   ;;("ამება" "ამებ[ა]-ჲ/ამ") 
   ("ამოყრევინება" "ამო-ყრევინებ[ა]-ჲ/ყრ") 
   ("ამპარტავნება" "ამპარტავნებ[ა]-ჲ/ამპარტავნ") 
   ("ამპარტავნობა" "ამპარტავნობ[ა]-ჲ/ამპარტავნ") 
   ("არევა" "ა-რევ[ა]-ჲ/რ") 
   ("აფეთქება" "ა-ფეთქებ[ა]-ჲ/ფეთქ") 
   ("აღგზნება" "აღ-გზნებ[ა]-ჲ/გზნ") 
   ("აღდგომა" "აღ-დგომ[ა]-ჲ/დგ") 
   ("აღება" "ა-ღებ[ა]-ჲ/ღ") 
   ("აღთქმა" "აღ-თქმ[ა]-ჲ/თქმ") 
   ("აღორძინება" "ა-ღორძინებ[ა]-ჲ/ღორძნ") 
   ("აღსრულება" "აღ-სრულებ[ა]-ჲ/სრულ") 
   ("აღტაცება" "აღ-ტაცებ[ა]-ჲ/ტაც") 
   ("აღქმა" "აღ-ქმ[ა]-ჲ/ქu") 
   ("აღწერა" "აღ-წერ[ა]-ჲ/წერ") 
   ("აყოლება" "ა-ყოლებ[ა]-ჲ/ყოლ") 
   ;; ("აშრიალ" "ა-შრიალ-ი/შრიალ") 
   ("აშრობა" "ა-შრობ[ა]-ჲ/შVრ
ა-შრობ[ა]-ჲ/შრ") 
   ("აცმა" "ა-ცმ[ა]-ჲ/ცმ
ა-ცმ[ა]-ჲ/ცu") 
   ("აწონა" "ა-წონ[ა]-ჲ/წონ") 
   ("აჯა" "აჯ[ა]-ჲ/აჯ") 
   ;;("ბანა" "ბან[ა]-ჲ/ბან") 
   ("ბარება" "ბარებ[ა]-ჲ/ბარ") 
   ("ბასვრა" "ბასვრ[ა]-ჲ/ბასვრ
ბასვრ[ა]-ჲ/ბასრ") 
   ("ბედება" "ბედებ[ა]-ჲ/ბედ") 
   ("ბზენა" "ბზენ[ა]-ჲ/ბზენ") 
   ("ბინავება" "ბინავებ[ა]-ჲ/ბინავ") 
   ;; ("ბიჭობა" "ბიჭობ[ა]-ჲ/ბიჭ") 
   ("ბოგინ" "ბოგინ-ი/ბოგინ") 
   ("ბრა" "ბრ[ა]-ჲ/ბრ") 
   ("ბრიალ" "ბრიალ-ი/ბრიალ") 
   ("ბრძოლა" "ბრძოლ[ა]-ჲ/ბრძ
ბრძოლა/ბრძ") 
   ("ბრჭობა" "ბრჭობ[ა]-ჲ/ბრჭ") 
   ("ბუქნა" "ბუქნ[ა]-ჲ/ბუქნ") 
   ("გაბმა" "გა-ბმ[ა]-ჲ/ბ") 
   ("გაგდებინება" "გა-გდებინებ[ა]-ჲ/გდ") 
   ("გაგება" "გა-გებ[ა]-ჲ/გ") 
   ("გაგებინება" "გა-გებინებ[ა]-ჲ/გ") 
   ("გადაბმა" "გადა-ბმ[ა]-ჲ/ბ") 
   ("გადაბრუნება" "გადა-ბრუნებ[ა]-ჲ/ბრუნ") 
   ("გადათქმევინება" "გადა-თქმევინებ[ა]-ჲ/თქ") 
   ("გადაკარება" "გადა-კარებ[ა]-ჲ/კარ") 
   ("გადაკეტვა" "გადა-კეტვ[ა]-ჲ/კეტ") 
   ("გადაქაჩვა" "გადა-ქაჩვ[ა]-ჲ/ქაჩ") 
   ("გადახდევინება" "გადა-ხდევინებ[ა]-ჲ/ხდ") 
   ("გაერთიანება" "გა-ერთიანებ[ა]-ჲ/ერთიან") 
   ("გათხრა" "გა-თხრ[ა]-ჲ/თხრ") 
   ("გალობა" "გალობ[ა]-ჲ/გალობ") 
   ("გამეხება" "გა-მეხებ[ა]-ჲ/მეხ") 
   ("გამოგება" "გამო-გებ[ა]-ჲ/გ") 
   ("გამოთიშვა" "გამო-თიშვ[ა]-ჲ/თიშ") 
   ("გამოტევება" "გამო-ტევებ[ა]-ჲ/ტევ") 
   ("გამოფენა" "გამო-ფენ[ა]-ჲ/ფინ
გამო-ფენ[ა]-ჲ/ფენ") 
   ("გამოცემა" "გამო-ცემ[ა]-ჲ/ცემ") 
   ("გამოხატვა" "გამო-ხატვ[ა]-ჲ/ხატ") 
   ("განგდება" "გან-გდებ[ა]-ჲ/გდ") 
   ("განგება" "გან-გებ[ა]-ჲ/გ") 
   ("განდგომა" "გან-დგომ[ა]-ჲ/დგ") 
   ("განსაკუთრება" "გან-საკუთრებ[ა]-ჲ/საკუთრ") 
   ("განსხვავება" "გან-სხვავებ[ა]-ჲ/სხვავ") 
   ("განჩინება" "გან-ჩინებ[ა]-ჲ/ჩინ") 
   ("განწყობა" "გან-წყობ[ა]-ჲ/წყ") 
   ("გარბენა" "გა-რბენ[ა]-ჲ/რბენ
გა-რბენ[ა]-ჲ/რბ") 
   ("გარდატეხა" "გარდა-ტეხ[ა]-ჲ/ტეხ") 
   ("გარება" "გარებ[ა]-ჲ/გარ") 
   ("გარემოცვა" "გარემო-ცვ[ა]-ჲ/ც") 
   ("გარიჟრაჟ" "გა-რიჟრაჟ-ი/რიჟრაჟ") 
   ("გასuენება" "გა-სuენებ[ა]-ჲ/სuენ") 
   ("გასამართლება" "გა-სამართლებ[ა]-ჲ/სამართლ") 
   ("გასაუბრება" "გა-საუბრებ[ა]-ჲ/საუბრ") 
   ("გატლეკა" "გა-ტლეკ[ა]-ჲ/ტლეკ") 
   ("გაფიცვა" "გა-ფიცვ[ა]-ჲ/ფიც") 
   ("გაქანება" "გა-ქანებ[ა]-ჲ/ქან") 
   ("გაყრევინება" "გა-ყრევინებ[ა]-ჲ/ყრ") 
   ("გაჩერება" "გა-ჩერებ[ა]-ჲ/ჩერ") 
   ("გაცვლა" "გა-ცვლ[ა]-ჲ/ცვლ") 
   ("გახარება" "გა-ხარებ[ა]-ჲ/ხარ") 
   ("გახრა" "გა-ხრ[ა]-ჲ/ხრ") 
   ("გახტომა" "გა-ხტომ[ა]-ჲ/ხტ") 
   ("გემოვნება" "გემოვნებ[ა]-ჲ/გემოვნ") 
   ("გრიალ" "გრიალ-ი/გრიალ") 
   ("გულოვნობა" "გულოვნობ[ა]-ჲ/გულოვან
გულოვნობ[ა]-ჲ/გულოვნ") 
   ("დაავადება" "და-ავადებ[ა]-ჲ/ავად") 
   ("დაბარვინება" "და-ბარვინებ[ა]-ჲ/ბარ") 
   ("დაბოლოება" "და-ბოლოებ[ა]-ჲ/ბოლოვ
და-ბოლოებ[ა]-ჲ/ბოლო") 
   ("დაგდებინება" "და-გდებინებ[ა]-ჲ/გდ") 
   ("დაგრილება" "და-გრილებ[ა]-ჲ/გრილ") 
   ("დაგრძელება" "და-გრძელებ[ა]-ჲ/გრძელ") 
   ("დაგუდლება" "და-გუდლებ[ა]-ჲ/გუდლ") 
   ("დადარება" "და-დარებ[ა]-ჲ/დარ") 
   ("დადევნა" "და-დევნ[ა]-ჲ/დევნ") 
   ("დადუმება" "და-დუმებ[ა]-ჲ/დუმ") 
   ("დაელექტროება" "და-ელექტროებ[ა]-ჲ/ელექტროვ
და-ელექტროებ[ა]-ჲ/ელექტრო") 
   ("დავაკება" "და-ვაკებ[ა]-ჲ/ვაკ") 
   ("დავლა" "და-ვლ[ა]-ჲ/ვლ") 
   ("დაზერგნა" "და-ზერგნ[ა]-ჲ/ზერგნ") 
   ("დაზმორება" "და-ზმორებ[ა]-ჲ/ზმორ") 
   ("დათავთავება" "და-თავთავებ[ა]-ჲ/თავთავ") 
   ("დათბუნვა" "და-თბუნვ[ა]-ჲ/თბუნ") 
   ("დათესვლა" "და-თესვლ[ა]-ჲ/თესლ") 
   ("დაკამკამება" "და-კამკამებ[ა]-ჲ/კამკამ") 
   ("დაკანვა" "და-კანვ[ა]-ჲ/კან") 
   ("დაკვლევინება" "და-კვლევინებ[ა]-ჲ/კვლ") 
   ("დაკლება" "და-კლებ[ა]-ჲ/კლ") 
   ("დაკრთომა" "და-კრთომ[ა]-ჲ/კრთ") 
   ("დალევინება" "და-ლევინებ[ა]-ჲ/ლ") 
   ("დამარაგება" "და-მარაგებ[ა]-ჲ/მარაგ") 
   ("დამართვა" "და-მართვ[ა]-ჲ/მართ") 
   ("დამეხვა" "და-მეხვ[ა]-ჲ/მეხ") 
   ("დამოქლონვა" "და-მოქლონვ[ა]-ჲ/მოქლონ") 
   ("დამსგავსება" "და-მსგავსებ[ა]-ჲ/მსგავს") 
   ("დამქრქალება" "და-მქრქალებ[ა]-ჲ/მქრქალ") 
   ("დამშევა" "და-მშევ[ა]-ჲ/მშ") 
   ("დამწყალობება" "და-მწყალობებ[ა]-ჲ/მწყალობ") 
   ("დანერბვა" "და-ნერბვ[ა]-ჲ/ნერბ") 
   ("დანიხვრა" "და-ნიხვრ[ა]-ჲ/ნიხრ") 
   ("დაპეპვლა" "და-პეპვლ[ა]-ჲ/პეპლა") 
   ("დარდიანობა" "დარდიანობ[ა]-ჲ/დარდიან") 
   ("დარტყმა" "და-რტყმ[ა]-ჲ/რტყ") 
   ("დარუფიავება" "და-რუფიავებ[ა]-ჲ/რუფიავ") 
   ("დარჩოლება" "და-რჩოლებ[ა]-ჲ/რჩოლ") 
   ("დასახლება" "და-სახლებ[ა]-ჲ/სახლ") 
   ("დასვლა" "და-სვლ[ა]-ჲ/სვლ
და-სვლ[ა]-ჲ/ვიდ
და-სვლ[ა]-ჲ/ვAლ
და-სვლ[ა]-ჲ/ვAლ
და-სვლ[ა]-ჲ/ვIდ") 
   ("დასობა" "და-სობ[ა]-ჲ/ს") 
   ("დატაბაკება" "და-ტაბაკებ[ა]-ჲ/ტაბაკ") 
   ("დატბორვა" "და-ტბორვ[ა]-ჲ/ტბორ") 
   ("დატუმბვა" "და-ტუმბვ[ა]-ჲ/ტუმბ") 
   ("დაუნჯება" "და-უნჯებ[ა]-ჲ/უნჯ") 
   ("დაურწყება" "და-ურწყებ[ა]-ჲ/ურწყ") 
   ("დაუქმება" "და-უქმებ[ა]-ჲ/უქმ") 
   ("დაუძლურება" "და-უძლურებ[ა]-ჲ/უძლურ") 
   ("დაფერდება" "და-ფერდებ[ა]-ჲ/ფერდ") 
   ("დაფლობა" "და-ფლობ[ა]-ჲ/ფლ") 
   ("დაქანება" "და-ქანებ[ა]-ჲ/ქან") 
   ("დაღალვა" "და-ღალვ[ა]-ჲ/ღალ") 
   ("დაღებინება" "და-ღებინებ[ა]-ჲ/ღ") 
   ("დაყოლება" "და-ყოლებ[ა]-ჲ/ყოლ") 
   ("დაყრევინება" "და-ყრევინებ[ა]-ჲ/ყრ") 
   ("დაშოშმინება" "და-შოშმინებ[ა]-ჲ/შოშმინ") 
   ("დაშრატება" "და-შრატებ[ა]-ჲ/შრატ") 
   ("დაცდევინება" "და-ცდევინებ[ა]-ჲ/ცდ") 
   ("დაცემინება" "და-ცემინებ[ა]-ჲ/ცემ") 
   ("დაცვა" "და-ცვ[ა]-ჲ/ც") 
   ("დაცხომა" "და-ცხომ[ა]-ჲ/ცხ") 
   ("დაძრობა" "და-ძრობ[ა]-ჲ/ძვრ
და-ძრობ[ა]-ჲ/ძრ") 
   ("დაწებოება" "და-წებოებ[ა]-ჲ/წებო") 
   ("დაწერინება" "და-წერინებ[ა]-ჲ/წერ") 
   ("დაწყებინება" "და-წყებინებ[ა]-ჲ/წყ") 
   ("დაწყობა" "და-წყობ[ა]-ჲ/წყ") 
   ("დახარება" "და-ხარებ[ა]-ჲ/ხარ") 
   ("დახარჯვინება" "და-ხარჯვინებ[ა]-ჲ/ხარჯ") 
   ("დახევინება" "და-ხევინებ[ა]-ჲ/ხ") 
   ("დახრა" "და-ხრ[ა]-ჲ/ხრ") 
   ("დახსოვნება" "და-ხსოვნებ[ა]-ჲ/ჴსოვნ") 
   ("დაჯარვა" "და-ჯარვ[ა]-ჲ/ჯარ") 
   ("დაჯგუფება" "და-ჯგუფებ[ა]-ჲ/ჯგუფ") 
   ("დგმა" "დგმ[ა]-ჲ/დგ") 
   ("დგომა" "დგომ[ა]-ჲ/დგ") 
   ("დედვა" "დედვ[ა]-ჲ/დედ") 
   ("დევნება" "დევნებ[ა]-ჲ/დევნ") 
   ("დეზერტირობა" "დეზერტირობ[ა]-ჲ/დეზერტირ") 
   ("დემორალიზება" "დემორალიზებ[ა]-ჲ/დემორალიზ") 
   ("დიდკაცობა" "დიდკაცობ[ა]-ჲ/დიდკაც") 
   ("დომინანტობა" "დომინანტობ[ა]-ჲ/დომინანტ") 
   ("დოსტაქრობა" "დოსტაქრობ[ა]-ჲ/დოსტაქრ") 
   ("დროება" "დროებ[ა]-ჲ/დრო") 
   ("დურგლობა" "დურგლობ[ა]-ჲ/დურგლ") 
   ("დღეგრძელობა" "დღეგრძელობ[ა]-ჲ/დღეგრძელ") 
   ("ელვა" "ელვ[ა]-ჲ/ელ") 
   ("ერობა" "ერობ[ა]-ჲ/ერ") 
   ("ექიმობა" "ექიმობ[ა]-ჲ/ექიმ") 
   ("ვაჟკაცობა" "ვაჟკაცობ[ა]-ჲ/ვაჟკაც") 
   ("ვარდობა" "ვარდობ[ა]-ჲ/ვარდ") 
   ("ვაჭრობა" "ვაჭრობ[ა]-ჲ/ვაჭრ") 
   ("ვნება" "ვნებ[ა]-ჲ/ვნ") 
   ("ვრდომა" "ვარდნ[ა]-ჲ/ვარდ") 
   ("ვსება" "ვსებ[ა]-ჲ/ვს") 
   ("ზარვა" "ზარვ[ა]-ჲ/ზარ") 
   ("ზელვა" "ზელვ[ა]-ჲ/ზილ
ზელვ[ა]-ჲ/ზელ") 
   ("ზიარება" "ზიარებ[ა]-ჲ/ზიარ") 
   ("ზლაზვნა" "ზლაზვნ[ა]-ჲ/ზლაზნ") 
   ("ზმა" "ზმ[ა]-ჲ/ზმ") 
   ("ზმანება" "ზმანებ[ა]-ჲ/ზმან") 
   ("ზმანვა" "ზმანვ[ა]-ჲ/ზმან") 
   ("ზორვა" "ზორვ[ა]-ჲ/ზორ") 
   ("ზრზენა" "ზრზენ[ა]-ჲ/ზრზენ
ზრზენ[ა]-ჲ/ზრზნ") 
   ("ზრზინება" "ზრზინებ[ა]-ჲ/ზრზინ
ზრზინებ[ა]-ჲ/ზრზენ") 
   ("ზრობა" "ზრობ[ა]-ჲ/ზრ") 
   ("ზღუდვა" "ზღუდვ[ა]-ჲ/ზღუდ") 
   ("თავგასულობა" "თავგასულობ[ა]-ჲ/თავგასულ") 
   ("თავისუფლება" "თავისუფლებ[ა]-ჲ/თავისუფლ") 
   ("თავკაცობა" "თავკაცობ[ა]-ჲ/თავკაც") 
   ("თავმჯდომარეობა" "თავმჯდომარეობ[ა]-ჲ/თავმჯდომარე") 
   ("თავცემა" "თავცემ[ა]-ჲ/თავცემ") 
   ("თათხვა" "თათხვ[ა]-ჲ/თათხ") 
   ("თანაგრძნობა" "თანა-გრძნობ[ა]-ჲ/გრძნ") 
   ("თანასწორება" "თანასწორებ[ა]-ჲ/თანასწორ") 
   ("თაოსნობა" "თაოსნობ[ა]-ჲ/თაოსნ") 
   ("თარგმანება" "თარგმანებ[ა]-ჲ/თარგმან") 
   ("თარგმნინება" "თარგმნინებ[ა]-ჲ/თარგმნ") 
   ("თარჯვა" "თარჯვ[ა]-ჲ/თარჯ") 
   ("თვლა" "თვლ[ა]-ჲ/თვლ") 
   ("თიბა" "თიბ[ა]-ჲ/თიბ") 
   ("თითლიბაზობა" "თითლიბაზობ[ა]-ჲ/თითლიბაზ") 
   ("თლა" "თლ[ა]-ჲ/თლ") 
   ("თმენინება" "თმენინებ[ა]-ჲ/თმენ") 
   ("თოვა" "თოვ[ა]-ჲ/თოვ") 
   ("თოხნა" "თოხნ[ა]-ჲ/თოჴნ") 
   ("თრთოლება" "თრთოლებ[ა]-ჲ/თრთოლ") 
   ("თრიმვლა" "თრიმვლ[ა]-ჲ/თრიმლ") 
   ("თქმა" "თქმ[ა]-ჲ/თქმ") 
   ("თქმევინება" "თქმევინებ[ა]-ჲ/თქ") 
   ("თხაპვა" "თხაპვ[ა]-ჲ/თხაპ") 
   ("იავარქმნა" "იავარ-ქმნ[ა]-ჲ/ქმნ") 
   ("კuეთება" "კuეთებ[ა]-ჲ/კuეთ") 
   ("კაკან" "კაკან-ი/კაკან") 
   ("კამკამ" "კამკამ-ი/კამკამ") 
   ("კანვა" "კანვ[ა]-ჲ/კან") 
   ("კარკვლა" "კარკვლ[ა]-ჲ/კარკლ") 
   ("კასკას" "კასკას-ი/კასკას") 
   ("კაშკაშ" "კაშკაშ-ი/კაშკაშ") 
   ("კეკელაობა" "კეკელაობ[ა]-ჲ/კეკელა") 
   ("კვრევინება" "კვრევინებ[ა]-ჲ/კვრ") 
   ("კითხვინება" "კითხვინებ[ა]-ჲ/კითხ") 
   ("კიკინ" "კიკინ-ი/კიკინ") 
   ("კიკნა" "კიკნ[ა]-ჲ/კიკნ") 
   ("კიკნა" "კიკნ[ა]-ჲ/კიკნ") 
   ("კილვა" "კილვ[ა]-ჲ/კილ") 
   ("კიჟინ" "კიჟინ-ი/კიჟინ") 
   ("კირცხვლა" "კირცხვლ[ა]-ჲ/კირცხლ") 
   ("კისრებინება" "კისრებინებ[ა]-ჲ/კისრ") 
   ("კლასიფიცირება" "კლასიფიცირებ[ა]-ჲ/კლასიფიცირ") 
   ("კლებინება" "კლებინებ[ა]-ჲ/კლ") 
   ("კოკნა" "კოკნ[ა]-ჲ/კოკნ") 
   ("კონება" "კონებ[ა]-ჲ/კონ") 
   ("კონვა" "კონვ[ა]-ჲ/კონ") 
   ("კოპწიაობა" "კოპწიაობ[ა]-ჲ/კოპწიავ
კოპწიაობ[ა]-ჲ/კოპწია") 
   ("კოტვა" "კოტვ[ა]-ჲ/კოტ") 
   ("კოტიტება" "კოტიტებ[ა]-ჲ/კოტიტ") 
   ("კოცნა" "კოცნ[ა]-ჲ/კოცნ
კოცნ[ა]-ჲ/კოც") 
   ("კოჭლობა" "კოჭლობ[ა]-ჲ/კოჭლ") 
   ("კოხვა" "კოხვ[ა]-ჲ/კოხ") 
   ("კრება" "კრებ[ა]-ჲ/კრიბ
კრებ[ა]-ჲ/კრებ
კრებ[ა]-ჲ/კრბ") 
   ("კრვა" "კრვა-ი/კრ
კრვ[ა]-ჲ/კრ
კრვ[ა]-ჲ/კვრ") 
   ("კრიახ" "კრიახ-ი/კრიახ") 
   ("კრუნჩხვა" "კრუნჩხვ[ა]-ჲ/კრუნჩხ") 
   ("კრუხვა" "კრუხვ[ა]-ჲ/კრუხ") 
   ("კუზვა" "კუზვ[ა]-ჲ/კუზ") 
   ("კუთხვა" "კუთხვ[ა]-ჲ/კუთხ") 
   ("კუილ" "კუილ-ი/კუ") 
   ("კუკნა" "კუკნ[ა]-ჲ/კუკნ") 
   ("კუნკულ" "კუნკულ-ი/კუნკულ") 
   ("კუნჭვა" "კუნჭვ[ა]-ჲ/კუნჭ") 
   ("კუსვა" "კუსვ[ა]-ჲ/კუს") 
   ("კუჭვა" "კუჭვ[ა]-ჲ/კუჭ") 
   ("ლამაზობა" "ლამაზობ[ა]-ჲ/ლამაზ") 
   ("ლამბვა" "ლამბვ[ა]-ჲ/ლამბ") 
   ("ლაპარაკ" "ლაპარაკ-ი/ლაპარაკ") 
   ("ლაპარაკ" "ლაპარაკ-ი/ლაპარაკ") 
   ("ლაჩრობა" "ლაჩრობ[ა]-ჲ/ლაჩრ") 
   ("ლეკვა" "ლეკვ[ა]-ჲ/ლეკ") 
   ("ლეკვა" "ლეკვ[ა]-ჲ/ლეკ") 
   ("ლექვა" "ლექვ[ა]-ჲ/ლექ") 
   ("ლეწვინება" "ლეწვინებ[ა]-ჲ/ლეწ") 
   ("ლიფვა" "ლიფვ[ა]-ჲ/ლიფ") 
   ("ლიცლიც" "ლიცლიც-ი/ლიცლიც") 
   ("ლოდინ" "ლოდინ-ი/ლოდ") 
   ("ლოდინება" "ლოდინებ[ა]-ჲ/ლოდინ") 
   ("ლოკვა" "ლოკვ[ა]-ჲ/ლოკ") 
   ("ლულვა" "ლულვ[ა]-ჲ/ლულ") 
   ("მადლობა" "მადლობ[ა]-ჲ/მადლ") 
   ("მამლაყინწობა" "მამლაყინწობ[ა]-ჲ/მამლაყინწ") 
   ("მარხვა" "მარხვ[ა]-ჲ/მარხ") 
   ("მგლოვიარება" "მგლოვიარებ[ა]-ჲ/მგლოვიარ") 
   ("მდგომარეობა" "მდგომარეობ[ა]-ჲ/მდგომარე") 
   ("მედიდურება" "მედიდურებ[ა]-ჲ/მედიდურ") 
   ("მედიდურობა" "მედიდურობ[ა]-ჲ/მედიდურ") 
   ("მედუქნეობა" "მედუქნეობ[ა]-ჲ/მედუქნე") 
   ("მევახშეობა" "მევახშეობ[ა]-ჲ/მევახშე") 
   ("მეძავობა" "მეძავობ[ა]-ჲ/მეძავ") 
   ("მეჭურჭლეობა" "მეჭურჭლეობ[ა]-ჲ/მეჭურჭლე") 
   ("მიდევნა" "მი-დევნ[ა]-ჲ/დევნ") 
   ("მიდენა" "მი-დენ[ა]-ჲ/დინ
მი-დენ[ა]-ჲ/დი") 
   ("მიდუღება" "მი-დუღებ[ა]-ჲ/დუღ") 
   ("მიზმანება" "მი-ზმანებ[ა]-ჲ/ზმან") 
   ("მილტოლვა" "მი-ლტოლვ[ა]-ჲ/ლტu") 
   ("მიმოქცევა" "მიმო-ქცევ[ა]-ჲ/ქც") 
   ("მიმოხილვა" "მიმო-ხილვ[ა]-ჲ/ხილ") 
   ("მინიშნება" "მი-ნიშნებ[ა]-ჲ/ნიშნ") 
   ("მირთვა" "მი-რთვ[ა]-ჲ/რთ") 
   ("მიფერება" "მი-ფერებ[ა]-ჲ/ფერ") 
   ("მიჩუმება" "მი-ჩუმებ[ა]-ჲ/ჩუმ") 
   ("მიცვლა" "მი-ცuალებ[ა]-ჲ/ცვლ") 
   ("მიხuევა" "მი-ხuევ[ა]-ჲ/ხu") 
   ("მიხედვა" "მი-ხედვ[ა]-ჲ/ხედ") 
   ("მოგება" "მო-გებ[ა]-ჲ/გ") 
   ("მოდგმა" "მო-დგმ[ა]-ჲ/დგ") 
   ("მოვლენა" "მო-ვლენ[ა]-ჲ/ვლინ
მო-ვლენ[ა]-ჲ/ვლენ") 
   ("მოთავეობა" "მოთავეობ[ა]-ჲ/მოთავე") 
   ("მოთხრობა" "მო-თხრობ[ა]-ჲ/თხრ") 
   ("მოკიდება" "მო-კიდებ[ა]-ჲ/კიდ") 
   ("მოკირიანება" "მო-კირიანებ[ა]-ჲ/კირიან") 
   ("მოლაპარაკება" "მო-ლაპარაკებ[ა]-ჲ/ლაპარაკ") 
   ("მოლოდება" "მო-ლოდებ[ა]-ჲ/ლოდ") 
   ("მომართვა" "მო-მართვ[ა]-ჲ/მართ") 
   ("მომდაბლება" "მო-მდაბლებ[ა]-ჲ/მდაბლ") 
   ("მომზადება" "მო-მზადებ[ა]-ჲ/მზად") 
   ("მომხრობა" "მო-მხრობ[ა]-ჲ/მხრ") 
   ("მონადირობა" "მო-ნადირობ[ა]-ჲ/ნადირ") 
   ("მონათვლინება" "მო-ნათვლინებ[ა]-ჲ/ნათლ") 
   ("მონანება" "მო-ნანებ[ა]-ჲ/ნან") 
   ("მონდომა" "მო-ნდომ[ა]-ჲ/ნდ") 
   ("მონება" "მონებ[ა]-ჲ/მონ") 
   ("მონიშვნა" "მო-ნიშვნ[ა]-ჲ/ნიშნ") 
   ("მოსაზრება" "მო-საზრებ[ა]-ჲ/საზრ") 
   ("მოსვლა" "მო-სვლ[ა]-ჲ/სვლ") 
   ("მოსრულება" "მო-სრულებ[ა]-ჲ/სრულ") 
   ("მოტლეკა" "მო-ტლეკ[ა]-ჲ/ტლეკ") 
   ("მოფოლხuება" "მო-ფოლხuებ[ა]-ჲ/ფოლხu") 
   ("მოფოცხვა" "მო-ფოცხვ[ა]-ჲ/ფოცხ") 
   ("მოფრენა" "მო-ფრენ[ა]-ჲ/ფრინ
მო-ფრენ[ა]-ჲ/ფრენ") 
   ("მოქმედება" "მო-ქმედებ[ა]-ჲ/ქმედ") 
   ("მოყuარება" "მო-ყuარებ[ა]-ჲ/ყuარ") 
   ("მოშურნეობა" "მოშურნეობ[ა]-ჲ/მოშურნე") 
   ("მოჩuენება" "მო-ჩuენებ[ა]-ჲ/ჩuენ") 
   ("მოწილვა" "მო-წილვ[ა]-ჲ/წილ") 
   ("მოწმობა" "მოწმობ[ა]-ჲ/მოწმ") 
   ("მოხოხვა" "მო-ხოხვ[ა]-ჲ/ხოხ") 
   ("მოჴსენება" "მო-ჴსენებ[ა]-ჲ/ჴსენ") 
   ("მოჯამაგირეობა" "მოჯამაგირეობ[ა]-ჲ/მოჯამაგირე") 
   ("მოჯახუნება" "მო-ჯახუნებ[ა]-ჲ/ჯახუნ") 
   ("მოჯრა" "მო-ჯრ[ა]-ჲ/ჯრ") 
   ("მუდარება" "მუდარებ[ა]-ჲ/მუდარ") 
   ("მუსიკობა" "მუსიკობ[ა]-ჲ/მუსიკ") 
   ("მყნა" "მყნ[ა]-ჲ/მყნ") 
   ("მშობიარობა" "მშობიარობ[ა]-ჲ/მშობიარ") 
   ("მცვრევა" "მცვრევ[ა]-ჲ/მცვრ") 
   ("მცნება" "მცნებ[ა]-ჲ/მცნ") 
   ("მძლეობა" "მძლეობ[ა]-ჲ/მძლე") 
   ("მხიარულობა" "მხიარულობ[ა]-ჲ/მხიარულ") 
   ("ნათვლა" "ნათვლ[ა]-ჲ/ნათლ") 
   ("ნატვრა" "ნატვრ[ა]-ჲ/ნატრ") 
   ("ნება" "ნებ[ა]-ჲ/ნებ") 
   ("ნეიტრალება" "ნეიტრალებ[ა]-ჲ/ნეიტრალ") 
   ("ნერბვა" "ნერბვ[ა]-ჲ/ნერბ") 
   ("ორგანიზება" "ორგანიზებ[ა]-ჲ/ორგანიზ") 
   ("ორგულება" "ორგულებ[ა]-ჲ/ორგულ") 
   ("პარაზიტობა" "პარაზიტობ[ა]-ჲ/პარაზიტ") 
   ("პარება" "პარებ[ა]-ჲ/პარ") 
   ("პატიოსნება" "პატიოსნებ[ა]-ჲ/პატიოსნ") 
   ("პკურება" "პკურებ[ა]-ჲ/პკურ") 
   ("პოვნინება" "პოვნინებ[ა]-ჲ/პოვნ") 
   ("პრეწა" "პრეწ[ა]-ჲ/პრიწ
პრეწ[ა]-ჲ/პრეწ") 
   ("პყრობა" "პყრობ[ა]-ჲ/პყრ") 
   ("ჟღლა" "ჟღლ[ა]-ჲ/ჟღლ") 
   ("რბენინება" "რბენინებ[ა]-ჲ/რბენ") 
   ("რეწა" "რეწ[ა]-ჲ/რეწ") 
   ("რკინება" "რკინებ[ა]-ჲ/რკინ") 
   ("რწყმა" "რწყმ[ა]-ჲ/რწყ") 
   ("საზრდოება" "საზრდოებ[ა]-ჲ/საზრდო") 
   ("საკუთრება" "საკუთრებ[ა]-ჲ/საკუთრ") 
   ("სარგებლობა" "სარგებლობ[ა]-ჲ/სარგებლ") 
   ("საუბარ" "საუბარ-ი/საუბრ
საუბ[ა]რ-ი/საუბრ") 
   ("საუბარ" "საუბარ-ი/საუბრ
საუბ[ა]რ-ი/საუბრ") 
   ("სიამტკბილობა" "სიამტკბილობ[ა]-ჲ/სიამტკბილ") 
   ("სითბო" "სითბო-ჲ/თბილ") 
   ("სმენა" "სმენ[ა]-ჲ/სმენ") 
   ("სობა" "სობ[ა]-ჲ/სu
სობ[ა]-ჲ/ს") 
   ("სტუმრობა" "სტუმრობ[ა]-ჲ/სტუმრ") 
   ("სუმა" "სუმ[ა]-ჲ/სუმ") 
   ("სყიდვა" "სყიდვ[ა]-ჲ/სყიდ") 
   ("სხდომა" "სხდომ[ა]-ჲ/სხდომ") 
   ("სხივოსნება" "სხივოსნებ[ა]-ჲ/სხივოსნ") 
   ("ტანა" "ტან[ა]-ჲ/ქu") 
   ("ტევება" "ტევებ[ა]-ჲ/ტევ") 
   ("ტმასნა" "ტმასნ[ა]-ჲ/ტმასნ") 
   ("ტრფიალება" "ტრფიალებ[ა]-ჲ/ტრფიალ") 
   ("ტყავება" "ტყავებ[ა]-ჲ/ტყავ") 
   ("ტყება" "ტყებ[ა]-ჲ/ტყებ") 
   ("უბრალოება" "უბრალოებ[ა]-ჲ/უბრალოვ
უბრალოებ[ა]-ჲ/უბრალო") 
   ("უკადრისობა" "უკადრისობ[ა]-ჲ/უკადრის") 
   ("უკმეხობა" "უკმეხობ[ა]-ჲ/უკმეხ") 
   ("უპატიურება" "უპატიურებ[ა]-ჲ/უპატიურ") 
   ("ურჩება" "ურჩებ[ა]-ჲ/ურჩ") 
   ("უცილობლობა" "უცილობლობ[ა]-ჲ/უცილობლ") 
   ("უწყება" "უწყებ[ა]-ჲ/უწყ") 
   ("უხერხულება" "უხერხულებ[ა]-ჲ/უხერხულ") 
   ("ფალვა" "ფალვ[ა]-ჲ/ფალ") 
   ("ფარდება" "ფარდებ[ა]-ჲ/ფარდ") 
   ("ფარჩხვა" "ფარჩხვ[ა]-ჲ/ფარჩხ") 
   ("ფასება" "ფასებ[ა]-ჲ/ფას") 
   ("ფაჩუნ" "ფაჩუნ-ი/ფაჩუნ") 
   ("ფეთქება" "ფეთქებ[ა]-ჲ/ფეთქ") 
   ("ფენა" "ფენ[ა]-ჲ/ფინ
ფენ[ა]-ჲ/ფენ") 
   ("ფერცხვა" "ფერცხვ[ა]-ჲ/ფერცხ") 
   ("ფიფქვა" "ფიფქვ[ა]-ჲ/ფიფქ") 
   ("ფიქსირება" "ფიქსირებ[ა]-ჲ/ფიქსირ") 
   ("ფრთხილება" "ფრთხილებ[ა]-ჲ/ფრთხილ") 
   ("ფუნქციონირება" "ფუნქციონირებ[ა]-ჲ/ფუნქციონირ
ფუნქციონირებ[ა]-ჲ/ჟონგლირ") 
   ("ფუღვრა" "ფუღვრ[ა]-ჲ/ფუღრ") 
   ("ფუშვა" "ფუშვ[ა]-ჲ/ფუშ") 
   ("ფხატვა" "ფხატვ[ა]-ჲ/ფხატ") 
   ("ფხეკა" "ფხეკ[ა]-ჲ/ფხიკ
ფხეკ[ა]-ჲ/ფხეკ") 
   ("ქადაგება" "ქადაგებ[ა]-ჲ/ქადაგ") 
   ("ქანდაკება" "ქანდაკებ[ა]-ჲ/ქანდაკ") 
   ("ქანება" "ქანებ[ა]-ჲ/ქან") 
   ("ქარაფშუტობა" "ქარაფშუტობ[ა]-ჲ/ქარაფშუტ") 
   ("ქილვა" "ქილვ[ა]-ჲ/ქილ") 
   ("ქლესა" "ქლეს[ა]-ჲ/ქლეს") 
   ("ქლესა" "ქლეს[ა]-ჲ/ქლეს") 
   ("ქნა" "ქნ[ა]-ჲ/ქნ") 
   ("ქოთობა" "ქოთობ[ა]-ჲ/ქოთ") 
   ("ქონა" "ქონ[ა]-ჲ/ქონი
ქონ[ა]-ჲ/ქვ%") 
   ("ქონა" "ქონ[ა]-ჲ/ქონი
ქონ[ა]-ჲ/ქვ%") 
   ("ქრევა" "ქრევ[ა]-ჲ/ქრ") 
   ("ქუშვა" "ქუშვ[ა]-ჲ/ქუშ") 
   ("ქუხილ" "ქუხილ-ი/ქუხ") 
   ("ქცევა" "ქცევ[ა]-ჲ/ქც") 
   ("ღალვა" "ღალვ[ა]-ჲ/ღალ") 
   ("ღება" "ღებ[ა]-ჲ/ღ
ღებ[ა]-ჲ/ღებულ") 
   ("ღებინება" "ღებინებ[ა]-ჲ/ღ") 
   ("ღერღეტ" "ღერღეტ-ი/ღერღეტ") 
   ("ღირღიტ" "ღირღიტ-ი/ღირღიტ") 
   ("ღიტინ" "ღიტინ-ი/ღიტინ") 
   ("ღონღიალ" "ღონღიალ-ი/ღონღიალ") 
   ("ღონღილ" "ღონღილ-ი/ღონღილ") 
   ("ღუება" "ღუებ[ა]-ჲ/ღუვ
ღუებ[ა]-ჲ/ღუ") 
   ("ღურღურ" "ღურღურ-ი/ღურღურ") 
   ("ყაზახობა" "ყაზახობ[ა]-ჲ/ყაზახ") 
   ("ყაირათობა" "ყაირათობ[ა]-ჲ/ყაირათ") 
   ("ყალბობა" "ყალბობ[ა]-ჲ/ყალბ") 
   ("ყიდვა" "ყიდვ[ა]-ჲ/ყიდულ
ყიდვ[ა]-ჲ/ყიდ") 
   ("ყნოსვა" "ყნოსვ[ა]-ჲ/ყნოს") 
   ("ყოფა" "ყოფ[ა]-ჲ/ყოფ
ყოფ[ა]-ჲ/ყოფ") 
   ("ყოფნა" "ყოფნ[ა]-ჲ/ყოფნ
ყოფნ[ა]-ჲ/ყოფნ") 
   ("ყოყინ" "ყოყინ-ი/ყოყინ") 
   ("ყოჩობა" "ყოჩობ[ა]-ჲ/ყოჩ") 
   ("ყრა" "ყრ[ა]-ჲ/ყრ") 
   ("ყრდენა" "ყრდენ[ა]-ჲ/ყრდენ") 
   ("ყრევინება" "ყრევინებ[ა]-ჲ/ყრ") 
   ("ყუდრება" "ყუდრებ[ა]-ჲ/ყუდრ") 
   ("ყუნთვა" "ყუნთვ[ა]-ჲ/ყუნთ") 
   ("ყუნცულ" "ყუნცულ-ი/ყუნცულ") 
   ("შuელა" "შuელ[ა]-ჲ/შuელ") 
   ("შეგნება" "შე-გნებ[ა]-ჲ/გნ") 
   ("შევრდომა" "შე-ვარდნ[ა]-ჲ/ვარდ") 
   ("შეიარაღება" "შე-იარაღებ[ა]-ჲ/იარაღ") 
   ("შეკვრა" "შე-კვრა-ი/კრ
შე-კვრ[ა]-ჲ/კრ
შე-კვრ[ა]-ჲ/კვრ") 
   ("შეკუმშვა" "შე-კუმშვ[ა]-ჲ/კუმშ") 
   ("შელოცვა" "შე-ლოცვ[ა]-ჲ/ლოც") 
   ("შემართება" "შე-მართებ[ა]-ჲ/მართ") 
   ("შემაღლება" "შე-მაღლებ[ა]-ჲ/მაღლ") 
   ("შემდურება" "შე-მდურებ[ა]-ჲ/მდურ") 
   ("შემთხuევა" "შე-მთხuევ[ა]-ჲ/მთხu") 
   ("შემოდგომა" "შემო-დგომ[ა]-ჲ/დგ") 
   ("შემორონინება" "შემო-რონინებ[ა]-ჲ/რონინ") 
   ("შემოწმება" "შე-მოწმებ[ა]-ჲ/მოწმ") 
   ("შემოხედვა" "შემო-ხედვ[ა]-ჲ/ხედ") 
   ("შემჩნევა" "შე-მჩნევ[ა]-ჲ/მჩნ") 
   ("შენდობა" "შე-ნდობ[ა]-ჲ/ნდ") 
   ("შეოჭვა" "შე-ოჭვ[ა]-ჲ/ოჭ") 
   ("შესხლტომა" "შე-სხლტომ[ა]-ჲ/სხლტ") 
   ("შეტაკება" "შე-ტაკებ[ა]-ჲ/ტაკ") 
   ("შეტყობინება" "შე-ტყობინებ[ა]-ჲ/ტყ") 
   ("შეფერდება" "შე-ფერდებ[ა]-ჲ/ფერდ") 
   ("შეფერფვლა" "შე-ფერფვლ[ა]-ჲ/ფერფლ") 
   ("შეფიცვრა" "შე-ფიცვრ[ა]-ჲ/ფიცრ") 
   ("შექცევა" "შე-ქცევ[ა]-ჲ/ქც") 
   ("შეღწევა" "შე-ღწევ[ა]-ჲ/ღწ") 
   ("შეყიდვა" "შე-ყიდვ[ა]-ჲ/ყიდ") 
   ("შეშთობა" "შე-შთობ[ა]-ჲ/შთ") 
   ("შეშფოთება" "შე-შფოთებ[ა]-ჲ/შფოთ") 
   ("შეცდომა" "შე-ცდომ[ა]-ჲ/ცდ") 
   ("შეჭუხვა" "შე-ჭუხვ[ა]-ჲ/ჭუხ") 
   ("შეჭყაპება" "შე-ჭყაპებ[ა]-ჲ/ჭყაპ") 
   ("შეხიზნება" "შე-ხიზნებ[ა]-ჲ/ხიზნ") 
   ("შეხრაკვა" "შე-ხრაკვ[ა]-ჲ/ხრაკ") 
   ("შთობა" "შთობ[ა]-ჲ/შთ") 
   ("შმორება" "შმორებ[ა]-ჲ/შმორ") 
   ("შობა" "შობ[ა]-ჲ/შობ") 
   ("შორვა" "შორვ[ა]-ჲ/შორ") 
   ("შუამდგომლობა" "შუამდგომლობ[ა]-ჲ/შუამდგომლ") 
   ("ჩადენა" "ჩა-დენ[ა]-ჲ/დინ
ჩა-დენ[ა]-ჲ/დენ
ჩადენ[ა]-ჲ/დენ") 
   ("ჩავარდნა" "ჩა-ვარდნ[ა]-ჲ/ვარდ") 
   ("ჩაკუზვა" "ჩა-კუზვ[ა]-ჲ/კუზ") 
   ("ჩალულვა" "ჩა-ლულვ[ა]-ჲ/ლულ") 
   ("ჩამატება" "ჩა-მატებ[ა]-ჲ/მატ") 
   ("ჩამობრუნება" "ჩამო-ბრუნებ[ა]-ჲ/ბრუნ") 
   ("ჩამორჩომა" "ჩამო-რჩენ[ა]-ჲ/რჩომ") 
   ("ჩამოფასება" "ჩამო-ფასებ[ა]-ჲ/ფას") 
   ("ჩამოყრევინება" "ჩამო-ყრევინებ[ა]-ჲ/ყრ") 
   ("ჩამტერება" "ჩა-მტერებ[ა]-ჲ/მტერ") 
   ("ჩამუხვლა" "ჩა-მუხვლ[ა]-ჲ/მუჴლ") 
   ("ჩამუხლისთავება" "ჩა-მუხლისთავებ[ა]-ჲ/მუჴლისთავ") 
   ("ჩამწნილება" "ჩა-მწნილებ[ა]-ჲ/მწნილ") 
   ("ჩასობა" "ჩა-სობ[ა]-ჲ/ს") 
   ("ჩასოლვა" "ჩა-სოლვ[ა]-ჲ/სოლ") 
   ("ჩატრიალება" "ჩა-ტრიალებ[ა]-ჲ/ტრიალ") 
   ("ჩაფისვა" "ჩა-ფისვ[ა]-ჲ/ფის") 
   ("ჩაფუკვა" "ჩა-ფუკვ[ა]-ჲ/ფუკ") 
   ("ჩაღრმავება" "ჩა-ღრმავებ[ა]-ჲ/ღრმავ") 
   ("ჩაყრევინება" "ჩა-ყრევინებ[ა]-ჲ/ყრ") 
   ("ჩაჩეხვა" "ჩა-ჩეხვ[ა]-ჲ/ჩეხ") 
   ("ჩაჩოქება" "ჩა-ჩოქებ[ა]-ჲ/ჩოქ") 
   ("ჩაციება" "ჩა-ციებ[ა]-ჲ/ცივ") 
   ("ჩაცხრომა" "ჩა-ცხრომ[ა]-ჲ/ცხრ") 
   ("ჩაწერინება" "ჩა-წერინებ[ა]-ჲ/წერ") 
   ("ჩაჭიდება" "ჩა-ჭიდებ[ა]-ჲ/ჭიდ") 
   ("ჩახრინწიანება" "ჩა-ხრინწიანებ[ა]-ჲ/ხრინწიან") 
   ("ჩერჩეტ" "ჩერჩეტ-ი/ჩერჩეტ") 
   ("ჩეხვა" "ჩეხვ[ა]-ჲ/ჩეხ") 
   ("ჩვევა" "ჩვევა/ჩვევ") 
   ("ჩინება" "ჩინებ[ა]-ჲ/ჩინ") 
   ("ჩოთირ" "ჩოთირ-ი/ჩოთირ") 
   ("ჩქაფუნ" "ჩქაფუნ-ი/ჩქაფუნ") 
   ("ჩხაბვა" "ჩხაბვ[ა]-ჲ/ჩხაბ") 
   ("ჩხაპნა" "ჩხაპნ[ა]-ჲ/ჩხაპნ") 
   ("ჩხორვა" "ჩხორვ[ა]-ჲ/ჩხორ") 
   ("ცბიერება" "ცბიერებ[ა]-ჲ/ცბიერ") 
   ("ცვლა" "ცuალებ[ა]-ჲ/ცვლ
ცვლ[ა]-ჲ/ცვლ") 
   ("ცივება" "ცივებ[ა]-ჲ/ცივ") 
   ("ცლა" "ცლ[ა]-ჲ/ცალ
ცლ[ა]-ჲ/ცლ") 
   ("ცოდვა" "ცოდვ[ა]-ჲ/ცოდ") 
   ("ცოდნა" "ცოდნ[ა]-ჲ/ცოდნ") 
   ("ცუდვა" "ცუდვ[ა]-ჲ/ცუდ") 
   ("ცუცქნა" "ცუცქნ[ა]-ჲ/ცუცქნ") 
   ("ცხელება" "ცხელებ[ა]-ჲ/ცხელ") 
   ("ცხოვრება" "ცხოვრებ[ა]-ჲ/ცხოვრ") 
   ("ძაღლობა" "ძაღლობ[ა]-ჲ/ძაღლ") 
   ("ძევა" "ძევ[ა]-ჲ/ძევ") 
   ("ძლიერება" "ძლიერებ[ა]-ჲ/ძლიერ") 
   ("ძმობა" "ძმობ[ა]-ჲ/ძმ") 
   ("წuეთა" "წuეთ[ა]-ჲ/წuეთ") 
   ("წuელა" "წuელ[ა]-ჲ/წuელ") 
   ("წაგდებინება" "წა-გდებინებ[ა]-ჲ/გდ") 
   ("წადება" "წადებ[ა]-ჲ/წად") 
   ("წავლა" "წა-ვლ[ა]-ჲ/ვლ") 
   ("წალულვა" "წა-ლულვ[ა]-ჲ/ლულ") 
   ("წამოგდებინება" "წამო-გდებინებ[ა]-ჲ/გდ") 
   ("წამოლანდება" "წამო-ლანდებ[ა]-ჲ/ლანდ") 
   ("წამოროტვა" "წამო-როტვ[ა]-ჲ/როტ") 
   ("წამოსარჩლება" "წამო-სარჩლებ[ა]-ჲ/სარჩლ") 
   ("წამოქროლვა" "წამო-ქროლვ[ა]-ჲ/ქროლ") 
   ("წამოჩრა" "წამო-ჩრ[ა]-ჲ/ჩრ") 
   ("წამოწყება" "წამო-წყებ[ა]-ჲ/წყ") 
   ("წარდგომა" "წარ-დგომ[ა]-ჲ/დგ") 
   ("წარმატება" "წარ-მატებ[ა]-ჲ/მატ") 
   ("წარმოდგომა" "წარმო-დგომ[ა]-ჲ/დგ") 
   ("წარმოდენა" "წარმო-დენ[ა]-ჲ/დი") 
   ("წარმოება" "წარმოებ[ა]-ჲ/წარმო") 
   ("წარწყმენდა" "წარ-წყმენდ[ა]-ჲ/წყმდ
წარ-წყმენდ[ა]-ჲ/წყმენდ") 
   ("წასარჩლება" "წა-სარჩლებ[ა]-ჲ/სარჩლ") 
   ("წასხმა" "წა-სხმ[ა]-ჲ/სხ") 
   ("წაფება" "წაფებ[ა]-ჲ/წაფ") 
   ("წაფრენა" "წა-ფრენ[ა]-ჲ/ფრენ") 
   ("წაშენა" "წა-შენ[ა]-ჲ/შენ") 
   ("წაწყმენდა" "წა-წყმენდ[ა]-ჲ/წყმდ
წა-წყმენდ[ა]-ჲ/წყმინდ
წა-წყმენდ[ა]-ჲ/წყმენდ") 
   ("წახედვა" "წა-ხედვ[ა]-ჲ/ხედ") 
   ("წახუმრება" "წა-ხუმრებ[ა]-ჲ/ხუმრ") 
   ("წბოლა" "წბოლ[ა]-ჲ/წბ") 
   ("წება" "წებ[ა]-ჲ/წ") 
   ("წევნა" "წევნ[ა]-ჲ/წ") 
   ("წერა" "წერ[ა]-ჲ/წერ") 
   ("წინააღმდეგობა" "წინააღმდეგობ[ა]-ჲ/წინააღმდეგ") 
   ("წინაგრძნობა" "წინა-გრძნობ[ა]-ჲ/გრძნ") 
   ("წინამძღოლობა" "წინამძღოლობ[ა]-ჲ/წინამძღოლ") 
   ("წინასწარმეტყuელება" "წინასწარმეტყuელებ[ა]-ჲ/წინასწარმეტყuელ") 
   ("წინაძღოლა" "წინა-ძღოლ[ა]-ჲ/ძღu") 
   ("წმასნა" "წმასნ[ა]-ჲ/წმასნ") 
   ("წმაწნა" "წმაწნ[ა]-ჲ/წმაწნ") 
   ("წნეხვა" "წნეხვ[ა]-ჲ/წნიხ
წნეხვ[ა]-ჲ/წნეხ") 
   ("წონა" "წონ[ა]-ჲ/წონ") 
   ("წრუპნა" "წრუპნ[ა]-ჲ/წრუპნ") 
   ("წურთვნა" "წურთვნ[ა]-ჲ/წურთნ") 
   ("წუწაობა" "წუწაობ[ა]-ჲ/წუწა") 
   ("წუხება" "წუხებ[ა]-ჲ/წუხ") 
   ("წუხვა" "წუხვ[ა]-ჲ/წუხ") 
   ("წყება" "წყებ[ა]-ჲ/წყ") 
   ("წყევლა" "წყევლ[ა]-ჲ/წყევლ") 
   ("წყენა" "წყენ[ა]-ჲ/წყინ
წყენ[ა]-ჲ/წყენ") 
   ("წყენინება" "წყენინებ[ა]-ჲ/წყენ") 
   ("წყვა" "წყვ[ა]-ჲ/წყ") 
   ("წყურება" "წყურებ[ა]-ჲ/წყურ") 
   ("ჭuრეტა" "ჭuრეტ[ა]-ჲ/ჭuრეტ") 
   ("ჭამა" "ჭამ[ა]-ჲ/ჭმ
ჭამ[ა]-ჲ/ჭამ
ჭამ[ა]-ჲ/ჭამ
ჭამ[ა]-ჲ/ჭამ") 
   ("ჭამა" "ჭამ[ა]-ჲ/ჭმ
ჭამ[ა]-ჲ/ჭამ
ჭამ[ა]-ჲ/ჭამ
ჭამ[ა]-ჲ/ჭამ") 
   ("ჭვარტვლა" "ჭვარტვლ[ა]-ჲ/ჭuარტლ") 
   ("ჭიდება" "ჭიდებ[ა]-ჲ/ჭიდ") 
   ("ჭიმვა" "ჭიმვ[ა]-ჲ/ჭიმ") 
   ("ჭნობა" "ჭნობ[ა]-ჲ/ჭნ") 
   ("ჭორიკანაობა" "ჭორიკანაობ[ა]-ჲ/ჭორიკანა") 
   ("ჭოჭმანება" "ჭოჭმანებ[ა]-ჲ/ჭოჭმან") 
   ("ჭრა" "ჭრ[ა]-ჲ/ჭრ") 
   ("ჭურვა" "ჭურვ[ა]-ჲ/ჭურ") 
   ("ჭუჭყნა" "ჭუჭყნ[ა]-ჲ/ჭუჭყნ") 
   ("ხuრეტა" "ხuრეტ[ა]-ჲ/ხuრეტ") 
   ("ხადილობა" "ხადილობ[ა]-ჲ/ხადილ") 
   ("ხალვა" "ხალვ[ა]-ჲ/ხალ") 
   ("ხამხამ" "ხამხამ-ი/ხამხამ") 
   ("ხარჯვა" "ხარჯვ[ა]-ჲ/ხარჯ") 
   ("ხასობა" "ხასობ[ა]-ჲ/ხას") 
   ("ხდევინება" "ხდევინებ[ა]-ჲ/ხდ") 
   ("ხდილობა" "ხდილობ[ა]-ჲ/ჴდილ") 
   ("ხევა" "ხევ[ა]-ჲ/ხ") 
   ("ხევა" "ხევ[ა]-ჲ/ხ") 
   ("ხელობა" "ხელობ[ა]-ჲ/ხელ") 
   ("ხერხიანობა" "ხერხიანობ[ა]-ჲ/ხერხიან") 
   ("ხლა" "ხლ[ა]-ჲ/ხლ") 
   ("ხრუსტვა" "ხრუსტვ[ა]-ჲ/ხრუსტ") 
   ("ხტუნაობა" "ხტუნაობ[ა]-ჲ/ხტუნავ
ხტუნაობ[ა]-ჲ/ხტუნა") 
   ("ხუმრობა" "ხუმრობ[ა]-ჲ/ხუმრ") 
   ("ჴელმწიფება" "ჴელმწიფებ[ა]-ჲ/ჴელმწიფ") 
   ("ჴშვა" "ჴშვ[ა]-ჲ/ჴშ") 
   ("ჯაგვა" "ჯაგვ[ა]-ჲ/ჯაგ") 
   ("ჯაგვა" "ჯაგვ[ა]-ჲ/ჯაგ") 
   ("ჯარვა" "ჯარვ[ა]-ჲ/ჯარ") 
   ("ჯახება" "ჯახებ[ა]-ჲ/ჯახ") 
   ("ჯობნა" "ჯობნ[ა]-ჲ/ჯობნ
ჯობნ[ა]-ჲ/ჯობ") 
   ("ჯრა" "ჯრ[ა]-ჲ/ჯრ")))

#+test
(with-transaction ()
  (dolist (n '())
    (insert-records
     :into [morph noun-features]
     :av-pairs
     `(([stem] ,n)
       ([code] "I")
       ([pos] "n")
       ([sub-id] 1)
       ([style-features] "+Rust")
       ([date] ,(get-universal-time))
       ([comment] "ვეფხისტყაოსანი")
       ([author] "PM")))))

#+test
(print (select [author] :distinct t :from [morph noun-features]))

#+test
(u:with-file-lines (line "projects:georgian-morph;lists;symphonie.txt")
  (destructuring-bind (lemma . forms) (u:split line #\|)
    (print (cons lemma forms))))

#+test
(with-transaction ()
  (dolist (name
	    #+done
	   '("აბან" "აბირ" "აბუბაქარ" "აბულ-ყასუმ" "აბულასან" "აბუტალიბ" "აბუტარ" "ადრაშურ" "აზარმანიკ"
	     "ალი-მოჰამად" "ამირ" "ასან" "ასფან" "აშტრაბ" "აჯერ" "ბალხამ" "ბაქაჯარ" "ბაყბაყ" "ბოლოსორ"
	     "დილამ" "დორაზ" "დორათ" "იბად" "იმარინდო" "ლოსარ" "ლოსიმან" "მახოსტან" "მახოსტო" "მოსორ"
	     "მოხოსტან" "მოჰამად" "ნადაბ" "ნადიბ" "ნოსარ" "ოსტარას" "პარსან" "რაბაგ" "რაბამ" "რაბიშინ"
	     "რაზიმან" "რაიბ" "როსაბ" "სავარ" "სავარსიმ" "სეფედავლე" "სიმან" "სორაზან" "უსენ" "უსიბ"
	     "უსტარაზან" "ფირუზენ" "ქაბარ" "ქამარ" "ქანიმან" "ქაოზ" "ღამაზ" "ღამარ" "ყამარ" "ხაზარან"
	     "ხვარაშან" "ჯაზდან" "ჯაზირ" "ჯამასარ" "ჯაუნარ" "ჯოჰარ")
	   #+done
	   '("დავარ" "დილარგეთ" "დიონოს" "დულარდუხტ" "ეზროს" "ზუალ" "კრონოს" "მუშთარ" "ნურადინ" "როსან"
	     "როშაქ" "სოგრატ" "ტარია"
	     "ფატმან")
	   #+done
	   '("ტარიასთა" "ტარიერისა" "ტარიერსა" "როდია")
	   #+done
	   '( "აზორ" "აქაზ" "აქიმ" "ესრომ" "იოათამ" "იობედ"  "სადუკ" "ამინადაბ" "ბოოს" "დედაკაცმან" "ელიაკიმ" "ელიუდ" "ზორობაბელ" "იექონია" "იოაკიმ" "იოსაფატ" "რობოამ" "სალათიელ" "სალმონ" "აბიოდ" "აბიუდ" "გალაად" "ელეაზარ" "ელიაზარ" "ისააკ" "ნაასომ" "რაქელ" "ავრამ" "არად" "აქილევ" "ბოოზ" "ეან" "ენოსოჲ" "ვარპანთერ" "თარროს" "იოსეფ" "ისმაილოს" "კაენ" "მათუსალა" "ნაასონ" "ნასონ" "თუმან" "დედან" "საბათ" "სალმუნ" "სეით" "სერუხოს" "ფარჱზ" "ასაფ")
	   #+done
	   '("იუდა" "დია" "ზარა" "ასა" "იოსია" "ოზია" "ეზეკია" "აბია" "ურია")
	   #+done
	   '("მატთან")
	   #+done
	   '("ანტონიოს" "დიმიტრიოს" "ლუკიოს" "პომპიიოს" "პორფირიოს" "ევსებიოს" "კორნელიოს" "ღავინიოს" "ანასტასიოს" "ევსტრატიოს" "კასსიოს" "ღაიოს" "დიონოსიოს" "დიონჳსიოს" "აკაკიოს" "ეფრემიოს" "ორენტიოს" "პანსოფიოს" "პოპლიოს" "ლეონტიოს" "პალადიოს" "ამონიოს" "ანთემიოს" "სერგიოს" "ადელფიოს" "დეონოსიოს" "სოსსიოს" "ჯერანტიოს" "ბალაკიოს" "ლიკინიოს" "სუქიოს" "არსაკიოს" "დემეტრიოს" "ევნომიოს" "ევსუქიოს" "იულიოს" "ლევკიოს" "ოკტავიოს" "აითარიოს" "აპოლლონიოს" "ასპიოს" "ევაგრიოს" "ვენტიდიოს" "კლავდიოს" "ნეკტარიოს" "პოლოხრონიოს" "სამბატიოს" "სერუილიოს" "ავღუსტალიოს" "გრიგოლიოს" "დიონისიოს" "ევსევიოს" "კესარიოს" "მაქსენტიოს" "ნარკიოს" "პაესიოს" "ურბილიოს" "ფლავიოს" "ავლოსიოს" "ავტროპიოს" "ამმონიოს" "აპოლინარიოს" "აფროდისიოს" "ბასილიოს" "გორდიოს" "ევგენიოს" "ევთჳმიოს" "თეოდოსიოს" "ირინიოს" "ნისტორიოს" "პავლაკიოს" "პომპიოს" "სჳნოდიოს" "ტიბერიოს" "უკესტინაჯიოს" "ფარკობიოს" "ფარნაკიოს" "ავლავიოს" "ავლუსიოს" "ავრილიოს" "ათანასიოს" "ალფიოს" "აპპოლლონიოს" "ევდოქსიოს" "ევპრეპიოს" "ევსტოქიოს" "ევტჳქიოს" "ევფემიოს" "ელჳსტრიოს" "ეპიფანიოს" "ვალერიოს" "ვასილიოს" "თევდოსიოს" "თევდოტიოს" "კლუსიოს" "კორნილიოს" "ლივმენიოს" "ნუმინიოს" "ონორიოს" "პელაგიოს" "პოლჳვიოს" "სალოსტიოს" "სემელიოს" "სოფრონიოს" "სხელიქიოს" "ტერენტიოს" "ტერტიოს" "ტონგიოს" "ტროფონიოს" "ფანიოს" "ფრურიოს" "აბრამიოს" "ადლიოს" "ავიოს" "ავლიოს" "აითაოიოს" "აიოს" "აკუმიოს" "ალადიოს" "ალექსანდრიოს" "ამანტიოს" "ანასტოლიოს" "ანატოლიოს" "ანტალიოს" "აპულიოს" "ასტერიოს" "ასტიოს" "ასტრომეთიოს" "ასჳრიოს" "ატილიოს" "აქთიბიოს" "ახონიოს" "ბადდიოს" "ბარურიასიოს" "გენადიოს" "გიორგიოს" "გუსტალიოს" "დავინიოს" "დალმატიოს" "დევნოსიოს" "დეონისიოს" "დომენტიოს" "დონობიოს" "დორეთიოს" "ევდიოს" "ევებიოს" "ევსტრაჯიოს" "ევტოქსიოს" "ევტუქიოს" "ელადიოს" "ელაფიოს" "ელესიოს" "ენთამნიოს" "ეპტაპლასიოს" "ეპტჳქიოს" "ერეპიოს" "ეცსტოქიოს" "ვიკტორიოს" "თალასიოს" "თალპიოს" "თელანიოს" "იგნატიოს" "იედდიოს" "იედიოს" "იკარიოს" "იობადიოს" "იოს" "იპერექიოს" "ისაკიოს" "ისჳქიოს" "კაისიოს" "კანინიოს" "კარპუნიოს" "კასიოს" "კიკლიოს" "კლაჳდიოს" "კლემენტიოს" "კონსტანტიოს" "კოპონიოს" "კოჳლიოს" "კურნელიოს" "ლამპეტიოს" "ლევანტიოს" "ლექტარიოს" "ლიბერიოს" "ლიკინნიოს" "ლჳკიოს" "ლუკკელლიოს" "მელეთიოს" "მელიტიოს" "მელქიოს" "მილისიოს" "მოსოლამიოს" "ნემესიოს" "ნერანგიოს" "ოთონიოს" "ორკჳლიოს" "ოსიოს" "ოფელლიოს" "ოქსინიოს" "პაკკიოს" "პამფილიოს" "პანეტიოს" "პანსიფიოს" "პაპინიოს" "პაპირიოს" "პაფნოტიოს" "პიპლიოს" "პლავტიოს" "პოლიოს" "პომპინიოს" "პომჟილიოს" "პომფილიოს" "პოფირიოს" "პრობინკალიოს" "პროტერიოს" "პუპლიოს" "რევილიოს" "როდიოს" "რუკიოს" "საბელიოს" "სალომიოს" "სასელიოს" "სევასტიოს" "სელევკიოს" "სენტიოს" "სერემიოს" "სერრიოს" "სერუინიოს" "სერჯიოს" "სიბილიოს" "სირჯიოს" "სისსიოს" "სონესიოს" "სოსივიოს" "სუნესიოს" "ტალმოთიოს" "ტარასიოს" "ტევტიოს" "ტიდიტიოს" "ტონეკნუკტოსაღნოსიოს" "ტოპრინიკონიტიხრიოს" "უალერიოს" "უკუმიოს" "უპავლიოს" "ფავიოს" "ფანნიოს" "ფართენიოს" "ფარმაკიოს" "ფლორანტიოს" "ფოლორანტიოს" "ფრომენტიოს" "ფრუმენტიოს" "ღალერიოს" "ღალლიოს" "ხურისაორიოს" "ჰრომანიოს")
	   #+done
	   '("აბდალმესია" "აზარია" "დასაბია" "ევდუკია" "ევლისანია" "ევსებია" "ელიადოჲსია" "ენდემია" "თემესტია" "კავტია" "ლუსია" "მატათია" "მატთანია" "ოქოზია" "პალადია" "სედეკია" "სელემია" "სეფელია" "ტერტია" "ფებრონია" "ჰეროდია")
	   #+done
	   '("აბდილა" "აბეშურა" "აბიბა" "ადა" "ადდა" "ათრაქა" "აინინა" "აკჳლა" "აკჳლინა" "ამონა" "აჲნინა" "არსამა" "არტაშა" "ასიმა" "ასონა" "ბაბილა" "ბარაბა" "ბარაქადრა" "ბარლაჰა" "ბარსაბა" "ბეტულუა" "დანანა" "დანინა" "დებორა" "ეგეა" "ევჰა" "ელიომა" "ენეა" "ესაჲა" "ზელფა" "ზუჰრა" "თეოდორა" "თუმა" "ივსტინა" "ითრუშანა" "კაიაფა" "კიურა" "კლეოპა" "კლეოპატრა" "კრისპა" "მასლამა" "მიქეა" "ნიკიტა" "პრინკა" "პროსილა" "რებეკა" "სალლა" "სარრა" "სეფორა" "ფადლა" "ფაკეა" "ქელკა" "ღადანა" "შალიტა" "შარვანშა" "შაჰანშა" "შეტერუა" "ჩიმაგა" "ხუანსუა" "ხუარა" "ჯიბღა"
)
	   #+done
	   '("აბდალმესე" "ადარნასე" "ადარნერსე" "ადარნესე" "ადონაე" "ადრიანე" "ადრნესე" "ანატოლე" "ანთიმე" "ანტონინე" "არისტაკე" "აროსტაკე" "არსესოქე" "არტაქსაქსე" "აფროდიტე" "ბაბილე" "ბართლომე" "ბართოლომე" "ბერსაბე" "გაჲანე" "დადიანე" "დეოკლიტიანე" "დიოკლიტიანე" "დიომიდე" "დიოსკორე" "ევაგრე" "ევანგერე" "ევთჳმე" "ევთჳმიე" "ევლალე" "ევსებიე" "ელისე" "ეპიფანე" "ერაკლე" "ერმოგინე" "ეტვიფანე" "ეტჳფანე" "ეფროსინე" "ვაჩე" "ვესპასიანე" "ზოტიკე" "თაგინე" "თათე" "თალელე" "თედორე" "თევდოტე" "თეოდოტე" "თეოფილაკტე" "თეოფილე" "იამბრე" "იანე" "იეფთაე" "ივბინიანე" "იოანნე" "იოფანე" "იოჰანნე" "იპოლიტე" "ირაკლე" "ისე" "ისტჳნანე" "ისტჳნიანე" "კალენიკე" "კელე" "კერინთე" "კლეონიკე" "კოსტინე" "კრისკე" "კჳირიკე" "კჳპრიანე" "კჳრიკე" "კჳრილე" "ლჳკიანე" "ლუკიანე" "მარკიანე" "მატრიანე" "მაქსიმიანე" "მიტროფანე" "მჲოსე" "ნერსე" "ონესიფორე" "ორთანე" "ოროგინე" "ოსე" "ოსკე" "პატროკლე" "პოლოკტე" "პროვატიკე" "პროხორე" "როგდანე" "სავლე" "სენედუხტე" "სკოჰე" "სჳმონ-პეტრე" "ტრაიანე" "ტრაჲანე" "ტროფიმე" "ურბანე" "ურდურე" "ფოტინე" "ქასრე" "ქრისტედოლე" "ქსანთიპე" "ქსერქსე" "ქსიფე" "ხუარანძე" "ჯერასიმე" "ჰელენე" "ჰერაკლე" "ჰოლონფერნე" "ჰრეპრებე")
	   )
    (insert-records :into [morph noun-features]
		    :av-pairs `(([stem] ,name)
				([code] "O");;"Z") ;"O")
				([pos] "N")
				([sub-id] 1)
				([features] "+Prop+Name")
				;; ([style-features] nil)
				([comment] "GNC-OGE")
				([date] ,(get-universal-time))
				([lang] "og")
				([author] "PM")))))



#+test
(update-records [morph noun-features]
		:av-pairs '(([lang] "og"))
		:where [= [comment] "AMIRDAR"])

#+test
(with-open-file (stream "projects:georgian-morph;lists;acia.txt1"
			:direction :output :if-exists :supersede)
  (u:with-file-lines (line "projects:georgian-morph;lists;acia.txt")
    (unless (search "იზაცია" line)
      (write-line line stream))))

:eof
