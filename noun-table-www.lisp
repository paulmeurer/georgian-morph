;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10 -*-

;; paul.meurer@uib.no
;; https://clarino.uib.no/

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

(define-url-function js/noun-features
    (request (search-stem action stem-key stem code pos features style-features lang template comment)
	     :xsl #'js/noun-features-xsl
	     :write-doctype-p nil ;; no HTML doctype
	     :path "/kartuli/js/noun-features.xml")
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

(defparameter *wrong-vn-root-table* (dat:make-string-tree))

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

;;; inclusion of Rayfield

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

(defun analize-headword (hw)
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

(defun extract-headwords (line &optional headword-end)
  (let ((headword-end (or headword-end (headword-end line))))
    (when headword-end
      (let ((headwords (mapcar (lambda (str) (string-trim " " str))
			       (string-parse
				(subseq line 0 headword-end)
				:brace-pairs '(("(" . ")"))
				:whitespace #(#\,)))))
	(mapcan #'analize-headword headwords)))))

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

:eof
