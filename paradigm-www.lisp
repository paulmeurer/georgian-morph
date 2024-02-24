;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10; Readtable: augmented-readtable -*-

#+test
(net.aserve::debug-on :notrap)
#+test
(net.aserve::debug-on :xmit)
#+test
(net.aserve::debug-off :notrap)
#+test
(net.aserve::debug-off :xmit)

;; URLs:
;; http://gekko.dyndns.org/kartuli/nouns
;; http://gekko.dyndns.org/kartuli/roots

;; paradigm checking: 133 begvavs

(in-package :fst)


(eval-when (:load-toplevel :compile-toplevel :execute)
  (unintern 'start :fst)
  (use-package :aserve)
  (use-package :net.aserve)
  (unintern '@ :fst)
  (use-package :js)
  (use-package :lxml))

(setf lxml::*newline-after-endtag-p* nil)

(defparameter *url-base* "kartuli")

(defparameter +georgian-unicode+
  (map 'string #'code-char #(4304 4305 4306 4307 4308 4309 4310 4311 4312 4313 4314 4315 4316 4317
			     4318 4319 4320 4321 4322 4323 4324 4325 4326 4327 4328 4329 4330 4331
			     4332 4333 4334 4335 4336)))

(defparameter +georgian-amirani+
  "abgdevzTiklmnopZrstuPKGqSXCjcxHJh")

(defparameter +georgian-geo+
  (map 'string #'code-char #(192 193 194 195 196 197 198 200 201 202 203 204 205 207 208
			     209 210 211 212 214 215 216 217 218 219 220 221 222 223 224 225
			     227 228)))

(defparameter +georgian-acad+
  "abgdevzTiklmnopJrstufqRySCcZwWxjh")

;;(print (convert-encoding "abgdevzTiklmnopZrstuPKGqSXCjcxHJh" :amirani :acad))
;;(print (convert-encoding "abgdevzTiklmnopZrstuPKGqSXCjcxHJh" :amirani :unicode))

(defun convert-encoding (string from-encoding to-encoding)
  (if (or (eq from-encoding to-encoding)
	  (null from-encoding)
	  (null to-encoding))
      string
      (let* ((from-vector
	      (ecase from-encoding
		(:unicode +georgian-unicode+)
		((:amirani :translit) +georgian-amirani+)
		(:acad +georgian-acad+)
		(:geo +georgian-geo+)))
	     (to-vector
	      (ecase to-encoding
		(:unicode +georgian-unicode+)
		((:amirani :translit) +georgian-amirani+)
		(:acad +georgian-acad+)
		(:geo +georgian-geo+)))
	     (result (copy-seq string)))
	(loop for c across result for i from 0
	      do (let ((pos (position c from-vector)))
		   (when pos (setf (char result i) (char to-vector pos)))))
	result)))

(defun comment-p (char)
  (not (eq char #\")))

(defun strip-comments (string)
  (with-output-to-string (stream)
    (labels ((strip (pos)
	       (let ((quote-pos (position #\" string :start pos)))
		 (cond ((null quote-pos)
			(write-string string stream :start pos))
		       ((and (> quote-pos 0)
			     (char= (char string (1- quote-pos)) #\`))
			(write-string string stream :start pos :end (1+ quote-pos))
			(strip (1+ quote-pos)))
		       (t
			(write-string string stream :start pos :end quote-pos)
			;; missing: quoted " in quote
			(strip (1+ (position #\" string :start (1+ quote-pos)))))))))
      (strip 0))))

(defun verb-template-p (name)
  (and (string= name "V-" :end1 2)
       (not (search "-SYN" name))
       (not (search "-COMMON" name))))		   

(defun font-encoding (font)
  (cond ((equal font "amirani")
	 :amirani)
	((equal font "translit")
	 :translit)
	((equal font "titus")
	 :unicode)
	((equal font "bpg")
	 :unicode)
	((equal font "geo")
	 :geo)
	((equal font "acad")
	 :acad)))

(defun ensure-list (obj)
  (if (listp obj) obj (list obj)))

(defparameter *xle-template-list* ())

#+test
(print (fst::kartuli-verb-morph "mesmis"))

#+test
(fst::generate-paradigm "3843-1"
			:allp nil
			:obj3sg-only-p t
			:standard-only-p t
			:printp nil)

;;(print (fst::id-paradigm-ids 2032))

(defparameter *paradigm-cache* (make-hash-table :test #'equal))
(defparameter *masdar-paradigm-cache* (make-hash-table :test #'equal))
(defparameter *paradigm-class-cache* (make-hash-table :test #'equal))

(defvar *parse-lock* (ccl:make-lock "parse-lock"))

(define-url-function paradigm-xml
    (request (c-root
	      word analyze id sub-id pid id-type font full-paradigm standard-only
	      (counts string) ;; nil :package)
	      ;; editing
	      update template add-template delete-template new-template
	      copy-unique-id delete-unique-id
	      edit-tense (sortinput keyword)
	      edit-pv add-feature
	      add-paradigm check-paradigm delete-paradigm
	      copy-paradigm copy-paradigm-new-id (new-id integer)
	      new-pv new-vn new-root new-c-root ;; comment
	      id-comment sub-id-comment write-lexicon
	      (lang keyword)
	      translation update-translation)
	     :uri (eq :get (request-method request))
	     :post (not (eq :get (request-method request)))
	     :xsl #'georgian-paradigm-xsl
	     :force-xslt :sablotron
	     :path (concat "/" *url-base* "/paradigm"))
  ;;(print (request-query request))
  (setf counts t)
  (setf standard-only t)
  (ccl:with-lock-grabbed (*parse-lock*)
    (with-database-connection ()
      (let* ((parser::*dg-vector* (make-array 0 :fill-pointer t :adjustable t))
	     (parser::*body-vector* (make-array 0 :fill-pointer t :adjustable t))
	     (parser::*flag-vector* (make-array 0 :fill-pointer t :adjustable t))
	     (parser::*dg-count-vector* (make-array 0 :fill-pointer t :adjustable t))
	     (parser::*dg-count* 0)
	     (user-id (get-basic-authorization request))
	     (*package* (find-package :fst))
	     (amirani-word word);; (convert-encoding word (font-encoding font) :amirani))
	     (id-type (if analyze :id (intern (string-upcase (or id-type :id)) :keyword)))
	     (p-id (if (eq id-type :id) id pid))
	     (delete (mapcar #'parse-integer (ensure-list delete-unique-id)))
	     (copy (mapcar #'parse-integer (ensure-list copy-unique-id)))
	     (edit-tenses (ensure-list edit-tense))
	     (changed-features (loop for pair in (request-query request)
				  when (find #\@ (car pair))
				  collect (cons (encoding:utf-8-decode (car pair))
						(encoding:utf-8-decode (cdr pair))))))
	#+debug(print (list :georgian-paradigm-xml (request-query request)))
	;; store changes
	(cond ((or update delete-paradigm add-template delete-template)
	       (let ((id-subid (subseq id (position-if (lambda (c) (find c "1234567890")) id))))
		 (remhash id-subid *paradigm-cache*)
		 (remhash id-subid *paradigm-class-cache*)
		 (destructuring-bind (id sub-id) (split id-subid #\-)
		   (store-paradigm-changes
		    :id id :sub-id sub-id
		    :delete delete
		    :copy copy
		    :changed-features changed-features
		    :add-template (cond ((null add-template)
					 nil)
					(new-template
					 new-template)
					((equal template "-")
					 nil)
					(t
					 template))
		    :delete-template delete-template
		    :delete-paradigm (when delete-paradigm
				       (subseq p-id (position-if (lambda (c) (find c "1234567890")) p-id)))
		    :id-comment id-comment
		    :sub-id-comment sub-id-comment))))
	      (update-translation
	       (store-translation-changes translation :paradigm-id
					  (subseq p-id (position-if (lambda (c) (find c "1234567890")) p-id))))
	      (write-lexicon
	       (with-open-file (stream "projects:xle;grammars;georgian;georgian-verb-lex.lfg" 
				       :direction :output :if-exists :supersede)
		 (write-lfg-verb-lexicon stream))))
	(when c-root (setf c-root (string-trim '(#\space #\tab) c-root)))
	(let* ((paradigm-ids+gv
		(cond ((eq id-type :pid)
		       (list (list pid (fst::get-gv pid) "?")))
		      (c-root
		       (destructuring-bind (%c-root id sub-id)
			   (fst::get-croot-id-subid c-root)
			 (setf pid (format nil "~a-~a" id sub-id)
			       p-id pid
			       c-root %c-root)
			 (list (list pid (fst::get-gv pid) "?"))))
		      (t
		       (nreverse (remove-duplicates
				  (mapcar (lambda (id+morph)
					    #+debug(print (list :id+morph id+morph))
					    (list (car id+morph)
						  (car (split (cadr id+morph) #\+ nil nil t))
						  (caddr id+morph))) ;; gv
					  (fst::kartuli-verb-morph amirani-word))
				  :test #'equal)))))
	       (id (if (and p-id (find p-id paradigm-ids+gv :test #'equal :key #'car))
		       p-id
		       (caar paradigm-ids+gv)))
	       (id-start (when id (position-if (lambda (c) (find c "1234567890")) id))) 
	       (paradigm-id (when id (subseq id id-start)))
	       (id-paradigm-ids (when id (fst::id-paradigm-ids paradigm-id)))
	       (classes ())
	       (id-paradigm-forms
		(mapcar (lambda (paradigm)
			  (or (gethash (car paradigm) *paradigm-cache*)
			      (setf (gethash (car paradigm) *paradigm-cache*)
				    (mapcar (lambda (tense)
					      #+debug(print (list :tense tense))
					      (multiple-value-bind (forms xle-templates class)
						  (fst::generate-paradigm (car paradigm)
									  :allp nil
									  :tense tense ;;'fst::future
									  :num 'fst::sg
									  :pers 1
									  :obj3sg-only-p t
									  :standard-only-p standard-only
									  :lang lang
									  :printp nil)
						(unless forms
						  (multiple-value-setq (forms xle-templates class)
						    (fst::generate-paradigm (car paradigm)
									    :allp nil
									    :tense tense ;;'fst::future
									    :num 'fst::sg
									    :pers 3
									    :obj3sg-only-p t
									    :standard-only-p standard-only
									    :lang lang
									    :printp nil)))
						#+debug(print (list forms xle-templates class))
						(when (eq tense 'fst::present)
						  (setf (gethash (car paradigm) *paradigm-class-cache*) class)
						  (push class classes))
						(caar (fst::merge-alternative-forms forms))))
					    '(fst::present #+ignore fst::future fst::aorist #+ignore fst::perfect)))))
			id-paradigm-ids)))
	  #+debug(debug id-paradigm-forms) ;;paradigm-ids+gv)
	  #m((paradigms :word #s word
			:c-root #s c-root
			:count #s(length paradigm-ids+gv)
			:id-type #s(string-downcase id-type)
			:pid #s pid 
			:font #s(or font "amirani")
			:full-paradigm #s full-paradigm
			;;:standard-only #s(or standard-only (null word))
			:counts #s counts)
	     (langs
	      #L(loop for (s name)
		     on '(:ng "Modern Georgian"
			  :og "Old Georgian"
			  :xanmeti "Xanmeti"
			  :haemeti "Haemeti")
		     by #'cddr
		     do #m(lang/ :value #s s :name #s name :selected #s (when (eq s lang) t))))
	     
	     ((root-paradigms :count #s(length id-paradigm-ids))
	      #L(loop for paradigm-id in id-paradigm-ids
		   for forms in id-paradigm-forms
		   for class = (gethash (car paradigm-id) *paradigm-class-cache*)
		   ;; in (nreverse classes)
		   do
		   (destructuring-bind (paradigm-id) paradigm-id
		     (destructuring-bind (pres #+ignore fut aor #+ignore perf) forms
		       #m((root-paradigm :id #s paradigm-id
					 :pres #s (or pres "-")
					 ;;:fut #s (or fut "-")
					 :aor #s (or aor "-")
					 ;;:perf #s (or perf "-")
					 :class #s class
					 :chosen #s(equal paradigm-id id)))))))
	     #L(let ((already-chosen-p nil))
		 (dolist (paradigm-id+gv paradigm-ids+gv)
		   #+debug(print (list :paradigm-ids+gv paradigm-ids+gv))
		   (let* ((paradigm-id (car paradigm-id+gv))
			  (gv (cadr paradigm-id+gv))
			  (tsch-class (caddr paradigm-id+gv))
			  (chosen-p (and (not already-chosen-p) (equal paradigm-id id))))
		     (when chosen-p (setf already-chosen-p t))
		     #m((reading :id #s paradigm-id
				 :gv #s gv
				 :class #s tsch-class
				 ;;:features #s(cadr reading)
				 :chosen #s(when chosen-p "yes"))
			#L(when chosen-p
			    (let* ((id-start (position-if (lambda (c) (find c "1234567890")) id)) 
				   (paradigm-id (subseq id id-start)))
			      (destructuring-bind (pid psubid) (mapcar #'parse-integer (split paradigm-id #\-))
				(multiple-value-bind (id-comment sub-id-comment) (get-paradigm-comments pid psubid)
				  #+debug(print (list :id pid :sub-id psubid
						      :pid paradigm-id :id-comment id-comment
						      :sub-id-comment sub-id-comment))
				  (multiple-value-bind (paradigm xle-templates class)
				      (fst::generate-paradigm paradigm-id
							      :allp nil
							      :obj3sg-only-p (not full-paradigm)
							      :standard-only-p standard-only
							      :lang lang
							      :printp nil)
				    #+ignore
				    (when (and add-template template (not (equal template "-")))
				      #-debug(print (list :template template))
				      (push (list template (cdr (car (cdr xle-templates))))
					    (cdr xle-templates)))
				    (setf paradigm (remove-duplicates paradigm :test #'equal))
				    #m((paradigm :id #s paradigm-id :pid #s pid
						 :psubid #s psubid :gv #s gv :class #s class)
				       (xle-templates
					#L(dolist (template (cdr xle-templates))
					    #m(template #s (car template))))
				       (available-xle-templates
					(template "-")
					#L(unless *xle-template-list*
					    (setf *xle-template-list* (get-available-xle-templates)))
					#L(dolist (xle-template *xle-template-list*)
					    (unless (string= xle-template template)
					      #m(template #s xle-template))))
				       (trans #s(or (verb-translation :paradigm-id paradigm-id) "-"))
				       #L(with-transaction () ;; change to maybe-with-transaction()?
					   #+obsolete
					   (let ((participles
						  (select [type] [stem] [code] [variety]
							  :from [morph participle]
							  :distinct t
							  :left-join [morph verb-paradigm]
							  :on [and [= [verb-participle id] [verb-paradigm id]]
								   [= [verb-participle sub-id] [verb-paradigm features-sub-id]]]
							  :where [and [= [verb-paradigm id] ?pid] 
								      [= [verb-paradigm sub-id] ?psubid]]
							  :order-by [type])))
					     (loop for (type stem code variety) in participles
						do #m((infinite-paradigm :tense #s (string-downcase type))
						      (cell
						       (cell-elt/
							:form #s stem
							;;:count #s(when counts (fullform-count form))
							:variety #s variety)))))
					   (dolist (tense (if (eq lang :ng)
							      '(fst::present fst::imperfect fst::conj-present
								fst::future fst::conditional fst::conj-future
								fst::aorist fst::optative
								fst::perfect fst::pluperfect fst::conj-perfect)
							      '(fst::present fst::imperfect fst::conj-present
								fst::iter-present fst::iter-imperfect
								fst::imperative-present
								fst::future fst::conditional fst::conj-future
								fst::aorist fst::optative fst::iter-aorist fst::imperative-aorist
								fst::perfect fst::pluperfect fst::conj-perfect fst::iter-perfect)))
					     (let ((has-tense-p (find tense paradigm :key #'cadr)))
					       #m((tense-paradigm :tense #s (string-downcase tense)
								  :empty #s(when (not has-tense-p) "yes"))
						  #L(when has-tense-p
						      (let ((obj-persons-p nil)
							    (has-no-dir-obj-pl 
							     (or (eq lang :ng)
								 (not (find (char gv 0) "TK"))
								 (not (find tense 
									    '(fst::aorist fst::optative fst::iter-aorist
									      fst::imperative-aorist
									      fst::perfect fst::pluperfect
									      fst::conj-perfect fst::iter-perfect))))))
							(dolist (subj-num '(fst::sg fst::pl))
							  (dolist (subj-pers '(1 2 3))
							    (unless (or
								     (and (= subj-pers 1)
									  (find tense '(fst::imperative-present
											fst::imperative-aorist)))
								     (and (find subj-pers '(1 2))
									  (find tense '(fst::iter-present))))
							      #m((subj-row :num #s (string-downcase subj-num) :pers #s subj-pers)
								 #L(dolist (obj-pers '(3 1 2))
								     (dolist (dir-obj-num '(fst::sg fst::pl))
								       (unless (and (= obj-pers 3)
										    (eq dir-obj-num 'fst::pl)
										    has-no-dir-obj-pl)
									 #m((cell :obj-pers #s obj-pers)
									    #L(let* ((form-list
										      (collecting
											(dolist (row paradigm)
											  (destructuring-bind
												(form %tense %subj-pers %subj-num
												      %obj-pers %obj-num %dir-obj-num
												      style replacement) row
											    (when (and (eq %tense tense)
												       (eq %subj-pers subj-pers)
												       (or (eq %subj-num subj-num)
													   (null %subj-num))
												       (eq %obj-pers obj-pers)
												       (or (eq %dir-obj-num dir-obj-num)
													   (null %dir-obj-num)))
											      #+debug
											      (print (list %dir-obj-num dir-obj-num form))
											      (collect (list form replacement)))))))
										     (forms (if counts
												;; don't merge
												form-list
												;; merge
												(fst::merge-alternative-forms form-list))))
										#+debug(print (list :forms forms :form-list form-list))
										(when add-paradigm
										  (dolist (f+r form-list)
										    (make-instance 'paradigm
												   :id pid :sub-id psubid
												   :tense (string-downcase tense)
												   :subj-num (string-downcase subj-num)
												   :subj-pers (format nil "~a" subj-pers)
												   :obj-num (string-downcase dir-obj-num)
												   :obj-pers (format nil "~a" obj-pers)
												   :verbform (car f+r))))
										(when (and forms (find obj-pers '(1 2)))
										  (setf obj-persons-p t))
										(dolist (form forms)
										  (destructuring-bind (form paradigm-replacement) form
										    #+debug(print (list :form form :paradigm-replacement paradigm-replacement))
										    #m(cell-elt/
										       :paradigm-replacement #s paradigm-replacement
										       :form #s form ;; (convert-encoding form :amirani (font-encoding font))
										       :count #s(when counts (fullform-count form))
										       :search-form #s(when (equal form word) "yes")
										       ;;:obj-num #s obj-num
										       ))))))))))))
							(when obj-persons-p
							  #m(label-row
							     #L(dolist (obj-pers '(3 1 2))
								 (dolist (dir-obj-num '(fst::sg fst::pl))
								   (unless (and (= obj-pers 3)
										(eq dir-obj-num 'fst::pl)
										has-no-dir-obj-pl)
								     #m(label/ :obj-pers #s obj-pers
									       :obj-num #s(unless (and (= obj-pers 3) has-no-dir-obj-pl)
											    (string-downcase dir-obj-num)))))))))))))))
				    (destructuring-bind (p-id p-sub-id) (split paradigm-id #\-)
				      (setf p-id (parse-integer p-id)
					    p-sub-id (parse-integer p-sub-id))
				      #m((features :id #s p-id :sub-id #s p-sub-id
						   :id-comment #s id-comment :sub-id-comment #s sub-id-comment)
					 #L(multiple-value-bind (features feature-count paradigm-features)
					       (get-paradigm-features
						p-id p-sub-id
						:tenses edit-tenses
						:sort-key (case sortinput
							    (:delete :unique-id)
							    (otherwise sortinput))
						:pv edit-pv
						:copy-p (or copy-paradigm copy-paradigm-new-id)
						:new-pv new-pv
						:new-vn new-vn
						:new-root new-root
						:new-c-root (or new-c-root new-root)
						:new-id (cond (copy-paradigm
							       p-id)
							      (new-id
							       new-id)
							      (t ;; create new superparadigm
							       nil)))
					     #m(feature-count
						#L(loop for fc-pair in feature-count
						     do (destructuring-bind (feature count) fc-pair
							  #m(fc-pair/ :feature #s feature
								      :count #s count
								      :add #s(when (string-equal feature add-feature) "yes")))))
					     #m((more-features :chosen #s add-feature)
						(feature/ :name "-")
						#L(loop for (feature count) in feature-count
						     when (zerop count)
						     do #m(feature/ :name #s feature)))
					     ;; the features for a given set of tenses
					     (dolist (tense+features features)
					       #m(tense-features
						  #L(loop for fv-pair in tense+features
						       for f-count in feature-count
						       when (or (not (zerop (cadr f-count)))
								(string-equal (car fv-pair) add-feature))
						       do (destructuring-bind (feature value) fv-pair
							    (if (listp value)
								#m((fv-pair :feature #s feature)
								   #L(dolist (v value)
								       #m(value #s v)))
								#m(fv-pair/ :feature #s feature :value #s value))))))
					     #m(edit-tenses
						#s(dolist (tense edit-tenses)
						    #m(tense/ :value #s tense)))))))))))))))))))))

;; fst::merge-two-alternative-forms

(defstylesheet georgian-paradigm-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")
     ((xsl:template :match "/paradigms")
      ((html)
       (head
        (title "Georgian Verb Paradigm"
	       ((xsl:if :test "@user")
		" [" (xsl:value-of/ :select "@user") "]"))
        ((style :type "text/css")
	 (!CDATA
	  (CSS-STYLE
	    (div :margin "16"
		 :color #-BW "#004499" #+BW "black"
		 :font-family "Verdana, Tahoma, MS Sans Serif, Arial, Geneva, Helvetica")
	    (div.title :font-size "18" :font-weight "bold" :text-align "center")
	    (div.link :font-size "12") ;; :font-weight "bold" :text-align "center")
	    (a :text-decoration "none" :color #-BW "#004499" #+BW "black")
	    (a\:hover :text-decoration "underline")
	    (span.feature-title\:hover :text-decoration "underline")
	    (td.chosen :color "red")
	    (div.label :font-size "12")
	    (div.text :font-size "8pt" :color "black" :margin-left "2px" :margin-bottom "2px")
	    (div.error :font-size "8pt" :color "red" :margin-left "2px" :margin-bottom "2px")
	    (span.error :font-size "10pt" :color "red" :font-weight "normal")
	    (td.value :background "white")
	    (td.pfeature :font-weight "bold")
	    (td.value-hilite :background "lightgray" :cursor "hand"))))
	((script :type "text/javascript" :language "javascript")
	 (!CDATA #L(js/georgian-morph stream))))
       ((body :style "font-family: Verdana, Tahoma, MS Sans Serif, Arial, Geneva, Helvetica")
	((div :class "title") "Georgian Verb Paradigm")
	((xsl:element :name "form")
	 ((xsl:attribute :name "method") "post")
	 ((xsl:attribute :name "name") "form")
	 ((xsl:attribute :name "action") "paradigm")
	 ((xsl:element :name "input")
	  ((xsl:attribute :name "type") "hidden")
	  ((xsl:attribute :name "name") "id-type")
	  ((xsl:attribute :name "id") "id-type")
	  ((xsl:attribute :name "value") (xsl:value-of/ :select "@id-type")))
	 (div
	  "Show paradigm for root: "
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "text")
	   ((xsl:attribute :name "name") "c-root")
	   ((xsl:attribute :name "value")
	    (xsl:value-of/ :select "@c-root")))
	  " or wordform: "
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "text")
	   ((xsl:attribute :name "name") "word")
	   ((xsl:attribute :name "style")
	    "font-size: 12pt"
	    #+ignore
	    (xsl:choose
	     ((xsl:when :test "/paradigms/@font='amirani'")
	      "font-family: Amirani, Verdana; font-size: 14pt")
	     ((xsl:when :test "/paradigms/@font='titus'")
	      "font-family: TITUS Cyberbit Basic; font-size: 14pt")
	     ((xsl:when :test "/paradigms/@font='bpg'")
	      "font-family: BPG SanSer UEm; font-size: 12pt")
	     ((xsl:when :test "/paradigms/@font='geo'")
	      "font-family: Geo_Literaturuli; font-size: 12pt")
	     ((xsl:when :test "/paradigms/@font='acad'")
	      "font-family: AcadNusx; font-size: 12pt")))
	   ((xsl:attribute :name "value")
	    (xsl:value-of/ :select "@word")))
	  (xsl:text " ")
	  (input/ :type "submit" :name "analyze" :value "Go")
	  " | "
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "checkbox")
	   ((xsl:attribute :name "name") "full-paradigm")
	   ((xsl:if :test "@full-paradigm")
	    ((xsl:attribute :name "checked") "yes")))
	  "full paradigm"
	  " | lang: "
	  ((xsl:element :name "select")
	   ((xsl:attribute :name "name") "lang")
	   ((xsl:attribute :name "onchange") "submit()")
	   (xsl:apply-templates/ :select "langs"))
	  #+old
	  ((xsl:element :name "select")
	   ((xsl:attribute :name "name") "font")
	   ((xsl:element :name "option")
	    ((xsl:attribute :name "value") "acad")
	    ((xsl:if :test "@font='acad'")
	     ((xsl:attribute :name "selected") "true"))
	    "AcadNusx")
	   ((xsl:element :name "option")
	    ((xsl:attribute :name "value") "amirani")
	    ((xsl:if :test "@font='amirani'")
	     ((xsl:attribute :name "selected") "true"))
	    "Amirani")
	   ((xsl:element :name "option")
	    ((xsl:attribute :name "value") "translit")
	    ((xsl:if :test "@font='translit'")
	     ((xsl:attribute :name "selected") "true"))
	    "Verdana")
	   ((xsl:element :name "option")
	    ((xsl:attribute :name "value") "bpg")
	    ((xsl:if :test "@font='bpg'")
	     ((xsl:attribute :name "selected") "true"))
	    "BPG")
	   ((xsl:element :name "option")
	    ((xsl:attribute :name "value") "geo")
	    ((xsl:if :test "@font='geo'")
	     ((xsl:attribute :name "selected") "true"))
	    "GeoLit")
	   ((xsl:element :name "option")
	    ((xsl:attribute :name "value") "titus")
	    ((xsl:if :test "@font='titus'")
	     ((xsl:attribute :name "selected") "true"))
	    "Titus"))
	  " | "
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "checkbox")
	   ((xsl:attribute :name "name") "standard-only")
	   ((xsl:if :test "@standard-only")
	    ((xsl:attribute :name "checked") "yes")))
	  "standard forms only"
	  " | "
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "checkbox")
	   ((xsl:attribute :name "name") "counts")
	   ((xsl:if :test "@counts")
	    ((xsl:attribute :name "checked") "yes")))
	  "show corpus counts")
	 ((table) ;; :width "600px")
	  (tr (td ((xsl:element :name "select")
		   ((xsl:attribute :name "onchange") "submit()")
		   ((xsl:attribute :name "name") "id")
		   ;;((xsl:attribute :name "style") "font-family: Amirani, Verdana; font-size: 14pt")
       		   (xsl:apply-templates/ :select "reading" :mode "all")))
	      (td " (" (xsl:value-of/ :select "@count") ") | "
		  ((xsl:element :name "a")
		   ((xsl:attribute :name "href")
		    "georgian-root-paradigms.xml?"
		    "id=" (xsl:value-of/ :select "reading[@chosen]/@id")
		    "&amp;font=" (xsl:value-of/ :select "/paradigms/@font"))
		   "all paradigms ")
		  (xsl:text " "))
	      (td ((xsl:element :name "select")
		   ((xsl:attribute :name "onchange")
		    "var idtype = document.getElementById('id-type'); idtype.value = 'pid'; submit()")
		   ((xsl:attribute :name "name") "pid")
		   (xsl:apply-templates/ :select "root-paradigms/root-paradigm" :mode "all")))
	      (td " (" (xsl:value-of/ :select "root-paradigms/@count") ")")
	      (td
	       " | "
	       ((xsl:element :name "a")
		((xsl:attribute :name "href")
		 "masdars?"
		 "id=" (xsl:value-of/ :select "@pid")
		 "&amp;lang=ng")
		"masdars")
	       " | "
	       ((xsl:element :name "a")
		((xsl:attribute :name "href")
		 "roots")
		"all roots"))))
	 (xsl:apply-templates/ :select "reading" :mode "paradigm")))))
     
     ((xsl:template :match "lang")
      ((xsl:element :name "option")
       ((xsl:attribute :name "value") (xsl:value-of/ :select "@value"))
       ((xsl:if :test "@selected")
	((xsl:attribute :name "selected") "true"))
       (xsl:value-of/ :select "@name")))
     
     ((xsl:template :match "reading" :mode "all")
      ((xsl:element :name "option")
       ;;((xsl:attribute :name "style") "font-family: Amirani, Verdana; font-size: 14pt")
       ((xsl:attribute :name "value") (xsl:value-of/ :select "@id"))
       ((xsl:if :test "@chosen")
	((xsl:attribute :name "selected") "true"))
       (xsl:value-of/ :select "@id")
       (xsl:text " ")
       (xsl:value-of/ :select "@gv")
       (xsl:text " ")
       (xsl:value-of/ :select "@class")))

     ((xsl:template :match "root-paradigm" :mode "all")
      ((xsl:element :name "option")
       ((xsl:attribute :name "value") (xsl:value-of/ :select "@id"))
       ((xsl:if :test "@chosen")
	((xsl:attribute :name "selected") "true"))
       (xsl:value-of/ :select "@id")
       (xsl:text " ")
       (xsl:value-of/ :select "@class")
       (xsl:text " ")
       (xsl:value-of/ :select "@pres")
       (xsl:text ", ")
       ;;(xsl:value-of/ :select "@fut") (xsl:text " ")
       (xsl:value-of/ :select "@aor")
       ;;(xsl:text " ") (xsl:value-of/ :select "@perf")
       ))
     
     ((xsl:template :match "reading" :mode "paradigm")
      ((xsl:if :test "@chosen")
       (xsl:apply-templates/ :select "paradigm")
       (br/)
       ;;#+GEKKO
       (input/ :type "submit" :name "add-paradigm" :value "Add paradigm")
       (xsl:text " ")
       ;;#+GEKKO
       (input/ :type "submit" :name "check-paradigm" :value "Check paradigm")
       (br/)
       (input/ :type "submit" :name "delete-paradigm" :value "Delete paradigm")
       (br/)
       (hr/)
       ;;#+GEKKO
       (xsl:apply-templates/ :select "features")))
     
     ((xsl:template :match "paradigm")
      ((div :style "font-size: 10pt; color: #004499")
       ((span :style "font-size: 10pt; color: black") "XLE templates: ")
       (xsl:apply-templates/ :select "xle-templates"))
      ;;#+GEKKO
      ((div :style "font-size: 10pt; color: #004499")
       ((span :style "font-size: 10pt; color: black") "Add XLE template: ")
       (xsl:apply-templates/ :select "available-xle-templates")
       (input/ :type "submit" :name "add-template" :value "Add")
       ((xsl:element :name "input")
	((xsl:attribute :name "type") "text")
	((xsl:attribute :name "name") "new-template") "")
       #+disabled(input/ :type "submit" :name "write-lexicon" :value "Write LFG Lexicon"))
      ((div :style "font-size: 12pt; color: gray")
       ((xsl:element :name "span")
	((xsl:attribute :name "style") "font-size: 10pt; color: black")
	((xsl:attribute :name "onclick")
	 "editText('translation', "
	 (xsl:value-of/ :select "@pid") ", "
	 (xsl:value-of/ :select "@psubid")
	 ")")
	"Tschenkéli translation: ")
       ((xsl:element :name "span")
	((xsl:attribute :name "id") "translation")
	((xsl:attribute :name "onclick")
	 "editText('translation', "
	 (xsl:value-of/ :select "@pid") ", "
	 (xsl:value-of/ :select "@psubid")
	 ")"
	 )
	(xsl:apply-templates/ :select "trans")))
      (table
       (xsl:apply-templates/ :select "infinite-paradigm[@tense='masdar']")
       (xsl:apply-templates/ :select "infinite-paradigm[@tense='past-part']")
       (xsl:apply-templates/ :select "infinite-paradigm[@tense='present-part']")
       (xsl:apply-templates/ :select "infinite-paradigm[@tense='future-part']")
       (xsl:apply-templates/ :select "infinite-paradigm[@tense='negative-part']"))
      (table
       (tr ((td :colspan "2" :align "center") "Series I")
	   ((td :align "center") "Series II")
	   ((td :align "center") "Series III"))
       (tr ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='present']")))
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='future']")))
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='aorist']")))
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='perfect']"))))
       (tr ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='imperfect']")))
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='conditional']")))
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='optative']")))
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='pluperfect']"))))
       (tr ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='conj-present']")))
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='conj-future']")))
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='iter-aorist']")))
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='conj-perfect']"))))
       (tr ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='iter-present']")))
	   (td)
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='imperative-aorist']")))
	   ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='iter-perfect']"))))
       (tr ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='iter-imperfect']")))
	   (td)
	   (td)
	   (td))
       (tr ((td :valign "top") (table (xsl:apply-templates/ :select "tense-paradigm[@tense='imperative-present']")))
	   (td)
	   (td)
	   (td)))
      )
     
     ((xsl:template :match "features")
      ((table :style "font-size: 10pt")
       (xsl:apply-templates/ :select "paradigm-features-list"))
      ((table :style "font-size: 10pt")
       (xsl:apply-templates/ :select "feature-count")
       (xsl:apply-templates/ :select "tense-features" :mode "values")
       )
      (table (tr (td "sub-id comment")
		 (td "id comment"))
	     (tr (td ((xsl:element :name "textarea")
		      ((xsl:attribute :name "name") "sub-id-comment")
		      ((xsl:attribute :name "rows") "3")
		      ((xsl:attribute :name "cols") "60")
		      ;;((xsl:attribute :name "onkeydown") "submitOnLinefeed(event)")
		      #+ignore
		      ((xsl:if :test "/parse/@lang='geo'")
		       ((xsl:attribute :name "style") "font-family: Amirani, Verdana; font-size: 12pt"))
		      (xsl:value-of/ :select "@sub-id-comment")))
		 (td ((xsl:element :name "textarea")
		      ((xsl:attribute :name "name") "id-comment")
		      ((xsl:attribute :name "rows") "3")
		      ((xsl:attribute :name "cols") "60")
		      ;;((xsl:attribute :name "onkeydown") "submitOnLinefeed(event)")
		      #+ignore
		      ((xsl:if :test "/parse/@lang='geo'")
		       ((xsl:attribute :name "style") "font-family: Amirani, Verdana; font-size: 12pt"))
		      (xsl:value-of/ :select "@id-comment")))))
      (div (input/ :type "submit" :name "update" :value "Update" :title "Store changes")
	   " | " (input/ :type "submit" :name "copy-paradigm" :value "Copy paradigm")
	   " | " (input/ :type "submit" :name "copy-paradigm-new-id" :value "Copy/new ID")
	   ((span :title "new id will be generated if left blank") " new id: ")
	   ((xsl:element :name "input")
	    ((xsl:attribute :name "type") "text")
	    ((xsl:attribute :name "name") "new-id"))
	   " new pv: "
	   ((xsl:element :name "input")
	    ((xsl:attribute :name "type") "text")
	    ((xsl:attribute :name "name") "new-pv"))
	   " new vn: "
	   ((xsl:element :name "input")
	    ((xsl:attribute :name "type") "text")
	    ((xsl:attribute :name "name") "new-vn"))
	   " new root: "
	   ((xsl:element :name "input")
	    ((xsl:attribute :name "type") "text")
	    ((xsl:attribute :name "name") "new-root"))
	   " new c-root: "
	   ((xsl:element :name "input")
	    ((xsl:attribute :name "type") "text")
	    ((xsl:attribute :name "name") "new-c-root"))
	   )
      (div "Show for superparadigm: "
       (table
	((tr :colspan "3")
	 (td "Preverb: "
	     ((xsl:element :name "input")
	      ((xsl:attribute :name "type") "text")
	      ((xsl:attribute :name "name") "edit-pv"))))
	(tr
	 #L(dolist (tense '("masdar" "past-part" "future-part" "present-part" "negative-part"))
	     #m(td ((xsl:element :name "input")
		    ((xsl:attribute :name "type") "checkbox")
		    ((xsl:attribute :name "name") "edit-tense")
		    ((xsl:attribute :name "value") #s tense)
		    ((xsl:if :test #s(concat "//edit-tenses/tense[@value='" tense "']"))
		     ((xsl:attribute :name "checked") "yes")))
		   #s tense)))
	(tr
	 #L(dolist (tense '("present"
			    "future"
			    "aorist"
			    "perfect"))
	     #m(td ((xsl:element :name "input")
		    ((xsl:attribute :name "type") "checkbox")
		    ((xsl:attribute :name "name") "edit-tense")
		    ((xsl:attribute :name "value") #s tense)
		    ((xsl:if :test #s(concat "//edit-tenses/tense[@value='" tense "']"))
		     ((xsl:attribute :name "checked") "yes")))
		   #s tense)))
	(tr
	 #L(dolist (tense '("imperfect"
			    "conditional"
			    "optative"
			    "pluperfect"))
	     #m(td ((xsl:element :name "input")
		    ((xsl:attribute :name "type") "checkbox")
		    ((xsl:attribute :name "name") "edit-tense")
		    ((xsl:attribute :name "value") #s tense)
		    ((xsl:if :test #s(concat "//edit-tenses/tense[@value='" tense "']"))
		     ((xsl:attribute :name "checked") "yes")))
		   #s tense)))
	(tr
	 #L(dolist (tense '("conj-present"
			    "conj-future"
			    nil
			    "conj-perfect"))
	     #m(td #L(when tense
		       #m((xsl:element :name "input")
			  ((xsl:attribute :name "type") "checkbox")
			  ((xsl:attribute :name "name") "edit-tense")
			  ((xsl:attribute :name "value") #s tense)
			  ((xsl:if :test #s(concat "//edit-tenses/tense[@value='" tense "']"))
			   ((xsl:attribute :name "checked") "yes")))
		       #m#s tense)))))
       ))
     #+ignore
     ((xsl:template :match "tense-features" :mode "features")
      ((xsl:if :test "not(following-sibling::*)")
       (tr (xsl:apply-templates/ :select "fv-pair" :mode "features"))))

     ((xsl:template :match "feature-count")
      ((xsl:element :name "input")
       ((xsl:attribute :name "type") "hidden")
       ((xsl:attribute :name "name") "sortinput")
       ((xsl:attribute :name "id") "sortinput")
       ;;((xsl:attribute :name "id") "id-type")
       ((xsl:attribute :name "value") ""))
      (tr ((td :style "color: gray; background: #ebf3ff")
	   (nobr "copy"))
	  (xsl:apply-templates/ :select "fc-pair" :mode "features")
	  ((xsl:if :test "../more-features/@chosen='-' or not(../more-features/@chosen)")
	   ((td :style "color: gray; background: #ebf3ff")
	    ((xsl:element :name "select")
	     ((xsl:attribute :name "name") "add-feature")
	     ((xsl:attribute :name "title") "add feature")
	     ((xsl:attribute :name "onchange") "submit()")
	     (xsl:apply-templates/ :select "../more-features/feature"))))))
     
     ((xsl:template :match "paradigm-features")
      (xsl:apply-templates/ :select "pfeature"))
     
     ((xsl:template :match "pfeature")
      (tr (td (xsl:value-of/ :select "@feature"))
	  ((td :class "value")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-"
	    (xsl:value-of/ :select "@feature"))
	   ((xsl:attribute :name "onclick")
	    "displayPChoices('"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','"
	    (xsl:value-of/ :select "@feature") "', '"
	    (xsl:value-of/ :select "@value")
	    (xsl:text "', true)"))
	   (xsl:value-of/ :select "@value")
	   )))
     
     ((xsl:template :match "feature")
      ((xsl:element :name "option")
       ((xsl:attribute :name "value") (xsl:value-of/ :select "@name"))
       ((xsl:if :test "@name=../@chosen")
	((xsl:attribute :name "selected") "true"))
       (xsl:value-of/ :select "@name")))

     ((xsl:template :match "tense-features" :mode "values")
      ((xsl:element :name "tr")
       ((xsl:attribute :name "id")
	(xsl:value-of/ :select "fv-pair[@feature='unique-id']/@value"))
       (xsl:apply-templates/ :select "fv-pair" :mode "values")
       (td)))
     
     ((xsl:template :match "fv-pair" :mode "values")
      (xsl:choose
       ((xsl:when :test "@feature='unique-id'")
	(td
	 (nobr ((xsl:element :name "input")
		((xsl:attribute :name "type") "checkbox")
		((xsl:attribute :name "name") "copy-unique-id")
		((xsl:attribute :name "onmouseover") "toggleCheckbox(this)")
		((xsl:attribute :name "value") (xsl:value-of/ :select "@value"))
		)))
	(td
	 (nobr ((xsl:element :name "input")
		((xsl:attribute :name "type") "checkbox")
		((xsl:attribute :name "name") "delete-unique-id")
		((xsl:attribute :name "onmouseover") "toggleCheckbox(this)")
		((xsl:attribute :name "value") (xsl:value-of/ :select "@value"))
		(xsl:value-of/ :select "@value")))))
       (xsl:otherwise
	((xsl:element :name "td")
	 ((xsl:attribute :name "class") "value")
	 ((xsl:attribute :name "id")
	  (xsl:value-of/ :select "../fv-pair[@feature='unique-id']/@value") "-" (xsl:value-of/ :select "@feature"))
	 ((xsl:attribute :name "onmouseout")
	  "this.className = 'value'")
	 ((xsl:attribute :name "onclick")
	  "displayChoices('"
	  (xsl:value-of/ :select "../fv-pair[@feature='unique-id']/@value") "', '"
	  (xsl:value-of/ :select "@feature") "', '"
	  (xsl:choose
	   ((xsl:when :test "@feature='tense'")
	    (xsl:apply-templates/ :select "value" ))
	   (xsl:otherwise
	    (xsl:value-of/ :select "@value")))
	  (xsl:text "', true)")
	  )
	 ((xsl:attribute :name "onmouseover")
	  "this.className = 'value-hilite'; displayChoices('"
	  (xsl:value-of/ :select "../fv-pair[@feature='unique-id']/@value") "', '"
	  (xsl:value-of/ :select "@feature") "', '"
	  (xsl:choose
	   ((xsl:when :test "@feature='tense'")
	    (xsl:apply-templates/ :select "value" ))
	   (xsl:otherwise
	    (xsl:value-of/ :select "@value")))
	  (xsl:text "', false)"))
	 (nobr (xsl:choose
		((xsl:when :test "@feature='tense'")
		 (xsl:apply-templates/ :select "value" ))
		(xsl:otherwise
		 (xsl:value-of/ :select "@value"))))))))
     
     ((xsl:template :match "value")
      ((xsl:if :test "preceding-sibling::value")
       ", ")
      (xsl:value-of/ :select "."))
     
     ;; features title cell
     ((xsl:template :match "fc-pair" :mode "features")
      (xsl:choose
       ((xsl:when :test "not(@count=0)")
	((td :style "color: gray; background: #ebf3ff")
	 ((xsl:element :name "span")
	  ((xsl:attribute :name "class") "feature-title")
	  ((xsl:attribute :name "style") "cursor: hand")
	  ((xsl:attribute :name "onclick") "sortinput.value = '" (xsl:value-of/ :select "@feature") "'; form.submit()")
	  (nobr (xsl:value-of/ :select "@feature")))))
       ((xsl:when :test "@add")
	((td :style "color: gray; background: #ebf3ff")
	 ((xsl:element :name "select")
	  ((xsl:attribute :name "name") "add-feature")
	  ((xsl:attribute :name "title") "add feature")
	  ((xsl:attribute :name "onchange") "submit()")
	  (xsl:apply-templates/ :select "../../more-features/feature"))))))
     
     ((xsl:template :match "trans")
      (xsl:choose
       ((xsl:when :test "ger|geo")
	(xsl:apply-templates/ :select "ger|geo"))
       (xsl:otherwise
	(span "-"))))

     ((xsl:template :match "geo")
      ((xsl:element :name "span")
       #+orig
       ((xsl:attribute :name "style")
	(xsl:choose
	 ((xsl:when :test "/paradigms/@font='amirani'")
	  "font-family: Amirani, Verdana; font-size: 14pt")
	 ((xsl:when :test "/paradigms/@font='titus'")
	  "font-family: TITUS Cyberbit Basic; font-size: 14pt")
	 ((xsl:when :test "/paradigms/@font='bpg'")
	  "font-family: BPG SanSer UEm; font-size: 12pt")
	 ((xsl:when :test "/paradigms/@font='geo'")
	  "font-family: Geo_Literaturuli; font-size: 12pt")
	 ((xsl:when :test "/paradigms/@font='acad'")
	  "font-family: AcadNusx; font-size: 12pt")))
       (xsl:apply-templates/)))

     ((xsl:template :match "ger")
      ((xsl:element :name "span")
       ((xsl:attribute :name "style")
	"font-family: Sabon, Amirani, Verdana; font-size: 12pt")
       (xsl:apply-templates/)))
     
     ((xsl:template :match "trans")
      (xsl:apply-templates/))
     
     ;;#-GEKKO
     ;;((xsl:template :match "xle-templates")
     ;; (xsl:apply-templates/ :select "template" :mode "list"))

     ;;;#+GEKKO
     ((xsl:template :match "xle-templates")
      (table
       (xsl:apply-templates/ :select "template" :mode "table")))

     ((xsl:template :match "template" :mode "list")
      (span (xsl:value-of/ :select "text()")
	    ((xsl:if :test "following-sibling::*")
	     (xsl:text ", "))))

     ((xsl:template :match "template" :mode "table")
      (tr (td ((span :style "font-size: 10pt; color: #004499") (xsl:value-of/ :select "text()")))
	  (td ((xsl:element :name "button")
	       ((xsl:attribute :name "style") "font-size: 6pt")
	       ((xsl:attribute :name "type") "submit")
	       ((xsl:attribute :name "name") "delete-template")
	       ((xsl:attribute :name "value")
		(xsl:value-of/ :select "text()"))
	       "-"))))
     
     ((xsl:template :match "available-xle-templates")
      ((xsl:element :name "select")
       ((xsl:attribute :name "name") "template")
       (xsl:apply-templates/ :select "template" :mode "add")))
     
     ((xsl:template :match "template" :mode "add")
       ((xsl:element :name "option")
	((xsl:attribute :name "value") (xsl:value-of/ :select "."))
	(xsl:value-of/ :select ".")))
       
     ((xsl:template :match "tense-paradigm")
      (tr ((td :colspan 7 :style "font-weight: bold")
	   (xsl:value-of/ :select "@tense")))
      (xsl:choose
       ((xsl:when :test "@empty")
	(tr (td "-")))
       (xsl:otherwise
	(xsl:apply-templates/ :select "label-row")
	(xsl:apply-templates/ :select "subj-row"))))

     ((xsl:template :match "infinite-paradigm")
      (tr ((td :colspan 1 :style "font-weight: bold; vertical-align: baseline")
	   (xsl:value-of/ :select "@tense"))
	  (xsl:choose
	   ((xsl:when :test "@empty")
	    (td "-"))
	   (xsl:otherwise
	    (xsl:apply-templates/ :select "cell")))))

     ((xsl:template :match "label-row")
      ((xsl:element :name "tr")
       (td/ :style "color: gray; background: #ebf3ff")
       (xsl:apply-templates/ :select "label")))
     
     ((xsl:template :match "label")
      ((td :style "color: gray; background: #ebf3ff; vertical-align: baseline")
       (xsl:value-of/ :select "@obj-pers")
       (xsl:value-of/ :select "@obj-num")))

     ((xsl:template :match "subj-row")
      ((xsl:element :name "tr")
       #+ignore
       ((xsl:attribute :name "style")
	((xsl:if :test "cell/cell-elt/@paradigm-replacement") "background: lightgray; "))
       ((td :style "color: gray; background: #ebf3ff; vertical-align: baseline")
	(xsl:value-of/ :select "@pers")
	(xsl:value-of/ :select "@num"))
       (xsl:apply-templates/ :select "cell")))
     
     ((xsl:template :match "cell")
      ((xsl:element :name "td")
       ;;((xsl:attribute :name "valign") "bottom")
       ((xsl:attribute :name "style")
	"padding-left: 0.4em; vertical-align: baseline; "
	#+ignore
	(xsl:choose
	 ((xsl:when :test "/paradigms/@font='amirani'")
	  "font-family: Amirani, Verdana; font-size: 12pt")
	 ((xsl:when :test "/paradigms/@font='titus'")
	  "font-family: TITUS Cyberbit Basic; font-size: 12pt")
	 ((xsl:when :test "/paradigms/@font='bpg'")
	  "font-family: BPG SanSer UEm; font-size: 12pt")
	 ((xsl:when :test "/paradigms/@font='geo'")
	  "font-family: Geo_Literaturuli; font-size: 12pt")
	 ((xsl:when :test "/paradigms/@font='acad'")
	  "font-family: AcadNusx; font-size: 12pt")))
       (xsl:apply-templates/ :select "cell-elt")))
     
     ((xsl:template :match "cell-elt")
      ((xsl:element :name "span")
       ((xsl:attribute :name "style")
	((xsl:if :test "@search-form") "background: yellow; ")
	((xsl:if :test "@paradigm-replacement") "color: red"))
       (xsl:value-of/ :select "@form"))
      ((xsl:if :test "@variety")
       ((xsl:element :name "span")
	((xsl:attribute :name "style") "font-size: 10pt; color: gray") 
	" [" (xsl:value-of/ :select "@variety") "]"))
      ((xsl:if :test "@count")
       ((xsl:element :name "span")
	((xsl:attribute :name "style") "font-size: 10pt; color: gray") 
	" (" (xsl:value-of/ :select "@count") ")"))
      ((xsl:if :test "following-sibling::*")
       (xsl:text ", ")))))

(define-javascript-writer js/georgian-morph (stream)
  #j((defun display-choices (uid feature value click)
       (when (or click event.ctrl-key)
	 #+debug(alert (+ uid " - " feature ": " value))
	 (let* ((doc window.parent.document)
		(td (doc.get-element-by-id (+ uid "-" feature)))
		(tr (doc.get-element-by-id uid))
		(req (new (XMLHttpRequest))))
	   (req.open "get" (+ "js/display-choices.xml?unique-id="
			      uid
			      "&feature=" feature
			      "&value=" (encodeURIComponent value))
		     false))
	 (req.send "")
	 (when (not (= req.responseText ""))
	   (setf td.outerHTML req.responseText))))

     (defun display-p-choices (impf-pf-pv feature value click)
       (when (or click event.ctrl-key)
	 #+debug(alert (+ uid " - " feature ": " value))
	 (let* ((td (document.get-element-by-id (+ impf-pf-pv "-" feature)))
		(req (new (XMLHttpRequest))))
	   (req.open "get" (+ "js/display-choices.xml?impf-pf-pv=" impf-pf-pv
			      "&feature=" feature
			      "&value=" (encodeURIComponent value))
		     false))
	 (req.send "")
	 (when (not (= req.responseText ""))
	   (setf td.outerHTML req.responseText))))
     
     (defun edit-text (type id sub-id)
       ;;(alert (+ type ": " id " - " sub-id))
       (let* ((doc window.parent.document)
	      (elt (doc.get-element-by-id type))
	      (req (new (XMLHttpRequest))))
	 (req.open "get" (+ "js/edit-text.xml?type="
			    type "&id=" id "&sub-id=" sub-id)
		   false))
       (req.send "")
       ;;(alert req.responseText)
       (when (not (= req.responseText ""))
	 (setf elt.outerHTML req.responseText)))
     
     (defun set-value (event feature value)
       ;;(alert event.ctrl-key)
       ;;(when event.ctrl-key
	 (let* ((tds (document.get-elements-by-name feature))
		(i 0))
	   (for ((< i tds.length) (incf i))
	     (let ((td (aref tds i)))
	       (set-option td.first-child value)))))

     (defun set-option (select-elt value)
       (let ((e select-elt.first-child))
	 (for (e (setf e e.next-sibling))
	   (cond ((= (e.get-attribute "value") value)
		  (setf e.selected "true"))
		 #+ignore(t
		  (setf e.selected null))))))

     (defun toggle-checkbox (checkbox)
       (when event.ctrl-key
	 ;;(alert checkbox.checked)
	 (setf checkbox.checked (not checkbox.checked))))
     ))

;; maps features to set of possible values
(defparameter *value-list-cache* (make-hash-table :test #'equal))

#+test
(push "dangerous" (gethash 'style *value-list-cache*))
#+test
(push "disabled" (gethash 'style *value-list-cache*))
#+test
(push "წინა" (gethash 'impf-pv *value-list-cache*))
#+test
(push "ზედა" (gethash 'pf-pv *value-list-cache*))
#+test
(push "*" (gethash 'pf-pv *value-list-cache*))
#+test
(push "rust" (gethash 'style *value-list-cache*))
#+test
(push "ს" (gethash 'type-subj3-sfx *value-list-cache*))

#+test
(push "false" (gethash 'disabled *value-list-cache*))

#+test
(push "x" (gethash 'TYPE-OBJ-3-PFX *value-list-cache*))

#+test
(print (get-paradigm-feature-choices 'class))

;; does not write changed value to database; this is done in marsdars-xml() 
(define-url-function display-choices-xml
    (request (unique-id impf-pf-pv feature value set-single)
	     :uri (eq :get (request-method request))
	     :post (not (eq :get (request-method request)))
	     :xsl #'display-choices-xsl
	     :force-xslt :sablotron
	     :write-doctype-p nil
	     :path (concat "/" *url-base* "/js/display-choices.xml"))
  ;;(print (request-query request))
  (with-database-connection ()
    (let ((sfeature (find feature '(sf caus-sf vv tsch-class class morph-type relation reduplication ;; red-dir-pv
				    ;;pv
				    stem-type pr-st-ext part-pfx part-sfx passive-sfx
				    nasal-infix type-aorist type-obj-3-pfx type-aorist-3sg
				    type-optative subj-pers subj-num obj-pers type-subj12-sfx
				    type-subj3-sfx type-subj2-pfx type-ev-sfx style lang
				    type-pr-st-ext
				    impf-pv pf-pv red-dir-pv disabled)
			  :test #'string-equal)))
      (cond (sfeature
	     (let ((values (or (gethash sfeature *value-list-cache*)
			       (setf (gethash sfeature *value-list-cache*)
				     (if impf-pf-pv
					 (delete-if #'null (get-paradigm-feature-choices sfeature))
					 (get-feature-choices sfeature))))))
	       #m((cell :type "list"
			:unique-id #s (or unique-id impf-pf-pv)
			:impf-pf-pv #s impf-pf-pv
			:feature #s feature
			:chosen-value #s value)
		  #s(dolist (value values)
		      ;;(debug value)
		      #m(value #s(substitute #\/ #\| (or value "")))))))
	    (t
	     #m(cell/ :type "text"
		      :unique-id #s (or unique-id impf-pf-pv)
		      :feature #s feature
		      :chosen-value #s value))))))

(defstylesheet display-choices-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")
     ((xsl:template :match "//cell[@type='text']")
      (td
       ((xsl:element :name "input")
	((xsl:attribute :name "type") "text")
	((xsl:attribute :name "name") (xsl:value-of/ :select "@unique-id") "@" (xsl:value-of/ :select "@feature") )
	((xsl:attribute :name "value")
	 (xsl:value-of/ :select "@chosen-value")))))
     
     ((xsl:template :match "//cell[@type='list']")
      ((xsl:element :name "td")
       ((xsl:attribute :name "name") (xsl:value-of/ :select "@feature"))
       ((xsl:element :name "select")
	((xsl:attribute :name "name") (xsl:value-of/ :select "@unique-id") "@" (xsl:value-of/ :select "@feature"))
	((xsl:attribute :name "feature") (xsl:value-of/ :select "@feature"))
	((xsl:attribute :name "onchange") "setValue(event,'" (xsl:value-of/ :select "@feature") "', this.value)")
	(xsl:apply-templates/ :select "value" :mode "list"))))
     
     ((xsl:template :match "value" :mode "list")
      ((xsl:element :name "option")
       ((xsl:attribute :name "value") (xsl:value-of/ :select "."))
       ((xsl:if :test "../@chosen-value=.")
	((xsl:attribute :name "selected") "true"))
       (xsl:value-of/ :select ".")))))

(defmethod edit-text-xml ((request http-request) entity)
  (with-database-connection ()
    (with-xml-response (request entity stream (type id sub-id)
				:xsl #'edit-text-xsl
				:force-xslt :sablotron
				:write-doctype-p nil
				)
      #+debug(print (list :request-query (request-query request)))
      (cond ((string= type "translation")
	     (let ((translation (verb-translation :id id :sub-id sub-id)))
	       #m(translation #s (or translation "-"))))))))

(defstylesheet edit-text-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")
     ((xsl:template :match "/translation")
      (div
       ((xsl:element :name "textarea")
	((xsl:attribute :name "name") "translation")
	((xsl:attribute :name "style") "font-family: BPG Glaho Regular; font-size: 12pt")
	((xsl:attribute :name "rows") "5")
	((xsl:attribute :name "cols") "100")
	(xsl:value-of/ :select "."))
       (br/)
       (input/ :type "submit" :name "update-translation" :value "Update")))))

(publish :path (concatenate 'string "/" *url-base* "/js/display-choices.xsl")
	 :content-type "text/xml"
	 :function #'display-choices-xsl)

(publish :path (concatenate 'string "/" *url-base* "/js/edit-text.xml")
	 :class 'xml/html-entity
	 :function #'edit-text-xml)

(publish :path (concatenate 'string "/" *url-base* "/js/edit-text.xsl")
	 :content-type "text/xml"
	 :function #'edit-text-xsl)

(publish :path (concat "/" *url-base* "/georgian-paradigm.xsl")
	 :content-type "text/xml"
	 :function #'georgian-paradigm-xsl)

(define-url-function root-paradigms-xml
    (request (id font next-root previous-root refresh delete-nonfinite
		 (drag-subid integer)
		 (drop-subid integer))
	     :xsl #'georgian-root-paradigms-xsl
	     :force-xslt :sablotron
	     :path (concat "/" *url-base* "/georgian-root-paradigms.xml"))
  ;;(print (request-query request))
  (with-database-connection ()
    ;;(debug id)
    (let* ((user-id (get-basic-authorization request))
	   (font (or font "amirani"))
	   (id (find-if (lambda (str) (find (char str 0) "123456789")) (split id #\-)))
	   (*package* (find-package :fst))
	   (id (parse-integer id)))
      (cond (next-root (incf id))
	    (previous-root (decf id)))
      (when (and drag-subid drop-subid)
	(copy-participles id :from-subid drag-subid :to-subid drop-subid))
      (setf id (format nil "~d" id))
      (when delete-nonfinite (delete-nonfinite-forms id delete-nonfinite))
      (multiple-value-bind (paradigm-tree classes) (fst::paradigm-classes id)
	#m((paradigms :id #s id :font #s font)
	   (classes
	    #L(dolist (class classes)
		#m((class :name #s(string-downcase class))
		   #s(getf '(fst::trans "I" fst::caus "I:caus" fst::unacc "II" fst::unerg "III" fst::inv "IV")
			   class))))
	   #L(let ((relation nil)
		   (preverb nil))
	       (dat:do-string-tree (vn pv+rel+paradigms paradigm-tree :end-marker #\:)
		 #m(vn #s vn)
		 (dat:do-string-tree (pv rel+paradigms pv+rel+paradigms :end-marker #\:)
		   #m((pv-group :pv #s(or pv ""))
		      #L(dat:do-string-tree (rel paradigms rel+paradigms)
			  (let ((paradigms (sort paradigms #'<
						 :key (lambda (pd)
							(parse-integer
							 (car pd)
							 :start (1+ (position #\- (car pd))))))))
			    #m((pv-row :pv #s(unless (equal pv preverb) pv (concat (or pv "") "-"))
				       :relation #s(unless (and (equal rel relation)
								(equal pv preverb))
						     rel))
			       #L(cond ((equal pv preverb)
					(setf relation rel))
				       (t
					(setf relation nil preverb pv)))
			       #L(dolist (class classes)
				   #m((class :name #s (string-downcase class))
				      #L(dolist (paradigm paradigms)
					  (paradigm-cell-xml paradigm stream :vn vn :class class :pv pv :font font))))))))))))))))

(defun paradigm-cell-xml (paradigm stream &key vn class pv font)
  (destructuring-bind (id+subid gv cl morph &key present aorist
				masdar past-part present-part future-part negative-part &allow-other-keys) paradigm
    (declare (ignore morph))
    (when (or (null class) (eq cl class))
      (destructuring-bind (id sub-id) (mapcar #'parse-integer (split id+subid #\-))
	(destructuring-bind (&optional base-sub-id participle-sub-id source) (paradigm-base/participle-sub-id+source id sub-id)
	  #+debug(print (list :id id :sub-id sub-id :base-sub-id base-sub-id :participle-sub-id participle-sub-id :source source))
	  (when base-sub-id
	    #m((paradigm :vn #s vn
			 :pv #s pv
			 :id #s id
			 :sub-id #s sub-id
			 :base-sub-id #s (unless (= sub-id base-sub-id) base-sub-id)
			 :participle-sub-id #s participle-sub-id
			 :source #s source
			 :gv #s gv ;; :morph #s morph
			 :pres #s(or present "-")
			 :aor #s (or aorist "-")
			 :past-part #s past-part
			 :present-part #s present-part
			 :future-part #s future-part
			 :negative-part #s negative-part
			 :masdar #s masdar
			 :font #s font)
	       #L(when (= sub-id base-sub-id)
		   (dolist (template (paradigm-xle-templates id sub-id))
		     #m(xle-template #s template))))))))))

(defstylesheet georgian-root-paradigms-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")
     ((xsl:template :match "/paradigms")
      ((html)
       (head
        (title "Georgian Verb Paradigms for Root #" (xsl:value-of/ :select "@id")
	       ((xsl:if :test "@user")
		" [" (xsl:value-of/ :select "@user") "]"))
        ((style :type "text/css")
	 (!CDATA
	  (CSS-STYLE
	    (div :margin "16"
		 :color #-BW "#004499" #+BW "black"
		 :font-family "Verdana, Tahoma, MS Sans Serif, Arial, Geneva, Helvetica")
	    (div.title :font-size "18" :font-weight "bold" :text-align "center")
	    (div.link :font-size "12");; :font-weight "bold" :text-align "center")
	    (a :text-decoration "none" :color #-BW "#004499" #+BW "black")
	    (a\:hover :text-decoration "underline")
	    (td.chosen :color "red")
	    (div.label :font-size "12")
	    (div.text :font-size "8pt" :color "black" :margin-left "2px" :margin-bottom "2px")
	    (div.error :font-size "8pt" :color "red" :margin-left "2px" :margin-bottom "2px")
	    (span.error :font-size "10pt" :color "red" :font-weight "normal")
	    (span.xle-template :font-size "8pt" :color "gray" :font-weight "normal"))))
	((script :type "text/javascript" :language "javascript")
	 (!CDATA #L(js/root-paradigms stream))))
       ((xsl:element :name "body")
	((xsl:attribute :name "style") "font-family: Verdana, Tahoma, MS Sans Serif, Arial, Geneva, Helvetica")
	((xsl:attribute :name "onkeydown") "handleKeyDown(event)")
	((xsl:element :name "form")
	 ((xsl:attribute :name "method") "post")
	 ((xsl:attribute :name "name") "previous-next-form")
	 ((xsl:attribute :name "id") "previousNextForm")
	 ((xsl:attribute :name "action") "georgian-root-paradigms.xml")
	 ((div :class "title") "Georgian Verb Paradigms for Root #" (xsl:value-of/ :select "@id"))
	 (div
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "hidden")
	   ((xsl:attribute :name "name") "previous-root")
	   ((xsl:attribute :name "id") "previousRoot")
	   ((xsl:attribute :name "value") ""))
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "hidden")
	   ((xsl:attribute :name "name") "next-root")
	   ((xsl:attribute :name "id") "nextRoot")
	   ((xsl:attribute :name "value") ""))
	  ((xsl:element :name "button")
	   ((xsl:attribute :name "type") "submit")
	   ((xsl:attribute :name "name") "previous-root")
	   ((xsl:attribute :name "value")
	    "previous")
	   "Previous")
	  ((xsl:element :name "button")
	   ((xsl:attribute :name "type") "submit")
	   ((xsl:attribute :name "name") "next-root")
	   ((xsl:attribute :name "value")
	    "next")
	   "Next")
	  ((xsl:element :name "button")
	   ;;((xsl:attribute :name "style") "font-size: 6pt")
	   ((xsl:attribute :name "type") "submit")
	   ((xsl:attribute :name "name") "refresh")
	   ((xsl:attribute :name "value")
	    "refresh")
	   "Refresh")
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "hidden")
	   ((xsl:attribute :name "name") "id")
	   ((xsl:attribute :name "id") "id")
	   ((xsl:attribute :name "value") (xsl:value-of/ :select "@id")))
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "hidden")
	   ((xsl:attribute :name "name") "font")
	   ((xsl:attribute :name "id") "font")
	   ((xsl:attribute :name "value") (xsl:value-of/ :select "@font")))
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "hidden")
	   ((xsl:attribute :name "name") "drag-subid")
	   ((xsl:attribute :name "id") "dragSubid"))
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "hidden")
	   ((xsl:attribute :name "name") "drop-subid")
	   ((xsl:attribute :name "id") "dropSubid"))
	  )
	 ((table :border "2" :frame "hsides"
		 :rules "groups")
	  (xsl:apply-templates/ :select "classes")
	  (xsl:apply-templates/ :select "vn|pv-group"))))))
     
     ((xsl:template :match "vn")
      (tbody
       ((tr)
	((xsl:element :name "td")
	 ((xsl:attribute :name "colspan") "8")
	 ((xsl:attribute :name "valign") "top")
	 ((xsl:attribute :name "style")
	  "border-top: 1px gray solid; border-bottom: 1px gray solid; text-align: center; font-size: 14pt; font-weight: bold; ")
	 (xsl:apply-templates/)))))

     ((xsl:template :match "pv-group")
      (tbody
       (xsl:apply-templates/ :select "pv-row")))
     
     ((xsl:template :match "pv-row")
      ((xsl:element :name "tr")
       ((xsl:attribute :name "style")
	(xsl:choose
	 ((xsl:when :test "@relation='relative'")
	  "background: #ebf3ff; ")
	 (xsl:otherwise
	  "background: white; ")))
       ((xsl:element :name "td")
	((xsl:attribute :name "valign") "top")
	((xsl:attribute :name "style")
	 "vertical-align: baseline; ")
	(xsl:value-of/ :select "@pv")) ;; fixme
       ((td :style "vertical-align: baseline; font-size: 10pt; font-style: italic")
	(xsl:value-of/ :select "@relation"))
       (xsl:apply-templates/)))
     
     ((xsl:template :match "class")
      ((td :valign "top") (xsl:apply-templates/)))

     ((xsl:template :match "classes")
      (thead
       (tr ((td :style "font-style: italic; font-size: 10pt") "preverb")
	   ((td :style "font-style: italic; font-size: 10pt") "relation")
	   (xsl:apply-templates/ :select "class" :mode "title"))))
     
     ((xsl:template :match "class" :mode "title")
      ((td :style "text-align: center") (xsl:value-of/ :select "text()")))

     #L(%paradigm-cell-xsl stream)))

(defstylesheet paradigm-cell-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")
     ((xsl:template :match "/")
      (xsl:apply-templates/))
     #L(%paradigm-cell-xsl stream)))
     
(defun %paradigm-cell-xsl (stream)
  #m((xsl:template :match "paradigm")
     ((xsl:element :name "span")
      ((xsl:attribute :name "id") "link-span" (xsl:value-of/ :select "@sub-id"))
      (nobr ((xsl:element :name "span")
	     ;;((xsl:attribute :name "id") "link-span" (xsl:value-of/ :select "@sub-id"))
	     (xsl:choose
	      ((xsl:when :test "not(@base-sub-id)")
	       ((xsl:element :name "button")
		((xsl:attribute :name "style") "font-size: 6pt")
		((xsl:attribute :name "type") "button")
		((xsl:attribute :name "onclick") "addMorphosyntacticLink("
		 (xsl:value-of/ :select "@id") "," (xsl:value-of/ :select "@sub-id") ",'" (xsl:value-of/ :select "@font") "')")
		((xsl:attribute :name "value")
		 (xsl:value-of/ :select "@sub-id"))
		"&lt;")
	       ((xsl:element :name "input")
		((xsl:attribute :name "type") "text")
		((xsl:attribute :name "name") "link-id" (xsl:value-of/ :select "@sub-id") )
		((xsl:attribute :name "id") "link-id" (xsl:value-of/ :select "@sub-id") )
		((xsl:attribute :name "style") "font-size: 5pt; width: 3em")
		((xsl:attribute :name "value")
		 (xsl:value-of/ :select "@link-candidate"))
		(xsl:text " ")))
	      (xsl:otherwise
	       ((xsl:element :name "button")
		((xsl:attribute :name "style") "font-size: 6pt")
		((xsl:attribute :name "type") "button")
		((xsl:attribute :name "onclick") "removeMorphosyntacticLink("
		 (xsl:value-of/ :select "@id") "," (xsl:value-of/ :select "@sub-id") ",'" (xsl:value-of/ :select "@font") "')")
		"+")
	       ((span :style "font-size: 10pt") "[&lt; " (xsl:value-of/ :select "@base-sub-id") "] ")))
	     )
	    ((span :style "font-size: 10pt")
	     (xsl:choose
	      ((xsl:when :test "@base-sub-id")
	       (xsl:value-of/ :select "@gv"))
	      (xsl:otherwise
	       ((span :style "font-weight: bold")
		(xsl:value-of/ :select "@gv"))))
	     ": ")
	    ((xsl:element :name "button")
	     ((xsl:attribute :name "style") "font-size: 6pt")
	     ((xsl:attribute :name "type") "button")
	     ((xsl:attribute :name "onclick") "deleteParadigm("
	      (xsl:value-of/ :select "@id") "," (xsl:value-of/ :select "@sub-id") ",'" (xsl:value-of/ :select "@font") "')")
	     ((xsl:attribute :name "value")
	      (xsl:value-of/ :select "@sub-id"))
	     "-")
	    (xsl:text " ")
	    ((xsl:element :name "a")
	     ((xsl:attribute :name "href")
	      "paradigm?"
	      "pid=" (xsl:value-of/ :select "@id") "-" (xsl:value-of/ :select "@sub-id")
	      "&amp;id-type=pid&amp;font=" (xsl:value-of/ :select "@font")
	      "&amp;standard-only=yes"
	      "&amp;lang=ng")
	     ((xsl:attribute :name "ondragenter") "entryDragEnter()")
	     ((xsl:attribute :name "ondragover") "entryDragOver()")
	     ((xsl:attribute :name "ondrop")
	      "entryDrop(" (xsl:value-of/ :select "@id") "," (xsl:value-of/ :select "@sub-id") ")")
	     (xsl:value-of/ :select "@sub-id")
	     #+ignore
	     ((xsl:if :test "@base-sub-id")
	      "[" (xsl:value-of/ :select "@base-sub-id") "]")
	     ": "
	     (xsl:value-of/ :select "@pres") ", "
	     (xsl:value-of/ :select "@aor"))
	    (xsl:choose
	     ((xsl:when :test "@masdar|@past-part|@present-part|@future-part|@negative-part")
	      (br/)
	      ((xsl:element :name "span")
	       ((xsl:attribute :name "style")
		(xsl:choose
		 ((xsl:when :test "@font='amirani'")
		  "font-family: Amirani, Verdana; font-size: 12pt;")
		 ((xsl:when :test "@font='titus'")
		  "font-family: TITUS Cyberbit Basic; font-size: 12pt;")
		 ((xsl:when :test "@font='bpg'")
		  "font-family: BPG SanSer UEm; font-size: 12pt;")
		 ((xsl:when :test "@font='geo'")
		  "font-family: Geo_Literaturuli; font-size: 12pt;")
		 ((xsl:when :test "@font='acad'")
		  "font-family: AcadNusx; font-size: 12pt;"))
		((xsl:if :test "not(@source='Tschenkeli')")
		 " color: red"))
	       ((xsl:element :name "button")
		((xsl:attribute :name "style") "font-size: 6pt")
		((xsl:attribute :name "type") "button")
		((xsl:attribute :name "onclick") "addParticipleLink("
		 (xsl:value-of/ :select "@id") "," (xsl:value-of/ :select "@sub-id") ",'" (xsl:value-of/ :select "@font") "')")
		((xsl:attribute :name "value")
		 (xsl:value-of/ :select "@sub-id"))
		"^")
	       ((xsl:element :name "input")
		((xsl:attribute :name "type") "text")
		((xsl:attribute :name "name") "participle-link-id" (xsl:value-of/ :select "@sub-id") )
		((xsl:attribute :name "id") "participle-link-id" (xsl:value-of/ :select "@sub-id") )
		((xsl:attribute :name "style") "font-size: 5pt; width: 3em")
		((xsl:attribute :name "value")
		 (xsl:value-of/ :select "@nonfinite-link-candidate"))
		(xsl:text " "))
	       ((xsl:element :name "button")
		((xsl:attribute :name "style") "font-size: 6pt")
		((xsl:attribute :name "type") "button")
		((xsl:attribute :name "onclick") "deleteParticiple("
		 (xsl:value-of/ :select "@id") "," (xsl:value-of/ :select "@sub-id")  ",'" (xsl:value-of/ :select "@font") "')")
		((xsl:attribute :name "value")
		 (xsl:value-of/ :select "@sub-id"))
		"-")
	       ((xsl:element :name "a")
		((xsl:attribute :name "style") "font-weight: bold; color: green; cursor: pointer")
		((xsl:attribute :name "ondragstart")
		 "entryDragStart(" (xsl:value-of/ :select "@sub-id") ")")
		((xsl:attribute :name "ondragenter") "entryDragEnter()")
		((xsl:attribute :name "ondragover") "entryDragOver()")
		#+ignore
		((xsl:attribute :name "ondrop")
		 "entryDrop(" (xsl:value-of/ :select "@sub-id") ")")
		((xsl:attribute :name "href") "#")
		
		(xsl:value-of/ :select "@masdar") ", "
		(xsl:value-of/ :select "@past-part") ", "
		(xsl:value-of/ :select "@present-part") ", "
		(xsl:value-of/ :select "@future-part") ", "
		(xsl:value-of/ :select "@negative-part")
		)))
	     ((xsl:when :test "@participle-sub-id")
	      (br/)
	      ((xsl:element :name "button")
	       ((xsl:attribute :name "style") "font-size: 6pt")
	       ((xsl:attribute :name "type") "button")
	       ((xsl:attribute :name "onclick") "removeParticipleLink("
		(xsl:value-of/ :select "@id") "," (xsl:value-of/ :select "@sub-id") ",'" (xsl:value-of/ :select "@font") "')")
	       "+")
	      ((span :style "font-size: 10pt") "[^ " (xsl:value-of/ :select "@participle-sub-id") "] "))
	     ((xsl:when :test "@base-sub-id")
	      )
	     (xsl:otherwise
	      (br/)
	      ((xsl:element :name "button")
	       ((xsl:attribute :name "style") "font-size: 6pt")
	       ((xsl:attribute :name "type") "button")
	       ((xsl:attribute :name "onclick") "addParticipleLink("
		(xsl:value-of/ :select "@id") "," (xsl:value-of/ :select "@sub-id")  ",'" (xsl:value-of/ :select "@font") "')")
	       ((xsl:attribute :name "value")
		(xsl:value-of/ :select "@sub-id"))
	       "^")
	      ((xsl:element :name "input")
	       ((xsl:attribute :name "type") "text")
	       ((xsl:attribute :name "name") "participle-link-id" (xsl:value-of/ :select "@sub-id") )
	       ((xsl:attribute :name "id") "participle-link-id" (xsl:value-of/ :select "@sub-id") )
	       ((xsl:attribute :name "style") "font-size: 5pt; width: 3em")
	       ((xsl:attribute :name "value")
		(xsl:value-of/ :select "@nonfinite-link-candidate"))
	       (xsl:text " ")))))
      (br/)
      (xsl:apply-templates/ )))
     
  #m((xsl:template :match "xle-template")
     ((span :class "xle-template") (xsl:value-of/ :select "text()"))
     (br/)))

(define-javascript-writer js/root-paradigms (stream)
  #j((defun delete-participle (id sub-id font)
       (let* ((link-span (document.get-element-by-id (+ "link-span" sub-id)))
	      (req (new (XMLHttpRequest))))
	 (req.open "get" (+ "js/delete-nonfinite.xml?id="
			    id "&sub-id=" sub-id "&font=" font)
		   false)
	 (req.send "")
	 (setf link-span.outerHTML req.responseText)))
     
     (defun delete-paradigm (id sub-id font)
       (let* ((link-span (document.get-element-by-id (+ "link-span" sub-id)))
	      (req (new (XMLHttpRequest))))
	 (req.open "get" (+ "js/delete-paradigm.xml?id="
			    id "&sub-id=" sub-id "&font=" font)
		   false)
	 (req.send "")
	 (setf link-span.outerHTML req.responseText)))
     
     (defun add-participle-link (id sub-id font)
       ;;(alert (+ id " - " sub-id))
       (let* ((link-span (document.get-element-by-id (+ "link-span" sub-id)))
	      (link-sub-id (document.get-element-by-id (+ "participle-link-id" sub-id)))
	      (req (new (XMLHttpRequest))))
	 (req.open "get" (+ #L(concatenate 'string "/" *url-base* "/js/add-participle-link.xml?id=")
			    id "&sub-id=" sub-id "&link-sub-id=" link-sub-id.value "&font=" font)
		   false)
	 (req.send "")
	 (setf link-span.outerHTML req.responseText)
	 ))
     
     (defun remove-participle-link (id sub-id font)
       ;;(alert (+ id " - " sub-id))
       (let* ((link-span (document.get-element-by-id (+ "link-span" sub-id)))
	      (req (new (XMLHttpRequest))))
	 (req.open "get" (+ #L(concatenate 'string "/" *url-base* "/js/remove-participle-link.xml?id=")
			    id "&sub-id=" sub-id "&font=" font)
		   false)
	 (req.send "")
	 (setf link-span.outerHTML req.responseText)))
          
     (defun add-morphosyntactic-link (id sub-id font)
       ;;(alert (+ id " - " sub-id " - " font))
       (let* ((link-span (document.get-element-by-id (+ "link-span" sub-id)))
	      (link-sub-id (document.get-element-by-id (+ "link-id" sub-id)))
	      (req (new (XMLHttpRequest))))
	 (req.open "get" (+ "js/add-morphosyntactic-link.xml?id="
			    id "&sub-id=" sub-id "&link-sub-id=" link-sub-id.value "&font=" font)
		   false)
	 (req.send "")
	 (setf link-span.outerHTML req.responseText)
	 ))
     
     (defun remove-morphosyntactic-link (id sub-id font)
       ;;(alert (+ id " - " sub-id))
       (let* ((link-span (document.get-element-by-id (+ "link-span" sub-id)))
	      (req (new (XMLHttpRequest))))
	 (req.open "get" (+ "js/remove-morphosyntactic-link.xml?id="
			    id "&sub-id=" sub-id "&font=" font)
		   false)
	 (req.send "")
	 (setf link-span.outerHTML req.responseText)
	 ))
     
     (defun handle-key-down (event)
       (when event.ctrlKey
	 (cond ((= event.keyCode 37) ;; arrow left
		(let ((page-form (document.get-element-by-id "previousNextForm"))
		      (page-input (document.get-element-by-id "previousRoot")))
		  (setf page-input.value "true")
		  (page-form.submit)))
	       ((= event.keyCode 39) ;; arrow right
		(let ((page-form (document.get-element-by-id "previousNextForm"))
		      (page-input (document.get-element-by-id "nextRoot")))
		  (setf page-input.value "true")
		  (page-form.submit))))
	 (setf event.return-value false)))
     
     (defun entry-drag-start (subid)
       (event.data-transfer.set-data "text" subid)
       )

     (defun entry-drag-enter ()
       (setf event.return-value false))
       
     (defun entry-drag-over ()
       (setf event.return-value false))
     
     (defun entry-drop (id subid)
       (setf drag-subid.value (event.data-transfer.get-data "text"))
       (setf drop-subid.value subid)
       ;;(alert (+ drag-subid.value " -> " drop-subid.value))
       
       (let* ((to-span (document.get-element-by-id (+ "link-span" subid)))
	      ;;(link-sub-id (document.get-element-by-id (+ "link-id" subid)))
	      (req (new (XMLHttpRequest))))
	 (req.open "get" (+ "js/copy-participles.xml?id=" id
			    "&from-subid=" (event.data-transfer.get-data "text")
			    "&to-subid=" subid
			    "&font=amirani")
		   false)
	 (req.send "")
	 (setf to-span.outerHTML req.responseText)
	 ))
     ))

(define-url-function delete-nonfinite-xml
    (request (id sub-id font)
	     :xsl #'paradigm-cell-xsl
	     :force-xslt :sablotron
	     :write-doctype-p nil
	     :path (concatenate 'string "/" *url-base* "/js/delete-nonfinite.xml"))
  (with-database-connection ()
    (delete-nonfinite-forms id sub-id)
    ;;(print (list id sub-id font))
    (paradigm-cell-xml (fst::paradigm-classes (parse-integer id) :sub-id (parse-integer sub-id)) stream :font font)))

(define-url-function delete-paradigm-xml
    (request ((id integer)
	      (sub-id integer)
	      font)
	     :xsl #'paradigm-cell-xsl
	     :force-xslt :sablotron
	     :write-doctype-p nil
	     :path (concatenate 'string "/" *url-base* "/js/delete-paradigm.xml"))
  (with-database-connection ()
    (with-transaction ()
      (let ((roots (select [root]
			   :distinct t :flatp t :from [morph verb-features]
			   :where [and [= [id] id] [= [sub-id] sub-id]])))
	(dolist (root roots)
	  (remhash root *feature-cache*)))
      (delete-records :from [morph verb-features] :where [and [= [id] id] [= [sub-id] sub-id]])
      (delete-records :from [morph verb-translation] :where [and [= [id] id] [= [sub-id] sub-id]])
      (delete-records :from [morph xle-template] :where [and [= [id] id] [= [sub-id] sub-id]])
      (let ((pcl (fst::paradigm-classes id :sub-id sub-id)))
	(when (cdr pcl)
	  (paradigm-cell-xml pcl stream :font font))))))

#+test
(print (fst::paradigm-classes 3803 :sub-id 11))

(define-url-function copy-participles-xml
    (request ((id integer)
	      (from-subid integer)
	      (to-subid integer) font)
	     :xsl #'paradigm-cell-xsl
	     :force-xslt :sablotron
	     :write-doctype-p nil
	     :path (concatenate 'string "/" *url-base* "/js/copy-participles.xml"))
  (with-database-connection ()
    (when (and from-subid to-subid)
      (copy-participles id :from-subid from-subid :to-subid to-subid))
    (paradigm-cell-xml (fst::paradigm-classes id :sub-id to-subid) stream :font font)))

(define-url-function add-morphosyntactic-link-xml
    (request (id sub-id link-sub-id font)
	     :xsl #'paradigm-cell-xsl
	     :force-xslt :sablotron
	     :write-doctype-p nil
	     :path (concatenate 'string "/" *url-base* "/js/add-morphosyntactic-link.xml"))
  (with-database-connection ()
    (add-morphosyntactic-link id sub-id link-sub-id)
    (print (list :add-morphosyntactic-link id sub-id link-sub-id font))
    (paradigm-cell-xml (fst::paradigm-classes (parse-integer id) :sub-id (parse-integer sub-id)) stream :font font)))

(define-url-function remove-morphosyntactic-link-xml
    (request (id sub-id font)
	     :xsl #'paradigm-cell-xsl
	     :force-xslt :sablotron
	     :write-doctype-p nil
	     :path (concatenate 'string "/" *url-base* "/js/remove-morphosyntactic-link.xml"))
  (with-database-connection ()
    (remove-morphosyntactic-link id sub-id)
    (print (list id sub-id font))
    (paradigm-cell-xml (fst::paradigm-classes (parse-integer id) :sub-id (parse-integer sub-id)) stream :font font)))

#+old
(defmethod remove-participle-link-xml ((request http-request) entity)
  #+debug(print (request-query request))
  (with-database-connection ()
    (with-html-response (request entity stream (id sub-id) :write-html-header-p nil)
      (remove-participle-link id sub-id))))

(define-url-function remove-participle-link-xml
    (request (id sub-id font)
	     :xsl #'paradigm-cell-xsl
	     :force-xslt :sablotron
	     :write-doctype-p nil
	     :path (concatenate 'string "/" *url-base* "/js/remove-participle-link.xml"))
  (with-database-connection ()
    (remove-participle-link id sub-id)
    (print (list id sub-id font))
    (paradigm-cell-xml (fst::paradigm-classes (parse-integer id) :sub-id (parse-integer sub-id)) stream :font font)))

(define-url-function add-participle-link-xml
    (request (id sub-id link-sub-id font)
	     :xsl #'paradigm-cell-xsl
	     :force-xslt :sablotron
	     :write-doctype-p nil
	     :path (concatenate 'string "/" *url-base* "/js/add-participle-link.xml"))
  (with-database-connection ()
    (add-participle-link id sub-id link-sub-id)
    (print (list id sub-id font))
    (paradigm-cell-xml (fst::paradigm-classes (parse-integer id) :sub-id (parse-integer sub-id)) stream :font font)))

;;(print (fst::paradigm-classes 8 :sub-id 5))

(defun id-paradigm-ids-pvs (id)
  (select [id] [sub-id] [impf-pv] [pf-pv]
	  :from [morph verb-paradigm]
	  :where [= [id] ?id]
	  :order-by '([sub-id] [impf-pv] [pf-pv])))


#+test
(print (select [count-distinct [c-root]]
	       :from [morph verb-paradigm]))

#+test
(print (select [c-root] :distinct t
	       :flatp t
	       :from [morph verb-paradigm]))

#+test
(print (car (select [id] :from [morph verb-paradigm]
		    :where [= [c-root] "დგ1"]
		    :flatp t :limit 1)))

#+test
(update-recordsx [morph verb-paradigm]
		:av-pairs '(([c-root] "რეგულირ"))
		:where [= [id] 3894])

(define-url-function masdars-xml
    (request (c-root
	      (id integer)
	      (lang keyword)
	      go-root go-id
	      previous next
	      update accept copy delete copy-full-paradigm new-root
	      show-participles)
	     :xsl #'masdars-xsl
	     :force-xslt :sablotron
	     :path (concat "/" *url-base* "/masdars"))
  (setf show-participles t)
  (ccl::with-lock-grabbed (*parse-lock*)
    (with-database-connection ()
      (unless (or id go-root) (setf id 1))
      (unless lang (setf lang :ng))
      (cond (go-root (setf id (or (car (select [id] :from [morph verb-paradigm]
					       :where [= [c-root] ?c-root]
					       :flatp t :limit 1))
				  id
				  0)))
	    (previous (decf id))
	    (next (incf id)))
      (cond (update
	     (with-transaction ()
	       (let ((changed-features (loop for pair in (request-query request)
					  when (find #\@ (car pair))
					  collect (cons (encoding:utf-8-decode (car pair))
							(encoding:utf-8-decode (cdr pair))))))
		 (loop for (f-id . value) in changed-features
		    do (destructuring-bind (id+sub-id+pvs feature) (split f-id #\@)
			 (destructuring-bind (id-subid impf-pv pf-pv) (split id+sub-id+pvs #\$)
			   (remhash id-subid *paradigm-cache*)
			   (destructuring-bind (id sub-id) (split id-subid #\-)
			     (setf feature (intern (string-upcase feature) :keyword))
			     (print (list id sub-id impf-pv pf-pv feature value))
			     (remhash (debug (list (parse-integer id) (parse-integer sub-id) impf-pv pf-pv))
				      *paradigm-class-cache*)
			     (when (and (equal value "-")
					(not (find feature '(:impf-pv :pf-pv))))
			       (setf value nil))
			     (update-records [morph verb-paradigm]
					     :av-pairs `((,(sql-expression :attribute feature) ,value))
					     :where [and [= [id] ?id]
							 [= [sub-id] ?sub-id]
							 [= [impf-pv] ?impf-pv]
							 [= [pf-pv] ?pf-pv]])))))
		 (cond (copy
			(loop for id+sub-id+pvs in (debug (ensure-list copy))
			   do (destructuring-bind (id-subid impf-pv pf-pv) (split id+sub-id+pvs #\$)
				(destructuring-bind (id sub-id) (mapcar #'parse-integer (split id-subid #\-))
				  (let ((new-sub-id (1+ (car (select [max [sub-id]] :flatp t
								     :from [morph verb-paradigm]
								     :where [= [id] ?id]))))
					(vpar (car (select 'verb-paradigm :refresh t :flatp t
							   :where [and [= [id] ?id]
								       [= [sub-id] ?sub-id]
								       [= [impf-pv] ?impf-pv]
								       [= [pf-pv] ?pf-pv]])))
					(participles
					 (select 'verb-participle :refresh t :flatp t
						 :where [and [= [id] ?id]
							     [= [sub-id] ?sub-id]
							     [or [null [wrong]]
								 [= [wrong] :false]]]))
					(templates
					 (select [template] :flatp t
						 :from [morph xle-template]
						 :where [and [= [id] ?id] [= [sub-id] ?sub-id]]))
					(now (get-universal-time)))
				    (make-instance 'verb-paradigm
						   :id id 
						   :sub-id new-sub-id
						   :c-root (c-root vpar)
						   :vn (vn vpar)
						   :impf-vn (impf-vn vpar)
						   :pf-vn (pf-vn vpar)
						   :tsch-class (tsch-class vpar)
						   :class (verb-class vpar)
						   :features-sub-id (or (features-sub-id vpar) (sub-id vpar))
						   :link-sub-id (link-sub-id vpar)
						   :base-sub-id new-sub-id ;; (base-sub-id vpar)
						   :participle-sub-id (participle-sub-id vpar)
						   ;; :pv (pv vpar)
						   :pf-pv (pf-pv vpar)
						   :impf-pv (impf-pv vpar)
						   :date now)
				    (dolist (part participles)
				      (make-instance 'verb-participle
						     :id id
						     :sub-id new-sub-id
						     :type (participle-type part)
						     :stem (participle-stem part)
						     :root (participle-root part)
						     :code (conjugation-code part)
						     :variety (variety part)
						     :aspect (aspect part)
						     :date now))
				    (insert-records
				     :into [morph verb-translation]
				     :attributes (list [id] [sub-id] [link-sub-id] [base-sub-id]
						       [pv] [translation])
				     :values (list id new-sub-id nil (sub-id vpar) "-" nil))
				    (dolist (template templates)
				      (insert-records
				       :into [morph xle-template]
				       :attributes (list [id] [sub-id] [template])
				       :values (list id new-sub-id template)))
				    (update-paradigm-table :id id))))))
		       (copy-full-paradigm
			(with-transaction ()
			  (copy-full-paradigm id new-root))) 
		       (delete
			(loop for id+sub-id+pvs in (ensure-list delete)
			   do (destructuring-bind (id-subid impf-pv pf-pv) (split id+sub-id+pvs #\$)
				(destructuring-bind (id sub-id) (split id-subid #\-)
				  (print (list :delete id sub-id impf-pv pf-pv))
				  (delete-records :from [morph verb-paradigm]
						  :where [and [= [id] ?id]
							      [= [sub-id] ?sub-id]
							      [= [impf-pv] ?impf-pv]
							      [= [pf-pv] ?pf-pv]])))))))))
	    (accept
	     (with-transaction ()
	       (update-records [morph verb-paradigm]
			       :av-pairs `(([accepted] t))
			       :where [= [id] ?id]))))
      (let* ((parser::*dg-vector* (make-array 0 :fill-pointer t :adjustable t))
	     (parser::*body-vector* (make-array 0 :fill-pointer t :adjustable t))
	     (parser::*flag-vector* (make-array 0 :fill-pointer t :adjustable t))
	     (parser::*dg-count-vector* (make-array 0 :fill-pointer t :adjustable t))
	     (parser::*dg-count* 0)
	     (*package* (find-package :fst))
	     (pfeatures-table (make-hash-table :test #'equal))
	     (vn-pv-table (make-hash-table :test #'equal))
	     
	     (id-paradigm-ids (when id (id-paradigm-ids-pvs id)))
	     ;;(classes ())
	     (id-paradigm-forms
	      (mapcar
	       (lambda (paradigm)
		 (or ;;(gethash paradigm *masdar-paradigm-cache*)
		     (setf (gethash paradigm *masdar-paradigm-cache*)
			   (destructuring-bind (id sub-id impf-pv pf-pv) paradigm
			     (mapcar
			      (lambda (tense)
				#-debug(print (list :tense tense))
				(multiple-value-bind (forms xle-templates class)
				    (fst::generate-paradigm (format nil "~d-~d" id sub-id)
							    :allp nil
							    :tense tense
							    :num 'fst::sg
							    :pers 1
							    :obj3sg-only-p t
							    :standard-only-p nil
							    :lang lang
							    :printp nil
							    :preverbless-aorists nil)
				  (unless forms
				    (multiple-value-setq (forms xle-templates class)
				      (fst::generate-paradigm (format nil "~d-~d" id sub-id)
							      :allp nil
							      :tense tense
							      :num 'fst::sg
							      :pers 3
							      :obj3sg-only-p t
							      :standard-only-p nil
							      :lang lang
							      :printp nil
							      :preverbless-aorists nil)))
				  #+debug(print (list forms xle-templates class))
				  #+ignore
				  (when (eq tense 'fst::present)
				    (setf (gethash paradigm *paradigm-class-cache*) class)
				    (push class classes))
				  (if (find tense '(fst::aorist fst::present))
				      (caar (fst::merge-alternative-forms forms))
				      (mapcar #'car forms))))
			      `(fst::present
				#+ignore fst::future
				fst::aorist #+ignore fst::perfect
				,@(when nil ;; show-participles
					'(fst::past-part fst::present-part fst::future-part fst::negative-part)
					)))))))
	       id-paradigm-ids))
	     (participles (select 'verb-participle
				  :flatp t :refresh t
				  :where [and [= [id] ?id]
					      [or [null [wrong]]
						  [= [wrong] :false]]]
				  :order-by '([sub-id] [type])))
	     (verbal-nouns (make-hash-table)))
	(loop for part in participles
	   when (and (eq (participle-type part) :masdar)
		     (main-form part))
	   do (push part (gethash (sub-id part) verbal-nouns)))
	(destructuring-bind (&optional c-root accepted)
	    (car (select [c-root] [accepted] :from [morph verb-paradigm]
			 :where [= [id] ?id]
			 :limit 1))
	  (do-query ((sub-id features-sub-id participle-sub-id base-sub-id
			     tsch-class class vn impf-vn pf-vn ;; pv
			     pf-pv pf-12-pv
			     impf-pv red-dir-pv no-preverbless-aor disabled)
		     [select [sub-id] [features-sub-id] [participle-sub-id] [base-sub-id]
			     [tsch-class] [class] [vn] [impf-vn] [pf-vn]
			     ;; [pv]
			     [pf-pv] [pf-12-pv] [impf-pv] [red-dir-pv]
			     [no-preverbless-aor] [disabled]
			     :from [morph verb-paradigm]
			     :where [= [id] ?id]])
	    (setf (gethash (list id sub-id impf-pv pf-pv) pfeatures-table)
		  (list features-sub-id participle-sub-id base-sub-id
			tsch-class class vn impf-vn pf-vn ;; pv
			pf-pv pf-12-pv impf-pv red-dir-pv 
			no-preverbless-aor disabled))
	    (when (equal impf-pv "-")
	      (pushnew pf-pv (gethash vn vn-pv-table) :test #'equal)))
	  #m((paradigms :c-root #s c-root
			:id #s id
			:accepted #s accepted
			:show-participles #s(when show-participles t))
	     (langs
	      #L(loop for (s name)
		   on '(:ng "Modern Georgian"
			:og "Old Georgian"
			:xanmeti "Xanmeti"
			:haemeti "Haemeti")
		   by #'cddr
		   do #m(lang/ :value #s s :name #s name :selected #s (when (eq s lang) t))))
	     ((root-paradigms :count #s(length id-paradigm-ids))
	      #L(loop for paradigm-id in id-paradigm-ids
		   for forms in id-paradigm-forms
		   ;;for class = (gethash paradigm-id *paradigm-class-cache*)
		   when forms
		   do ;;(destructuring-bind (paradigm-id) paradigm-id
		     (destructuring-bind (pres #+ignore fut aor #+ignore perf
					       &optional past-part present-part future-part negative-part)
			 forms
		       (destructuring-bind (features-sub-id participle-sub-id base-sub-id
							    tsch-class class vn impf-vn pf-vn ;; pv
							    pf-pv pf-12-pv impf-pv red-dir-pv
							    no-preverbless-aor disabled)
			   (gethash paradigm-id pfeatures-table)
			 (let (#+orig
			       (real-impf-vn (or impf-vn
						 (let ((pv-list (gethash vn vn-pv-table)))
						   (cond ((not (equal impf-pv "-"))
							  (u:concat impf-pv vn))
							 ((and (null (cdr pv-list))
							       (equal pf-pv "-"))
							  vn)
							 ((fullform-count vn) ;; impf form is attested
							  vn)
							 (t
							  (u:concat "*" vn))))))
			       #+orig
			       (real-pf-vn (or pf-vn
					       (if (equal pf-pv "-")
						   vn
						   (u:concat pf-pv vn)))))
			   (destructuring-bind (id sub-id &rest rest) paradigm-id
			     (declare (ignore rest))
			     #m((root-paradigm :id #s (format nil "~d-~d" id sub-id)
					       :show-participles #s(when show-participles t)
					       :features-sub-id #s features-sub-id
					       :participle-sub-id #s participle-sub-id
					       :base-sub-id #s base-sub-id
					       :pres #s (or pres "-")
					       :pres-count #s(fullform-count (or pres "-"))
					       :aor #s (or aor "-")
					       :aor-count #s(fullform-count (or aor "-"))
					       :vn #s vn
					       :impf-vn #s (or impf-vn "-")
					       :pf-vn #s (or pf-vn "-")
					       ;;:real-impf-vn #s real-impf-vn
					       ;;:real-impf-vn-count #s(fullform-count (string-trim "*" real-impf-vn))
					       ;;:real-pf-vn #s real-pf-vn
					       ;;:real-pf-vn-count #s(fullform-count (string-trim "*" real-pf-vn))
					       ;;:pv #s pv
					       :impf-pv #s impf-pv
					       :pf-pv #s pf-pv
					       :pf-12-pv #s (or pf-12-pv "-")
					       :red-dir-pv #s (or red-dir-pv "-")
					       :no-preverbless-aor #s no-preverbless-aor
					       :disabled #s disabled
					       :class #s class
					       :tsch-class #s tsch-class)
				#L(dolist (ptype '("masdar" "past-part" "present-part" "future-part" "negative-part"))
				    ;;(debug ptype)
				    #m((participle :type #s ptype)
				       #L(when (= sub-id features-sub-id)
					   (loop for part in participles
					      when (and (eq (sub-id part) sub-id)
							(string-equal (participle-type part) ptype))
					      do #m(form/ :count #s(stem-fullform-count (participle-stem part))
							  :stem #s (participle-stem part)
							  :root #s (or (participle-root part) '-)
							  :aspect #s (aspect part)
							  :main-form #s (main-form part)
							  :variety #s (variety part)))))))))))))))))))

(defun stem-fullform-count (stem)
  (when stem
    (if (find (last-char stem) "აეიოუჱჲჳ")
	(fullform-count stem)
	(fullform-count (u:concat stem "ი")))))

(publish-directory :prefix "/kartuli/javascript"
		   :destination (namestring (translate-logical-pathname #p"projects:framework;javascript;")))

(defstylesheet masdars-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")
     ((xsl:template :match "/paradigms")
      ((html)
       (head
        (title "Georgian Verb Paradigm"
	       ((xsl:if :test "@user")
		" [" (xsl:value-of/ :select "@user") "]"))
	(script/ :src "javascript/jquery-1.9.1.min.js"
                 :type "text/javascript"
                 :language "javascript") 
	((script :type "text/javascript" :language "javascript")
	 (!CDATA #L(js/masdar stream)))
	((style :type "text/css")
	 (!CDATA
	  (CSS-STYLE
	      (div :margin "16"
		 :color #-BW "#004499" #+BW "black"
		 :font-family "Verdana, Tahoma, MS Sans Serif, Arial, Geneva, Helvetica")
	    (div.title :font-size "18" :font-weight "bold" :text-align "center")
	    (div.link :font-size "12") ;; :font-weight "bold" :text-align "center")
	    (a :text-decoration "none" :color #-BW "#004499" #+BW "black")
	    (a\:hover :text-decoration "underline")
	    (span.feature-title\:hover :text-decoration "underline")
	    (td.chosen :color "red")
	    (div.label :font-size "12")
	    (div.text :font-size "8pt" :color "black" :margin-left "2px" :margin-bottom "2px")
	    (div.error :font-size "8pt" :color "red" :margin-left "2px" :margin-bottom "2px")
	    (span.error :font-size "10pt" :color "red" :font-weight "normal")
	    (td.heading :font-weight "bold")
	    (td.value :background "white")
	    (td.pfeature :font-weight "bold")
	    (td.value-hilite :background "lightgray" :cursor "hand"))))
	((script :type "text/javascript" :language "javascript")
	 (!CDATA #L(js/georgian-morph stream))))
       ((body :style "font-family: Verdana, Tahoma, MS Sans Serif, Arial, Geneva, Helvetica")
	((div :class "title") "Georgian Verb Paradigm")
	((xsl:element :name "form")
	 ((xsl:attribute :name "method") "post")
	 ((xsl:attribute :name "name") "form")
	 ((xsl:attribute :name "action") "masdars")
	 ((xsl:element :name "input")
	  ((xsl:attribute :name "type") "hidden")
	  ((xsl:attribute :name "name") "id-type")
	  ((xsl:attribute :name "id") "id-type")
	  ((xsl:attribute :name "value") (xsl:value-of/ :select "@id-type")))
	 (div
	  "Show paradigm for root: "
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "text")
	   ((xsl:attribute :name "name") "c-root")
	   ((xsl:attribute :name "style")"font-size: 12pt")
	   ((xsl:attribute :name "value")
	    (xsl:value-of/ :select "@c-root")))
	  (input/ :type "submit" :name "go-root" :value "Go")
	  " or id: "
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "text")
	   ((xsl:attribute :name "name") "id")
	   ((xsl:attribute :name "style") "font-size: 12pt")
	   ((xsl:attribute :name "value")
	    (xsl:value-of/ :select "@id")))
	  (xsl:text " ")
	  (input/ :type "submit" :name "go-id" :value "Go")
	  (input/ :type "submit" :name "previous" :value "Prev")
	  (input/ :type "submit" :name "next" :value "Next")
	  ((input :type "hidden" :name "next")
	   ((xsl:attribute :name "id") "next"))
	  ((input :type "hidden" :name "previous")
	   ((xsl:attribute :name "id") "previous"))
	  
	  ((input :type "hidden" :name "update")
	   ((xsl:attribute :name "id") "update"))
	  ((input :type "hidden" :name "accept")
	   ((xsl:attribute :name "id") "accept"))
	   
	  " | lang: "
	  ((xsl:element :name "select")
	   ((xsl:attribute :name "name") "lang")
	   ((xsl:attribute :name "onchange") "submit()")
	   (xsl:apply-templates/ :select "langs"))
	  " | "
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "checkbox")
	   ((xsl:attribute :name "name") "show-participles")
	   ((xsl:if :test "@counts")
	    ((xsl:attribute :name "checked") "yes")))
	  "include participles")
	 
	 (div "Root: "
	      (span
	       (xsl:choose
		((xsl:when :test "@accepted")
		 ((xsl:attribute :name "style") "font-weight: bold; color: green; border: 2px solid red"))
		(xsl:otherwise
		 ((xsl:attribute :name "style") "font-weight: bold; color: black")))
	       (xsl:value-of/ :select "@c-root"))
	      ;;(xsl:text " | ")
              #+disabled
	      ((xsl:element :name "a")
	       ((xsl:attribute :name "href")
		"georgian-root-paradigms.xml?"
		"id=" (xsl:value-of/ :select "/paradigms/@id")
		;; "&amp;font=" (xsl:value-of/ :select "/paradigms/@font")
		)
	       "all paradigms ")
	      ;; " (" (xsl:value-of/ :select "root-paradigms/@count") ")"
	      " | "
	      ((xsl:element :name "a")
	       ((xsl:attribute :name "href")
		"roots")
	       "all roots"))
	 (div (input/ :type "submit" :name "update" :value "Update" :title "Store changes")
	      (input/ :type "submit" :name "accept" :value "Accept" :title "Confirm the correctness of the values"))
	 ((table :style "padding: 1em")
	  (tr ((td :class "heading") "Id")
	      ((td :class "heading" :title "Feature sub-id") "FSid")
	      ((td :class "heading" :title "participle sub-id, not used in GNC?") "PSid")
	      ((td :class "heading" :title "base sub-id, for frame and alternation") "BSid")
	      ((td :class "heading") "Cp/Del")
	      ((td :class "heading") "Dis")
	      ((td :class "heading") "Tsch")
	      ((td :class "heading") "Class")
	      ((td :class "heading") "Present")
	      ((td :class "heading") "Aorist")
	      ((td :class "heading") (nobr "ImpfPv"))
	      ((td :class "heading") (nobr "PfPv"))
	      ((td :class "heading") (nobr "Pf12Pv"))
	      ((td :class "heading") (nobr "RedDirPv"))
	      ((td :class "heading") "Vn")
	      ((xsl:if :test "@show-participles")
	       ((td :class "heading") "PastPart")
	       ((td :class "heading") "PresPart")
	       ((td :class "heading") "FutPart")
	       ((td :class "heading") "NegPart"))
	      ((td :class "heading") (nobr "NoPvlessAor")))
	  (xsl:apply-templates/ :select "root-paradigms/root-paradigm" :mode "all"))
	 (xsl:apply-templates/ :select "reading" :mode "paradigm")))))
     
     ((xsl:template :match "lang")
      ((xsl:element :name "option")
       ((xsl:attribute :name "value") (xsl:value-of/ :select "@value"))
       ((xsl:if :test "@selected")
	((xsl:attribute :name "selected") "true"))
       (xsl:value-of/ :select "@name")))
     
     ((xsl:template :match "reading" :mode "all")
      ((xsl:element :name "option")
       ;;((xsl:attribute :name "style") "font-family: Amirani, Verdana; font-size: 14pt")
       ((xsl:attribute :name "value") (xsl:value-of/ :select "@id"))
       ((xsl:if :test "@chosen")
	((xsl:attribute :name "selected") "true"))
       (xsl:value-of/ :select "@id")
       (xsl:text " ")
       (xsl:value-of/ :select "@gv")
       (xsl:text " ")
       (xsl:value-of/ :select "@tsch-class")))

     ((xsl:template :match "root-paradigm" :mode "all")
      (tr (td
	   (nobr ((xsl:element :name "a")
		  ((xsl:attribute :name "href")
		   "paradigm?"
		   "pid=" (xsl:value-of/ :select "@id")
		   "&amp;id-type=pid"
		   "&amp;standard-only=yes"
		   "&amp;lang=ng")
		  (xsl:value-of/ :select "@id"))))
	  (td
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-features-sub-id")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','features-sub-id','"
	    (xsl:value-of/ :select "@features-sub-id") "',true)")
	   ((xsl:attribute :name "onmouseover")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','features-sub-id','"
	    (xsl:value-of/ :select "@features-sub-id") "',false)")
	   (xsl:value-of/ :select "@features-sub-id"))
	  (td
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-participle-sub-id")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','participle-sub-id','"
	    (xsl:value-of/ :select "@participle-sub-id") "',true)")
	   (xsl:value-of/ :select "@participle-sub-id"))
	  (td
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-base-sub-id")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','base-sub-id','"
	    (xsl:value-of/ :select "@base-sub-id") "',true)")
	   (xsl:value-of/ :select "@base-sub-id"))
	  (td
	   ((xsl:element :name "input")
	    ((xsl:attribute :name "type") "checkbox")
	    ((xsl:attribute :name "name") "copy")
	    ((xsl:attribute :name "value")
	     (xsl:value-of/ :select "@id") "$"
	     (xsl:value-of/ :select "@impf-pv") "$"
	     (xsl:value-of/ :select "@pf-pv")))
	   ((xsl:if :test "@id = preceding-sibling::*/@id or @id = following-sibling::*/@id")
	    ((xsl:element :name "input")
	     ((xsl:attribute :name "type") "checkbox")
	     ((xsl:attribute :name "name") "delete")
	     ((xsl:attribute :name "value")
	      (xsl:value-of/ :select "@id") "$"
	      (xsl:value-of/ :select "@impf-pv") "$"
	      (xsl:value-of/ :select "@pf-pv")))))
	  (td 
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-disabled")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','disabled','"
	    (xsl:value-of/ :select "@red-dir-pv") "',true)")
	   (xsl:value-of/ :select "@disabled"))
	  #+ignore
	  (td
	   ((xsl:element :name "input")
	    ((xsl:attribute :name "type") "checkbox")
	    ((xsl:attribute :name "name") "disabled")
	    ((xsl:attribute :name "onclick")
	     "editVn('"
	     (xsl:value-of/ :select "@id") "$"
	     (xsl:value-of/ :select "@impf-pv") "$"
	     (xsl:value-of/ :select "@pf-pv") "','disabled','"
	     (xsl:value-of/ :select "@tsch-class") "',true)")
	    ((xsl:attribute :name "value")
	     (xsl:value-of/ :select "@id") "$"
	     (xsl:value-of/ :select "@impf-pv") "$"
	     (xsl:value-of/ :select "@pf-pv"))))
	  
	  (td 
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-tsch-class")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','tsch-class','"
	    (xsl:value-of/ :select "@tsch-class") "',true)")
	   (nobr (xsl:value-of/ :select "@tsch-class")))
	  ;; Class
	  (td 
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-class")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','class','"
	    (xsl:value-of/ :select "@class") "',true)")
	   (nobr (xsl:value-of/ :select "@class")))
	  #+orig
	  (td (xsl:value-of/ :select "@class"))
	  (td (nobr (xsl:value-of/ :select "@pres")
		    ((xsl:if :test "@pres-count")
		     ((span :style "font-size: 8pt; color: blue")
		      " (" (xsl:value-of/ :select "@pres-count") ")"))))
	  (td (nobr (xsl:value-of/ :select "@aor")
		    ((xsl:if :test "@aor-count")
		     ((span :style "font-size: 8pt; color: blue")
		      " (" (xsl:value-of/ :select "@aor-count") ")"))))
	  
	  (td 
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-impf-pv")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','impf-pv','"
	    (xsl:value-of/ :select "@impf-pv") "',true)")
	   (xsl:value-of/ :select "@impf-pv"))
	  
	  (td 
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-pf-pv")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','pf-pv','"
	    (xsl:value-of/ :select "@pf-pv") "',true)")
	   (xsl:value-of/ :select "@pf-pv"))
	  
	  ;; (td (xsl:value-of/ :select "@pf-12-pv"))
	  (td 
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-pf-12-pv")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','pf-12-pv','"
	    (xsl:value-of/ :select "@pf-12-pv") "',true)")
	   (xsl:value-of/ :select "@pf-12-pv"))

	  (td 
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-red-dir-pv")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','red-dir-pv','"
	    (xsl:value-of/ :select "@red-dir-pv") "',true)")
	   (xsl:value-of/ :select "@red-dir-pv"))

	  (xsl:apply-templates/ :select "participle[@type='masdar']")
	  ((xsl:if :test "@show-participles")
	   (xsl:apply-templates/ :select "participle[@type='past-part']")
	   (xsl:apply-templates/ :select "participle[@type='present-part']")
	   (xsl:apply-templates/ :select "participle[@type='future-part']")
	   (xsl:apply-templates/ :select "participle[@type='negative-part']"))
	  (td 
	   ((xsl:attribute :name "style") "cursor: hand")
	   ((xsl:attribute :name "id")
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "-no-preverbless-aor")
	   ((xsl:attribute :name "onclick")
	    "editVn('"
	    (xsl:value-of/ :select "@id") "$"
	    (xsl:value-of/ :select "@impf-pv") "$"
	    (xsl:value-of/ :select "@pf-pv") "','no-preverbless-aor','"
	    (xsl:value-of/ :select "@no-preverbless-aor") "',true)")
	   (xsl:value-of/ :select "@no-preverbless-aor"))
	  
	  ;;(td (xsl:value-of/ :select "@red-dir-pv"))
	  ))

     ((xsl:template :match "participle")
      ((td :style "cursor: hand")
       ((xsl:attribute :name "id")
	(xsl:value-of/ :select "../@id") "$"
	(xsl:value-of/ :select "../@impf-pv") "$"
	(xsl:value-of/ :select "../@pf-pv") "-" (xsl:value-of/ :select "@type"))
       ((xsl:attribute :name "onclick")
	"editParticiple('"
	(xsl:value-of/ :select "../@id") "$"
	(xsl:value-of/ :select "../@impf-pv") "$"
	(xsl:value-of/ :select "../@pf-pv") "','"
	(xsl:apply-templates/ :select "@type") "')")
       (xsl:apply-templates/ :select "form")))
     
     ((xsl:template :match "form")
      (nobr (xsl:value-of/ :select "@stem")
	    ((xsl:if :test "@root and not(@root='-')")
	     "/" (xsl:value-of/ :select "@root"))
	    ((xsl:if :test "@count or @variety")
	     ((span :style "font-size: 8pt; color: blue")
	      ((xsl:if :test "@variety")
	       " [" (xsl:value-of/ :select "@variety") "]")
	      ((xsl:if :test "@count")
	       " (" (xsl:value-of/ :select "@count") ")")))
	    ((xsl:if :test "following-sibling::*") ", ")))
     
     ))

(define-javascript-writer js/masdar (stream)
  #j((defvar replace-value nil)
     
     (defun edit-vn (id-impf-pf-pv feature value click)
       (when (or click event.ctrl-key)
	 (when event.ctrl-key
	   (cond (event.shift-key
		  (setf replace-value value))
		 (replace-value
		  (setf value replace-value))
		 (t
		  (setf replace-value value))))
	 (let* ((td (document.get-element-by-id (+ id-impf-pf-pv "-" feature)))
		(req (new (XMLHttpRequest))))
	   (req.open "get" (+ "js/display-choices.xml?impf-pf-pv=" id-impf-pf-pv
			      "&feature=" feature
			      "&value=" (encodeURIComponent value)
			      )
		     false))
	 (req.send "")
	 (when (not (= req.responseText ""))
	   (setf td.outerHTML req.responseText))))
     
     (defun edit-participle (id-impf-pf-pv type)
       ;;(alert (+ id-impf-pf-pv " " type))
       #+debug(alert (+ uid " - " feature ": " value))
       (let* ((td (document.get-element-by-id (+ id-impf-pf-pv "-" type)))
	      (req (new (XMLHttpRequest))))
	 (req.open "get" (+ "js/display-participles.xml?impf-pf-pv=" id-impf-pf-pv
			    "&type=" type "&mode=edit")
		   false)
	 (req.send "")
	 ;;(alert req.responseText)
	 (when (not (= req.responseText ""))
	   (setf td.outerHTML req.responseText))))
     
     (defun update-participle (id-impf-pf-pv type count mode)
       ;;(alert id-impf-pf-pv)
       ;;(alert (+ id ":" sub-id ":" count))
       (let* ((td (document.get-element-by-id (+ id-impf-pf-pv "-" type)))
	      (req (new (XMLHttpRequest)))
	      (nr 0)
	      (query (+ "?impf-pf-pv=" id-impf-pf-pv
			"&type=" type)))
	 (cond ((= mode "cancel")
		nil)
	       ((= mode "update")
		(for ((< nr count) (incf nr))
		  (let ((del (document.get-element-by-id (+ "delete-participle-" nr)))
			(stem (document.get-element-by-id (+ "participle-stem-" nr)))
			(root (document.get-element-by-id (+ "participle-root-" nr)))
			(code (document.get-element-by-id (+ "participle-code-" nr)))
			(variety (document.get-element-by-id (+ "participle-variety-" nr)))
			(aspect (document.get-element-by-id (+ "participle-aspect-" nr)))
			(main-form (document.get-element-by-id (+ "participle-main-form-" nr))))
		    (cond (del.checked
			   (setf query (+ query "&delete=" (encodeURIComponent stem.value))))
			  (t
			   (when code.value
			     (setf query (+ query "&stem=" (encodeURIComponent stem.value)
					    "&root=" (encodeURIComponent root.value)
					    "&code=" (or code.value "A")
					    "&variety=" (or variety.value "-")
					    "&aspect=" (or aspect.value "-")
					    "&main-form=" main-form.checked)))))))))
	 (let ((stem (document.get-element-by-id (+ "participle-stem-" count)))
	       (root (document.get-element-by-id (+ "participle-root-" count)))
	       (code (document.get-element-by-id (+ "participle-code-" count)))
	       (variety (document.get-element-by-id (+ "participle-variety-" count)))
	       (aspect (document.get-element-by-id (+ "participle-aspect-" count)))
	       (main-form (document.get-element-by-id (+ "participle-main-form-" count))))
	   (when stem.value
	     (setf query (+ query "&stem=" (encodeURIComponent stem.value)
			    "&root=" (encodeURIComponent root.value)
			    "&code=" (or code.value "A")
			    "&variety=" (or variety.value "-")
			    "&aspect=" (or aspect.value "-")
			    "&main-form=" main-form.checked
			    ))))
	 (req.open "get" (+ "js/display-participles.xml" query "&mode=show") false)
	 (req.send "")
	 ;;(alert req.responseText)
	 ;;(alert td.outerHTML)
	 (when (not (= req.responseText ""))
	   (setf td.outerHTML req.responseText))))
       
     (let ((doc ($ document)))
       (doc.keydown handle-key-down1))
     
     (defun handle-key-down1 (event)
       ;;(alert event.keyCode)
       (when event.ctrlKey
	 (cond ((= event.keyCode 37) ;; arrow left
		(let ((prev (document.get-element-by-id "previous")))
		  (setf prev.value "true"))
		(form.submit)
		(event.prevent-default))
	       ((= event.keyCode 39) ;; arrow right
		(let ((next (document.get-element-by-id "next")))
		  (setf next.value "true"))
		(form.submit)
		(event.prevent-default))
	       ((= event.keyCode 65) ;; A: accept
		(let ((accept (document.get-element-by-id "accept")))
		  (setf accept.value "true"))
		(form.submit)
		(event.prevent-default))
	       
	       ((= event.keyCode 83) ;; S: update
		(let ((update (document.get-element-by-id "update")))
		  (setf update.value "true"))
		(form.submit)
		(event.prevent-default))
		 
	       ((= event.keyCode 13) ;; return
		(event.prevent-default)))))))

(define-url-function display-participles-xml
    (request (delete update cancel mode
		     stem root (code keyword)
		     (variety keyword)
		     (aspect keyword)
		     (main-form keyword)
		     impf-pf-pv
		     (type keyword))
	     ;;:uri (eq :get (request-method request))
	     ;;:post (not (eq :get (request-method request)))
	     :xsl #'display-participles-xsl
	     :write-doctype-p nil
	     :path (concat "/" *url-base* "/js/display-participles.xml"))
  ;;(print (request-query request))
  (with-database-connection ()
    (destructuring-bind (id-subid impf-pv pf-pv) (split impf-pf-pv #\$)
      (destructuring-bind (id sub-id) (mapcar #'parse-integer (u:split id-subid #\-))
	(when (or delete stem code)
	  (with-transaction ()
	    (delete-records :from [morph participle]
			    :where [and [= [id] ?id]
					[= [sub-id] ?sub-id]
					[= [type] ?type]])
	    (debug (list stem root code variety aspect main-form))
	    (loop for stem in (ensure-list stem)
	       and root in (ensure-list root)
	       and code in (ensure-list code)
	       and variety in (ensure-list variety)
	       and aspect in (ensure-list aspect)
	       and main-form in (ensure-list main-form)
	       with now = (get-universal-time)
	       do (make-instance 'verb-participle
				 :id id
				 :sub-id sub-id
				 :type type
				 :stem stem
				 :root (unless (eq root :-) root)
				 :code code
				 :variety (unless (eq variety :-) variety)
				 :aspect (unless (eq aspect :-) aspect) 
				 :main-form (when (eq main-form :true) t)
				 :date now))))
	(let ((participles (select 'verb-participle
				   :flatp t :refresh t
				   :where [and [= [id] ?id]
					       [= [sub-id] ?sub-id]
					       [= [type] ?type]
					       [or [null [wrong]]
						   [= [wrong] :false]]]
				   :order-by [stem])))
	  #m((participle :mode #s (or mode "edit")
			 :type #s type
			 :id #s id
			 :sub-id #s sub-id
			 :id-impf-pf-pv #s impf-pf-pv
			 :count #s(length participles))
	     #L(loop for nr from 0
		  for part in participles
		  do #m(form/ :type #s (participle-type part)
			      :code #s (conjugation-code part)
			      :root #s (debug (or (participle-root part) '-))
			      :stem #s (participle-stem part)
			      :variety #s (variety part)
			      :aspect #s (aspect part)
			      :main-form #s (main-form part)
			      :nr #s nr))))))))

(defstylesheet display-participles-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")
     ((xsl:template :match "/participle[@mode='edit']")
      ((td :style "cursor: hand")
       ((xsl:attribute :name "id")
	(xsl:value-of/ :select "@id-impf-pf-pv") "-"
	(xsl:value-of/ :select "@type"))
       ((table :style "border: 1px solid gray")
	(tr
	 (td "Del")
	 (td "Stem")
	 (td "Root")
	 (td "Code")
	 (td "Var")
	 (td "Aspect")
	 (td "Main"))
	(xsl:apply-templates/ :select "form" :mode "edit")
	(tr
	 (td/)
	 (td
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "text")
	   ((xsl:attribute :name "name") "part-stem")
	   ((xsl:attribute :name "id")
	    "participle-stem-" (xsl:value-of/ :select "@count"))
	   ((xsl:attribute :name "style") "font-size: 12pt")
	   ((xsl:attribute :name "value")
	    ""
	    #+ignore
	    (xsl:value-of/ :select "@stem"))))
	 (td
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "text")
	   ((xsl:attribute :name "name") "part-root")
	   ((xsl:attribute :name "id")
	    "participle-root-" (xsl:value-of/ :select "@count"))
	   ((xsl:attribute :name "style") "font-size: 12pt")
	   ((xsl:attribute :name "value") "-")))
	 (td
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "text")
	   ((xsl:attribute :name "name") "part-code")
	   ((xsl:attribute :name "id")
	    "participle-code-" (xsl:value-of/ :select "@count"))
	   ((xsl:attribute :name "style") "font-size: 12pt; width: 2em")
	   ((xsl:attribute :name "value") "")))
	 #+orig
	 (td
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "text")
	   ((xsl:attribute :name "name") "part-variety")
	   ((xsl:attribute :name "id")
	    "participle-variety-" (xsl:value-of/ :select "@count"))
	   ((xsl:attribute :name "style") "font-size: 12pt; width: 2em")
	   ((xsl:attribute :name "value") "")))
	 
	 (td
	  ((xsl:element :name "select")
	   ((xsl:attribute :name "name") "part-variety")
	   ((xsl:attribute :name "id")
	    "participle-variety-" (xsl:value-of/ :select "@count"))
	   ((option :value "-") "-")
	   ((option :value "NG") "NG")
	   ((option :value "OG") "OG")))
	 
	 (td
	  ((xsl:element :name "select")
	   ((xsl:attribute :name "name") "aspect")
	   ((xsl:attribute :name "id")
	    "participle-aspect-" (xsl:value-of/ :select "@count"))
	   ((option :value "-") "-")
	   ((option :value "imperf") "imperf")
	   ((option :value "perf") "perf")))
	 (td
	  ((xsl:element :name "input")
	   ((xsl:attribute :name "type") "checkbox")
	   ((xsl:attribute :name "name") "main-form")
	   ((xsl:attribute :name "id")
	    "participle-main-form-" (xsl:value-of/ :select "@count"))
	   ((xsl:attribute :name "style") "font-size: 12pt; width: 2em"))))
	(tr
	 ((td :colspan "5")
	  ((input :type "button" :value "Update")
	   ((xsl:attribute :name "onclick")
	    "updateParticiple('"
	    (xsl:value-of/ :select "@id-impf-pf-pv")  "','"
	    (xsl:value-of/ :select "@type") "',"
	    (xsl:value-of/ :select "@count")",'update')"))
	  (xsl:text " ")
	  ((input :type "button" :value "Cancel")
	   ((xsl:attribute :name "onclick")
	    "updateParticiple('"
	    (xsl:value-of/ :select "@id-impf-pf-pv")  "','"
	    (xsl:value-of/ :select "@type") "',"
	    (xsl:value-of/ :select "@count")",'cancel')")
	   ))))))
     
     ((xsl:template :match "/participle[@mode='show']")
      ((td :style "cursor: hand")
       ((xsl:attribute :name "id")
	(xsl:value-of/ :select "@id-impf-pf-pv") "-" (xsl:apply-templates/ :select "@type"))
       ((xsl:attribute :name "onclick")
	"editParticiple('"
	(xsl:value-of/ :select "@id-impf-pf-pv") "','"
	(xsl:apply-templates/ :select "@type") "')")
       (xsl:apply-templates/ :select "form" :mode "show")))
     
     ((xsl:template :match "form" :mode "show")
      (nobr (xsl:value-of/ :select "@stem")
	    ((xsl:if :test "@root and not(@root='-')")
	     "/" (xsl:value-of/ :select "@root"))
	    ((xsl:if :test "@count or @variety")
	     ((span :style "font-size: 8pt; color: blue")
	      ((xsl:if :test "@variety")
	       " [" (xsl:value-of/ :select "@variety") "]")
	      ((xsl:if :test "@count")
	       " (" (xsl:value-of/ :select "@count") ")")))
	    ((xsl:if :test "following-sibling::*") ", ")))
     
     ((xsl:template :match "form" :mode "edit")
      (tr
       (td
	((xsl:element :name "input")
	 ((xsl:attribute :name "type") "checkbox")
	 ((xsl:attribute :name "id")
	  "delete-participle-" (xsl:value-of/ :select "@nr"))
	 ;;((xsl:attribute :name "value") "checkbox")
	 ))
       (td
	((xsl:element :name "input")
	 ((xsl:attribute :name "type") "text")
	 ((xsl:attribute :name "name") "part-stem")
	 ((xsl:attribute :name "id")
	  "participle-stem-" (xsl:value-of/ :select "@nr"))
	 ((xsl:attribute :name "style") "font-size: 12pt")
	 ((xsl:attribute :name "value")
	  (xsl:value-of/ :select "@stem"))))
       (td ;; new oct. 2013
	((xsl:element :name "input")
	 ((xsl:attribute :name "type") "text")
	 ((xsl:attribute :name "name") "part-root")
	 ((xsl:attribute :name "id")
	  "participle-root-" (xsl:value-of/ :select "@nr"))
	 ((xsl:attribute :name "style") "font-size: 12pt")
	 ((xsl:attribute :name "value")
	  (xsl:value-of/ :select "@root"))))
       (td
	((xsl:element :name "input")
	 ((xsl:attribute :name "type") "text")
	 ((xsl:attribute :name "name") "part-code")
	 ((xsl:attribute :name "id")
	  "participle-code-" (xsl:value-of/ :select "@nr"))
	 ((xsl:attribute :name "style") "font-size: 12pt; width: 2em")
	 ((xsl:attribute :name "value")
	  (xsl:value-of/ :select "@code"))))
       #+orig
       (td
	((xsl:element :name "input")
	 ((xsl:attribute :name "type") "text")
	 ((xsl:attribute :name "name") "part-variety")
	 ((xsl:attribute :name "id")
	  "participle-variety-" (xsl:value-of/ :select "@nr"))
	 ((xsl:attribute :name "style") "font-size: 12pt; width: 2em")
	 ((xsl:attribute :name "value")
	  (xsl:value-of/ :select "@variety"))))
       
       (td
	((xsl:element :name "select")
	 ((xsl:attribute :name "name") "part-variety")
	 ((xsl:attribute :name "id")
	  "participle-variety-" (xsl:value-of/ :select "@nr"))
	 ((option :value "-")
	  ((xsl:if :test "not(@variety)")
	   ((xsl:attribute :name "selected") "true"))
	  "-")
	 ((option :value "NG")
	  ((xsl:if :test "@variety='ng'")
	   ((xsl:attribute :name "selected") "true"))
	  "NG")
	 ((option :value "OG")
	  ((xsl:if :test "@variety='og'")
	   ((xsl:attribute :name "selected") "true"))
	  "OG")))
       
       (td
	((xsl:element :name "select")
	 ((xsl:attribute :name "name") "aspect")
	 ((xsl:attribute :name "id")
	  "participle-aspect-" (xsl:value-of/ :select "@nr"))
	 ((option :value "-")
	  ((xsl:if :test "not(@aspect)")
	   ((xsl:attribute :name "selected") "true"))
	  "-")
	 ((option :value "imperf")
	  ((xsl:if :test "@aspect='imperf'")
	   ((xsl:attribute :name "selected") "true"))
	  "imperf")
	 ((option :value "perf")
	  ((xsl:if :test "@aspect='perf'")
	   ((xsl:attribute :name "selected") "true"))
	  "perf")))
       (td
	((xsl:element :name "input")
	 ((xsl:attribute :name "type") "checkbox")
	 ((xsl:attribute :name "name") "main-form")
	 ((xsl:attribute :name "id")
	  "participle-main-form-" (xsl:value-of/ :select "@nr"))
	 ((xsl:if :test "@main-form")
	  ((xsl:attribute :name "checked") "yes"))))
       ))))

(define-url-function root-list-xml
    (request ()
	     :xsl #'root-list-xsl
	     ;;:force-xslt nil
	     :path (concat "/" *url-base* "/roots"))
  ;;(print (request-query request))
  (with-database-connection ()
    #m(roots
       #L(do-query ((id c-root)
		    [select [id] [c-root]
			    :distinct t  :from [morph verb-features]
			    :order-by [id]])
	   #m(root/ :id #s id :c-root #s c-root)))))

(defstylesheet root-list-xsl ()
  #m((xsl:stylesheet xmlns:xsl "http://www.w3.org/1999/XSL/Transform" :version "1.0")
     ((xsl:template :match "/roots")
      (table
       (xsl:apply-templates/ :select "root")))
     
     ((xsl:template :match "root")
      (tr
       (td (xsl:value-of/ :select "@id"))
       (td 
	((xsl:element :name "a")
	 #+ignore
	 ((xsl:attribute :name "href")
	  "paradigm?lang=ng&amp;c-root=" (xsl:value-of/ :select "@c-root"))
	 ((xsl:attribute :name "href")
	  "masdars?id=" (xsl:value-of/ :select "@id"))
	 (xsl:value-of/ :select "@c-root")))))))

;;;;

;; lookup /home/paul/xledir/pargram/kartuli/kartuli-morph.fst < opentext-sorted-words.txt > opentext-sorted-tagged-words.txt


:eof
