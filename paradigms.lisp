;;;-*- Mode: Lisp; Package: TRANSDUCER -*-

(in-package :fst)

(defun normalize-full-gv (gv)
  gv)

#+test
(print (paradigm-classes 2154 :sub-id 1))

#+test
(print (paradigm-classes "2180"))

;; supersedes function in feature-templates.lisp
(defun paradigm-classes (id &key sub-id)
  ;;(debug id) (debug sub-id)
  (let* ((paradigm-tree (dat:make-string-tree))
	 (classes ())
         (fun (lambda (str dg &key paradigm-replacement-p &allow-other-keys)
		(let* ((vn (path-value dg 'vn))
                       (gv (path-value dg 'gv))
                       (morph-type (path-value dg 'morph-type))
                       (pv (path-value dg 'pv))
                       (reduplication (path-value dg 'reduplication))
		       (red-dir-pv (cond ((null reduplication)
					  nil)
					 ((eq reduplication '-)
					  nil)
					 ((path-value dg 'red-dir-pv))
					 ((equal pv "მი")
					  "მო")
					 ((equal pv "გადა")
					  "გადმო")
					 ((equal pv "გარდა")
					  "გარდმო")
					 (t
					  (concat pv "მო"))))
                       (relation (when gv (gethash (car (split (convert gv) #\space))
						   *relation-table*)))
                       (syntax (when gv (gv-syntax gv (path-value dg 'morph-type)))))
                  (pushnew syntax classes)
		  (list str
			(unless paradigm-replacement-p vn)
			(unless paradigm-replacement-p gv)
			(unless paradigm-replacement-p syntax)
			relation morph-type
			(if red-dir-pv
			    (concat pv "-" red-dir-pv)
			    pv))))))
    (mapc (lambda (paradigm)
            (let* ((preverb nil)
                   (syntax nil)
                   (morph-type nil)
                   (verbal-noun nil)
                   (genus-verbi nil)
                   (relation nil)
                   (forms (mapcar (lambda (tense)
				    ;;(debug tense)
				    (if (keywordp tense)
					tense
					(multiple-value-bind (forms xle-templates) ;; class)
					    (or (generate-paradigm (car paradigm)
								   :allp nil
								   :tense tense
								   :num 'sg
								   :pers 1
								   :obj3sg-only-p t
								   :standard-only-p t
								   :printp nil
								   :function fun)
						(generate-paradigm (car paradigm)
								   :allp nil
								   :tense tense
								   :num 'sg
								   :pers 3
								   :obj3sg-only-p t
								   :standard-only-p t
								   :printp nil
								   :function fun))
					  (declare (ignore xle-templates))
					  #-debug(print (list :paradigm (car paradigm) :forms forms))
					  ;;(when (eq tense 'present) (push class classes))
					  (when forms
					    (destructuring-bind (str vn gv synt rel morph-tp pv)
						(car (merge-alternative-forms forms))
					      (unless morph-type (setf morph-type morph-tp))
					      (unless verbal-noun (setf verbal-noun vn))
					      (unless syntax (setf syntax synt))
					      (unless relation (setf relation rel))
					      (unless genus-verbi (setf genus-verbi gv))
					      (when (stringp pv)
						(when (and (stringp preverb)
							   (not (string= pv preverb)))
						  (warn "Different preverbs for pres and aor: ~s, ~s" preverb pv))
						(setf preverb pv))
					      (convert str))))))
                                  '(:present present :future future
				    :masdar masdar
				    :past-part past-part :present-part present-part
				    :future-part future-part :negative-part negative-part
				    :aorist aorist :perfect perfect)))) ;; obs: a finite form must be a the end of the list
              (when genus-verbi
                #+debug(print (list :tree-key (list (normalize-causative (convert verbal-noun))
						    (convert preverb)
						    (if relation (string-downcase relation) ""))
				    genus-verbi syntax forms))
                (if sub-id
		    (return-from paradigm-classes
		      (list* (car paradigm)
			     (normalize-full-gv genus-verbi) syntax morph-type forms))
		    (push (list* (car paradigm) (normalize-full-gv genus-verbi) syntax morph-type forms)
			  (dat::string-tree-get-list
			   paradigm-tree
			   (list (related-verbal-nouns id (normalize-causative (convert verbal-noun)))
				 (convert preverb)
				 (if relation (string-downcase relation) ""))))))))
	  (if sub-id
	      (list (list (format nil "~d-~d" id sub-id)))
	      (id-paradigm-ids id)))
    (values paradigm-tree
            (collecting
              (dolist (cl '(trans caus unacc unerg inv
			    active passive medioactive mediopassive stative))
                (when (find cl classes)
                  (collect cl)))))))

(defun related-verbal-nouns (id vn)
  (let ((related-vns (sort (cons vn (clsql:select [related-vn]
						  :flatp t
						  :from [morph related-verbal-nouns]
						  :where [and [= [id] id]
							      [= [vn] vn]]))
			   #'string<)))
    (if related-vns
	(format nil "~{~a~^/~}" related-vns)
	vn)))

;; uid of first of c-root homonyms + first sub-id
(defun get-croot-id-subid (c-root)
  (or (car (select [c-root] [id] [sub-id]
		   :from [morph verb-features]
		   :where [= [c-root] ?c-root]
		   :limit 1
		   :order-by '([sub-id])))
      (car (select [c-root] [id] [sub-id]
		   :from [morph verb-features]
		   :where [= [c-root] (u:concat c-root "1")]
		   :limit 1
		   :order-by '([sub-id])))))

(defun get-gv (id+subid)
  (destructuring-bind (id sub-id) (u:split id+subid #\-)
    (let ((gv (car (select [tsch-class] :from [morph verb-paradigm] ;; [morph verb-features]
			   :where [and [= [id] ?id] [= [sub-id] ?sub-id]]
			   :limit 1
			   :flatp t))))
      (subseq gv 0 (position #\space gv)))))

;;(print (get-gv "658-18"))


#+test
(print (get-croot-id "ხვრეტ"))

#+test
(print (related-verbal-nouns 2138 "Tana"))

#+test
(print (paradigm-classes 1919 :sub-id 1))
#+test
(print (paradigm-classes 2751 :sub-id 3))

#+test
(print (generate-paradigm "1919-1" :tense 'past-part :pers 1 :num 'sg :obj3sg-only-p t :standard-only-p t :printp nil) )

#+test
(print (generate-paradigm "3260-10" :tense 'iter-present :pers 3 :num 'sg :obj3sg-only-p t :standard-only-p nil :printp nil) )

#+test
(print (generate-paradigm "3842-1" :tense 'present :pers 3 :num 'sg :obj3sg-only-p t :standard-only-p nil :printp nil) )


#+test
(clrhash *feature-cache*)

(defparameter *phonotactics-ng* nil)
(defparameter *phonotactics-og* nil)
(defparameter *phonotactics-xanmeti* nil)
(defparameter *phonotactics-haemeti* nil)

(defun init-transducers (&key compilep)
  (when compilep
    (ccl::cwd "projects:georgian-morph;regex;")
    (ccl::run-program "fst" (list "-utf8" "-f" "phonotactics.regex") :wait t :output *standard-output*))
  (setf *phonotactics-ng* (make-instance 'cl-fst:fst-net :file "projects:georgian-morph;regex;pht-gen-ng.fst")
	*phonotactics-og* (make-instance 'cl-fst:fst-net :file "projects:georgian-morph;regex;pht-gen-og.fst")
	*phonotactics-xanmeti* (make-instance 'cl-fst:fst-net :file "projects:georgian-morph;regex;pht-gen-xanmeti.fst")
	*phonotactics-haemeti* (make-instance 'cl-fst:fst-net :file "projects:georgian-morph;regex;pht-gen-haemeti.fst")))

(init-transducers :compilep nil)

#+test
(time (init-transducers :compilep t))
#+test
(clrhash *feature-cache*)

#+test
(cl-fst:fst-lookup *phonotactics-og*
		   "ჴელ#Hე<წიფ>nეს:+Aor+Cond"
		   ;; "ჴელ#Hე<წიფ>ებოდა:+Cond" ;; "ვH<ჴშ>ავ:+Pres+Absolute" ;;"ძალ#გი<ძ>ს:+Pres"
		   (lambda (upper lower net)
		     (declare (ignore upper))
		     (debug (u:split lower #\newline nil nil t)))
		   :side :upper)

;; applies phonotactic rules
(defun surface-forms (str &key lang transp absolutep tense)
  ;;(print (encoding:utf-8-encode str))
  (cl-fst:fst-lookup (ecase lang
		       (:ng *phonotactics-ng*)
		       (:og *phonotactics-og*)
		       (:xanmeti *phonotactics-xanmeti*)
		       (:haemeti *phonotactics-haemeti*))
		     (let ((upper (format nil "~a:~:[~;+IntrAbs~]+~a~:[~;+Absolute~]"
						  str
						  (and (not transp) absolutep)
						  (tense-code tense)
						  absolutep)))
		       #-gekko
		       (print (encoding:utf-8-encode upper))
		       #+gekko
		       (debug upper)
		       upper)
		     (lambda (upper lower net)
		       (declare (ignore upper net))
		       (mapcar (lambda (form) (subseq form 0 (position #\+ form)))
			       (u:split lower #\newline nil nil t)))
		     :side :upper))

#+test
(cond ((null obj-3-pfx)
       '--)
      ((eq obj-3-pfx :optional)
       (case (root-type root)
	 (null-root '--)			  ;; - only
	 (otherwise '-)))			  ;; + is subnorm
      (t
       (case (root-type root)
	 (s-root '+) ;; - is subnorm| S=OBen | S=OLoc | S=OTh
	 (null-root '--)
	 (otherwise '+-))))

#+test ;; შევH<დრკ>ები
(generate-paradigm "3260-71" :tense 'AORIST :pers 1 :num 'SG :allp T :paradigm-replacement-p T :lang :og)
#+test
(generate-paradigm "3260-10" :tense 'AORIST :pers 1 :num 'SG :allp nil :paradigm-replacement-p T :lang :og)
#+test
(generate-paradigm "2751-2" :tense 'PRESENT :pers 1 :num 'SG :allp nil :paradigm-replacement-p T :lang :ng)


#+test
(list-to-dg '((style { norm bracket})
	      (lang ng)))

#+test
(update-recordsxx [morph verb-paradigm]
		:av-pairs '(([features-sub-id] 3))
		:where [and [= [id] 215] [= [sub-id] 3]])

#+test
(print (paradigm-classes 2751 :sub-id 3))

;; used in paradigm XML
;; from feature-templates.lisp
(defun generate-paradigm (paradigm-id &key tense pers num (fst *full-verb-fst*) debug allp (printp t) obj3sg-only-p
			  paradigm-replacement-p standard-only-p (lang :ng) (preverbless-aorists t) function)
  #-debug(print (list :generating paradigm-id tense pers num allp obj3sg-only-p
		      :para-rep paradigm-replacement-p standard-only-p printp function))
  (let ((parser::*dg-vector* (or parser::*dg-vector* (make-array 0 :fill-pointer t :adjustable t)))
	(parser::*body-vector* (or parser::*body-vector* (make-array 0 :fill-pointer t :adjustable t)))
	(parser::*flag-vector* (or parser::*flag-vector* (make-array 0 :fill-pointer t :adjustable t)))
	(parser::*dg-count-vector* (or parser::*dg-count-vector* (make-array 0 :fill-pointer t :adjustable t)))
	(parser::*dg-count* (or parser::*dg-count* 0))
	(tenses (cond ((null tense)
		       (case lang
			 (:ng
			  '(present future imperfect conditional conj-present conj-future
			    aorist optative
			    perfect pluperfect conj-perfect
			    past-part future-part present-part negative-part
			    masdar))
			 (otherwise
			  '(present future imperfect conditional conj-present conj-future
			    iter-present iter-imperfect imperative-present
			    aorist optative iter-aorist imperative-aorist
			    perfect pluperfect conj-perfect iter-perfect
			    past-part future-part present-part negative-part
			    masdar))))
                      ((listp tense)
                       tense)
                      (t
                       (list tense))))
        (paradigm-gv-roots (gethash paradigm-id *paradigm-table*))
	(xle-templates (cons nil (mapcar #'list
					 (destructuring-bind (id sub-id) 
					     (mapcar #'parse-integer (split paradigm-id #\-))
					   (paradigm-xle-templates id sub-id))))))
    #+debug(print paradigm-gv-roots)
    (values
     (collecting
       ;;(debug tenses)
       (dolist (tense tenses)
	 ;;(debug tense)
	 (dolist (root (cddr paradigm-gv-roots))
	   ;;(debug root)
	   (when (stringp root)	;; bug: for some masdars, root = -
	     (let* ((*print-case* :capitalize)
		    (root-override-features (get-root-override-features 
					     root
					     :clear-all-caches-p nil
					     :preverbless-aorists preverbless-aorists
					     ;; new Nov. 2014
					     ;;:paradigm-replacement-p paradigm-replacement-p
					     ))
		    (root-type (when root (root-type root))))
	       (dolist (gv-features root-override-features)
		 (destructuring-bind (gv id pv . features) gv-features
		   (declare (ignore pv))
		   (when (equal id paradigm-id)
		     ;;(debug gv)
		     (let ((tense-list (cadr (assoc 'tense features)))
			   (morph-type (cadr (assoc 'morph-type features)))
			   (paradigm-replacement (cadr (assoc 'paradigm-replacement features))))
		       (when (if (parser::extended-list-p tense-list)
				 (find tense (parser::extended-list-form tense-list))
				 (eq tense tense-list))
			 (when debug
			   (print (list :features features
					:filled-template
					(fill-template (feature-template root gv tense morph-type) features))))
			 (cond (paradigm-replacement
				(unless paradigm-replacement-p
				  (collect-append (generate-paradigm paradigm-replacement
								     :tense tense
								     :pers pers
								     :num num
								     :fst fst
								     :debug debug
								     :printp printp
								     :obj3sg-only-p obj3sg-only-p
								     :paradigm-replacement-p t
								     :standard-only-p standard-only-p
								     :function function
								     :lang lang))))
			       (t
				(multiple-value-bind (restriction-features invertedp)
				    (paradigm-person-marking-restrictions
				     (cdr xle-templates)
				     (syntax-type gv morph-type)
				     gv
				     tense)
				  #-debug(print (list :xle-templates xle-templates invertedp restriction-features))
				  (let* ((template
					  `((tense ,tense)
					    (root ,root)
					    ((type root) ,root-type)
					    ;; workaround to avoid generating Tschenkeli-dict special 
					    ;; forms with --(parsing -)
					    ,@(fill-template (feature-template root gv tense morph-type)
							     features
							     :except '(tense root (type root)))))
					 (invertedp
					  (if (cdr xle-templates)
					      invertedp
					      (or (and (find tense '(perfect pluperfect conj-perfect iter-perfect))
						       (let ((morph-type (cadr (find 'morph-type template :key #'car))))
							 (if (parser::extended-list-p morph-type)
							     (or (find 'active (parser::extended-list-form morph-type))
								 (find 'causative (parser::extended-list-form morph-type)))
							     (find morph-type '(active causative)))))
						  (eq (search "IV" gv) 0)
						  (search "(OR)" gv)))) ;; ??
					 (dg (cond ((find tense '(past-part future-part present-part
								  negative-part masdar))
						    #+debug(pprint (list template (list-to-dg template)))
						    (unify (list-to-dg template)
							   (list-to-dg
							    `((parsing -)))))
						   (allp
						    (list-to-dg template))
						   (t
						    (unify (list-to-dg template)
							   (list-to-dg
							    (if (and (cdr xle-templates) (not obj3sg-only-p))
								restriction-features
								(if invertedp
								    `(((subj pers) 3)
								      ((subj num) sg)
								      ,@(when pers `(((obj pers) ,pers)))
								      ,@(when num `(((obj num) ,num))))
								    `(((obj pers) 3)
								      ,@(when (or (eq lang :ng)
										  (not (find tense 
											     '(aorist optative
											       iter-aorist
											       imperative-aorist))))
									      `(((obj num) sg)
										((dir-obj num) sg))) ;; new
								      ,@(when pers `(((subj pers) ,pers)))
								      ,@(when num `(((subj num) ,num)))))))))))
					 (dg (when dg
					       (case lang
						 (:ng
						  (if standard-only-p
						      (unify dg (list-to-dg '((style {norm bracket})
									      (lang ng))))
						      (unify dg (list-to-dg '((style {norm bracket ng subnorm old rust})
									      (lang ng))))))
						 (otherwise
						  (unify dg (list-to-dg '( ;;(style {norm bracket og subnorm old})
									  (lang og)))))
						 ))))
				    (when debug
				      (write-line (#+gekko progn #-gekko encoding:utf-8-encode
						   (write-to-string (list :template template :dg dg)))))
				    (u-generate
				     dg fst
				     :fun (if function
					      (lambda (str dg)
						(collect (funcall function str dg
								  :paradigm-replacement-p paradigm-replacement-p)))
					      (lambda (str dg)
						;;(debug str)
						;;(debug (path-value dg 'obj 'num))
						(let ((gv (path-value dg 'gv))
						      (tense (path-value dg 'tense))
						      (subj-pers (path-value dg 'subj 'pers))
						      (subj-num (path-value dg 'subj 'num))
						      (obj-pers (path-value dg 'obj 'pers))
						      (obj-num (path-value dg 'obj 'num))
						      ;; OG
						      (dir-obj-num (path-value dg 'dir-obj 'num)))
						  ;;(debug tense)
						  ;;(debug dir-obj-num)
						  (dolist (str (surface-forms
								str
								:lang lang
								:transp (and gv (find (char gv 0) "TKI"))
								:absolutep (and gv
										(or (and (char= (char gv 0) #\T)
											 (find (char gv 1) "12"))
										    (char= (char gv 0) #\P)
										    (char= (char gv 0) #\M)))
								:tense tense))
						    (collect
							(if invertedp
							    (list (convert str) tense obj-pers obj-num 
								  subj-pers subj-num dir-obj-num
								  (path-value dg 'style)
								  (when paradigm-replacement-p paradigm-id))
							    (list (convert str) tense subj-pers subj-num
								  obj-pers obj-num dir-obj-num
								  (path-value dg 'style)
								  (when paradigm-replacement-p paradigm-id))))
						    (when printp
						      (format t "~&~a    +~a+FSubj~d~a~a~@[+~a~]~@[+~a~]~%"
							      str
							      (tense-code tense)
							      subj-pers
							      subj-num
							      (if obj-pers
								  (format nil "+FObj~d~a" obj-pers
									  (or obj-num ""))
								  "")
							      (path-value dg 'style)
							      (when paradigm-replacement-p "Repl"))))))))))))))))))))))
     xle-templates
     (normalize-gv (car paradigm-gv-roots)))))

(defun paradigm-person-marking (xle-template morph-syntax)
  (case (intern (string-upcase xle-template) :keyword)
    ((:V-TRANS-SUBJ-OBJ
      :V-TRANS-SUBJ-OBJ-PREDLINKadv
      :V-TRANS-O-SUBJ-OBJ
      :V-TRANS-O-SUBJ-COMP
      :V-DITRANS-SUBJ-OBJ-OBJ
      :V-CAUS-SUBJ-OBJ-OBJ)
     :bipers)
    ((:V-INTR-SUBJ-OBJ)
     :bipers-unerg)
    ((:V-TRANS-SUBJ3-OBJ
      :V-TRANS-S-SUBJ-OBJ3
      :V-TRANS-SUP-SUBJ-OBJ3
      :V-TRANS-SUBJ-OBJ3
      :V-TRANS-S-SUBJ3-OBJ
      :V-TRANS-SUP-SUBJ3-OBJ
      :V-INTR-O-SUBJ)
     :monopers-unerg)
    ((:V-INTR-SUP-SUBJ)
     (case morph-syntax
       ((unerg medioactive) :monopers-unerg)
       ((unacc mediopassive stative) :monopers-unacc)))
    ((:V-INTR-SUBJ
      ;;:V-INTR-SUBJ3-OBJ
      :V-INTR-SUBJ-OBJ3)
     (case morph-syntax
       ((unerg medioactive) :monopers-unerg)
       ((unacc mediopassive stative) :monopers-unacc)
       ((trans active) :monopers-unerg)))
    ((:V-INTR-SUBJ3-OBJ)
     :monopers-inv)
    ((:V-QOPNA
      :V-QOPNA-AUX
      :V-INTR-SUBJ-PREDLINKnom
      :V-QOPNA-PREDLINK)
     :monopers-unacc)
    ((:V-IV-SUBJ-OBJ
      :V-IV-SUBJ-OBJ3 ;; prelim.
      )
     :bipers-inv)
    ((;;:V-IV-SUBJ-OBJ3
      :V-IV-SUBJ
      :V-IV-SUBJ-XCOMP-MOD
      :V-IV-SUBJ-OBJgen
      :V-KONA-PERF
      :V-STATE-PASS-SUBJ-IV)
     :monopers-inv)
    ((:V-STATE-PASS-SUBJ)
     :monopers)
    ((:V-STATE-PASS-O-SUBJ
      :V-STATE-PASS-SUP-SUBJ
      :V-STATE-PASS-SUBJ-OBJ)
     ;;:bipers-unerg
     :bipers-inv)
    (:V-EMPTY
     :unpers)
    (otherwise
     :monopers-unerg)))

(defun alternate-stem (stem conjugation)
  (ecase conjugation ;;(intern (string-upcase conjugation) :keyword)
    ((:A :O :A1 :O1 :O2 :C) nil)
    (:B (values :C (syncope-stem stem)))
    (:P (values :Q (ablaut-stem stem #\v 1)))
    (:U (values :V (truncate-stem stem 2)))
    (:D (values :E (truncate-syncope-stem stem)))
    (:F (values :G (truncate-syncope-stem stem))) ;; ??
    (:I (values :J (truncate-stem stem)))
    (:K (values :L (truncate-stem stem)))
    (:M (values :N (truncate-stem stem)))
    (:R (values :S (truncate-stem stem)))
    (:X (values :Y (truncate-stem stem)))))

#+test
(print (select [root] [type-obj-3-pfx]
	       [count [type-obj-3-pfx]]
	       :from [morph verb-features]
	       :where [not [null [type-obj-3-pfx]]]
	       :group-by '([root] [type-obj-3-pfx]))) 


#+main
(with-open-file (stream "projects:georgian-morph;regex;noun-list.regex" :direction :output :if-exists :supersede)
  (write-fst-noun-stems-sql stream))

#+test
(write-fst-noun-stems-sql *standard-output*)

#+test
(block block
  (let ((i 100))
    (maphash (lambda (key val)
	       (unless (clsql:select [stem] :from [noun-features] :where [= [stem] (convert key)])
		 (print (list (convert key) val))
		 (decf i)
		 (when (zerop i) (return-from block))))
	     *noun-table*)))

#+test
(print (select [*] :from [morph verb-features]
	       :where [= [id] 3837]))

:eof
