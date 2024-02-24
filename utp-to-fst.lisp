;;;-*- Mode: Lisp; Package: TRANSDUCER -*-
;;;
;;; 
;;; (C) Paul Meurer 2006-2013

#||

How to build the fst morphology:

load the project kartuli-paradigm.asd;
eval this file;
run #+main-ccl below;

write-fst-noun-stems-sql()
write-fst-participle-stems-sql() ; contains VN
write-verb-lexicon-regexp()
  verb-lexicon-regexp()
    get-root-override-features()
see verb-feature-table-sql.lisp: get-root-override-features() for vn etc.

write-verb-regex-file()
write-noun-regex-file()

fst to foma: see regex/syntax.regex


{@ობ[ა]-ჲ/@}:{@}

define R {თინათინ}|{თულ} ;

define obs R:0 {ობ[ა]-ჲ/}:0 R ;

||#


(in-package :fst)

(pushnew :fst *features*)

(defun utp-to-fst (list-regexp &key (stream *standard-output*) name (fst-table *u-fst-table*)
                               (fst-class 'u-fst))
  (format stream "define ~a " (format-symbol-fst name))
  (let ((fsa (make-instance fst-class)))
    (labels 
      ((walk (exp)
          (etypecase exp
            (list
             (ecase (car exp) 
               (seq
                (write-string "[ " stream)
                (loop for subexp in (cdr exp)
                      do (walk subexp)
                      (write-char #\space stream))
                (write-string "]" stream))
               (or
                (write-string "[ " stream)
                (loop for (subexp . rest) on (cdr exp)
                      do (walk subexp)
                      when rest do (write-string " | " stream))
                (write-line "]" stream))
               (?
                (assert (null (cddr exp)))
                (write-string "( " stream)
                (walk (cadr exp))
                (write-string " )" stream))
               (*
                (assert (null (cddr exp)))
                (write-string "[ " stream)
                (walk (cadr exp))
                (write-string " ]*" stream)
                ;;(minimize (fsa-closure (walk (cadr exp))))
                )
               (+
                (assert (null (cddr exp)))
                (assert (null (cddr exp)))
                (write-string "[ " stream)
                (walk (cadr exp))
                (write-string " ]+" stream))))
            (string
             (write-string " {" stream)
             (write-string (convert exp) stream)
             (write-string "} " stream))
            (symbol
             (case exp
               ((@ %)
                (fsa-symbol exp fsa))
               (otherwise
                (write-string " " stream)
                (write-string (format-symbol-fst (symbol-name exp)) stream)
                (write-string " " stream))))
            (transducer-pair
             (transducer-pair-to-fst exp stream)))))
      (walk list-regexp)
      (write-line " ;" stream)
      (terpri stream)
      nil)))

(defun transducer-pair-to-fst (utp stream)
  (let* ((symbol (tp-upper utp))
         (dag (tp-lower utp))
         (type (tp-augment utp))
         (flag-diacritics
          (with-output-to-string (str-stream)
            (let ((lex (dag-to-flag-diacritics dag type str-stream)))
              (cond ((equal symbol "epsilon")
                     (when lex (format stream "{~a}:0" (convert lex))))
                    (lex
                     (format stream "{~a}:{~a}" (convert lex) (convert symbol)))
                    (t
                     (format stream "0:{~a}" (convert symbol))))
              (case type
                (der
                 (write-string " AugmentFlagDiacritics " stream))
                (card
                 (write-string " AugmentCardFlagDiacritics " stream))
                (conj-num
                 (write-string " AugmentConjNumFlagDiacritics " stream))
		(not
		 nil))))))
    (write-string flag-diacritics stream)))

(defun format-symbol-fst (str)
  (delete #\/ (nsubstitute #\Q #\? (delete #\. (delete #\- (string-capitalize str))))))

(defparameter *flag-diacritics* ())

(defun write-eliminate-flag-diacritics (stream)
  (dolist (flag (remove-duplicates (mapcar #'car *flag-diacritics*) :test #'string=))
    (format stream "echo > eliminate flag ~a~%eliminate flag ~a~%" flag flag)))

#+test
(write-eliminate-flag-diacritics *standard-output*)

(defparameter *root-list* ())
(defparameter *c-root-list* ())

#+test
(precompile-u-transducer
 (utp-not '((root {"a" "b"})
	     (sf -)))
 :name 'test)

#+test
(write-noun-regex-file)

;; add warning if dag is not a tree!
(defun dag-to-flag-diacritics (dag type stream)
  (let ((lex nil)
	(*print-case* :downcase))
    (labels ((walk (dag path)
               (cond ((listp (cdr dag))
		      (loop for (att+dag . rest) on (cdr dag)
			   do (walk (cadr att+dag) (cons (format-symbol-fst (car att+dag)) path))
			   when (and rest (eq type 'not))
			   do (write-string " | " stream)
			   ))
                     ((transducer-pair-p (cdr dag))
                      (error "Not implemented."))
                     ((parser::extended-list-p (cdr dag))
                      (when (eq (parser::extended-list-char (cdr dag)) #\[)
                        (error "Not implemented."))
                      (write-string "[ " stream)
		      (when (and (null (cdr path))
				 (string-equal (car path) "root"))
			(dolist (root (parser::extended-list-form (cdr dag)))
			  (pushnew root *root-list* :test #'string=)))
                      (let ((path (format nil "~{~a~^-~}" (reverse path))))
			(loop for (disj . rest) on (parser::extended-list-form (cdr dag))
			   do (let ((value (format nil "~a" (convert disj))))
				(pushnew (cons path value) *flag-diacritics* :test #'equal)
				(format stream " \"@~c.~a.~a@\" "
					(case type
					  (not #\D)
					  (require #\R)
					  (otherwise #\U))
					path value))
			   when rest do (if (eq type 'not)
					    (write-string " " stream)
					    (write-string " | " stream))))
		      (write-line "]" stream))
                     ((string-equal (car path) "FstUpper")
                      (format stream "\"~a\":0" (cdr dag)))
                     (t
                      (when (null (cdr path))
                        (cond ((string-equal (car path) "lex")
                               (setf lex (cdr dag)))
                              ((string-equal (car path) "root")
			       (pushnew (cdr dag) *root-list* :test #'string=))
                              ((string-equal (car path) "croot")
                               (pushnew (cdr dag) *c-root-list* :test #'string=))))
                      (unless (string-equal (car path) "lex") ;; new Dec. 2014
			(let ((path (format nil "~{~a~^-~}" (reverse path)))
			      (value (format nil "~a" (convert (cdr dag)))))
			  (pushnew (cons path value) *flag-diacritics* :test #'equal)
			  (format stream " \"@~c.~a.~a@\" "
				  (case type
				    (not #\D)
				    (require #\R)
				    (otherwise #\U))
				  path value)))))))
      (when (eq type 'not) (write-string "[ " stream))
      (walk dag nil)
      (when (eq type 'not) (write-string " ]" stream)))
    lex))

#+test
(write-verb-regex-file)

(defun precompile-u-transducer (list-regexp &key name (fst-table *u-fst-table*)
                                                (fst-class 'u-fst))
  (utp-to-fst list-regexp :stream *standard-output* :name name :fst-table fst-table
              :fst-class fst-class))

(defmethod compile-u-transducer ((list-regexp list) &key name (fst-table *u-fst-table*)
                                     (fst-class 'u-fst) &allow-other-keys)
  (precompile-u-transducer list-regexp :name name :fst-table fst-table :fst-class fst-class))

(defun resolve-flag-diacritics (stream)
  (write-string "define ResolveFlagDiacritics " stream)
  (dolist (fd (sort (copy-seq *flag-diacritics*) #'string< :key #'car))
    (unless (string-equal (car fd) "lex")
      (format stream (if (equal (cdr fd) "-")
                       " [ \"@R.~a.~a@\" \"+~a\":0 %.%~a:0 | \"@D.~a.~a@\" ]~% "
                       " [ \"@R.~a.~a@\" \"+~a\":0 \".~a\":0 | \"@D.~a.~a@\" ]~% ")
              (car fd) (cdr fd) (car fd) (cdr fd)  (car fd) (cdr fd))))
  (write-line ";" stream)
  (values))

#+test
(resolve-flag-diacritics *standard-output*)

(defun augment-flag-diacritics (stream)
  (write-string "define AugmentFlagDiacritics " stream)
  (let ((atts ())
        (flag-diacritics (sort (copy-seq *flag-diacritics*) #'string< :key #'car)))
    (dolist (fd flag-diacritics)
      (when (find (car fd) '(case cat num form stemtype syncstem rigidstem plstem pers casetype) :test #'string-equal)
        (pushnew (car fd) atts :test #'string-equal)
        (format stream (if (equal (cdr fd) "-")
                         " [ \"@R.~a.~a@\" \"+D~a\":0 %.%~a:0 | \"@D.~a.~a@\" ]~% "
                         " [ \"@R.~a.~a@\" \"+D~a\":0 \".~a\":0 | \"@D.~a.~a@\" ]~% ")
                (car fd) (cdr fd) (car fd) (cdr fd)  (car fd) (cdr fd))))
    (dolist (att atts)
      (format stream "\"@C.~a@\" " att)))
  (write-line ";" stream)
  (values))

#+test
(augment-flag-diacritics *standard-output*)

(defun resolve-noun-flag-diacritics (stream)
  (write-string "define ResolveFlagDiacritics " stream)
  (dolist (att '(case casetype pers num oldpl long pp modsfx relsfx))
    (dolist (fd *flag-diacritics*)
      (when (string-equal (car fd) att)
        (let ((fst-feature (fst-feature-string (cdr fd) att)))
          (when fst-feature
            (format stream " [ \"@R.~a.~a@\" \"+~a\":0 | \"@D.~a.~a@\" ]~% "
                    (car fd) (cdr fd)
                    fst-feature
                    (car fd) (cdr fd)))))))
  (write-line ";" stream)
  (values))

(defun fst-feature-string (f att)
  (cond ((string-equal f '+)
         (string-capitalize att))
        ((string-equal f '-)
         nil)
        (t
         (string-capitalize f))))

#+test
(resolve-noun-flag-diacritics *standard-output*)


(defun root-regexp (stream)
  (write-string "define Root [ " stream)
  (with-file-lines (line "projects:georgian-morph;root-lists.txt")
    (destructuring-bind (c-root . roots) (split (inv-convert line) #\space)
      (loop for root in roots
            do (if (equal c-root root)
                 (format stream "{~a} " (convert c-root))
                 (format stream "{~a}:{~a} " (convert c-root) (convert root)))
            do (write-string "| " stream))))
  (write-line "] ;" stream))

#+test
(root-regexp *standard-output*)

(defparameter *attpaths* ())

(defun %root-type (root)
  (let ((first-char (if (char= (char root 0) #\E)
			(char root 1)
			(char root 0))))
    (cond ((find root '("ვალ" "ველ" "ვედ" "ვიდ" "ვAლ") :test #'string=)
           "x-root")
          ((find first-char "დთტძცწჯჩჭ")
           "s-root")
          ((find first-char "გკქპყ")
           "h-root")
          ((find first-char "ბფზსჟშღხლრმნვჳ")
           '{"h-root" "null-root"})
          (t "null-root"))))


(defparameter *missing-templates* (make-hash-table :test #'equal))

#+test
(print (get-root-override-features "ცოდნ" :use-base-sub-id-p t
				   :all-participles-p t))

#+test
(verb-lexicon-regexp 3117 "ცოდინ" *standard-output* :c-root "ცოდნ")
#+test
(verb-lexicon-regexp 3117 "ც" *standard-output* :c-root "ცოდნ")

#+test
(verb-lexicon-regexp 2032 "ვAლ" *standard-output* :c-root "სვლ")
#+test
(verb-lexicon-regexp 1266 "თხრ" *standard-output* :c-root "მბობ")
#+test
(verb-lexicon-regexp 3848 "მარდ" *standard-output* :c-root "მარდ")

#+obsolete
(defun resolve-root-symbols (root)
  (subst-substrings root '("E" "[ე]" "A" "[ა]" "I" "[ი]")))

#+test
(print (resolve-root-symbols "ცAნ"))

#+test
(verb-lexicon-regexp 1235 "მარტ" *standard-output* :c-root "მარტ")

#+test
(pprint
 (get-root-override-features
  "თამაშ"
  :use-base-sub-id-p t ;; take frame (and more?) from base-sub-id
  :all-participles-p t ;; fetch from database
  :skip-disabled t))

#+test
(do-query ((root c-root)
	   [select [root] [c-root] :distinct t
		   :from [morph verb-features]
		   :where [like [c-root] "%ევ%"]])
  (when (= (+ (length root) 3) (length c-root))
    (print (list root c-root))))

#+test
(verb-lexicon-regexp 3117 "ც" *standard-output* :c-root "ცოდნ")
#+test
(verb-lexicon-regexp 1853 "რ" *standard-output* :c-root "რევ")
#+test
(verb-lexicon-regexp 3842 "ავად" *standard-output* :c-root "ავად")

(defun verb-lexicon-regexp (root-id root stream &key c-root tenses template-hash (first-p t))
  (assert (not (null *c-root-list*)))
  (let* ((feature-hash (make-hash-table :test #'equal))
	 (root-override-features (get-root-override-features
				  root
				  :use-base-sub-id-p t ;; take frame (and more?) from base-sub-id
				  :all-participles-p t ;; fetch from database
				  :skip-disabled t))
         (root-type (when root (%root-type root)))
	 (written-p nil))
    #+debug(debug c-root)
    #+debug(debug root-override-features)
    #+debug(print (mapcar (lambda (rof) (list (car rof) (cadr rof))) root-override-features))
    (dolist (gv-features root-override-features)
      (destructuring-bind (gv id+sub-id pv . features) gv-features
	;;(debug id+sub-id)
	(destructuring-bind (id sub-id) (mapcar #'parse-integer (split id+sub-id #\-))
	  (when (= root-id id)
	    (let* ((xle-templates (fst::paradigm-xle-templates id sub-id))
		   (parse-tense-list (or tenses
					 '(present future conditional conj-future
					   conj-present imperfect
					   aorist optative
					   perfect pluperfect conj-perfect
					   iter-present iter-imperfect iter-aorist
					   iter-perfect iter-perfect1
					   imperative-present imperative-aorist)))
		   (tense-list (cadr (assoc 'tense features)))
		   (morph-type (cadr (assoc 'morph-type features)))
		   (tenses (if (parser::extended-list-p tense-list)
			       (if (listp parse-tense-list)
				   (intersection (parser::extended-list-form tense-list)
						 parse-tense-list :test #'string-equal)
				   (when (find parse-tense-list (parser::extended-list-form tense-list)
					       :test #'string-equal)
				     (list parse-tense-list)))
			       (if (listp parse-tense-list)
				   (when (find tense-list parse-tense-list :test #'string-equal)
				     (list tense-list))
				   (when (string-equal parse-tense-list tense-list)
				     (list tense-list))))))
	      #+debug(print tenses)
	      (when (null xle-templates)
		      (unless (gethash (format nil "~d-~d" id sub-id) *missing-templates*)
			(setf (gethash (format nil "~d-~d" id sub-id) *missing-templates*) root)
			(format t "No XLE templates found for ~d-~d (~a).~%" id sub-id root)))
	      (dolist (xle-template (or xle-templates '("??")))
		(let ((frame (xle-template-to-frame xle-template)))
		  ;;(print (list :id id :sub-id sub-id :frame frame))
		  (block tense
		    (dolist (tense tenses)
		      (let* ((finite-tense-p
			      (not (find tense '(masdar past-part future-part
						 present-part negative-part)
					 :test #'string-equal)))
			     (template
			      (list* `(tense ,tense)
				     `((type root) ,root-type)
				     `(frame ,frame)
				     '(parsing +)
				     '(disabled -)
				     (fill-template (feature-template root gv tense morph-type) features
						    :except (list 'tense '(type root)))))
			     (vn (cadr (assoc 'vn template)))
			     (gv (cadr (assoc 'gv template)))
			     (paradigm-replacement (cadr (assoc 'paradigm-replacement template)))
			     (morph-type (cadr (assoc 'morph-type template))))
			(when paradigm-replacement (return-from tense))
			(labels ((root-features ()
				   (list (list (sconvert vn)
					       id
					       root
					       (string-downcase (syntax-type gv morph-type)))
					 (list* (if (and pv (not (eq pv '-)))
						    (sconvert pv)
						    "")
						sub-id
						xle-templates))))
			  (let ((feature-string
				 (with-output-to-string (stream)
				   (when finite-tense-p
				     ;; changed 11. Juli 2015
				     (if (find root *root-list* :test #'string=)
					 (format stream " \"@U.Root.~a@\" " (delete #\% root))
					 (format stream " \"@U.Root.DEFAULT@\" "))
				     (if (find c-root *c-root-list* :test #'string=)
					 (format stream " \"@U.CRoot.~a@\" " c-root)
					 (format stream " \"@U.CRoot.DEFAULT@\" ")))
				   (dolist (attpath+val template)
				     (destructuring-bind (attpath val) attpath+val
				       (let ((attpath (if (consp attpath)
							  (format nil "~{~a~^-~}" (mapcar #'format-symbol-fst attpath))
							  (format-symbol-fst attpath))))
					 (pushnew attpath *attpaths* :test #'string=)
					 (cond ((not (find attpath '("Tense" "Alternation" "MorphType" "Frame" "Pv" "Sf"
								     "PassiveSfx" "Vv"
								     "Type-Root" "Type-RootVowel" "Type-Subj2Pfx" "Type-Obj3Pfx"
								     "Type-Subj2Pfx"  "Type-Subj12Sfx" "Type-Subj3Sfx"
								     "Relation" ;; needed for passive perfect series
								     "CausSf" "Inverted" "PpPfx"
								     "Type-Aorist3sg" "Type-Aorist"
								     "Type-EvSfx" "Type-Optative3plEn"
								     "Type-Optative" "Type-PrStExt"
								     "Obj-Pers" "Subj-Pers" "Subj-Num"
								     "PartSfx" "PartPfx" "Type-EvSfx" 
								     "Type-Optative3plEn" "IInfix"
								     "CausSf" "NasalInfix"
								     "StemType" "Style" "Subnorm" "Dialect"
								     "Lang" "Aspect"
								     ;;"RedDirPv"
								     ;;"Reduplication"
								     "Version"
								     "Genus"
								     )
							   :test #'string=))
						nil)
					       ((string-equal attpath "CRoot")
						(debug attpath))
					       ((parser::extended-list-p val)
						(cond ((equal (parser::extended-list-form val) '(1 2 3))
						       nil)
						      (t
						       (write-string " [ " stream)
						       (loop for (alt . rest) on (parser::extended-list-form val)
							  do (format stream " \"@U.~a.~a@\" " attpath (sconvert alt))
							  (if rest
							      (write-string " | " stream)
							      (write-string " ] " stream))))))
					       ((null val)
						nil)
					       (t
						(let ((val (sconvert val)))
						  (if (or (null val) (equal val ""))
						      (warn "val is nil: ~a: ~s -> ~s" id attpath val)
						      (format stream " \"@U.~a.~a@\" " attpath val)))))))))))
			    (destructuring-bind (common pv+sub-id.templates) (root-features)
			      (let ((partition (find common
						     (gethash feature-string feature-hash)
						     :test #'equal :key #'car)))
				(if partition
				    (push pv+sub-id.templates (cdr partition))
				    (push (list common pv+sub-id.templates)
					  (gethash feature-string feature-hash)))))))))))))))))
    ;; write XLE templates and fst regexp
    ;;(debug feature-hash)
    (maphash (lambda (feature-string root-feature-partitions)
	       ;; these have a common feature string
	       (loop for root-features in root-feature-partitions
		  with pack-count = -1
		  do (destructuring-bind (common . template-features) root-features
		       ;;(debug template-features)
		       ;;(debug common)
		       (destructuring-bind (vn id root syntax-type) common
			 (multiple-value-bind (partitions packed-p) (partition-root-features template-features)
			   (when packed-p (incf pack-count))
			   #+debug(print (list :packed-p packed-p :pack-count pack-count :partitions partitions))
			   (dolist (partition partitions)
			     (destructuring-bind (xle-template pv sub-id-list sub-id-range) partition
			       ;;(debug c-root) (debug root)
			       (let* ((masdar (if (string= pv "") vn (concat pv "·" vn)))
				      (masdar (delete #\( (delete #\) masdar)))
				      ;; -ევ is not part of the root in the db, but should be part of the lemma root
				      (ev (eq 0 (search (u:concat root "ევ") (string-right-trim "123" c-root))))
				      ;; see fst::get-root-override-features() for vn
				      (vn/root (format nil "~a/~a~@[EV~]" vn root ev))
				      ;;(vn/root (format nil "~a/~a" vn root))
				      (ckey (format nil "~a/~a" vn c-root)))
				 ;; collect templates
				 #+obsolete
				 (when template-hash
				   (let ((paradigm-templates (gethash ckey template-hash)))
				     (if paradigm-templates
					 (pushnew xle-template (cdr paradigm-templates))
					 (setf (gethash ckey template-hash)
					       (list (list id masdar pv
							   (fst::get-tsch-classes id sub-id-list) sub-id-range)
						     xle-template)))))
				 ;; write fst regexp
				 (when stream
				   (if first-p
				       (setf first-p nil)
				       (write-line " |" stream) )
				   (setf written-p t)
				   ;;(debug vn/root)
				   (format stream
					   "{~a}:~@[{~a}~]~:*~:[0~;~] \"@U.VClass.~a@\" ~a"
					   (remove #\` vn/root) ;; vn/root: see get-root-override-features()
					   root
					   syntax-type
					   feature-string))))))))))
	     feature-hash)
    written-p))

;; assigns each template its sub-ids, given a an id and a feature pattern
(defun partition-root-features (list)
  (let ((packed-p nil))
    (values
     (collecting
       (let ((template-list ())
	     #+ignore(packed-p (list nil))) ;; set to (list T) when first packing is observed
	 (dolist (pv+sub-id.templates list)
	   (destructuring-bind (pv sub-id . templates) pv+sub-id.templates
	     (dolist (template templates)
	       (push (list pv sub-id) (getf template-list (intern template :keyword))))))
	 ;;(print template-list)
	 (loop for (template pv+sub-ids) on template-list by #'cddr
	    do (cond ((null (cdr pv+sub-ids))
		      (destructuring-bind (pv sub-id) (car pv+sub-ids)
			(collect (list (string template)
				       pv
				       sub-id
				       (format nil "~a" sub-id)
				       ))))
		     (t
		      (let ((cpv nil)
			    (sub-ids ()))
			(loop for (pv sub-id) in pv+sub-ids
			   do (pushnew sub-id sub-ids)
			   (when (cdr sub-ids) (setf packed-p t))
			   ;;(when (cdr sub-ids) (setf (car packed-p) t))
			   (cond ((null cpv)
				  (setf cpv pv))
				 ((string= cpv pv)
				  nil)
				 (t
				  (setf cpv "`*·")))) ;; '*' has to be escaped for XLE
			(collect (list (string template) cpv sub-ids (abbreviate-sub-ids sub-ids) ))))))))
     packed-p)))

(defun abbreviate-sub-ids (sub-ids)
  (with-output-to-string (stream)
    (let ((sub-ids (sort sub-ids #'<)))
      (loop for (sub-id . rest) on sub-ids
	 with range-start = t
	 and range-start-sub-id = nil
	 and prev-sub-id = nil
	 do (cond (range-start
		   (when prev-sub-id (write-char #\- stream))
		   (write sub-id :stream stream)
		   (setf prev-sub-id sub-id
			 range-start-sub-id sub-id
			 range-start nil))
		  ((= sub-id (1+ prev-sub-id))
		   (setf prev-sub-id sub-id)
		   (unless rest
		     (write-char #\- stream)
		     (write sub-id :stream stream)))
		  (t
		   (unless (= range-start-sub-id prev-sub-id)
		     (write-char #\- stream)
		     (write prev-sub-id :stream stream))
		   (write-string "`/" stream) ;; '/' has to be escaped for XLE
		   (write sub-id :stream stream)
		   (setf prev-sub-id sub-id range-start-sub-id sub-id)
		   (setf range-start nil)))))))

(defun write-verb-lexicon-regexp (&key tenses (name "all-roots") template-hash templates-only-p
				  max max-count c-root-list id)
  (let ((first-p (make-list (or max-count 1) :initial-element t))
	(max-id 0))
    (when c-root-list
      (setf name (u:concat name "-subset")
	    max nil
	    max-count nil))
    (if id
	(setf max-id id)
	(maphash (lambda (id val)
		   (declare (ignore val))
		   (setf max-id (max id max-id)))
		 fst::*id-to-roots-table*))
    (flet ((write-lexicon (&optional streams)
	     (block done
	       (loop for id from (or id 0) to max-id
		  do (cond ((null max)
			    nil)
			   ((zerop max)
			    (return-from done))
			   (t
			    (decf max)))
		  (when t
		    #+ignore(and (or (null count)
				     (= (mod id max-count) count)))
		    (let* ((count (mod id max-count))
			   (stream (nth count streams))
			   (root-list (gethash id fst::*id-to-roots-table*))
			   (c-root (gethash id fst::*id-to-c-root-table*)))
		      (when (or (null c-root-list) (find c-root c-root-list :test #'string=))
			;;(debug root-list)
			(Print (list :id id :roots c-root root-list))
			(dolist (root root-list)
			  (when (verb-lexicon-regexp id
						     root ;; "Tval" ;; "cer"
						     stream
						     :c-root c-root
						     :tenses tenses
						     :template-hash template-hash
						     :first-p (nth count first-p))
			    (setf (nth count first-p) nil))))))))))
      (cond (templates-only-p
	     (write-lexicon))
	    ((null max-count)
	     (setf max-count 1)
	     (with-open-file (stream (format nil "projects:georgian-morph;regex;~a.regex" name)
				     :direction :output :if-exists :supersede)
	       (write-string "read regex [ " stream)
	       (write-lexicon (list stream))
	       (format stream " ] ;")
	       (write-char #\linefeed stream)
	       (format stream "save stack ~a.fst" name)
	       (write-char #\linefeed stream)
	       (values)))
	    (t
	     (let ((streams nil)
		   (done nil))
	       (unwind-protect
		    (progn (setf streams
				 (loop for i below max-count
				    for stream = (open (format nil "projects:georgian-morph;regex;~a~a.regex" name i)
						       :direction :output :if-exists :supersede)
				    do (write-string "read regex [ " stream)
				    collect stream))
			   (write-lexicon streams)
			   (loop for stream in streams
			      for count from 0
			      do (format stream " ] ;")
			      (write-char #\linefeed stream)
			      (format stream "save stack ~a~a.fst" name count)
			      (write-char #\linefeed stream))
			   (setf done t))
		 (loop for stream in streams
		    when stream
		    do (close stream :abort (null done))))))))))



#+test
(print (gethash 3117 fst::*id-to-roots-table*))
#+test
(print (gethash 3117 fst::*id-to-c-root-table*))

#+test
(write-verb-lexicon-regexp :tenses '(present future conditional conj-future conj-present imperfect
				     aorist optative
				     perfect pluperfect conj-perfect)
			   :name "test-finite-root-list"
			   :count 0
			   :max-count 8)

(defmethod write-xle-templates (template-hash (file string))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (write-xle-templates template-hash stream)))

(defmethod write-xle-templates (template-hash (stream stream))
  (write-line "STANDARD GEORGIAN-VERBS LEXICON (1.0)" stream)
  (terpri stream)
  (maphash (lambda (key value)
	     (destructuring-bind ((id masdar pv gv sub-id-range) . templates) value
	       (let ((masdar (convert-encoding masdar :amirani :unicode))
		     (pv (convert-encoding pv :amirani :unicode)))
		 (debug masdar)
		 (when templates
		   (setf masdar (delete #\( (delete #\) masdar))) 
		   (format stream "~a V XLE " (convert-encoding key :amirani :unicode))
		   (if (equal pv "")
		       (format stream "@(VERB ~a - ~a V~a-~a)"
			       masdar gv id sub-id-range)
		       (format stream "@(VERB ~a ~a ~a V~a-~a)"
			       masdar pv gv id sub-id-range))
		   #||(format stream ";~%")
		   (format stream "~c~c Vpart XLE " #\Tab #\Tab)
		   (if (equal pv "")
		       (format stream "@(VERB ~a - ~a V~a-~a)"
			       masdar gv id sub-id-range)
		       (format stream "@(VERB ~a ~a ~a V~a-~a)"
			       masdar pv gv id sub-id-range))||#))
	       (format stream "; ETC.~%")))
	   template-hash)
  (write-line "----" stream))

#+main ;; verb templates alone; first evaluate verb-transducers.lisp
(write-xle-verb-templates)

(defun write-xle-verb-templates ()
  (write-verb-regex-file)
  (let ((template-hash (make-hash-table :test #'equal)))
    (fst::fetch-id-roots)
    (fst::fetch-tsch-classes)
    (clrhash fst::*feature-cache-base*)
    (write-verb-lexicon-regexp :tenses '(present future conditional conj-future
					 conj-present imperfect
					 aorist optative
					 perfect pluperfect conj-perfect
					 masdar past-part future-part present-part negative-part)
			       :templates-only-p t
			       :template-hash template-hash)
    ;; write templates
    (print :writing-templates)
    (write-xle-templates template-hash "projects:xle;grammars;georgian;georgian-verb-lex.lfg")))

(defun write-noun-regex-file ()
  (with-open-file (stream "projects:georgian-morph;regex;noun.regex" :direction :output :if-exists :supersede)
    (with-file-lines (line "projects:georgian-morph;regex;noun-splice.regex")
      (when (string= line "# <--- automatically generated code will be inserted here --->")
	(return))
      (write-line line stream))
    (format stream "~%# ------ automatically inserted start ---->~2%")
    (let ((*standard-output* stream))
      (load "projects:georgian-morph;noun-transducer.lisp"))
    (format stream "~%# <----- automatically inserted end   -----~2%")
    (let ((write-p nil))
      (with-file-lines (line "projects:georgian-morph;regex;noun-splice.regex")
	(when write-p (write-line line stream))
	(when (string= line "# <--- automatically generated code will be inserted here --->")
	  (setf write-p t))))))

(defun write-guessed-noun-regex-file ()
  (with-open-file (stream "projects:georgian-morph;regex;noun-guessed.regex" :direction :output :if-exists :supersede)
    (with-file-lines (line "projects:georgian-morph;regex;noun-guessed-splice.regex")
      (when (string= line "# <--- automatically generated code will be inserted here --->")
	(return))
      (write-line line stream))
    (format stream "~%# ------ automatically inserted start ---->~2%")
    (let ((*standard-output* stream))
      (load "projects:georgian-morph;noun-transducer.lisp"))
    (format stream "~%# <----- automatically inserted end   -----~2%")
    (let ((write-p nil))
      (with-file-lines (line "projects:georgian-morph;regex;noun-guessed-splice.regex")
	(when write-p (write-line line stream))
	(when (string= line "# <--- automatically generated code will be inserted here --->")
	  (setf write-p t))))))

(defun write-verb-regex-file ()
  (with-open-file (stream "projects:georgian-morph;regex;verb.regex" :direction :output :if-exists :supersede)
    (with-file-lines (line "projects:georgian-morph;regex;verb-splice.regex")
      (when (string= line "# <--- automatically generated code will be inserted here --->")
	(return))
      (write-line line stream))
    (format stream "~%# ------ automatically inserted start ---->~2%")
    (let ((*standard-output* stream))
      (load "projects:georgian-morph;verb-transducer.lisp"))
    (format stream "~%# <----- automatically inserted end   -----~2%")
    (let ((write-p nil))
      (with-file-lines (line "projects:georgian-morph;regex;verb-splice.regex")
	(when write-p (write-line line stream))
	(when (string= line "# <--- automatically generated code will be inserted here --->")
	  (setf write-p t))))))


#+test
(write-noun-regex-file)
#+test
(write-guessed-noun-regex-file)
#+test
(write-verb-regex-file)

#+test
(compile-morphology :c-root-list '("წერ" "კითხვ" "თხოვნ" "ჭერ" "ჭამ" "ხილ") :nouns nil)
#+test
(compile-morphology :c-root-list '("კბენ") :nouns nil)

#+main-ccl ;; 816/938 s (with/without hyperthreading, lists = nil) ;; 7711 s, 12588 s, 12500
(time (compile-morphology :lists nil :nouns nil))

#+main-ccl ;; 
(time (compile-morphology :fetch nil))

#+main-ccl ;;
(time (compile-morphology :lists nil :fetch nil :nouns nil :verbs nil))

#+main-ccl ;;
(time (compile-morphology :lists nil :fetch nil :nouns t :nouns-only t))

#+main-ccl ;; 10269s; 7574s, 7374s, 8246s, 7764s, 26791, 27992, 26565, 15121, 21155, 31191, 35000
(time (compile-morphology))

#+main-ccl ;; 
(time (compile-morphology :foma t))

#+test ;; writes only one!
(write-fst-noun-stems-sql :count 1 :max-count 8)

#+test
(dolist (type '(:masdar :present-part :past-part :future-part :negative-part))
  (write-fst-participle-stems-sql :type type))

#+test
(let* ((max-count 8)
       (proc-list (loop for i from 0 below max-count
		     collect
		     (ccl:process-run-function
		      (format nil "write-noun-stems-~a" i)
		      (lambda (i)
			(with-database-connection ()
			  (write-fst-noun-stems-sql :count i :max-count max-count)))
		      i))))
  (loop while (find-if-not #'ccl::process-exhausted-p proc-list)
     do (sleep 0.5)))

#+test
(progn
  (ccl::cwd "projects:georgian-morph;regex;foma;")
  (let* ((fst "foma")
	 (foma t)
	 (threads 8)
	 (procs (loop for i below threads
		   collect (ccl::run-program fst
					     (if foma
						 (list "-f" (format nil "verb~a.infile" i))
						 (list "-flush" "-f" (format nil "verb~a.infile" i)))
					     :wait nil
					     :output *standard-output*
					     :error :output))))
    (print procs)
    (loop (if (find-if (lambda (proc) (eq (ccl::external-process-%status proc) :running)) procs)
	      (sleep 0.1)
	      (return)))
    (print :done)))

(defun compile-morphology (&key (lists t) (verbs t) (fetch t) (nouns t) (threads 8)
			     c-root-list templates
			     nouns-only foma)
  (let ((fst (if foma "foma" "fst")))
    (with-database-connection ()
      (progn
	(write-verb-regex-file)
	(write-noun-regex-file)
	(write-guessed-noun-regex-file))

      (write-fst-geo-stems)

      (if foma
	  (ccl::cwd "projects:georgian-morph;regex;foma;")
	  (ccl::cwd "projects:georgian-morph;regex;"))
    
      (when (and nouns (not foma))
	(dolist (type '(:masdar :present-part :past-part :future-part :negative-part))
	  (write-fst-participle-stems-sql :type type)))
      (when lists
	(when (and verbs fetch (not foma))
	  (let ((template-hash (make-hash-table :test #'equal))
		(max-count (if c-root-list 1 8)))
	    (fst::fetch-id-roots)
	    (fst::fetch-tsch-classes)
	    (clrhash fst::*feature-cache-base*)
	    (print :write-verb-lexicon-regexp)
	    (write-verb-lexicon-regexp :tenses '(present future conditional conj-future
						 conj-present imperfect
						 aorist optative
						 perfect  iter-perfect1 pluperfect conj-perfect
						 iter-present iter-imperfect iter-aorist
						 iter-perfect
						 imperative-present imperative-aorist)
				       :name "finite-root-list"
				       :template-hash template-hash
				       :max-count max-count
				       :c-root-list c-root-list)
	    (print :finite-root-list)
	    (when templates
	      ;; write templates
	      (write-xle-templates template-hash "projects:xle;grammars;georgian;georgian-verb-lex.lfg")
	      (print :templates))
	    #+allegro
	    (generate-georgian-fst)))
	(unless foma
	  (write-fst-extracted-stems))
	(when verbs
	  (let ((procs (mapcar (lambda (file)
				 (ccl::run-program fst
						   (list "-f" file) :wait nil 
						   :output *standard-output*
						   :error :output))
			       (if c-root-list
				   '("future-part-list-subset.regex"
				     "past-part-list-subset.regex"
				     "present-part-list-subset.regex"
				     "negative-part-list-subset.regex"
				     "masdar-list-subset.regex"
				     "finite-root-list-subset.regex")
				   '("extracted-noun-adj.regex"
				     "extracted-adv.regex"
				     "extracted-interj.regex"
				     "geonames-list.regex"
				     "future-part-list.regex"
				     "past-part-list.regex"
				     "present-part-list.regex"
				     "negative-part-list.regex"
				     "masdar-list.regex"
				     "finite-root-list0.regex" "finite-root-list1.regex"
				     "finite-root-list2.regex"
				     "finite-root-list3.regex" "finite-root-list4.regex"
				     "finite-root-list5.regex"
				     "finite-root-list6.regex" "finite-root-list7.regex")))))
	    (print procs)
	    (loop (if (find-if (lambda (proc) (eq (ccl::external-process-%status proc) :running)) procs)
		      (sleep 0.1)
		      (return)))
	    (print :verb-lists-done))))
      (when (and nouns (not foma))
        #+moved
	(dolist (type '(:masdar :present-part :past-part :future-part :negative-part))
	  (write-fst-participle-stems-sql :type type))
	(Print :writing-noun-stems)
	(let* ((max-count threads)
	       (proc-list (loop for i from 0 below max-count
			     collect
			       (ccl:process-run-function
				(format nil "write-noun-stems-~a" i)
				(lambda (i)
				  (with-database-connection ()
				    (write-fst-noun-stems-sql :count i :max-count max-count)))
				i))))
	  (loop while (find-if-not #'ccl::process-exhausted-p proc-list)
	     do (sleep 0.5))))
      (when nouns
	(let ((procs (append 
		      (loop for i below threads
			 collect (ccl::run-program
				  fst (list "-f" (format nil "noun-list~a.regex" i))
				  :wait nil
				  :output *standard-output*
				  :error :output))
		      (loop for type in '(:masdar :present-part :past-part :future-part :negative-part)
			 collect (ccl::run-program
				  fst 
				  (list "-f" (format nil "~(~a~)-list.regex" type))
				  :wait nil
				  :output *standard-output*
				  :error :output)))))
	  (print procs)
	  (loop (if (find-if (lambda (proc) (eq (ccl::external-process-%status proc) :running)) procs)
		    (sleep 0.1)
		    (return)))
	  (print :done)))
      (ccl::run-program fst (list "-f" "noun-productive.regex") :wait t)
      (ccl::run-program fst (list "-f" "numbers.regex") :wait t)
      ;; writes noun morphology to noun.fst
      #+ignore
      (ccl::run-program fst (list "-f" "noun-all.regex")
			:wait t
			:output *standard-output*
			:error :output)
      (when nouns
        (ccl::run-program fst
                          (if foma
                              (list "-f" "noun.regex")
                              (list "-flush" "-f" "noun.regex"))
                          :wait t
                          :output *standard-output*
                          :error :output))
      (when nouns-only (return-from compile-morphology))
      (print :verbs)
      (cond (c-root-list
	     (ccl::run-program fst (list "-f" "verb-subset.infile") :wait t
			       :output *standard-output* :error :output)
	     )
            ((not verbs)
             nil)
	    ((>= threads 8)
	     (let ((procs (loop for i below threads
			     collect (ccl::run-program fst
						       (if foma
							   (list "-f" (format nil "verb~a.infile" i))
							   (list "-flush" "-f" (format nil "verb~a.infile" i)))
						       :wait nil
						       :output *standard-output*
						       :error :output))))
	       (print procs)
	       (loop (if (find-if (lambda (proc) (eq (ccl::external-process-%status proc) :running)) procs)
			 (sleep 0.1)
			 (return)))
	       (print :done)))
	    ((= threads 4)
	     (let ((procs (mapcar (lambda (file)
				    (ccl::run-program fst
						      (list "-f" file) :wait nil :output *standard-output* :error :output))
				  '("verb0.infile" "verb1.infile" "verb2.infile" "verb3.infile"))))
	       (print :verbs-0-3)
	       (print procs)
	       (loop (if (find-if (lambda (proc) (eq (ccl::external-process-%status proc) :running)) procs)
			 (sleep 0.1)
			 (return)))
	       (print :done))
	     (let ((procs (mapcar (lambda (file)
				    (ccl::run-program fst
						      (list "-f" file) :wait nil :output *standard-output* :error :output))
				  '("verb4.infile" "verb5.infile" "verb6.infile" "verb7.infile"))))
	       (print :verbs-4-7)
	       (print procs)
	       (loop (if (find-if (lambda (proc)
				    (eq (ccl::external-process-%status proc) :running))
				  procs)
			 (sleep 0.1)
			 (return)))
	       (print :done)))
	    ((= threads 3)
	     (let ((procs (mapcar (lambda (file)
				    (ccl::run-program fst
						      (list "-f" file) :wait nil :output *standard-output* :error :output))
				  '("verb0.infile" "verb1.infile" "verb2.infile"))))
	       (print procs)
	       (loop (if (find-if (lambda (proc) (eq (ccl::external-process-%status proc) :running)) procs)
			 (sleep 0.1)
			 (return)))
	       (print :done))
	     (let ((procs (mapcar (lambda (file)
				    (ccl::run-program fst (list "-f" file) :wait nil :output *standard-output* :error :output))
				  '("verb3.infile" "verb4.infile" "verb5.infile"))))
	       (print procs)
	       (loop (if (find-if (lambda (proc) (eq (ccl::external-process-%status proc) :running)) procs)
			 (sleep 0.1)
			 (return)))
	       (print :done))
	     (let ((procs (mapcar (lambda (file)
				    (ccl::run-program fst
						      (list "-f" file)
						      :wait nil :output *standard-output* :error :output))
				  '("verb6.infile" "verb7.infile"))))
	       (print procs)
	       (loop (if (find-if (lambda (proc) (eq (ccl::external-process-%status proc) :running)) procs)
			 (sleep 0.1)
			 (return)))
	       (print :done))))
   
      (print :verbs-done)
      
      (cond (c-root-list
	     (ccl::run-program fst (list "-f" "syntax-subset.regex") :wait t :output *standard-output* :error :output))
	    (t
	     (print :syntax)
	     (let ((proc (ccl::run-program fst
					   (if foma 
					       (list "-f" "syntax.regex")
					       (list "-flush" "-f" "syntax.regex"))
					   :wait nil
					   :output *standard-output* :error :output)))
	       (loop (if (eq (ccl::external-process-%status proc) :running)
			 (sleep 1)
			 (return))))
	     (let ((proc (ccl::run-program fst (list "-f" "compound-noun.regex") :wait nil
					   :output *standard-output* :error :output)))
	       (loop (if (eq (ccl::external-process-%status proc) :running)
			 (sleep 1)
			 (return))))
	     (let ((proc (ccl::run-program fst (list "-f" "noun-guessed.regex") :wait nil
					   :output *standard-output* :error :output)))
	       (loop (if (eq (ccl::external-process-%status proc) :running)
			 (sleep 1)
			 (return))))))
      (print :all-done))))


;;(ccl::run-program "fst" (list "-f" "gaga.regex") :wait nil :output *standard-output* :error :output)

#+test
(dolist (root '("cer" "laparak"))
  (verb-filter-regexp (inv-convert root) *standard-output*))

#+test
(dolist (root '("cqevl"))
  (verb-filter-regexp (inv-convert root) *standard-output*))
;; nouns

#+test
(maphash (lambda (stem features-list) (print (list stem features-list))) *noun-table*)

;; see also match-stems()

#+test
(write-fst-noun-lexicon *standard-output*)

(defun convert-val (val)
  (delete #\. (copy-seq (if (stringp val)
			    (convert val)
			    (string-downcase (convert val))))))

#+test
(load-noun-table)

#+main
(with-open-file (stream "projects:georgian-morph;regex;noun-list.regex" :direction :output :if-exists :supersede)
  (write-fst-noun-stems stream))

#+test
(write-fst-noun-stems *standard-output*)

(defparameter *cat-list* ())
(defparameter *subcat-list* ())

(defun resolve-noun-cat-flag-diacritics (stream)
  (write-string "define ResolveCatFlagDiacritics " stream)
  (dolist (pair *flag-diacritics*)
    (when (string-equal (car pair) "cat")
      (pushnew (cdr pair) *cat-list* :test #'string-equal))
    (when (string-equal (car pair) "subcat")
      (pushnew (cdr pair) *subcat-list* :test #'string-equal)))
  (dolist (cat *cat-list*)
    (let ((cats (mapcar #'string-capitalize (split (string-downcase cat) #\+ nil nil t))))
      (format stream " [ \"@R.Cat.~a@\" [~{\"+~a\"~}]:0 | \"@D.Cat.~a@\" ]~% "
              cat cats cat)))
  (dolist (cat *subcat-list*)
    (let ((cats (mapcar #'string-capitalize (split (string-downcase cat) #\+ nil nil t))))
      (format stream " [ \"@R.SubCat.~a@\" [~{\"+~a\"~}]:0 | \"@D.SubCat.~a@\" ]~% "
              cat cats cat)))
  (write-line ";" stream)
  (values))

:eof
