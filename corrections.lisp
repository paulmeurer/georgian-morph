;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10 -*-

;; paul.meurer@uni.no
;; Uni Research
;; http://www.uni.no/


(in-package :fst)

(defparameter *tokenizer* nil)
(defparameter *og-analyzer* nil)
(defparameter *ng-analyzer* nil)
(defparameter *mg-analyzer* nil)

(defun init-transducers (&optional (transducer-dir "projects:gnc;morphology;fst;"))
  (setf *tokenizer*
	(make-instance 'cl-fst:fst-tokenizer :token-boundary #.(string #\newline)
		       :file (u:concat transducer-dir "geo-tokenize.fst"))
	*og-analyzer*
	(loop for name
	   in '("georgian-morph-og"
		"georgian-morph-ng"
		;;"georgian-comp-ng"
		;;"georgian-noun-guessed"
		)
	   for file = (u:concat transducer-dir name ".fst")
	   do (format t "~&loading: ~s~%" file) 
	   when (probe-file file)
	   collect (print (make-instance 'cl-fst:fst-net :file file)))
	*mg-analyzer*
	(loop for name
	   in '("georgian-morph-mg"
		"georgian-morph-ng"
		;;"georgian-comp"
		)
	   for file = (u:concat transducer-dir name ".fst")
	   do (format t "~&loading: ~s~%" file) 
	   when (probe-file file)
	   collect (print (make-instance 'cl-fst:fst-net :file file)))
	*ng-analyzer*
	(loop for name
	   in '("georgian-morph-ng"
		"georgian-comp-ng"
		#+ignore "georgian-noun-guessed"
		"foreign-morph")
	   for file = (u:concat transducer-dir name ".fst")
	   do (format t "~&loading: ~s~%" file) 
	   when (probe-file file)
	   collect (print (make-instance 'cl-fst:fst-net :file file)))))

(init-transducers)

(defun lookup-morphology (word &key (variety :og))
  (let ((lemmas+features ()))
    (cl-fst:fst-lookup
     (ecase variety
       (:og *og-analyzer*)
       (:mg *mg-analyzer*)
       (:ng *ng-analyzer*))
     (remove #\Armenian_Modifier_Letter_Left_Half_Ring
	     (remove #\‹ (remove #\› (remove #\\ word))))
     (lambda (w l+f)
       (declare (ignore w))
       ;; strip some markers and features
       (dolist (reading (u:split l+f #\newline nil nil t))
	 (let* ((f-start (position #\+ reading))
		(end (or #+disabled end f-start))
		(features (when f-start (subseq reading f-start)))
		(vpart-p (eq (string< "+VPart" features) 6))
		;; remove frame from VPart
		(frame-start (when vpart-p (position #\< features :from-end t)))
		(features (if frame-start (subseq features 0 (1- frame-start)) features))
		(reading (subseq reading 0 end)))
	   ;;(when hyphenp (setf reading (delete #\- reading))) ;; only in verb
	   (pushnew (list reading
			  (if features (subseq (substitute #\space #\+ features) 1) "")
			  nil  ;; flag
			  nil) ;; trace (used CG rules)
		    lemmas+features :test #'equal)))))
    lemmas+features))

#+test
(print (lookup-morphology "დამწერ"))

;;; Qual

#+done
(with-database-connection ()
  (with-transaction ()
    (let ((now (get-universal-time)))
      (with-file-lines (line "lisp:projects;georgian-morph;regex;qual1.txt")
	(setf line (string-right-trim "" line))
	(unless (char= (char line 0) #\#)
	  (destructuring-bind (code stem) (split line #\tab)
	    (cond ((eq (string< "qa" code) 2)
		   #+done
		   (update-records [morph noun-features]
				   :av-pairs `(([features] "+N+Anim+Qual")
					       ([comment] "extracted-Qual")
					       ([date] ,now))
				   :where [and [= [stem] ?stem]
					       [= [pos] "n"]
					       [or [null [features]]
						   [not [like [features] "%Prop%"]]]])
		   #+test
		   (let ((rows (select stem [features]
				       :from [morph noun-features]
				       :where [and [= [stem] ?stem]
						   [= [pos] "n"]
						   [or [null [features]]
						       [not [like [features] "%Prop%"]]]]
				       :flatp t)))
		     (when rows (print rows))))
		  ((eq (string< "q" code) 1)
		   #+done
		   (update-records [morph noun-features]
				   :av-pairs `(([features] "+N+Qual")
					       ([comment] "extracted-Qual")
					       ([date] ,now))
				   :where [and [= [stem] ?stem]
					       [= [pos] "n"]
					       [or [null [features]]
						   [not [like [features] "%Prop%"]]]])
		   #+test
		   (let ((rows (select stem [features]
				       :from [morph noun-features]
				       :where [and [= [stem] ?stem]
						   [= [pos] "n"]
						   [or [null [features]]
						       [not [like [features] "%Prop%"]]]]
				       :flatp t)))
		     (when rows (print rows)))))))))))


;;; Participles

#+test ;; try fixing conj. class, not yet done
(with-database-connection ()
  (do-query ((stem)
	     [select [stem]
		     :from [morph participle]
		     :where [and [= [code] "B"]]])
    (print stem)))

;; has to be done for a: 240 for PastPart, 180 PresPart, 600 FutPart, 110 NegPart
#+done
(let ((pos-list '(;;"p.p."
		  ;;"p.a."
		  ;;"p.f."
		  ;;"p.n."
		  ;;"masd"
		  "a"
		  ))
      (now (get-universal-time)))
  (with-database-connection ()
    (with-transaction ()
      (do-query ((stem)
		 [select [stem]
			 :from [morph noun-features]
			 :where [and [in [pos] ?pos-list]
				     [or [null [comment]]
					 [not [= [comment] "wrong-pos"]]]]])
	(when (find-if (lambda (reading)
			 (search
			  ;;"PastPart"
			  ;;"PresPart"
			  ;;"FutPart"
			  "NegPart"
			  ;;"Masdar"
			  (cadr reading)))
		       (lookup-morphology stem :variety :ng))
	  #+test
	  (update-records [morph noun-features]
			  :av-pairs `(([comment] "verbal-morph")
				      ([date] ,now))
			  :where [and [= [stem] ?stem]
				      [in [pos] ?pos-list]
				      [or [null [comment]]
					  [not [= [comment] "wrong-pos"]]]])
      	  (print stem))))))


#+done
(let ((type :past-part ;; :present-part ;; :negative-part ;; :future-part
	))
  (do-query ((stem1 pv id sub-id code)
	     [select [stem] [pv] [participle id] [participle sub-id] [code]
		     :from [morph participle]
		     :left-join [morph verb-paradigm]
		     :on [and [= [participle id] [verb-paradigm id]]
			      [= [participle sub-id] [verb-paradigm sub-id]]]
		     :where [= [type] ?type]])
    (let ((stem (if (char= (char stem1 0) #\*)
		    (concat pv (subseq stem1 1))
		    stem1)))
      (when (and (select [stem]
			 :from [morph noun-features]
			 :where [and [= [stem] ?stem]
				     [= [pos] "a"]
				     [or [null [comment]]
					 [and [not [= [comment] "verbal-morph"]]
					      [not [= [comment] "wrong-pos"]]]]])
		 (not (select [stem]
			      :from [morph noun-features]
			      :where [and [= [stem] ?stem]
					  [= [pos] "p.p."]
					  [or [null [comment]]
					      [not [= [comment] "wrong-pos"]]]])))
	(print stem)
	#+orig
	(update-records [morph participle]
			:av-pairs '(([wrong] :true))
			:where [and [= [id] ?id]
				    [= [sub-id] ?sub-id]
				    [= [type] ?type]
				    [= [stem] ?stem1]
				    [= [code] ?code]])
      
	))))



:eof