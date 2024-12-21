;;;-*- Mode: Lisp; Package: TRANSDUCER -*-

(in-package :fst)

#|

morph-tense-template()
used in feature-template()

|#

;; "projects:georgian-morph;parsed-verb-entries.txt"

(defun extract-bracketed-pr-ids (file)
  (collecting
    (with-open-file (stream file)
      (loop for form = (read stream nil nil)
	 while form
	 do (dolist (paradigm (cdr form)) 
	      (destructuring-bind (&key id pr &allow-other-keys) paradigm
		(when (and (> (length pr) 0)
			   (char= (char pr 0) #\[))
		  (collect (mapcar #'parse-integer (split id #\-))))))))))

;; not used?
(defun override-features (c-root full-f-list gv) (break)
  #+debug(print (list c-root :full-f-list full-f-list :gv gv))
  (collecting
    (dolist (tense '(present imperfect conj-present iter-present
		     iter-imperfect imperative-present ;; I
                     future conditional conj-future
                     aorist optative iter-aorist imperative-aorist ;; II
                     perfect pluperfect conj-perfect iter-perfect iter-perfect1 ;; III
                     ))
      (dolist (tense-f-list full-f-list)
        (when (find-if (lambda (equation)
                         (and (eq (car equation) 'tense)
                              (or (eq (cadr equation) tense)
                                  (and (parser::extended-list-p (cadr equation))
                                       (find tense (parser::extended-list-form (cadr equation)))))))
                       tense-f-list)
          (collect (list* tense
                          (list 'c-root c-root)
                          (find-tense-override-features c-root tense-f-list gv tense))))))))

(defun fill-template (template overrides &key except)
  #+debug(print (list :template template :overrides overrides :except except)) 
  (collecting
    (dolist (equation template)
      (collect (or ;;(find (car equation) overrides :key #'car :test #'equal)
                   (find-if (lambda (o-eq)
                              (and (equal (car equation) (car o-eq))
                                   (cadr o-eq)))
                            overrides)
                   equation)))
    ;; collect equations in overrides that are not mentioned in the template, except those in the except-list
    (dolist (override-equation overrides)
      (unless (or (find (car override-equation) except :test #'equal)
                  (find (car override-equation) template :key #'car :test #'equal))
        (collect override-equation)))))

;; not used?
(defun find-tense-override-features (c-root tense-f-list gv tense) (break)
  (let ((paradigm-replacement (find 'paradigm-replacement tense-f-list :key #'car)))
    (if paradigm-replacement
      (progn (print (list :tense-f-list tense-f-list))
             (list (find 'tense tense-f-list :key #'car)
                   (find 'vn tense-f-list :key #'car)
                   (find 'paradigm-replacement tense-f-list :key #'car)))
      (let* ((morph-type (cadr (assoc 'morph-type tense-f-list)))
             (template (feature-template c-root gv tense morph-type)))
        #+debug(print (list :gv gv :morph-type morph-type :tense tense :template template
                            :tense-f-list tense-f-list))
        (if template
          (collecting
            (dolist (tpl-eq template)
              (dolist (f-eq tense-f-list)
                (when (and (equal (car tpl-eq) (car f-eq))
                           (not (equal (cadr tpl-eq) (cadr f-eq))))
                  (collect f-eq)))))
          (warn "Template for ~s not found." gv))))))

(defvar *parsed-verb-table* (dat:make-string-tree))

#+obsolete?
(defun get-paradigm-features (root-list paradigm-list)
  (collecting
    (let* ((c-root (car root-list))
           (roots (change-ev-roots root-list (mapcar #'remove-root-number root-list))))
      ;;(format t "~%c-root: ~a, root-list: ~a, expanding: ~s" c-root root-list paradigm-list)
      (dolist (form (expand-alternatives paradigm-list roots))
        ;;(format t "~%expanded form: ~s" form)
        (let ((root-feature-list (build-root-feature-list form roots c-root)))
          (collect-append root-feature-list))))))

#+test
(print (feature-template "სვლ" "IV1" nil 'active))

(defun normalize-gv (gv)
  (destructuring-bind (gv &optional comment) (split gv #\Space 2)
    (declare (ignore comment))
    (with-output-to-string (stream)
      (loop for c across gv
            do (let ((pos (position c "1234567¸¹°")))
                 (write-char (if pos (char "1234567890" pos) c) stream))))))

(defun gv-xle-template (gv &key masdar pv id)
  (let ((template
         (cadr (find gv `(;; transitive
                          ("T1" "V-TRANS-SUBJ-OBJ3")
                          ("T1 (OR)" "V-TRANS-SUBJ3-OBJ")
                          ("T2" "V-TRANS-S-SUBJ-OBJ3")
                          ("T2 (OR)" "V-TRANS-S-SUBJ3-OBJ")
                          ("T3" "V-TRANS-O-SUBJ-OBJ3")
                          ("T3 (OR)" "V-TRANS-O-SUBJ3-OBJ")
                          ("T3 (nur mit i.O.)" "V-INTR-O-SUBJ")
                          ("T4" "V-TRANS-SUP-SUBJ-OBJ3")
                          ("T4 (OR)" "V-TRANS-SUP-SUBJ3-OBJ")
                          ("T4 (nur mit i.O.)" "V-INTR-SUP-SUBJ")
                          ("T5" "V-DITRANS-SUBJ-OBJ3-OBJ3")
                          ("T5 (OR)" "V-DITRANS-SUBJ3-OBJ-OBJ3")
                          ("T5 (nur mit i.O.)" "V-INTR-SUBJ-OBJ3")
                          ("T5 (OR) (nur mit i.O.)" "V-INTR-SUBJ3-OBJ")
                          ;; causative
                          ("KT" "V-CAUS-SUBJ-OBJ-OBJ")
                          ("KT (OR)" "V-CAUS-SUBJ-OBJ-OBJ")
                          ("KT (nur mit i.O.)" "V-CAUS-SUBJ-OBJ") ;; ??
                          ;; unergative
                          ("MV" "V-INTR-SUBJ")
                          ;; passive/unaccusative
                          ("P1" "V-INTR-SUBJ")
                          ("P2" "V-INTR-SUBJ")
                          ("P3" "V-INTR-SUBJ")
                          ;; relative passive
                          ("RP1" "V-INTR-SUBJ-OBJ")
                          ("RP1 (ohne i.O.)" "V-INTR-SUBJ")
                          ("RP1 (mit" "V-INTR-SUBJ-OBJ3")
                          ("RP1 (OR)" "V-INTR-SUBJ3-OBJ")
                          ("RP2" "V-INTR-SUBJ-OBJ3")
                          ("RP2 (OR)" "V-INTR-SUBJ3-OBJ")
                          ("RP3" "V-INTR-O-SUBJ")
                          ("RP3 (OR)" "V-INTR-O-SUBJ")
                          ("RP4" "V-INTR-SUP-SUBJ")
                          ("RP4 (OR)" "V-INTR-SUP-SUBJ")
                          ("RP5" "V-INTR-SUBJ-OBJ3")
                          ("RP5 (OR)" "V-INTR-SUBJ3-OBJ")
                          ("RP6" "V-INTR-O-SUBJ")
                          ("RP6 (OR)" "V-INTR-O-SUBJ")
                          ("RP7" "V-INTR-SUP-SUBJ")
                          ("RP7 (OR)" "V-INTR-SUP-SUBJ")
                          ("RP7 (ohne i.O.)" "V-INTR-SUBJ")
                          ;; relative medium
                          ("RM1" "V-INTR-SUBJ-OBJ3")
                          ("RM1 (OR)" "V-INTR-SUBJ3-OBJ")
                          ("RM2" "V-INTR-O-SUBJ")
                          ("RM2 (OR)" "V-INTR-O-SUBJ")
                          ("RM3" "V-INTR-SUP-SUBJ")
                          ("RM3 (OR)" "V-INTR-SUP-SUBJ")
                          ("RM4" "V-INTR-SUBJ-OBJ3")
                          ("RM4 (OR)" "V-INTR-SUBJ3-OBJ")
                          ;; indirect
                          ("IV1" "V-IV-SUBJ-OBJ3")
                          ("IV2" "V-IV-O-SUBJ")
                          ("IV3" "V-IV-SUP-SUBJ")
                          ("IV4" "V-IV-SUBJ-OBJ3") ; rel. pass. ე
                          ;; passive of state
                          ("ZP1" "V-STATE-PASS-SUBJ")
                          ("ZP2" "V-STATE-PASS-O-SUBJ")
                          ("ZP3" "V-STATE-PASS-SUP-SUBJ"))
                     :key #'car :test #'string=))))
    (if template
      (let ((pv-p (and pv (not (or (eq pv '-) (equal pv "-"))))))
        (values (cons (format nil "~a~a~a-~a"
                              (if (null pv-p)
                                ""
                                (convert pv) )
                              (if pv-p "-" "")
                              (convert masdar)
                              id)
                      (format nil "XLE @(~a ~a~a~a ~a ~a V~a)"
                              template
                              (if (equal pv "-")
                                "?"
                                (if pv-p (convert pv) ""))
                              (if pv-p "-" "")
                              (convert masdar)
                              (or (convert pv) '-)
                              (normalize-gv gv)
                              id))
                (convert masdar)
                (list template ;; simplify!
                      (format nil "~a~a~a"
                              (if (equal pv "-")
                                "?"
                                (if pv-p (convert pv) ""))
                              (if pv-p "-" "")
                              (convert masdar))
                      (or (convert pv) '-)
                      (normalize-gv gv)
                      (format nil "V~d" id))))
      (warn "No XLE template found for ~s." gv))))

#+test
(gv-xle-template "T1" :masdar "cera" :id 234 :pv "da")

(defun root-vowel (root)
  (if (find-if (lambda (c) (find c "აეიოუჱ")) root)
    '+ '-))

(defun morph-tense-template (root morph-type tense gv
			     &key (relation 'absolute) vv fut-vv passive-sfx obj-3-pfx maybe-causative)
  #+debug(print (list root morph-type tense gv relation vv passive-sfx obj-3-pfx))
  (case tense
    (past-part `((finite -)
                 (pv -)
                 (sf -)
                 (part-pfx -)
                 (part-sfx -)))
    (future-part `((finite -)
                   (pv -)
                   (sf -)
                   (part-pfx -)
                   (part-sfx -)))
    (present-part `((finite -)
                    (pv -)
                    (part-pfx "მ")
                    (vv -)
                    (sf -)
                    (part-sfx -)))
    (negative-part `((finite -)
                     (pv -)
                     (sf -)
                     (part-pfx "უ")
                     (part-sfx -)))
    (masdar `((finite -)
              (morph-type non-causative)
              (pv -)
              (sf -)
              (part-pfx -)
              (part-sfx -)
              ;;(stem-type v)
              ))
    (otherwise
     (collecting
       (collect-append
        `((paradigm-replacement nil)
          (vn nil)
          (finite +)
          ((type root-vowel) ,(root-vowel root))
          (morph-type nil)
          ((subj pers) {1 2 3})
          ((obj pers) {1 2 3})
          (pv -)
          (gv ,gv)
          (reduplication -)
          (red-dir-pv "მო")
          ((type subj2-pfx) -)
          ((type subj12-sfx) -)))
       ;;(print (list :root root :root-type (root-type root) :obj-3-pfx obj-3-pfx))
       (let ((obj-3-pfx ;; '- #+old
	      (cond ((null obj-3-pfx)
		     '--)
		    ((eq obj-3-pfx :optional)
		     '-
		     #+ignore
		     (case (root-type root)
		       (null-root '--)	;; - only
		       (otherwise '-)))	;; + is subnorm
		    (t
		     (case (root-type root)
		       (s-root '+) ;; - is subnorm
		       ;;(null-root '--)
		       (otherwise '+-)))) ;; + and - are norm
	       )
             (morph-type (if (parser::extended-list-p morph-type)
                           (cond ((parser::extended-list-equal morph-type {active passive})
                                  'active) ;; prelim
                                 ((parser::extended-list-equal morph-type {passive stative-passive})
                                  'passive)
                                 (t
                                  "Error: not implemented: ~s" morph-type))
                           morph-type)))
         (case morph-type
           (active
            (collect `(sf -))
            (when maybe-causative
              (collect `(caus-sf -)))
            (case tense
              ((present future imperative-present)
               (collect-append
                `(((type subj3-sfx) "ს")
                  ((type obj-3-pfx) ,obj-3-pfx))))
              ((imperfect conditional conj-present conj-future iter-imperfect)
               (collect-append
                `(((type obj-3-pfx) ,obj-3-pfx)
                  ((type pr-st-ext) "დ"))))
              ((aorist optative iter-aorist imperative-aorist)
               (collect-append
                `((passive-sfx -)
                  ((type aorist) weak)
                  ((type aorist-3sg) "ა")
                  ((type obj-3-pfx) ,obj-3-pfx)
                  ((type optative) "ო"))))
              ((perfect pluperfect conj-perfect iter-perfect iter-perfect1)
               (collect-append
                '((nasal-infix -)
                  ((type aorist) weak)
                  ((type aorist-3sg) "ა")
                  ((type optative) "ო")))))
            (when (find gv '("T1" "T2" "T3" "T4" "T5") :test #'equal)
              (case tense
                ((perfect iter-perfect1) nil #+ignore(collect `(,vv "უ")))
                ((pluperfect conj-perfect iter-perfect) nil #+ignore (collect `(vv "ე")))
                (otherwise (collect `(vv ,vv)))))
            (when (find gv '("MV" "RM1" "RM2" "RM3" "RM4") :test #'equal)
              (collect-append
               (case tense 
                 ((future conj-future conditional aorist optative iter-aorist imperative-aorist)
                  `((vv ,(or fut-vv vv))))
                 ((perfect pluperfect conj-perfect iter-perfect iter-perfect1)
                  nil)
                 (otherwise
                  `((vv ,vv)))))
              (when (find tense '(future conj-future conditional))
                (collect `((type subj2-pfx) -)))
              (when (find tense '(present imperfect conj-present 
				  iter-present iter-imperfect imperative-present))
                (collect-append
                 `(((type subj2-pfx) -) ;; aux present only!
                   ))))
            (if (or (find gv '("IV1" "IV2" "IV3" "IV4") :test #'equal)
                    (find tense '(perfect pluperfect conj-perfect iter-perfect iter-perfect1)))
              (collect '(inverted +))
              (collect '(inverted -)))
            (when (find gv '("IV1" "IV2" "IV3" "IV4") :test #'equal)
              (collect-append
               (if (find tense '(present imperfect conj-present iter-present
				 iter-imperfect imperative-present))
                 `((vv ,vv))
                 '((vv -))))))
           (causative
            (collect `(sf -))
            (collect `(caus-sf "ინ"))
            (case tense
              ((present future imperfect conditional conj-present conj-future
			iter-present iter-imperfect imperative-present)
               (collect-append
                `(;((type subj3-sfx) "ს")
                  (vv "ა")
                  (inverted -)
                  ((type obj-3-pfx) ,obj-3-pfx))))
              ((aorist optative iter-aorist imperative-aorist)
               (collect-append
                `((passive-sfx -)
                  (vv "ა")
                  ((type aorist) weak)
                  ((type aorist-3sg) "ა")
                  #-test((type optative) "ო")
                  ((type obj-3-pfx) ,obj-3-pfx)
                  (inverted -))))
              ((perfect pluperfect conj-perfect iter-perfect iter-perfect1)
               (collect-append
                '(;;(nasal-infix -)
                  ((type aorist) weak)
                  ((type aorist-3sg) "ა")
                  #-test((type optative) "ო")
                  (inverted +))))))
           (passive
            (collect-append
             `((relation ,relation)
               (sf -)
               (caus-sf -)))
            (if (find gv '("IV1" "IV2" "IV3" "IV4") :test #'equal)
              (collect '(inverted +))
              (collect '(inverted -)))
            (case tense
              ((perfect pluperfect conj-perfect iter-perfect iter-perfect1)
               (if (eq relation 'absolute)
                 (collect-append ;; past-participle-stem
                  `((part-pfx -)
                    ;;(sf -)
                    (part-sfx -))))
               (collect-append
                `((vv -)
                  (i-infix "ი")
                  (nasal-infix -)
                  ((type subj3-sfx) "ა")
                  ,@(case (root-type root)
                      (s-root
                       '(((type obj-3-pfx) +)))
		      #+ignore
                      (null-root
                       '(((type obj-3-pfx) -)))
                      (otherwise
                       nil)))))
              ((aorist optative iter-aorist imperative-aorist)
               (collect-append
                `((vv ,vv)
                  ((type obj-3-pfx) ,obj-3-pfx)
                  (passive-sfx ,passive-sfx)
                  ;;(nasal-infix -)
                  ((type aorist) weak)
                  ((type aorist-3sg) "ა")
                  ((type optative) "ო")
                  ((type optative-3pl-en) -)
                  ((type ev-sfx) -)))) ;; mixed means alternation in all persons except 3sg (see Tsch. p. 267)
              ((present future)
               (collect-append
                `((vv ,vv)
                  ((type obj-3-pfx) ,obj-3-pfx)
                  ((type subj3-sfx) "ა")
                  (passive-sfx ,passive-sfx))))
              (otherwise
               (collect-append
                `((vv ,vv)
                  ((type obj-3-pfx) ,obj-3-pfx)
                  (passive-sfx ,passive-sfx))))))
           (stative-passive
            (collect-append
             `((relation ,relation)
               (sf -)
               (caus-sf -)))
            (if t;;(eq tense 'present)
              (collect '(inverted -))
              (collect '(inverted +)))
            (case tense
              ((present iter-present)
	       (collect-append
		`((vv ,vv)
		  (passive-sfx "ი")
		  ((type obj-3-pfx) ,obj-3-pfx)
		  ((type subj3-sfx) "ა")
		  )))
              ((perfect pluperfect conj-perfect iter-perfect iter-perfect1) ;; ?? under construction
               (if (eq relation 'absolute)
		   (collect-append ;; past-participle-stem
		    `((part-pfx -)
		      (part-sfx -))))
               (collect-append
                `((vv -)
                  (i-infix "ი")
                  (nasal-infix -)
                  ((type subj3-sfx) "ა")
                  ,@(case (root-type root)
			  (s-root
			   '(((type obj-3-pfx) +)))
			  (null-root
			   '(((type obj-3-pfx) -)))
			  (otherwise
			   nil)))))
              ((aorist optative iter-aorist imperative-aorist)
               (collect-append
                `((vv "ე")
                  ((type obj-3-pfx) ,obj-3-pfx)
                  (passive-sfx -)	;,passive-sfx)
                  ;;(nasal-infix -)
                  ((type aorist) weak)
                  ((type aorist-3sg) "ა")
                  ((type optative) "ო")
                  ((type optative-3pl-en) -)
                  ((type ev-sfx) -)))) ;; mixed means alternation in all persons except 3sg (see Tsch. p. 267)
              (future
               (collect-append
                `((vv "ე")
                  ((type obj-3-pfx) ,obj-3-pfx)
                  ((type subj3-sfx) "ა")
                  (passive-sfx ,passive-sfx))))
              (otherwise
               (collect-append
                `((vv ,vv)
                  ((type obj-3-pfx) ,obj-3-pfx)
                  (passive-sfx ,passive-sfx))))))
           (otherwise
            (format t "~&unknown morph-type: ~a for~{ ~a~}~%" morph-type
                    (list root morph-type tense gv relation vv fut-vv passive-sfx)))))))))

;(pprint (update-root-features "Jdom"))

;(kartuli-morph "ecereboda")
;(kartuli-morph "daiSala")
;(kartuli-morph "datova")

#| obs!
no present parse: gadivleba, roots: (svl di val va vl ved vel vid s ar)
no present parse: gadaivleba, roots: (svl di val va vl ved vel vid s ar)
no present parse: amevleba, roots: (svl di val va vl ved vel vid s ar)
no present parse: amomevleba, roots: (svl di val va vl ved vel vid s ar)
no present parse: gamevleba, roots: (svl di val va vl ved vel vid s ar)
no present parse: gamomevleba, roots: (svl di val va vl ved vel vid s ar)
|#

;(pprint (update-root-features "svl"))
;(generate-paradigm "2032-1" :debug nil :tense 'aorist)
;(generate-paradigm "2032-1" :debug nil :tense 'future)
;(generate-paradigm "2032-1" :debug nil :tense 'perfect)

;(pprint (gethash "cer" *features-table*))

;(u-transduce "mivida" *fst*)


(defun feature-template (root gv tense morph-type)
  ;;(print (list :root root :gv gv :tense tense :morph-type morph-type))
  (let* ((gv-list (split gv #\Space 2))
         (gv (car gv-list))
	 (%gv (intern (if (equal (cadr gv-list) "(ohne i.O.)")
			  (u:concat gv "-OHNE")
			  gv)
		      :keyword)))
    (case %gv
      (:T1
       (morph-tense-template root 'active tense gv :vv '- :obj-3-pfx :optional))
      (:T2
       (morph-tense-template root 'active tense gv :vv "ი"))
      (:T3
       (morph-tense-template root 'active tense gv :vv "უ"))
      (:T4
       (morph-tense-template root 'active tense gv :vv "ა"))
      (:T5
       (morph-tense-template root 'active tense gv :vv '- :obj-3-pfx t :maybe-causative t :relation 'relative))
      ;; causative
      (:KT ;; what about causative of intransitives?
       (morph-tense-template root 'causative tense gv :vv "ა"))
      ;; passive
      (:P1
       (morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "ი"))
      (:P2
       (morph-tense-template root 'passive tense gv :passive-sfx "დ+ებ" :vv '-))
      (:P3
       (morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv '-))
      ;; relative passive
      (:RP1-ohne ;; "(ohne i.O.)")
       (morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "ე"))
      (:RP1
       (morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "ე" :relation 'relative))
      (:RP2
       (morph-tense-template root 'passive tense gv :passive-sfx "დ+ებ" :vv '-
			     :obj-3-pfx t :relation 'relative))
      (:RP3
       (morph-tense-template root 'passive tense gv :passive-sfx "დ+ებ" :vv "უ" :relation 'relative))
      (:RP4
       (morph-tense-template root 'passive tense gv :passive-sfx "დ+ებ" :vv "ა" :relation 'relative))
      (:RP5
       (morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv '- :relation 'relative :obj-3-pfx t))
      (:RP6
       (morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "უ" :relation 'relative))
      (:RP7-ohne ;; "(ohne i.O.)")
       (morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "ა"))
      (:RP7
       (morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "ა" :relation 'relative))
      ;; medium
      (:MV
       (morph-tense-template root morph-type tense gv :vv '- :fut-vv "ი" :obj-3-pfx :optional))
      ;; relative medium
      (:RM1
       (morph-tense-template root morph-type tense gv :vv '- :fut-vv "უ" :relation 'relative
			     :obj-3-pfx t))
      (:RM2
       (morph-tense-template root morph-type tense gv :vv "უ" :relation 'relative))
      (:RM3
       (morph-tense-template root morph-type tense gv :vv "ა" :relation 'relative))
      (:RM4 ; rel. pass. e
       (morph-tense-template root morph-type tense gv :obj-3-pfx t ;; e.g. ჰკითხა
			     :relation 'relative))
      ;; inverted
      (:IV1 ;; might be causative; see amomaGebinebs
       (morph-tense-template root morph-type tense gv :vv '- :obj-3-pfx t))
      (:IV2
       (morph-tense-template root morph-type tense gv :vv "უ" :obj-3-pfx t))
      (:IV3
       (morph-tense-template root morph-type tense gv :vv "ა" :obj-3-pfx t))
      (:IV4
       (morph-tense-template root morph-type tense gv :vv "ე" :passive-sfx "ებ")) ; rel. pass. e
      ;; passive of state
      (:ZP1
       (morph-tense-template root morph-type tense gv :vv '- :obj-3-pfx t))
      (:ZP2
       (morph-tense-template root morph-type tense gv :vv "უ" :relation 'relative :obj-3-pfx t))
      (:ZP3
       (morph-tense-template root morph-type tense gv :vv "ა" :relation 'relative :obj-3-pfx t)))))

#+orig
(defun feature-template (root gv tense morph-type)
  (print (list :root root :gv gv :tense tense :morph-type morph-type))
  (let* ((gv-list (split gv #\Space 2))
         (gv (car gv-list)))
    (cdr (find gv-list
               `(("T1"
                  ,@(morph-tense-template root 'active tense gv :vv '- :obj-3-pfx :optional) ;; დასწერს
                  (a-struct "P<ag,th>")
                  ;;(c-vv -)
                  )
                 ("T2"
                  ,@(morph-tense-template root 'active tense gv :vv "ი")
                  (a-struct "P<ag,ben,th>")
                  ;;(c-vv "i")
                  )
                 ("T3"
                  ,@(morph-tense-template root 'active tense gv :vv "უ")
                  (a-struct "P<ag,ben,th>")
                  ;; (a-struct "P<ag,ben>") for "(nur mit i.O.)"
                  ;(c-vv "u")
                  )
                 ("T4"
                  ,@(morph-tense-template root 'active tense gv :vv "ა")
                  (a-struct "P<ag,th,loc>")
                  ;; (a-struct "P<ag,loc>") for "(nur mit i.O.)"
                  ;(c-vv "a")
                  )
                 ("T5"
                  ,@(morph-tense-template root 'active tense gv :vv '- :obj-3-pfx t :maybe-causative t)
                  (a-struct "P<ag,ben,th>")
                  ;; (a-struct "P<ag,ben>") for "(nur mit i.O.)" and "(OR) (nur mit i.O.)"
                  ;;(c-vv -)
                  )
                 ;; causative
                 ("KT" ;; what about causative of intransitives?
                  ,@(morph-tense-template root 'causative tense gv :vv "ა")
                  (a-struct "P<ag,ben,th>") ;; ??
                  ;; (a-struct "P<ag,ben>") for "(nur mit i.O.)" ??
                  )
                 ;; passive
                 ("P1"
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "ი"))
                 ("P2"
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "დ+ებ" :vv '-))
                 ("P3"
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv '-))
                 ;; relative passive
                 (("RP1" "(ohne i.O.)")
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "ე"))
                 ("RP1"
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "ე" :relation 'relative))
                 ("RP2"
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "დ+ებ" :vv '-
                                          :obj-3-pfx t :relation 'relative))
                 ("RP3"
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "დ+ებ" :vv "უ" :relation 'relative))
                 ("RP4"
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "დ+ებ" :vv "ა" :relation 'relative))
                 ("RP5"
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv '- :relation 'relative :obj-3-pfx t))
                 ("RP6"
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "უ" :relation 'relative))
                 (("RP7" "(ohne i.O.)")
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "ა"))
                 ("RP7"
                  ,@(morph-tense-template root 'passive tense gv :passive-sfx "ებ" :vv "ა" :relation 'relative))
                 ;; medium
                 ("MV"
                  ,@(morph-tense-template root morph-type tense gv :vv '- :fut-vv "ი" :obj-3-pfx :optional))
                 ;; relative medium
                 ("RM1"
                  ,@(morph-tense-template root morph-type tense gv :vv '- :fut-vv "უ" :relation 'relative
                                          :obj-3-pfx t))
                 ("RM2"
                  ,@(morph-tense-template root morph-type tense gv :vv "უ" :relation 'relative))
                 ("RM3"
                  ,@(morph-tense-template root morph-type tense gv :vv "ა" :relation 'relative))
                 ("RM4"
                  ,@(morph-tense-template root morph-type tense gv :relation 'relative)) ; rel. pass. e
                 ;; inverted
                 ("IV1" ;; might be causative; see amomaGebinebs
                  ,@(morph-tense-template root morph-type tense gv :vv '- :obj-3-pfx t))
                 ("IV2"
                  ,@(morph-tense-template root morph-type tense gv :vv "უ" :obj-3-pfx t))
                 ("IV3"
                  ,@(morph-tense-template root morph-type tense gv :vv "ა" :obj-3-pfx t))
                 ("IV4"
                  ,@(morph-tense-template root morph-type tense gv :vv "ე" :passive-sfx "ებ")) ; rel. pass. e
                 ;; passive of state
                 ("ZP1"
                  ,@(morph-tense-template root morph-type tense gv :vv '- :obj-3-pfx t))
                 ("ZP2"
                  ,@(morph-tense-template root morph-type tense gv :vv "უ" :relation 'relative :obj-3-pfx t))
                 ("ZP3"
                  ,@(morph-tense-template root morph-type tense gv :vv "ა" :relation 'relative :obj-3-pfx t)))
               :key #'car
               :test (lambda (gv-list key)
                       (if (stringp key)
                         (string= (car gv-list) key)
                         (and (string= (car gv-list) (car key))
                              (equal (cadr gv-list) (cadr key)))))))))

(defun merge-tenses (f-lists vn)
  (collecting
    (let ((table (make-hash-table :test #'equal :size 12)))
      (dolist (tense-list f-lists)
        (push (car tense-list) (gethash (cdr tense-list) table)))
      (maphash (lambda (f tenses)
                 (collect (list* `(tense ,(parser::make-extended-list :form tenses :char #\{)) `(vn ,vn) f)))
               table))))

(defun override-features-equal (f1 f2)
  (and (equal (car f1) (car f2))
       (equal (cadr f1) (cadr f2))
       (equal (caddr f1) (caddr f2))
       (= (length f1) (length f2))
       (loop for eq1 in (cdddr f1)
             always
             (find-if (lambda (eq2)
                        (and (eq (car eq1) (car eq2))
                             (parser::extended-list-equal (cdr eq1) (cdr eq2))))
                      (cdddr f2)))))

(defparameter *features-table* (make-hash-table :test #'equal))

(defun georgian-char-p (char)
   (<= (char-code #\a) (char-code char) (char-code #\ú)))

(defun xle-verb-name (vn c-root)
  (let* ((vn-end (position-if-not (lambda (c) (or (georgian-char-p c)
                                                  (find c "()[]")))
                                  vn))
         (vn (remove #\( (remove #\) (remove #\[ (remove #\] (subseq vn 0 vn-end)))))))
    (if (zerop (length vn)) (concat (string-right-trim "1234567¸¹" c-root) "#") vn)))

(defun get-pv (features)
  (cadr (assoc 'c-pv (car features))))

(defun add-root-features (c-root &optional (stream1 *standard-output*) stream2 stream3)
  (let* ((root-entry (dat::string-tree-get *parsed-verb-table* c-root))
         (roots (car root-entry))
         (prev-xle-template nil)
         (templates ()))
    (dolist (paradigm-list (cdr root-entry))
      #+debug(print paradigm-list)
      (let* ((gv (getf paradigm-list :gv))
             (vn (getf paradigm-list :vn))
             (vn (xle-verb-name vn c-root))
             (id (getf paradigm-list :id))
             (dir (getf paradigm-list :dir)))
        (unless (or (getf paradigm-list :ignore)
                    (eq dir :first-second))
          (unless (gethash id *paradigm-table*)
            (setf (gethash id *paradigm-table*) (list gv c-root)))
          (let ((paradigm-features
                 (get-paradigm-features roots paradigm-list))) ;; this calls build-root-feature-list()
            #+debug(print (list :paradigm-features paradigm-features))
            (dolist (root-features paradigm-features)
              (destructuring-bind (root . features) root-features
                #+debug(print (list root gv))
                (let* ((root-id (parse-integer (car (split id #\-))))
                       (pv (get-pv features))
                       (stream (cond ((< root-id 1300) stream1)
                                     ((<= 1300 root-id 2599) (or stream2 stream1))
                                     (t (or stream3 stream2 stream1)))))
                  (multiple-value-bind (xle-template masdar template-list)
                                       (gv-xle-template gv :pv pv :masdar (xle-verb-name vn c-root) :id id)
                    #+debug(print (list :pv pv))
                    (when xle-template
                      (when (not (equal prev-xle-template xle-template))
                        (destructuring-bind (key . template) xle-template
                          (when (eq stream t) (terpri))
                          (when stream
                            (cond ((eq (char gv 0) #\T)
                                   (format stream "~a V ~a;~c" key template #\Linefeed)
                                   (when (eq stream t) (terpri))
                                   (format stream "~c~c Vpart ~a; ETC.~c" #\Tab #\Tab template #\Linefeed))
                                  (t
                                   (format stream "~a V ~a; ETC.~c" key template #\Linefeed)))))
                        (push (list id masdar template-list) templates)
                        (setf prev-xle-template xle-template)) 
                      (pushnew root (cddr (gethash id *paradigm-table*)) :test #'string=)
                      (dolist (of (merge-tenses (override-features c-root features (getf paradigm-list :gv)) vn))
                        #+debug(print (list* root gv id pv of))
                        (pushnew (list* gv id pv of)
                                 (gethash root *features-table*)
                                 :test #'override-features-equal)))))))))))
    templates))

(defun update-root-features (c-root &key (update-templates-p t))
  (let* ((parsed-verb-entry (dat:string-tree-get *parsed-verb-table* c-root))
         (roots (car parsed-verb-entry))
         (id (getf (cadr parsed-verb-entry) :id))
         (id (subseq id 0 (1+ (position #\- id)))))
    ;; remove old root features
    (dolist (r roots)
      (setf (gethash (remove-root-number r) *features-table*)
            (delete-if (lambda (subentry)
                         (eq (string< id (cadr subentry)) (length id)))
                       (gethash (remove-root-number r) *features-table*) )))
    (let ((templates (add-root-features c-root nil)))
      (add-root-participles c-root) ;; participle templates are written in write-xle-verb-lexicon-entry()
      (when update-templates-p
        (dolist (template-list templates)
          (setf (dat:string-tree-get *xle-template-table* (car template-list))
                (cdr template-list)))))))

;; obs: almost the same as update-root-features()
(defun add-corrected-root-features (paradigm-id features &key (override-p t))
  (let ((roots (cddr (gethash paradigm-id *paradigm-table*))))
    (when (null roots) (error "Paradigm id ~a does not exist" paradigm-id))
    ;; remove paradigm entries for root
    (when override-p
      (dolist (root roots)
        (setf (gethash root *features-table*)
              (delete-if (lambda (gv-features)
                           (equal (cadr gv-features) paradigm-id))
                         (gethash root *features-table*)))))
    ;; add new features
    (dolist (f-list features)
      (destructuring-bind (root . gv-features) f-list
        (push gv-features (gethash root *features-table*))))))

(defun print-dag (dg)
  (labels ((pd (dg level)
             (loop for av-pair in (cdr dg)
                   with first = t
                   do
                   (if first
                     (setf first nil)
                     (dotimes (i level) (write-char #\Space)))
                   (write (car av-pair))
                   (write-char #\space)
                   (cond ((atom (cdadr av-pair))
                          (write (cdadr av-pair))
                          (terpri))
                         (t
                          (pd (cadr av-pair)
                              (+ level (length (string (car av-pair))) 1)))))))
    (terpri)
    (pd dg 0))
  (values))


(defun tense-code (tense-list)
  (case tense-list
    (present "Pres")
    (iter-present "IterPres")
    (imperative-present "ImpvPres")
    (future "Fut")
    (imperfect "Impf")
    (iter-imperfect "IterImpf")
    (conditional "Cond")
    (conj-present "ConjPres")
    (conj-future "ConjFut")
    (aorist "Aor")
    (optative "Opt")
    (iter-aorist "IterAor")
    (imperative-aorist "ImpvAor")
    (perfect "Perf")
    (pluperfect "PluPerf")
    (conj-perfect "ConjPerf")
    (iter-perfect "IterPerf")
    (iter-perfect1 "IterPerf1")
    (past-part "PastPart")
    (future-part "FutPart")
    (present-part "PresPart")
    (negative-part "NegPart")
    (masdar "Masdar")
    ;; ...
    (otherwise
     (cond ((parser::extended-list-equal
             tense-list
             (parser::make-extended-list :form '(present future) :char #\{))
            "PresFut")
           ((parser::extended-list-equal
             tense-list
             (parser::make-extended-list :form '(conditional imperfect) :char #\{))
            "CondImpf")
           (t
            tense-list)))))

(defun root-type (root)
  (let ((first-char (char root 0)))
    (cond ((find root '("ვალ" "ველ" "ვედ" "ვიდ") :test #'string=)
           'x-root)
          ((find first-char "დთტძცწჯჩჭ")
           's-root)
          ((find first-char "გკქპყჴ")
           'h-root)
          ((find first-char "ბფზსშღხლრმნვჳ")
           '{h-root null-root})
          (t 'null-root))))

(defun passive-morphology-p (morph-type)
  (let ((passive-types '(passive stative-passive causative-passive)))
    (if (parser::extended-list-p morph-type)
      (find-if (lambda (type) (find type passive-types)) (parser::extended-list-form morph-type))
      (find morph-type passive-types))))

(defun gv-syntax (gv morph-type)
  (ecase (char gv 0)
    (#\T 'active)
    (#\K 'caus)
    (#\P 'passive)
    (#\R
     (ecase (char gv 1)
       (#\P 'passive)
       (#\M (if (passive-morphology-p morph-type)
              'mediopassive
              'medioactive))))
    (#\M (if (passive-morphology-p morph-type)
           'mediopassive
           'medioactive))
    (#\I 'inv)
    (#\Z 'stative)
    ))

;; trans -> active
;; unerg -> medioactive
;; unacc -> passive, mediopassive, stative
;; inv
;; caus

#+ignore ;; see verb-feature-table-sql.lisp
(defun get-root-override-features (root)
  (gethash root *features-table*))

(defun kartuli-verb-morph (verb)
  (let ((parses (u-transduce verb *fst*)))
    #+debug(format t "~&~a: ~d parse(s)~%" verb (length parses))
    (collecting
      (dolist (parse parses)
        #+debug(pprint parse)
        #+debug(terpri)
        (let* ((root (cdadr (assoc 'root (cdr parse))))
               (root-override-features (get-root-override-features root))
               (root-type (when root (root-type root))))
          #-debug(print (cons root (mapcar (lambda (rof) (list (car rof) (cadr rof))) root-override-features)))
          (dolist (gv-features root-override-features)
            (destructuring-bind (gv id pv . features) gv-features
              (let* ((parse-tense-list (path-value parse 'tense)) 
                     (tense-list (cadr (assoc 'tense features)))
                     (morph-type (cadr (assoc 'morph-type features)))
                     (tenses (if (parser::extended-list-p tense-list)
                               (if (parser::extended-list-p parse-tense-list)
                                 (intersection (parser::extended-list-form tense-list)
                                               (parser::extended-list-form parse-tense-list))
                                 (when (find parse-tense-list (parser::extended-list-form tense-list))
                                   (list parse-tense-list)))
                               (if (parser::extended-list-p parse-tense-list)
                                 (when (find tense-list (parser::extended-list-form parse-tense-list))
                                   (list tense-list))
                                 (when (eq parse-tense-list tense-list)
                                   (list tense-list))))))
                (dolist (tense tenses)
                  (let* ((template (list* `(tense ,tense)
                                          `((type root) ,root-type)
                                          '(parsing +)
                                          (fill-template (feature-template root gv tense morph-type) features
                                                         :except (list 'tense '(type root)))))
                         (dg (list-to-dg template))
                         (unification (unify parse dg)))
                    #+debug(print (list* gv id pv template))
                    #+debug
                    (when (and template unification)
                      #+debug(print (list (car gv-features)
                                   :id id
                                   :tenses tense-list
                                   :parse parse
                                   :features features
                                   :template template))
                      (print unification)
                      )
                    (when (and template unification)
                      (let ((*print-case* :capitalize)
                            (syntax (gv-syntax gv (path-value unification 'morph-type)))
                            (pv-p (and pv (not (eq pv '-)))))
                        (collect (list (format nil "~a/~a"
                                               (convert (path-value unification 'vn))
                                               (convert (path-value unification 'c-root)))
                                       (case tense
                                         ((past-part future-part present-part negative-part masdar)
                                          (format nil "+~a+Vpart+~a+~a+~a~a~@[+~a~]"
                                                  syntax
                                                  (tense-code (path-value unification 'tense))
                                                  (feature-string (path-value unification 'case) nil)
                                                  (feature-string (path-value unification 'case-type) nil)
                                                  (let ((val (path-value unification 'num)))
                                                    (if val
                                                      (feature-string (path-value unification 'num) nil t)
                                                      ""))
                                                  ;;(normalize-gv gv)
                                                  (convert (path-value unification 'style))))
                                         (otherwise
                                          (format nil "+~a+V+~a+FSubj~d~a~a~@[+~a~]"
                                                  syntax
                                                  (tense-code (path-value unification 'tense))
                                                  (path-value unification 'subj 'pers)
                                                  (or (path-value unification 'subj 'num) "")
                                                  (let ((obj-pers (path-value unification 'obj 'pers)))
                                                    (if obj-pers
                                                      (format nil "+FObj~d~@[~a~]" obj-pers
                                                              (path-value unification 'obj 'num))
                                                      ""))
                                                  ;;(normalize-gv gv)
                                                  (convert (path-value unification 'style)))))
                                       (normalize-gv gv)))))))))))))))

(defun id-paradigm-ids (id)
  (let ((root-id (if (integerp id) id (subseq id 0 (position #\- id)))))
    (collecting
      (dotimes (i 250)
        (let ((id (format nil "~a-~d" root-id i)))
          (when (gethash id *paradigm-table*)
            (collect (list id))))))))

;; all ids sharing its c-root with id
#+mcl
(defun id-paradigm-ids+gv (id)
  (print id)
  (root-paradigm-ids+gv (cadr (gethash id *paradigm-table*))))

#+mcl
(defun root-paradigm-ids+gv (c-root)
  (Print c-root)
  (let ((roots.paradigm-list (dat:string-tree-get *parsed-verb-table* c-root)))
    (when (print roots.paradigm-list)
      (destructuring-bind (roots . paradigm-list) roots.paradigm-list 
        (declare (ignore roots))
        (collecting
          (dolist (paradigm paradigm-list)
            (collect (list (getf paradigm :id) (getf paradigm :gv)))))))))

;;(cadr (assoc 'relation (gethash (car (split "MV" #\space)) *genus-verbi-expansion-table*)))

;; copied from tschenkeli-verb-parser.lisp
(progn
  (defparameter *relation-list*
  '(;; active
    ("T1"
     ; vasunTKvineb
     absolute) 
    ("T2"
     absolute)
    ("T3"
     relative)
    ("T4"
     relative)
    ("T5"
     relative)
    ;; causative
    ("KT"
     absolute) ;; ??
    ;; medium voice
    ("MV"
     absolute)
    ;; passive
    ("P1"
     absolute)
    ("P2"
     absolute
     (passiv-d "d"))
    ("P3"
     absolute
     (passiv-sfx "eb"))
    ;; relative passive
    ("RP1"
     relative)
    ("RP1 (ohne i.O.)"
     absolute)
    ("RP2"
     relative)
    ("RP3"
     relative)
    ("RP4"
     relative)
    ("RP5"
     relative)
    ("RP7"
     relative
     (passiv-sfx "eb"))
    ("RP6"
     relative
     (passiv-sfx "eb"))
    ("RP7"
     relative
     (passiv-sfx "eb"))
    ;; relative medium
    ("RM1"
     relative)
    ("RM2"
     relative)
    ("RM3"
     relative)
    ("RM4"
     relative) ; rel. pass. e
    ;; inverted
    ("IV1"
     absolute)
    ("IV2"
     absolute)
    ("IV3"
     absolute)
    ("IV4"
     absolute)
    ;; passive of state
    ("ZP1"
     absolute) ;; ??
    ("ZP2"
     relative)
    ("ZP3"
     relative)))
  
  (defparameter *relation-table*
    (make-hash-table :test #'equal))
  (dolist (gvr *relation-list*)
    (setf (gethash (car gvr) *relation-table*)
          (cadr gvr))))

(defun normalize-causative (vn)
  (when vn
    (let ((h-pos (position #\- vn)))
      (if (and h-pos (string= vn "-KT" :start1 h-pos))
        (subseq vn 0 h-pos)
        vn))))

(defun paradigm-classes (id)
  (let* ((paradigm-tree (dat:make-string-tree))
         (classes ())
         (fun (lambda (str dg &key paradigm-replacement-p &allow-other-keys)
                (declare (ignore paradigm-replacement-p))
                (let* ((vn (path-value dg 'vn))
                       (gv (path-value dg 'gv))
                       (morph-type (path-value dg 'morph-type))
                       (pv (path-value dg 'pv))
                       (relation (gethash (car (split (convert gv) #\space))
                                          *relation-table*)
                                 #+old(path-value dg 'relation))
                       (syntax (gv-syntax gv (path-value dg 'morph-type))))
                  (pushnew syntax classes)
                  (list str vn gv syntax relation morph-type pv)))))
    (mapc (lambda (paradigm)
            (let* ((preverb nil)
                   (syntax nil)
                   (morph-type nil)
                   (verbal-noun nil)
                   (genus-verbi nil)
                   (relation nil)
                   (forms (mapcar (lambda (tense)
                                    (multiple-value-bind (forms xle-templates) ;; class)
                                                         (generate-paradigm (car paradigm)
                                                                            :allp nil
                                                                            :tense tense
                                                                            :num 'sg
                                                                            :pers 1
                                                                            :obj3sg-only-p t
                                                                            :standard-only-p t
                                                                            :printp nil
                                                                            :function fun
                                                                            )
                                      (declare (ignore xle-templates))
                                      ;;(print (list :paradigm (car paradigm) :forms forms))
                                      ;;(when (eq tense 'present) (push class classes))
                                      (when forms
                                        (destructuring-bind (str vn gv synt rel morph-tp pv)
                                                            (car (merge-alternative-forms forms))
                                          (setf verbal-noun vn
                                                syntax synt relation rel morph-type morph-tp genus-verbi gv)
                                          (when (stringp pv)
                                            (when (and (stringp preverb)
                                                       (not (string= pv preverb)))
                                              (warn "Different preverbs for pres and aor: ~s, ~s" preverb pv))
                                            (setf preverb pv))
                                          (convert str)))))
                                  '(present #+ignore future aorist #+ignore perfect))))
              (when genus-verbi
                ;;(print (list genus-verbi syntax forms))
                (push (list* (car paradigm) (normalize-gv genus-verbi) syntax morph-type forms)
                      (dat::string-tree-get-list
                       paradigm-tree
                       (list (normalize-causative (convert verbal-noun))
                             (convert preverb)
                             (string-downcase relation)))))))
          (id-paradigm-ids id))
    (values paradigm-tree
            (collecting
              (dolist (cl '(trans caus unacc unerg inv
			    active passive medioactive mediopassive stative))
                (when (find cl classes)
                  (collect cl)))))))

;; maps paradigm ids to roots
(defvar *paradigm-table* (make-hash-table :test #'equal))

;; populate *paradigm-table*
#+mcl ;; see xle::update-paradigm-table()
(progn
  (clrhash *paradigm-table*)
  (maphash (lambda (root override-feature-lists)
             (dolist (of override-feature-lists)
               (destructuring-bind (gv id pv . features) of
                 (declare (ignore gv pv features))
                 (pushnew root (gethash id *paradigm-table*) :test #'string=))))
           *features-table*)
  (dat:do-string-tree (c-root roots.paradigm-list *parsed-verb-table*)
    (dolist (paradigm (cdr roots.paradigm-list))
      (push c-root (gethash (getf paradigm :id) *paradigm-table*))
      (push (getf paradigm :gv) (gethash (getf paradigm :id) *paradigm-table*)))))

(defparameter *xle-template-table* (dat:make-string-tree))

;; used in verb-lexicon-regexp()
(defun xle-template-to-frame (xle-template #+ignore tense)
  (let ((xle-template (intern (string-upcase (delete #\3 xle-template)) :keyword)))
    (ecase xle-template
      (:?? "??")
      (:V-TRANS-SUBJ-OBJ "S-DO")
      (:V-TRANS-S-SUBJ-OBJ "S-DO-R")
      (:V-TRANS-O-SUBJ-OBJ "S-DO3-OBen")
      (:V-TRANS-SUP-SUBJ-OBJ "S-DO3-OLoc")
      (:V-TRANS-SUBJ-OBJ-PREDLINKadv "S-DO-Pred" #+orig "S-DO-Padv")
      (:V-TRANS-SUBJ-OBJ-OBLth_ze "S-DO" #+orig "S-DO-Obl_ze")
      (:V-TRANS-SUBJ-OBJ-OBLth_tan "S-DO" #+orig "S-DO-Obl_tan")
      (:V-TRANS-SUBJ-OBJrefl-PREDLINKadv "S-DO-Pred" #+orig "S-DOR-Padv")
      (:V-TRANS-S-SUBJ-OBJ-OBLth_ze "S-DO" #+orig "S-DO-Obl_ze")
      (:V-TRANS-O-SUBJ-OBJ-PREDLINKadv "S-DO3-OBen-Pred")
    
      (:V-DITRANS-SUBJ-OBJ-OBJ "S-DO3-OTh")
      (:V-DITRANS-O-SUBJ-OBJ-OBJ "S-DO3-OTh-OBen") ;; მიჭმიე
    
      (:V-CAUS-SUBJ-OBJ-OBJ "S-DO3-OTh")
      (:V-CAUS-SUBJ-OBJ "S-DO")
      
      (:V-INTR-SUBJ "S")
      (:V-INTR-SUBJ-PREDLINKnom "S-Pred")
      (:V-INTR-SUBJ-PREDLINKadv "S-Pred")
 
      (:V-INTR-SUBJ-OBLth_ze "S-Obl_ze")
      (:V-INTR-SUBJ-OBLinst "S-Obl_inst")
      (:V-INTR-SUBJ-OBLadv "S-Obl_adv")
      (:V-INTR-S-SUBJ-OBLadv "S-Obl_adv") ;; გადავიცვამ -ად
 
      (:V-INTR-SUBJ-OBJ "S-OTh")
      (:V-INTR-SUBJ-OBJ-OBJ "S-DO3-OTh") ;; დაგპირდი მას
      (:V-INTR-SUBJ-OBJ-COMP "S-OTh")	 ;; prelim
      (:V-INTR-SUBJ-OBJ-OBL_Si "S-OTh")
      (:V-INTR-SUBJ-OBJ-PREDLINKADV "S-OTh-Pred")
      (:V-INTR-S-SUBJ "S-R")
      (:V-INTR-S-SUBJ-OBL_Si "S-R")
      (:V-INTR-O-SUBJ "S-OBen")
      (:V-INTR-SUP-SUBJ "S-OLoc")
      (:V-INTR-SUBJ-COMP "S") ;; prelim.
 
      (:V-IV-SUBJ "S")
      (:V-IV-SUBJ-OBJ "S-DO")
      (:V-IV-SUBJ-OBJgen "S-OGen")
      (:V-IV-SUBJ-OBJ-PREDLINKadv "S-DO-Pred")
      (:V-IV-SUBJ-OBJ-PREDLINKnom "S-DO-Pred")
 
      (:V-STATE-PASS-SUBJ "S-[OTh]") ;; new August 2016, needed for relative markers
      (:V-STATE-PASS-SUBJ-IV "S-[OTh]inv") ;; new August 2016, needed for relative markers
      (:V-STATE-PASS-SUBJ-OBJ "S-OTh")
      (:V-STATE-PASS-O-SUBJ "S-OBen")
      (:V-STATE-PASS-SUP-SUBJ "S-OLoc")
 
      (:V-EMPTY "Empty")
 
      (:V-CODNA "S-DO")
      (:V-KONA-AUX "AuxTrans")
      (:V-QOLA-AUX "AuxTransHum")
      (:V-QOPNA-AUX "AuxIntr")
      )))

(defun paradigm-person-marking-restrictions (xle-templates morph-syntax gv tense)
  (let* ((markings (mapcar (lambda (template)
                             (paradigm-person-marking (car template) morph-syntax))
                           xle-templates))
         (marking (block find
                    (dolist (m '(:bipers :bipers-unacc :bipers-unerg :bipers-inv :monopers-unacc 
				 :monopers-inv :unpers :codna :monopers-unerg))
                      (when (find m markings)
                        (return-from find m)))))
         (or-p (search "(OR)" gv))
         (split-inversion-p (when (or or-p
                                      (find tense '(perfect pluperfect conj-perfect iter-perfect)))
			      t)))
    (when (eq marking :codna)
      (setf marking (if (find tense '(present imperfect conj-present))
			:monopers-unerg
			:monopers-inv)))
    ;;(print (list :templates xle-templates :morph-syntax morph-syntax :tense tense :marking marking))
    (case marking
      ((:bipers :bipers-unerg)
       (values nil #+orig(when split-inversion-p
                 `(((subj pers) 3)
                   ((subj num) sg)))
               split-inversion-p
               marking))
      (:bipers-inv
       (values nil t marking))
      (:bipers-unacc
       (values nil or-p marking))
      (:monopers-unerg
       (values (if split-inversion-p
                 `(((subj pers) 3)
                   ((subj num) sg))
                 `(((obj pers) 3)
                   ((obj num) sg)))
               split-inversion-p
               marking))
      (:monopers-unacc
       (values `(((obj pers) 3)
                 ((obj num) sg))
               or-p
               marking))
      (:monopers-inv
       (values `(((subj pers) 3)
                 ((subj num) sg))
               t
               marking))
      (:unpers
       (values `(((subj pers) 3)
                 ((subj num) sg)
                 ((obj pers) 3)
                 ((obj num) sg))
               nil
               marking))
      (:codna
       (values `(((subj pers) 3)
                 ((subj num) sg)
                 ((obj pers) 3)
                 ((obj num) sg))
               nil
               marking)))))

(defun syntax-type (gv morph-type)
  (if (and gv morph-type)
    (gv-syntax gv morph-type)
    "part"))

(defun paradigm-features (paradigm-id &optional (print t))
  (let ((paradigm-features 
         (cons paradigm-id
               (collecting
                 (dolist (root (cddr (gethash paradigm-id *paradigm-table*)))
                   #+debug(print root)
                   (let* ((root-features (get-root-override-features root))
                          #+ignore(root-type (when root (root-type root))))
                     (dolist (gv-features root-features)
                       (when (equal (cadr gv-features) paradigm-id)
                         (collect (cons root gv-features))))))))))
    (if print
      (pprint paradigm-features)
      paradigm-features)))

(defun merge-alternative-forms (forms)
  (cond ((null (cdr forms))
         forms)
        ((null (cddr forms))
         (or (apply #'merge-two-alternative-forms forms)
             forms))
        (t
         (let ((merger nil)
               (form nil))
           (loop for form2 in (cdr forms)
                 do (setf form form2 merger (merge-two-alternative-forms (car forms) form))
                 until merger)
           #+debug(when merger (print (list (car forms) form merger)))
           (if merger
             (merge-alternative-forms (append merger (remove form (cdr forms))))
             (let ((merged-cdr (merge-alternative-forms (cdr forms))))
               (if (< (length merged-cdr) (length (cdr forms)))
                 (merge-alternative-forms (cons (car forms) merged-cdr))
                 forms)))))))

;; fixme: (cdr form1) overrides (cdr form2), not merged
(defun merge-two-alternative-forms (form1 form2)
  (let ((f1 (car form1))
	(f2 (car form2)))
    (cond ((string= f1 f2)
	   (return-from merge-two-alternative-forms (list form1)))
          ((= (length f1) (length f2))
	   (return-from merge-two-alternative-forms nil))
	  ((> (length f1) (length f2))
	   (let ((f nil)) ;; swap
	     (setf f f1 f1 f2 f2 f)))
	  (t
	   nil))
    (list (cons (with-output-to-string (stream)
		  (loop with in-par = nil and i1 = 0 and i2 = 0
			do (let ((c1 (unless (= i1 (length f1)) (char f1 i1)))
				 (c2 (char f2 i2)))
			     (cond ((eq c1 c2)
				    (when in-par 
				      (write-char #\) stream)
				      (setf in-par nil))
				    (write-char c1 stream)
				    (incf i1) (incf i2))
				   ((eq c2 #\()
				    (return-from merge-two-alternative-forms nil))
				   (in-par
				    (write-char c2 stream)
				    (incf i2))
				   (t
				    (write-char #\( stream)
				    (setf in-par t)
				    (write-char c2 stream)
				    (incf i2))))
			until (or ;;(and (not in-par) (= i1 (length form1)))
			       (= i2 (length f2)))
			finally
			(unless (and (= i1 (length f1))
				     (= i2 (length f2)))
			  (return-from merge-two-alternative-forms nil))
			(when in-par (write-char #\) stream))))
		(cdr form1)))))

(defun extract-pp-features (paradigm-features &optional pv-p)
  (collecting
    (dolist (tense-features paradigm-features)
      (destructuring-bind (root gv paradigm-id pv . features) tense-features
        (declare (ignore gv paradigm-id pv))
        ;; to do: test for passive and perfect
        (let ((part-pfx (get-feature-value 'part-pfx features))
              (part-sfx (get-feature-value 'part-sfx features)))
          (when (or part-pfx part-sfx)
            (collect `(,root
                       ,@(when pv-p `((pv ,(get-feature-value 'pv features '-))))
                       (part-pfx ,(or part-pfx '-))
                       (root ,root)
                       (sf ,(get-feature-value 'sf features '-))
                       (part-sfx ,(or part-sfx '-))))))))))
#+test
(add-participles)

(defun add-participles ()
  (dat:do-string-tree (c-root paradigm-list *parsed-verb-table*)
    (declare (ignore paradigm-list))
    (add-root-participles c-root))
  (add-future-participles :printp nil)
  (add-masdars :printp nil))

(defun add-root-participles (c-root)
  (let* ((paradigm-list (dat:string-tree-get *parsed-verb-table* c-root)) 
         (pp-list
          (collecting
            (dolist (paradigm (cdr paradigm-list))
              (let ((id (getf paradigm :id)))
                (collect-append (extract-pp-features (cdr (paradigm-features id nil)) nil;t
                                                     )))))))
    (when pp-list
      (let ((pp-list (remove-duplicates pp-list :test #'equal))
            (merge-list ())) ;; ??
        #+debug(pprint (cons c-root (remove-duplicates pp-list :test #'equal)))
        (dolist (paradigm (cdr paradigm-list))
          (unless (getf paradigm :ignore)
            (let* ((id (getf paradigm :id))
                   (paradigm-features (cdr (paradigm-features id nil))))
              (when paradigm-features
                (destructuring-bind (root gv id pv . features) (car paradigm-features)
                  (declare (ignore root))
                  (when (eq (char gv 0) #\T) ;; only transitives have periphrastic perfects ;; oh no
                    ;;(print paradigm)
                    (dolist (pp-features pp-list)
                      (pushnew (list* gv id pv
                                      '(tense past-part)
                                      `(vn ,(get-feature-value 'vn features '-))
                                      `(pv ,pv)
                                      (cdr pp-features))
                               (gethash (car pp-features) *features-table*)
                               :test #'override-features-equal)
                      (pushnew (parse-integer id :start (1+ (position #\- id))) merge-list))))))))))))

:eof
