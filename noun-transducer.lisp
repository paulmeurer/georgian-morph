;;-*- Mode: Lisp; Package: TRANSDUCER; Readtable: fst-readtable -*-
;;;
;;; Georgian verb mophological parser unsing a finite state 
;;; transducer with unification
;;; (C) Paul Meurer 1999-2017

;; bugs: mers, merve -> meri +Prop

;; missing: OG ესე
;; მეორეჯერ

;; Nov. 2013
;; OG -გან Inst
;; ვითარცა-იგი C ??
;; დამართ
;; ვისა-მე
;; ვჰყვანდი


(in-package :fst)

(defclass georgian-dfa (extended-dfa)
  ())

(precompile-u-transducer
 `(seq ,(utp "ა" '((encl-qopna +) (lang ng)))) 
 :name 'encl-aux)

(precompile-u-transducer
 `(seq ,(utp-e '((lang ng)))
       (or (seq (? ,(utp-or '("ვე" "ღა" "მც") `((mod-sfx ,morph))))
		,(utp "ც" `((rel-sfx "ც")))
		(? (seq ,(utp "ა" `((long +))) ;; ???
			(? encl-aux))))
	   (seq ,(utp "აც" `((rel-sfx "ც"))) ;; fixme: -შიაც only
		(? (seq ,(utp "ა" `((long +))) ;; ???
			(? encl-aux))))
	   (seq ,(utp-or '("ვე" "ღა" "მც") `((mod-sfx ,morph)))
		(? encl-aux))
	   (? encl-aux)))
 :name 'v-enclitica)

(precompile-u-transducer
 `(? (seq ,(utp-e '((lang ng)))
	  (or (seq (? (seq (or ,(utp "ა")
			       ,(utp-or '("ვე" "ღა" "მც") `((mod-sfx ,morph))))
			   ,(utp "ც" `((rel-sfx "ც")))))
		   (? (seq ,(utp "ა" `((long +)))
			   (? encl-aux))))
	      (seq (or ,(utp-or '("ვე" "ავე") `((mod-sfx "ვე")))
		       ,(utp "ღა" `((mod-sfx "ღა"))))
		   (? encl-aux))))) 
 :name 'c-enclitica)

(precompile-u-transducer
 `(seq ,(utp-e '((lang ng)))
       (or (seq ;; (? ,(utp-or '("ღა" "მც") `((mod-sfx ,morph))))
		,(utp "ც" `((rel-sfx "ც")))
		(? (seq ,(utp "ა" `((long +)))
			(? encl-aux))))
	   (seq ,(utp-or '("ღა" "მც") `((mod-sfx ,morph)))
		(? encl-aux))
	   (? encl-aux))) 
 :name 'v-enclitica-Ga-C-a)

;; prelim; for relatives (ვინც)
(precompile-u-transducer
 `(seq ,(utp-e '((lang ng)))
       (or (seq ,(utp-or '("ღა" "მც") `((mod-sfx ,morph)))
		(? encl-aux))
	   (? encl-aux))) 
 :name 'v-enclitica-Ga-a)

(precompile-u-transducer
 `(seq (? ,(utp "-"))
       ,(utp-or '("ცა" "ესე" "ეგე" "იგი" "ცა-ესე" "ცა-ეგე" "ცა-იგი")
		`((rel-sfx ,morph)
		  (lang og)))) 
 :name 'og-rel-enclitica)

;; OG: -მე, -ცა +V, -ვე, -ღა +V, -ძი +V, -მცა +V, -და ??

(precompile-u-transducer
 `(seq (? ,(utp "-"))
       (or ,(utp "მე" `((q-sfx "მე")))
	   ,(utp "ცა" `((rel-sfx "ცა")))
	   ,(utp "ღა" `((mod-sfx "ღა")))
	   ,(utp "ვე" `((mod-sfx1 "ვე")))
	   ,(utp "მცა" `((mod-sfx2 "მცა")))
	   ,(utp "რე" `((mod-sfx1 "რე")))
	   ,(utp "ძი" `((cj-sfx "ძი")))))
 :name 'og-enclitica-one)

;; not used?!?, see OGNounEnclitics in syntax.regex
(precompile-u-transducer
 `(seq ,(utp-e '((lang og)))
       og-enclitica-one
       (? (seq og-enclitica-one
	       (? (seq og-enclitica-one
		       (? (seq og-enclitica-one
			       (? og-enclitica-one))))))))
:name 'og-enclitica)

;; ღა or ც, not both
(precompile-u-transducer
 `(? (seq ,(utp-e '((lang ng)))
	  (or (seq (? (seq ,(utp "ა")
			   (or ,(utp "ც" `((rel-sfx "ც")))
			       ,(utp-or '("მც") `((mod-sfx ,morph))))))
		   (? (seq ,(utp "ა" `((long +)))
			   (? encl-aux))))
	      (seq ,(utp "ღა" `((mod-sfx "ღა")))
		   (? encl-aux))))) 
 :name 'c-enclitica-Ga-C-a)

(precompile-u-transducer
 `(? (seq ,(utp-e '((lang ng)))
	  (? (seq ,(utp "ა")
		  ,(utp "ც" `((rel-sfx "ც")))))
	  (? (seq ,(utp "ა" `((long +)))
		  (? encl-aux))))) 
 :name 'c-enclitica-C-a)

(precompile-u-transducer
 `(? (seq ,(utp-e '((lang ng)))
	  (? ,(utp "ც" `((rel-sfx "ც"))))
	  (? (seq ,(utp "ა" `((long +)))
		  (? encl-aux))))) 
 :name 'v-enclitica-C-a)

(precompile-u-transducer
 `(? (seq ,(utp-e '((lang ng)))
	  (or (seq (? (seq ,(utp "ა" `((long +)))
			   (? encl-aux))))
	      (seq ,(utp "ღა" `((mod-sfx "ღა")))
		   (? encl-aux))))) 
 :name 'c-enclitica-Ga-a)

;; revise this
(precompile-u-transducer
 `(? (seq ,(utp-e '((lang ng)))
	  (or ,(utp "ც" '((rel-sfx "ც")))
	      ,(utp-or '("ვე" "ღა" "მც") `((mod-sfx ,morph)))))) 
 :name 'v-mod-enclitica)

(precompile-u-transducer
 `(? (seq ,(utp-e '((lang ng)))
	  (or ,(utp "აც" '((rel-sfx "ც")))
	      (seq ,(utp-or '("ვე" "ღა" "მც") `((mod-sfx ,morph)))))))
 :name 'c-mod-enclitica)

(precompile-u-transducer
 `(seq (or ,(utp-e '((stem-type {c v propv "ა" "ი"})
                     (sync-stem +)))
           ,(utp-e '((stem-type {"ე" prop-ე})
                     (sync-stem -)
		     (pl-stem +))))
       ,(utp "ებ" `((num pl)
                    (old-pl -)
                    (cat {n a num masdar v-part pron prop pron+indef }))))
 :name 'plural)

;; adzghabgogovaizaZ

(precompile-u-transducer
 `(seq (or ,(utp-e '((stem-type {c v propv "ა" "ი"})
                     (sync-stem +)))
           ,(utp-e '((stem-type {"ე" prop-ე})
                     (sync-stem -)
		     (pl-stem +))))
       ,(utp "ებ" `((num pl)
                    (old-pl -)
		    (lex "ებ")
                    (cat {n a masdar v-part}))))
 :name 'der-plural)

;; erg-group
(precompile-u-transducer
 `(or (seq plural
           (or (seq ,(utp "მა" `((case erg)
				 (cat {n a masdar num v-part prop pron q allq})
				 (case-type full)
				 (lang ng)))
		    (? v-enclitica))
	       (seq ,(utp "მან" `((case erg)
				  (cat {n a masdar num v-part prop pron q allq})
				  (case-type full)
				  (style old)))
		    (? c-enclitica))))
      (seq ,(utp "მა" `((case erg)
                        (cat {n a num masdar v-part prop pron q allq pron+neg+inanim})
                        (num sg)
                        (case-type full)
                        (sync-stem -)
                        (stem-type {c bare})
			(lang ng)))
           v-mod-enclitica)
      (seq ,(utp "მან" `((case erg)
			 (cat {n a num masdar v-part prop pron q allq pron+neg+inanim})
			 (num sg)
			 (case-type full)
			 (sync-stem -)
			 (stem-type {c bare})
			 (style old)
			 ))
	   ;; ,(utp-not '((sub-cat name+firstname)))
           c-mod-enclitica)
      ,(utp "ი" `((case erg)
		  (cat prop)
		  (num sg)
		  (case-type full)
		  (stem-type {c bare})
		  (lang og)
		  (sub-cat {name+firstname name})))
      ,(utp "მა" `((case erg)
                   (cat {a
			 pron ;; შენმა
			 num masdar v-part q allq})
                   (sync-stem -)
                   (case-type reduced)
                   (stem-type c)
		   (lang ng)))
      ,(utp-e `((case erg)
		(cat prop)
		(sync-stem -)
		(case-type reduced)
		(stem-type bare)
		(lang og)))
      ,(utp-e `((case erg)
		(cat prop)
		(stem-type {v propv prop-ე})
		(sub-cat {name+firstname name}) ;; name -> Prop Anthr
		(lang og)))
      (seq
       ,(utp-e '((sub-cat { meas qual anim+qual })) :augment 'require)
       (or ,(utp "მა" `((case erg)
			(cat n)
			(sync-stem -)
			(case-type reduced)
			(stem-type c)
			(lang ng)))
	   ,(utp "მან" `((case erg)
			 (cat n)
			 (sync-stem -)
			 (case-type reduced)
			 (stem-type c)
			 (style old)))))
      ;; ??
      (seq (or ,(utp-e `((stem-type {v propv prop-ე "ა" "ე" "ო" "ი"})))
	       ,(utp "-" `((stem-type abbr)))) ;; აშშ-მ
           (or (seq ,(utp "მ"  `((case erg) ;; -am should be possible according to Hewitt p. 44?
				 (sync-stem -)
				 (num sg)
				 (case-type full)
				 (lang ng)))
		    (? ,(utp "ა" '((long +))))) ;; ჭავჭავაძე: ვიღაცამა
	       ,(utp "მან"  `((case erg)
			      (sync-stem -)
			      (num sg)
			      (case-type full)
			      (style old)))
	       ,(utp-e `((case erg)
			 (cat prop)
			 (sub-cat {name+firstname name})
			 (lang og))))
           c-mod-enclitica))
 :name 'erg-group)

;; dat-group, full decl.
(precompile-u-transducer
 `(or (seq (or plural
               ,(utp-e '((stem-type {c "ი" bare}) ;; v is under gen-group!
                         (sync-stem -)
                         (num sg)))
               ,(utp "-" '((stem-type abbr)
			   (num sg)))
               ,(utp "ო" `((stem-type "ო")
			   (num sg)))
               ,(utp-e `((stem-type {"ა" "ე"})
                         (sync-stem -)
                         (num sg))))
           (or (seq ,(utp "ს" `((case dat)))
                    (or c-enclitica
                        ,(utp "ა" '((long +) (lang og)))
                        (seq (or ,(utp "ავით" '((pp "ვით")))
                                 ,(utp "თან" '((pp "თან")
                                               (num sg)
					       (stem-type {v propv prop-ე "ა" "ე" "ი" "ო" abbr}))))
                             c-enclitica)))
               (seq ,(utp-or '("ში" "ზე" #+ignore"ზედა" "შუა") `((case dat) (pp ,morph)))
                    (? v-enclitica))
               (seq ,(utp-or '("თან" "წინ" "ქვეშ" "ზედ")
                             `((case dat)
                               (pp ,morph)
                               (stem-type {c bare})))
                    c-enclitica)
	       (seq ,(utp-or '("თან" "წინ" "ქვეშ" "ზედ") ;; ??
                             `((case dat)
                               (pp ,morph)
			       #+ignore ;; ??
                               (num pl)))
                    c-enclitica)))
      (seq ,(utp-e '((stem-type {v propv prop-ე})))
           ,(utp-or '("ში" "ზე" "შუა") `((num sg) (case dat) (pp ,morph)))
           (? v-enclitica))
      (seq ,(utp-e '((stem-type {v propv prop-ე})))
           ,(utp-or '("ზედ") `((num sg) (case dat) (pp "ზედ")))
           (? c-enclitica)))
 :name 'dat-group)

;; adv-group
(precompile-u-transducer
 `(seq (or (seq plural ,(utp "ა"))
           (seq (or ,(utp "ა" '((stem-type {c "ი" bare})
                                (sync-stem +)))
                    ,(utp-or '("ა" "ე" "ო") `((stem-type ,morph)
                                              (num sg)))
                    ,(utp-e '((stem-type {v propv prop-ე})))
		    ,(utp "-" '((stem-type abbr))))
                ,(utp-e `((num sg)))))
       (or (seq (or ,(utp "დ" `((case adv)
				(lang ng)))
		    ,(utp "d" `((case adv)
				(lang og)))
		    ,(utp "თ" `((case adv)
				(lang ng)
				(style nonstandard))))
                (or ,(utp "ა" '((long +))) c-enclitica))
	   ,(utp-e `((case adv-tr) ;; truncated Advb
		     (stem-type {c "ი" bare})
		     (lang og)))
	   ;; not necessary? see syntax.regex
	   ,(utp "d" `((case adv) ;; იერუსალიმდ, სიკუდილდ
		       (stem-type {c "ი" bare})
		       (lang og)))
	   (seq (? ,(utp "d" '((lang og))))
		(or (seq ,(utp-or '("მდის" "მდინ")
				  `((pp ,morph)
				    (case adv)))
			 c-enclitica)
		   (seq ,(utp-or '("მდე" "მდი")
				 `((pp ,morph) ;; TODO: add მი-/მო- prefix for OG (done?)
				   (case adv)))
			 (? v-enclitica))))))
 :name 'adv-group)

;; inst-group
(precompile-u-transducer
 `(or (seq (or (seq plural ,(utp "ი" `((cat {n a masdar v-part prop pron}))))
               ,(utp "ი" `((stem-type {c "ე" "ო"})
                           (sync-stem +)
                           (num sg)))
               ,(utp "ი" `((stem-type "ა")
                           (sync-stem +)
                           (rigid-stem -)
                           (cat {n a num masdar v-part q allq prop})
                           (num sg)))
               ,(utp-e `((stem-type "ი") 
                         (cat {n a num masdar v-part pron prop})
                         (num sg)))
	       ,(utp "-" `((stem-type abbr) 
			   (cat {n prop})
			   (num sg)))
	       ,(utp "ი" `((stem-type {v propv prop-ე})
                           (sync-stem -)
                           (rigid-stem +)
			   (lang ng)
			   (style old) ;; e.g., ჭავჭავაძე (but: დროის!)
                           (cat {n a num masdar v-part q allq prop})
                           (num sg)))
	       ;;; Old Georgian
	       ,(utp "ჲ" `((stem-type {v propv prop-ე})
                           (sync-stem -)
                           (rigid-stem +)
			   (lang og)
                           (cat {n a num masdar v-part q allq prop})
                           (num sg))))
           (or (seq ,(utp "თ" '((case inst))) 
                    (or ,(utp "ა" '((long +)))
                        c-enclitica))
               (seq ,(utp-or '("დან" "დამ")
			     `((pp ,morph)
			       (case inst)))
                    c-enclitica)
	       (seq ,(utp "თურთ" `((pp "ურთ")
				   (case inst)))
                    c-enclitica)
	       (seq ,(utp "თგან" `((pp "გან")
				   (case inst)))
                    c-enclitica)
	       (seq ,(utp "დგან" `((pp "გან")
				   (case inst)
				   (style nonstandard)))
		    c-enclitica)))
      (seq (or ,(utp "თი" `((stem-type {v propv prop-ე abbr})
			    (cat {n a num masdar v-part pron prop})
			    (num sg)
			    (case inst)))
               ,(utp "თი" `((stem-type "ა")
			    (sync-stem -)
			    (rigid-stem +)
			    (cat {n a num masdar v-part pron prop})
			    (num sg)
			    (case inst)))
	       ,(utp "ჲთ" `((stem-type {v propv prop-ე abbr})
			    (cat {n a num masdar v-part pron prop})
			    (num sg)
			    (case inst)
			    (lang og)))
	       ,(utp "ჲთ" `((stem-type "ა")
			    (sync-stem -)
			    (rigid-stem +)
			    (cat {n a num masdar v-part pron prop})
			    (num sg)
			    (case inst)
			    (lang og))))
           (? v-enclitica))
      (seq ,(utp-or '("დან" "დამ")
		    `((pp ,morph)
		      (num sg)
		      (stem-type {v propv prop-ე})
		      (case inst)))
           c-enclitica)
      (seq ,(utp "ე" `((stem-type "ე") ;; საკაცედან
		       (sync-stem +)
		       (num sg)))
	   ,(utp-or '("დან" "დამ")
		    `((pp ,morph)
		      (case inst)))
           c-enclitica))
 :name 'inst-group)

;; nom-group
(precompile-u-transducer
 `(or
   ;; consonant stem or pl
   (seq (or (seq plural
		 ,(utp "ი" `((case nom)
			     (cat {n a num masdar v-part pron prop pron+indef })
			     (case-type full))))
	    ;; consonant stem, bare stem
	    ,(utp "ი" `((case nom)
			(num sg)
			(sync-stem -)
			(stem-type {c bare})
			(cat {n a num masdar v-part pron prop q allq pron+neg+inanim pron+indef })
			(case-type full)))
	    ,(utp "ი" `((case nom) ;; ივანეი: ჭავჭავაძე
			(num sg)
			(sync-stem -)
			(stem-type {propv prop-ე})
			(cat prop)
			(case-type full)))
	    ;; bare stem OG
	,(utp-e `((case { nom erg voc } ) ;; fixme for enclitica etc,
		      (num sg)
		      (stem-type bare)
		      (cat {n a num masdar v-part prop pron})
		      ;; (case-type full) ;; ??
		      (lang og))))
	(or (? v-enclitica)
	    (seq ,(utp "ვით" '((pp "ვით")))
		 c-enclitica)))
   ;; reduced bare, propv
   #+fst
   ,(utp-e `((case {nom erg dat gen adv inst})
	     (num sg)
	     (stem-type {bare propv prop-ე})
	     (cat prop) ;; n?
	     (case-type reduced)
	     (lang ng)))
   (seq ,(utp-e `((case nom) ;; fixme for enclitica etc,
		  (num sg)
		  (stem-type abbr)
		  (cat {n prop})
		  (lang ng)))
	(or (? (seq ,(utp "-") (or c-enclitica v-enclitica)))
	    (seq ,(utp "-ვით" '((pp "ვით")))
		 c-enclitica)))
   (seq ,(utp "-ი" `((case nom)
		     (num sg)
		     (stem-type abbr)
		     (cat {n prop})
		     (case-type full)))
	(or (? v-enclitica)
	    (seq ,(utp "ვით" '((pp "ვით")))
		 c-enclitica)))
   ,(utp "ი" `((case {nom gen inst})
	       (sync-stem -)
	       (stem-type c)
	       (cat {a num v-part q allq pron quant})
	       (case-type reduced)))
   ,(utp "ის" `((case inst) ;; კარგის თვალით
		(sync-stem -)
		(stem-type c)
		(cat {a num v-part q allq pron})
		(case-type reduced-ext)))
   ;; old Georgian
   (seq plural
	,(utp-e `((case abs)
		  (cat { n a num masdar v-part }) ;; more?
		  (case-type full)
		  (lang og))))
   #+fst
   (seq ;; to understand this, see flag resolution in noun-splice.regex
    ,(utp-e '((sub-cat { meas qual anim+qual })) :augment 'require)
    ,(utp "ი" `((case {nom gen inst})
		(sync-stem -)
		(stem-type c)
		(cat n)
		(case-type reduced))))
   ;; vowel stem
   (seq (or ,(utp-e `((stem-type "ი")))
	    ,(utp-e `((stem-type {v propv prop-ე})
		      (lang ng)))
	    ,(utp-e `((stem-type propv)
		      (lang og)))
	    ;; gives also -იჲ, which is removed in phonotactic rule
	    (seq ,(utp "ჲ" `((stem-type {v propv prop-ე})
			     (lang og))))
	    (seq ,(utp "ი" `((stem-type v)
			     (style old)
			     (lang ng))))
	    (seq
	     (or ,(utp-e `((stem-type {"ა" "ე"})
			   (sync-stem -)))
		 ,(utp "ო" `((stem-type "ო"))))
	     (or ,(utp-e '((lang ng)))
		 ,(utp "ი" '((lang ng)
			     (style old)))
		 ,(utp "ჲ" '((lang og)))))
	    ;; `orive' etc
	    )
	(or (seq ,(utp-e '((case nom)
			   (num sg)
			   (cat {n pron prop a num masdar v-part q allq pron+indef })
			   (case-type full)))
		 (? v-enclitica))
	    ,(utp-e '((case {nom erg dat gen inst adv})
		      (cat {a num pron v-part q allq})
		      (case-type reduced)
		      (lang ng)))
	    #+fst
	    (seq
	     ,(utp-e '((sub-cat { meas qual anim+qual })) :augment 'require)
	     ,(utp-e '((case {nom erg dat gen inst adv})
		       (cat n)
		       (case-type reduced))))))
   ;; OG stem case (abs)
   (seq ,(utp-e '((lang og)))
	(or ,(utp-e `((stem-type {c v propv prop-ე "ი" bare})))
	    ,(utp-e `((stem-type {"ა" "ე"})
		      (sync-stem -)))
	    ,(utp "ო" `((stem-type "ო"))))
	(or (seq ,(utp-e '((case abs)
			   (num sg)
			   (cat {n pron prop a num masdar v-part q allq pron+indef })))
		 (? ,(utp-or '("ებრ")
			     `((pp ,morph)))
		    #+ignore v-enclitica))
	    #+fst ;; ??
	    (seq
	     ,(utp-e '((sub-cat { meas qual anim+qual })) :augment 'require)
	     ,(utp-e '((case {nom erg dat gen inst adv})
		       (cat n))))))
   ;; reduced Dat, Adv
   #+fst
   ,(utp-e '((case {dat adv})
	     (stem-type c)
	     (cat {a num pron v-part q allq quant})
	     (case-type reduced)))
   #+fst
   ,(utp-e '((case gen) ;; ძველ ბაბილონელთა
	     (stem-type c)
	     (cat {a num pron v-part q allq})
	     (case-type reduced-oldpl)))
   (seq plural
	,(utp-e '((case dat)
		  (cat n) ;; e.g. მთებ შუა
		  (style nonstandard)
		  (case-type reduced))))
   #+fst
   (or (seq
	;; don't just add this sub-cat if none present
	,(utp-e '((sub-cat { qual anim+qual })) :augment 'require)
	,(utp-e '((case {dat gen inst adv}) ;; OBS: also gen + inst
		  (stem-type c)
		  (cat n)
		  (sub-cat { qual anim+qual })
		  (case-type reduced))))
       (seq
	,(utp-e '((sub-cat { meas })) :augment 'require)
	,(utp-e '((case {dat adv})
		  (stem-type c)
		  (cat n)
		  (sub-cat { meas })
		  (case-type reduced)))))
   ;; rare double plural: რომლებთა, დამკირველებთა (not to be confused with double declension)
   (seq plural ,(utp "თა" `((case {erg dat gen})
			    (double-old-pl +)
			    (cat {n pron})
			    (lang ng)))))
 :name 'nom-group)

;; voc-group
(precompile-u-transducer
 `(seq (or
	;; consonant stem
	(seq (or (seq plural
		      ,(utp "ო" `((case voc)
				  (cat {n a num v-part})
				  (case-type full))))
		 ,(utp "ო" `((case voc)
			     (sync-stem -)
			     (stem-type c)
			     (cat { a num v-part })
			     (case-type reduced)))
		 (seq ,(utp "ო" `((case voc)
				  (sync-stem -)
				  (stem-type c)
			     (cat {n a num v-part pron prop pron+indef })))
		      (or (seq ,(utp-e '((case-type reduced)))
			       ,(utp-e '((sub-cat { qual anim+qual })) :augment 'require))
			  ,(utp-e '((num sg)
				    (case-type full)))))))
	,(utp-e `((case voc)
		  (num sg)
		  (stem-type bare )
		  (cat prop)))
	;; vowel stem, needs elaboration
	(seq ,(utp-e `((stem-type {"ა" "ე" v propv prop-ე})
		       (sync-stem -)))
	     (or (seq ,(utp-or '("ვ" "ო") '((case voc) ;; მამაო, ძმაო
					    (cat {n a prop})))
		      (or (seq ,(utp-e '((case-type reduced)))
			       ,(utp-e '((sub-cat { qual anim+qual })) :augment 'require))
			  ,(utp-e '((num sg)
				    (case-type full)))))
		 ,(utp-e '((case voc)
			   (num sg)
			   (case-type full)
			   ;; (cat prop) ;; ჩემო მუზა!
			   ))))))
 :name 'voc-group)

;; used for double declension; we should be able to use old-plural, but set recursive-define ON
;; doesn't seem to be honoured by fst
(precompile-u-transducer
 `(seq (or ,(utp-e '((stem-type {v propv prop-ე c "ი" "ა"})
                     (sync-stem -)))
           ,(utp-or '("ე" "ო") `((stem-type ,morph))))
       (or (seq ,(utp "ნი" `((case nom)
			     (num pl)
			     (old-pl +)
			     (cat {n pron a num masdar v-part q allq pron+indef })))
		(? v-enclitica))
           ,(utp "ნო" `((case voc)
                         (num pl)
                         (old-pl +)
                         (cat {n a num masdar v-part pron pron+indef })))
           (seq ,(utp "თ" `((case {erg dat gen})
                             (num pl)
                             (old-pl +)
                             (cat {n pron a masdar num v-part q allq pron+indef })))
                (? ,(utp "ა")))))
 :name 'old-plural-d)

#+ignore
(precompile-u-transducer
 `(or 
   ;; Consonant or truncated stem
   (seq (or (seq plural ,(utp "ი" `((cat {n a num v-part masdar pron prop pron+indef }))))
	    ,(utp "ი" `((stem-type {c "ა" "ე" "ო" bare})
			(sync-stem +)
			(rigid-stem -)
			(cat {n a num masdar v-part q allq pron prop quant pron+neg+inanim pron+indef })
			(num sg)))
	    ,(utp "-ი" `((stem-type abbr)
			 (cat {n prop})
			 (num sg))))
	(or ,(utp "სა" '((case dir)
			 (lang og)))
	    ,(utp "სა" '((case ben)
			 (lang og)))
	    ,(utp "სა" '((case gen)
			 (lang og)))
	    )))
 :name 'gen-group-d)

;; A+Gen+Full missing?
;; -euli ?
;; gen-group, contains dat for vowel stem
#+orig ;; for FST
(precompile-u-transducer
 `(or 
   ;; Consonant or truncated stem
   (seq (or (seq plural ,(utp "ი" `((cat {n a num v-part masdar pron prop pron+indef }))))
	    ,(utp "ი" `((stem-type {c "ა" "ე" "ო" bare})
			(sync-stem +)
			(rigid-stem -)
			(cat {n a num masdar v-part q allq pron prop pron+neg+inanim pron+indef })
			(num sg)))
	    ,(utp "-ი" `((stem-type abbr)
			 (cat {n prop})
			 (num sg))))
	(or ,(utp "სა" '((case dir)
			 (lang og)))
	    ,(utp "სა" '((case ben)
			 (lang og)))
	    (seq ,(utp "ს" '((case gen)))
		 (or (seq (or (seq ,(utp "ა" '((long +)))
				   ,(utp-or '("გან" "თვის" "ვით" "კენ" "მებრ")
					    `((pp ,morph))))
			      ,(utp-or '("გან" "თვის" "ვით" "კენ" "ებრ" "ებრივ" "თან")
				       `((pp ,morph))))
			  c-enclitica)
		     c-enclitica
		     (seq ,(utp "ა")
			  ,(utp-or '("დმი" "კე")
				   `((pp ,morph)))
			  (? v-enclitica))
		     ,(utp "ა" `((lang og)
				 (cat {n a num masdar v-part q allq pron pron+indef })
				 (long +)))
		     ,(utp "ა" `((lang ng) (long +)))
		     ;; double case; gen is base case
		     (seq (or (seq ,(utp-e '((stem-type bare) ;; truncated stem in OG is augmented to c
					     (lang og)))      ;; e.g., შუშანიკისი
				   ,(utp-e '((stem-type c)
					     (cat n)
					     (case {nom erg dat gen ben dir adv adv-tr inst voc})
					     ;; no reduced case in double case
					     (case-type full))
					   :augment 'der))
			      (seq (or ,(utp-not '((stem-type bare))) ;; consonant stem
				       ,(utp-e '((stem-type bare) ;; truncated stem
						 (lang ng))))
				   (or ,(utp "ა" '((stem-type v) ; ###
						   (cat n)
						   (case {nom erg dat adv voc}) ;; no abs?
						   ;; no reduced case in double declension
						   (case-type full))
					     :augment 'der)
				       ,(utp "ა" '((stem-type v)
						   (cat n)
						   (lang og)
						   (case {gen ben dir inst}) ;; no abs?
						   ;; no reduced case in double declension
						   (case-type full))
					     :augment 'der)
				       ,(utp-e '((stem-type c) ;; ბიჭისით, ბიჭისის (??) Hewitt p. 44
						 (cat n)
						 (lang ng)
						 (case {gen inst})
						 ;; no reduced case in double declension
						 (case-type full))
					       :augment 'der))))
			  (or nom-group erg-group dat-group gen-group adv-group
			      (seq (or inst-group voc-group)
				   #+why,(utp-e '((lang og))))
			      old-plural-d))
		     ;; double declension plural
		     (seq ,(utp-e `((num pl)
				    (cat n)
				    (case {nom erg gen ben dir dat adv inst voc})
				    (stem-type c)
				    (case-type full))
				  :augment 'der)
			  (or nom-group erg-group dat-group gen-group inst-group adv-group voc-group))))))
   (seq ;; Vowel stem
    (or ,(utp "ჲ" `((stem-type {v propv prop-ე "ა"})
		    (sync-stem -)
		    (rigid-stem +)
		    (case { gen dir ben })
		    (lang og)))
	,(utp-e `((stem-type {v prop-ე "ა"}) ;; სარჯ. p. 33, შენ. 8
		  (sync-stem -)
		  (rigid-stem +)
		  (case { gen dir ben })
		  (lang og)))
	,(utp "ი" `((stem-type {v propv prop-ე "ა"})
		    (sync-stem -)
		    (rigid-stem +)
		    (case gen)
		    (style old)
		    (lang ng)))
	,(utp-e `((stem-type "ა") ;; new March 2015
		  (sync-stem -)
		  (rigid-stem +)
		  (cat {n a num masdar v-part q allq pron prop pron+indef })
		  (lang ng)
		  (num sg)))
	;; #-ignore ;; removed April 2016: erroneous forms გულსა -> გალა·ჲ Dat
	,(utp-e `((case dat)
		  (stem-type {v propv prop-ე "ი"})
		  (lang og)))
	,(utp-e `((lang ng)
		  ;; new
		  (stem-type {v propv prop-ე "ი"}))))
    (or ,(utp "სა" '((case dir)
		     (num sg)
		     (lang og)))
	,(utp "სა" '((case ben)
		     (num sg)
		     (lang og)))
	(seq ,(utp "ს" `((stem-type {v "ა" propv prop-ე "ი"})
			 (cat {n a q allq num masdar v-part pron prop})
			 (num sg)
			 (case {dat gen})))
	     (or (seq (or (seq ,(utp "ა")
			       ,(utp-or '("გან" "თვის" "ვით" "კენ" "მებრ")
					`((pp ,morph)
					  (case gen))))
			  ,(utp-or '("გან" "თვის" "ვით" "კენ" "ებრ" "ებრივ")
				   `((pp ,morph)
				     (case gen)))) 
		      c-enclitica)
		 c-enclitica
		 (seq ,(utp "ა" '((long +)))
		      ,(utp-or '("დმი" "კე")
			       `((pp ,morph)))
		      (? v-enclitica))
		 (seq ,(utp-or '("დამი") ;; შევარდნაძისდამი
			       `((pp ,morph)))
		      (? v-enclitica))
		 ,(utp "ა" `((lang og) (long +) (cat {n a q allq num masdar v-part pron})))
		 ,(utp "ა" `((lang ng) (long +)))
		 ;; dative
		 (seq (or ,(utp-or '("თან" "ზედ")
				   `((pp ,morph)
				     (case dat)))
			  ,(utp-or '("ავით")
				   '((pp "ვით")
				     (long +)
				     (case dat))))
		      c-enclitica)
		 ;; double declension
		 (or (seq ,(utp-e '((case gen)))
			  ,(utp-e '((stem-type c)
				    (cat n)
				    (case {nom adv inst})
				    (case-type full)
				    (num sg))
				  :augment 'der)
			  (or nom-group
			      adv-group
			      inst-group))
		     (seq ,(utp-e '((case gen)
				    (lang og)
				    (cat prop)))
			  ;; the following are already augmented, coming after AugmentFlagDiacritics
			  ,(utp-e '((stem-type c)
				    (cat n)
				    (case {erg dat gen})
				    (case-type full)
				    (num sg))
				  :augment 'der)
			  (or gen-group
			      dat-group;; ქრისტჱსსა
			      erg-group))
		     (seq ,(utp-e '((case gen)))
			  (or (seq ,(utp-e '((stem-type {v "ი"}))) ;; ??? nom?? where is stem-type ა?
				   ,(utp "ა" '((stem-type v)
					       (cat n)
					       (case {nom erg gen dat adv inst})
					       (case-type full))
					 :augment 'der))
			      (seq ,(utp-e '((stem-type {propv prop-ე})))
				   ,(utp-e '((stem-type c)
					     (cat n)
					     (case {nom erg gen dat adv inst})
					     (case-type full))
					   :augment 'der)))
			  (or nom-group
			      dat-group
			      gen-group ;; dat is in gen-group for stem-type v
			      erg-group
			      inst-group
			      adv-group
			      voc-group
			      old-plural-d)))
		 ;; double declension plural
		 (seq ,(utp-e '((case gen)))
		      ,(utp-e `((num pl)
				(old-pl -)
				(cat n)
				(case {nom erg gen dat adv inst voc})
				(stem-type c)
				(case-type full))
			      :augment 'der)
		      (or nom-group erg-group dat-group gen-group inst-group adv-group voc-group))))))
   ;; old pl
   (seq (or ,(utp-e '((stem-type {v propv prop-ე c "ი" "ა" bare})
		      (sync-stem -)))
	    ,(utp-or '("ე" "ო") `((stem-type ,morph))))
	,(utp "თა" `((case gen)
		     (num pl)
		     (old-pl +)
		     (cat {n pron a num masdar v-part q allq pron+indef })))
	;; double declension
	(? (seq ,(utp-e '((stem-type v)
			  (cat n)
			  (case-type full)
			  (case {nom erg gen dat adv inst voc}) ;; what about ben, dir?
			  )
			:augment 'der)
		(or ;; avoid double declension without nom marker
		    (seq nom-group ,(utp-e '((lang og) (case nom))))
		    gen-group ;; dat is in gen-group for stem-type v
		    inst-group
		    adv-group
		    erg-group
		    old-plural-d)))))
 :name 'gen-group)


;; for FOMA. recursive-define = ON doesn't work in FOMA
(precompile-u-transducer
 `(or 
   ;; Consonant or truncated stem
   (seq (or (seq plural ,(utp "ი" `((cat {n a num v-part masdar pron prop pron+indef }))))
	    ,(utp "ი" `((stem-type {c "ა" "ე" "ო" bare})
			(sync-stem +)
			(rigid-stem -)
			(cat {n a num masdar v-part q allq pron prop pron+neg+inanim pron+indef })
			(num sg)))
	    ,(utp "-ი" `((stem-type abbr)
			 (cat {n prop})
			 (num sg))))
	(or ,(utp "სა" '((case dir)
			 (lang og)))
	    ,(utp "სა" '((case ben)
			 (lang og)))
	    (seq ,(utp "ს" '((case gen)))
		 (or (seq (or (seq ,(utp "ა" '((long +)))
				   ,(utp-or '("გან" "თვის" "ვით" "კენ" "მებრ")
					    `((pp ,morph))))
			      ,(utp-or '("გან" "თვის" "ვით" "კენ" "ებრ" "ებრივ" "თან")
				       `((pp ,morph))))
			  c-enclitica)
		     c-enclitica
		     (seq ,(utp "ა")
			  ,(utp-or '("დმი" "კე")
				   `((pp ,morph)))
			  (? v-enclitica))
		     ,(utp "ა" `((lang og)
				 (cat {n a num masdar v-part q allq pron pron+indef })
				 (long +)))
		     ,(utp "ა" `((lang ng) (long +)))
		     ;; double case; gen is base case
		     ))))
   (seq ;; Vowel stem
    (or ,(utp "ჲ" `((stem-type {v propv prop-ე "ა"})
		    (sync-stem -)
		    (rigid-stem +)
		    (case { gen dir ben })
		    (lang og)))
	,(utp-e `((stem-type {v prop-ე "ა"}) ;; სარჯ. p. 33, შენ. 8
		  (sync-stem -)
		  (rigid-stem +)
		  (case { gen dir ben })
		  (lang og)))
	,(utp "ი" `((stem-type {v propv prop-ე "ა"})
		    (sync-stem -)
		    (rigid-stem +)
		    (case gen)
		    (style old)
		    (lang ng)))
	,(utp-e `((stem-type "ა") ;; new March 2015
		  (sync-stem -)
		  (rigid-stem +)
		  (cat {n a num masdar v-part q allq pron prop pron+indef })
		  (lang ng)
		  (num sg)))
	;; #-ignore ;; removed April 2016: erroneous forms გულსა -> გალა·ჲ Dat
	,(utp-e `((case dat)
		  (stem-type {v propv prop-ე "ი"})
		  (lang og)))
	,(utp-e `((lang ng)
		  ;; new
		  (stem-type {v propv prop-ე "ი"}))))
    (or ,(utp "სა" '((case dir)
		     (num sg)
		     (lang og)))
	,(utp "სა" '((case ben)
		     (num sg)
		     (lang og)))
	(seq ,(utp "ს" `((stem-type {v "ა" propv prop-ე "ი"})
			 (cat {n a q allq num masdar v-part pron prop})
			 (num sg)
			 (case {dat gen})))
	     (or (seq (or (seq ,(utp "ა")
			       ,(utp-or '("გან" "თვის" "ვით" "კენ" "მებრ")
					`((pp ,morph)
					  (case gen))))
			  ,(utp-or '("გან" "თვის" "ვით" "კენ" "ებრ" "ებრივ")
				   `((pp ,morph)
				     (case gen)))) 
		      c-enclitica)
		 c-enclitica
		 (seq ,(utp "ა" '((long +)))
		      ,(utp-or '("დმი" "კე")
			       `((pp ,morph)))
		      (? v-enclitica))
		 (seq ,(utp-or '("დამი") ;; შევარდნაძისდამი
			       `((pp ,morph)))
		      (? v-enclitica))
		 ,(utp "ა" `((lang og) (long +) (cat {n a q allq num masdar v-part pron})))
		 ,(utp "ა" `((lang ng) (long +)))
		 ;; dative
		 (seq (or ,(utp-or '("თან" "ზედ")
				   `((pp ,morph)
				     (case dat)))
			  ,(utp-or '("ავით")
				   '((pp "ვით")
				     (long +)
				     (case dat))))
		      c-enclitica)
		 ;; double declension
		 
		 ;; double declension plural
		 ))))
   ;; old pl
   (seq (or ,(utp-e '((stem-type {v propv prop-ე c "ი" "ა" bare})
		      (sync-stem -)))
	    ,(utp-or '("ე" "ო") `((stem-type ,morph))))
	,(utp "თა" `((case gen)
		     (num pl)
		     (old-pl +)
		     (cat {n pron a num masdar v-part q allq pron+indef })))
	;; double declension
	))
 :name 'gen-group2)

(precompile-u-transducer
 `(or 
   ;; Consonant or truncated stem
   (seq (or (seq plural ,(utp "ი" `((cat {n a num v-part masdar pron prop pron+indef }))))
	    ,(utp "ი" `((stem-type {c "ა" "ე" "ო" bare})
			(sync-stem +)
			(rigid-stem -)
			(cat {n a num masdar v-part q allq pron prop pron+neg+inanim pron+indef })
			(num sg)))
	    ,(utp "-ი" `((stem-type abbr)
			 (cat {n prop})
			 (num sg))))
	(or ,(utp "სა" '((case dir)
			 (lang og)))
	    ,(utp "სა" '((case ben)
			 (lang og)))
	    (seq ,(utp "ს" '((case gen)))
		 (or (seq (or (seq ,(utp "ა" '((long +)))
				   ,(utp-or '("გან" "თვის" "ვით" "კენ" "მებრ")
					    `((pp ,morph))))
			      ,(utp-or '("გან" "თვის" "ვით" "კენ" "ებრ" "ებრივ" "თან")
				       `((pp ,morph))))
			  c-enclitica)
		     c-enclitica
		     (seq ,(utp "ა")
			  ,(utp-or '("დმი" "კე")
				   `((pp ,morph)))
			  (? v-enclitica))
		     ,(utp "ა" `((lang og)
				 (cat {n a num masdar v-part q allq pron pron+indef })
				 (long +)))
		     ,(utp "ა" `((lang ng) (long +)))
		     ;; double case; gen is base case
		     (seq (or (seq ,(utp-e '((stem-type bare) ;; truncated stem in OG is augmented to c
					     (lang og)))      ;; e.g., შუშანიკისი
				   ,(utp-e '((stem-type c)
					     (cat n)
					     (case {nom erg dat gen ben dir adv adv-tr inst voc})
					     ;; no reduced case in double case
					     (case-type full))
					   :augment 'der))
			      (seq (or ,(utp-not '((stem-type bare))) ;; consonant stem
				       ,(utp-e '((stem-type bare) ;; truncated stem
						 (lang ng))))
				   (or ,(utp "ა" '((stem-type v) ; ###
						   (cat n)
						   (case {nom erg dat adv voc}) ;; no abs?
						   ;; no reduced case in double declension
						   (case-type full))
					     :augment 'der)
				       ,(utp "ა" '((stem-type v)
						   (cat n)
						   (lang og)
						   (case {gen ben dir inst}) ;; no abs?
						   ;; no reduced case in double declension
						   (case-type full))
					     :augment 'der)
				       ,(utp-e '((stem-type c) ;; ბიჭისით, ბიჭისის (??) Hewitt p. 44
						 (cat n)
						 (lang ng)
						 (case {gen inst})
						 ;; no reduced case in double declension
						 (case-type full))
					       :augment 'der))))
			  (or nom-group erg-group dat-group gen-group2 adv-group
			      (seq (or inst-group voc-group)
				   #+why,(utp-e '((lang og))))
			      old-plural-d))
		     ;; double declension plural
		     (seq ,(utp-e `((num pl)
				    (cat n)
				    (case {nom erg gen ben dir dat adv inst voc})
				    (stem-type c)
				    (case-type full))
				  :augment 'der)
			  (or nom-group erg-group dat-group gen-group2 inst-group adv-group voc-group))))))
   (seq ;; Vowel stem
    (or ,(utp "ჲ" `((stem-type {v propv prop-ე "ა"})
		    (sync-stem -)
		    (rigid-stem +)
		    (case { gen dir ben })
		    (lang og)))
	,(utp-e `((stem-type {v prop-ე "ა"}) ;; სარჯ. p. 33, შენ. 8
		  (sync-stem -)
		  (rigid-stem +)
		  (case { gen dir ben })
		  (lang og)))
	,(utp "ი" `((stem-type {v propv prop-ე "ა"})
		    (sync-stem -)
		    (rigid-stem +)
		    (case gen)
		    (style old)
		    (lang ng)))
	,(utp-e `((stem-type "ა") ;; new March 2015
		  (sync-stem -)
		  (rigid-stem +)
		  (cat {n a num masdar v-part q allq pron prop pron+indef })
		  (lang ng)
		  (num sg)))
	;; #-ignore ;; removed April 2016: erroneous forms გულსა -> გალა·ჲ Dat
	,(utp-e `((case dat)
		  (stem-type {v propv prop-ე "ი"})
		  (lang og)))
	,(utp-e `((lang ng)
		  ;; new
		  (stem-type {v propv prop-ე "ი"}))))
    (or ,(utp "სა" '((case dir)
		     (num sg)
		     (lang og)))
	,(utp "სა" '((case ben)
		     (num sg)
		     (lang og)))
	(seq ,(utp "ს" `((stem-type {v "ა" propv prop-ე "ი"})
			 (cat {n a q allq num masdar v-part pron prop})
			 (num sg)
			 (case {dat gen})))
	     (or (seq (or (seq ,(utp "ა")
			       ,(utp-or '("გან" "თვის" "ვით" "კენ" "მებრ")
					`((pp ,morph)
					  (case gen))))
			  ,(utp-or '("გან" "თვის" "ვით" "კენ" "ებრ" "ებრივ")
				   `((pp ,morph)
				     (case gen)))) 
		      c-enclitica)
		 c-enclitica
		 (seq ,(utp "ა" '((long +)))
		      ,(utp-or '("დმი" "კე")
			       `((pp ,morph)))
		      (? v-enclitica))
		 (seq ,(utp-or '("დამი") ;; შევარდნაძისდამი
			       `((pp ,morph)))
		      (? v-enclitica))
		 ,(utp "ა" `((lang og) (long +) (cat {n a q allq num masdar v-part pron})))
		 ,(utp "ა" `((lang ng) (long +)))
		 ;; dative
		 (seq (or ,(utp-or '("თან" "ზედ")
				   `((pp ,morph)
				     (case dat)))
			  ,(utp-or '("ავით")
				   '((pp "ვით")
				     (long +)
				     (case dat))))
		      c-enclitica)
		 ;; double declension
		 (or (seq ,(utp-e '((case gen)))
			  ,(utp-e '((stem-type c)
				    (cat n)
				    (case {nom adv inst})
				    (case-type full)
				    (num sg))
				  :augment 'der)
			  (or nom-group
			      adv-group
			      inst-group))
		     (seq ,(utp-e '((case gen)
				    (lang og)
				    (cat prop)))
			  ;; the following are already augmented, coming after AugmentFlagDiacritics
			  ,(utp-e '((stem-type c)
				    (cat n)
				    (case {erg dat gen})
				    (case-type full)
				    (num sg))
				  :augment 'der)
			  (or gen-group2
			      dat-group;; ქრისტჱსსა
			      erg-group))
		     (seq ,(utp-e '((case gen)))
			  (or (seq ,(utp-e '((stem-type {v "ი"}))) ;; ??? nom?? where is stem-type ა?
				   ,(utp "ა" '((stem-type v)
					       (cat n)
					       (case {nom erg gen dat adv inst})
					       (case-type full))
					 :augment 'der))
			      (seq ,(utp-e '((stem-type {propv prop-ე})))
				   ,(utp-e '((stem-type c)
					     (cat n)
					     (case {nom erg gen dat adv inst})
					     (case-type full))
					   :augment 'der)))
			  (or nom-group
			      dat-group
			      gen-group2 ;; dat is in gen-group for stem-type v
			      erg-group
			      inst-group
			      adv-group
			      voc-group
			      old-plural-d)))
		 ;; double declension plural
		 (seq ,(utp-e '((case gen)))
		      ,(utp-e `((num pl)
				(old-pl -)
				(cat n)
				(case {nom erg gen dat adv inst voc})
				(stem-type c)
				(case-type full))
			      :augment 'der)
		      (or nom-group erg-group dat-group gen-group2 inst-group adv-group voc-group))))))
   ;; old pl
   (seq (or ,(utp-e '((stem-type {v propv prop-ე c "ი" "ა" bare})
		      (sync-stem -)))
	    ,(utp-or '("ე" "ო") `((stem-type ,morph))))
	,(utp "თა" `((case gen)
		     (num pl)
		     (old-pl +)
		     (cat {n pron a num masdar v-part q allq pron+indef })))
	;; double declension
	(? (seq ,(utp-e '((stem-type v)
			  (cat n)
			  (case-type full)
			  (case {nom erg gen dat adv inst voc}) ;; what about ben, dir?
			  )
			:augment 'der)
		(or ;; avoid double declension without nom marker
		    (seq nom-group ,(utp-e '((lang og) (case nom))))
		    gen-group2 ;; dat is in gen-group for stem-type v
		    inst-group
		    adv-group
		    erg-group
		    old-plural-d)))))
 :name 'gen-group1)

(precompile-u-transducer
 `(or 
   ;; Consonant or truncated stem
   (seq (or (seq plural ,(utp "ი" `((cat {n a num v-part masdar pron prop pron+indef }))))
	    ,(utp "ი" `((stem-type {c "ა" "ე" "ო" bare})
			(sync-stem +)
			(rigid-stem -)
			(cat {n a num masdar v-part q allq pron prop pron+neg+inanim pron+indef })
			(num sg)))
	    ,(utp "-ი" `((stem-type abbr)
			 (cat {n prop})
			 (num sg))))
	(or ,(utp "სა" '((case dir)
			 (lang og)))
	    ,(utp "სა" '((case ben)
			 (lang og)))
	    (seq ,(utp "ს" '((case gen)))
		 (or (seq (or (seq ,(utp "ა" '((long +)))
				   ,(utp-or '("გან" "თვის" "ვით" "კენ" "მებრ")
					    `((pp ,morph))))
			      ,(utp-or '("გან" "თვის" "ვით" "კენ" "ებრ" "ებრივ" "თან")
				       `((pp ,morph))))
			  c-enclitica)
		     c-enclitica
		     (seq ,(utp "ა")
			  ,(utp-or '("დმი" "კე")
				   `((pp ,morph)))
			  (? v-enclitica))
		     ,(utp "ა" `((lang og)
				 (cat {n a num masdar v-part q allq pron pron+indef })
				 (long +)))
		     ,(utp "ა" `((lang ng) (long +)))
		     ;; double case; gen is base case
		     (seq (or (seq ,(utp-e '((stem-type bare) ;; truncated stem in OG is augmented to c
					     (lang og)))      ;; e.g., შუშანიკისი
				   ,(utp-e '((stem-type c)
					     (cat n)
					     (case {nom erg dat gen ben dir adv adv-tr inst voc})
					     ;; no reduced case in double case
					     (case-type full))
					   :augment 'der))
			      (seq (or ,(utp-not '((stem-type bare))) ;; consonant stem
				       ,(utp-e '((stem-type bare) ;; truncated stem
						 (lang ng))))
				   (or ,(utp "ა" '((stem-type v) ; ###
						   (cat n)
						   (case {nom erg dat adv voc}) ;; no abs?
						   ;; no reduced case in double declension
						   (case-type full))
					     :augment 'der)
				       ,(utp "ა" '((stem-type v)
						   (cat n)
						   (lang og)
						   (case {gen ben dir inst}) ;; no abs?
						   ;; no reduced case in double declension
						   (case-type full))
					     :augment 'der)
				       ,(utp-e '((stem-type c) ;; ბიჭისით, ბიჭისის (??) Hewitt p. 44
						 (cat n)
						 (lang ng)
						 (case {gen inst})
						 ;; no reduced case in double declension
						 (case-type full))
					       :augment 'der))))
			  (or nom-group erg-group dat-group gen-group1 adv-group
			      (seq (or inst-group voc-group)
				   #+why,(utp-e '((lang og))))
			      old-plural-d))
		     ;; double declension plural
		     (seq ,(utp-e `((num pl)
				    (cat n)
				    (case {nom erg gen ben dir dat adv inst voc})
				    (stem-type c)
				    (case-type full))
				  :augment 'der)
			  (or nom-group erg-group dat-group gen-group1 inst-group adv-group voc-group))))))
   (seq ;; Vowel stem
    (or ,(utp "ჲ" `((stem-type {v propv prop-ე "ა"})
		    (sync-stem -)
		    (rigid-stem +)
		    (case { gen dir ben })
		    (lang og)))
	,(utp-e `((stem-type {v prop-ე "ა"}) ;; სარჯ. p. 33, შენ. 8
		  (sync-stem -)
		  (rigid-stem +)
		  (case { gen dir ben })
		  (lang og)))
	,(utp "ი" `((stem-type {v propv prop-ე "ა"})
		    (sync-stem -)
		    (rigid-stem +)
		    (case gen)
		    (style old)
		    (lang ng)))
	,(utp-e `((stem-type "ა") ;; new March 2015
		  (sync-stem -)
		  (rigid-stem +)
		  (cat {n a num masdar v-part q allq pron prop pron+indef })
		  (lang ng)
		  (num sg)))
	;; #-ignore ;; removed April 2016: erroneous forms გულსა -> გალა·ჲ Dat
	,(utp-e `((case dat)
		  (stem-type {v propv prop-ე "ი"})
		  (lang og)))
	,(utp-e `((lang ng)
		  ;; new
		  (stem-type {v propv prop-ე "ი"}))))
    (or ,(utp "სა" '((case dir)
		     (num sg)
		     (lang og)))
	,(utp "სა" '((case ben)
		     (num sg)
		     (lang og)))
	(seq ,(utp "ს" `((stem-type {v "ა" propv prop-ე "ი"})
			 (cat {n a q allq num masdar v-part pron prop})
			 (num sg)
			 (case {dat gen})))
	     (or (seq (or (seq ,(utp "ა")
			       ,(utp-or '("გან" "თვის" "ვით" "კენ" "მებრ")
					`((pp ,morph)
					  (case gen))))
			  ,(utp-or '("გან" "თვის" "ვით" "კენ" "ებრ" "ებრივ")
				   `((pp ,morph)
				     (case gen)))) 
		      c-enclitica)
		 c-enclitica
		 (seq ,(utp "ა" '((long +)))
		      ,(utp-or '("დმი" "კე")
			       `((pp ,morph)))
		      (? v-enclitica))
		 (seq ,(utp-or '("დამი") ;; შევარდნაძისდამი
			       `((pp ,morph)))
		      (? v-enclitica))
		 ,(utp "ა" `((lang og) (long +) (cat {n a q allq num masdar v-part pron})))
		 ,(utp "ა" `((lang ng) (long +)))
		 ;; dative
		 (seq (or ,(utp-or '("თან" "ზედ")
				   `((pp ,morph)
				     (case dat)))
			  ,(utp-or '("ავით")
				   '((pp "ვით")
				     (long +)
				     (case dat))))
		      c-enclitica)
		 ;; double declension
		 (or (seq ,(utp-e '((case gen)))
			  ,(utp-e '((stem-type c)
				    (cat n)
				    (case {nom adv inst})
				    (case-type full)
				    (num sg))
				  :augment 'der)
			  (or nom-group
			      adv-group
			      inst-group))
		     (seq ,(utp-e '((case gen)
				    (lang og)
				    (cat prop)))
			  ;; the following are already augmented, coming after AugmentFlagDiacritics
			  ,(utp-e '((stem-type c)
				    (cat n)
				    (case {erg dat gen})
				    (case-type full)
				    (num sg))
				  :augment 'der)
			  (or gen-group1
			      dat-group;; ქრისტჱსსა
			      erg-group))
		     (seq ,(utp-e '((case gen)))
			  (or (seq ,(utp-e '((stem-type {v "ი"}))) ;; ??? nom?? where is stem-type ა?
				   ,(utp "ა" '((stem-type v)
					       (cat n)
					       (case {nom erg gen dat adv inst})
					       (case-type full))
					 :augment 'der))
			      (seq ,(utp-e '((stem-type {propv prop-ე})))
				   ,(utp-e '((stem-type c)
					     (cat n)
					     (case {nom erg gen dat adv inst})
					     (case-type full))
					   :augment 'der)))
			  (or nom-group
			      dat-group
			      gen-group1 ;; dat is in gen-group for stem-type v
			      erg-group
			      inst-group
			      adv-group
			      voc-group
			      old-plural-d)))
		 ;; double declension plural
		 (seq ,(utp-e '((case gen)))
		      ,(utp-e `((num pl)
				(old-pl -)
				(cat n)
				(case {nom erg gen dat adv inst voc})
				(stem-type c)
				(case-type full))
			      :augment 'der)
		      (or nom-group erg-group dat-group gen-group1 inst-group adv-group voc-group))))))
   ;; old pl
   (seq (or ,(utp-e '((stem-type {v propv prop-ე c "ი" "ა" bare})
		      (sync-stem -)))
	    ,(utp-or '("ე" "ო") `((stem-type ,morph))))
	,(utp "თა" `((case gen)
		     (num pl)
		     (old-pl +)
		     (cat {n pron a num masdar v-part q allq pron+indef })))
	;; double declension
	(? (seq ,(utp-e '((stem-type v)
			  (cat n)
			  (case-type full)
			  (case {nom erg gen dat adv inst voc}) ;; what about ben, dir?
			  )
			:augment 'der)
		(or ;; avoid double declension without nom marker
		    (seq nom-group ,(utp-e '((lang og) (case nom))))
		    gen-group1 ;; dat is in gen-group for stem-type v
		    inst-group
		    adv-group
		    erg-group
		    old-plural-d)))))
 :name 'gen-group)

;; old-plural
(precompile-u-transducer
 `(seq (or ,(utp-e '((stem-type {v propv prop-ე c "ი" "ა"})
                     (sync-stem -)))
           ,(utp-or '("ე" "ო") `((stem-type ,morph))))
       (or (seq ,(utp "ნი" `((case nom)
			     (num pl)
			     (old-pl +)
			     (cat {n pron prop a num masdar v-part q allq pron+indef })))
		(? v-enclitica))
           ,(utp "ნო" `((case voc)
                         (num pl)
                         (old-pl +)
                         (cat {n a num masdar v-part pron pron+indef })))
           ,(utp "თა" `((case dir)
			(num pl)
			(old-pl +)
			(lang og)
			(cat {n a})))
           ,(utp "თა" `((case ben)
			(num pl)
			(old-pl +)
			(lang og)
			(cat {n a})))
           (seq ,(utp "თ" `((case {erg dat gen})
			    (num pl)
			    (old-pl +)
			    (cat {n pron a num masdar v-part q allq pron+indef })))
                (? (or (seq (? ,(utp "ა" '(#+ignore(long +)))) ;; check when ა can be dropped
			    (or (seq ,(utp-or '("თვის" "ვით" "კენ" "ებრ" "ებრივ")
					      `((pp ,morph)
						(case gen)))
				     c-enclitica)
				(seq ,(utp-or '("კე")
					      `((pp ,morph)
						(case gen)))
				     v-enclitica)
				(seq ,(utp "გან" `((pp "გან")
						   (case gen)))
				     (or c-enclitica
					 (? (seq ,(utp-e '((double +)))
						 ,(utp-e '((stem-type c)
							   (cat n))
							 :augment 'der)
						 (or nom-group 
						     erg-group dat-group
						     gen-group adv-group inst-group voc-group
						     (seq ,(utp-e '((double -))) ;; blocks recursion
							  old-plural-d))))))))
		       (seq ,(utp "ა") (? v-enclitica)))))))
 :name 'old-plural)

;; -iani obsolete?
#+ignore
(precompile-u-transducer
 `(seq (or (seq plural
		,(utp "ი" `((case nom)
			    (cat {n a ;; v-part masdar
				 pron pron+indef })
			    (case-type full))))
	   ,(utp "ი" `((case nom)
		       (num sg)
		       (sync-stem +) ;; was -
		       (stem-type c)
		       (cat {n a num ;; v-part
			    pron prop q allq
			    pron+indef })
		       (case-type full)))
	   ;; what about vowel stems?
	   ,(utp-e `((case nom)	;; fixme for enclitica etc,
		     (num sg)
		     (stem-type { abbr bare } )
		     (cat {n prop})
		     (case-type full))))
       (seq ,(utp "ან" '((stem-type c)
			 (der-type "იანი")
			 (cat a))
		  :augment 'der)
	    (or nom-group      ;; contains
		erg-group      ;; adjectives
		(seq ,(utp-e '((case-type full)))
		     (or dat-group gen-group inst-group adv-group
			 old-plural)))))
 :name 'iani)

#+ignore
(precompile-u-transducer
 `(seq (or (seq der-plural
		,(utp "ი" `((case nom)
			    (cat {n pron})
			    (case-type full))))
	   ,(utp "ი" `((case nom)
		       (num sg)
		       (sync-stem -)
		       (stem-type c)
		       (cat {n a num ;; masdar v-part
			    pron prop})
		       (case-type full)))
	   ,(utp-e `((case nom)	;; fixme for enclitica etc,
		     (num sg)
		     (stem-type { abbr bare } )
		     (cat {n prop})
		     (case-type full))))
       (seq ,(utp "ან" '((stem-type c)
			 (lex "იან·ი")
			 (cat a))
		  :augment 'der)
	    (or nom-group      ;; contains
		erg-group      ;; adjectives
		(seq ,(utp-e '((case-type full)))
		     (or dat-group gen-group inst-group adv-group
			 old-plural)))))
 :name 'iani1)

(precompile-u-transducer
 `(or (seq ,(utp-or '("ქვეშ" "წინ" "უკან" "შესახებ" "მიერ" "მიმართ" "შემდეგ"
                      "წინააღმდეგ" ;; "დროს"
		      "ნაცვლად"
		      "მაგიერ" "მაგიერად" "მაგივრად" "ძირს" 
                      "გულისთვის" "მიხედვით" "შედეგად" "ირგვლივ"
		      ;;"გარდა"
		      "მიუხედავად" "თანახმად" "ნაცვლად"
		      "მხრივ" ;; ??
		      "ბოლოს"
		      "ძირას"
		      "მაგივრად"
		      "გავლით"
		      "გარეთ"
		      "გასწვრივ"
		      "ახლოს"
		      "შემდგომ"
		      "გვერდით"
		      "კვალდაკვალ"
		      )
                    `((cat pp)
                      (lex ,morph) 
		      (lang ng)
		      ((comp case) gen)))
           (? c-enclitica))
      (seq ,(utp-or '("წინაშე" "გამო" "გარშემო" "თაობაზე" "მერე")
                    `((cat pp)
                      (lex ,morph) 
		      (lang ng)
                      ((comp case) gen)))
           (? v-enclitica))
      ,(utp-or '("ზედა" "თანა" ;; "შორის" "შოვრის"
		 "შინა" "წინაშე" "კუეშე"
		 ;;"უწინარეს"
		 "უწინარჱს"
		 "წინ"
		 "ზემოჲთ"
		 "ქუe" "ქუeშა" "ქუeშe" "დაშუე"
		 "შიდა"
		 "მარცხლ" "მარჯულ" "მარჯუნ"
		 ;;"გარეშე"
		 "გარეშენ"
		 "გარე" ;; "გარეშე"
		 "გარეშe"
		 "უკანაკერძო" "უკუანაკერძო"
		 "პირისპირ"
		 "წიაღ" ;; OG; dat + gen + inst?
		 )
	       `((cat pp)
		 (lex ,morph) 
		 (lang og)
		 ((comp case) dat)))
      
      ,(utp-or '("ზე" "თვინიერ" "თანა" "წინაშე" "მიერ" "ზედა" "წილ"
		 "ქუeშe"
		 "წინა" "ზედა"
		 "ზესთა" "ზეშთა" "ჟეჟთა"
		 "შუვა" "შოვა"
		 "უკანა" "უკუანა"
		 ;; not sure which case
		 "ზემოჲთ" "მარჯუენით" "მარჯუნით" "მარცხენით"
		 "გარემო" "უკუანაგან"
		 "უკუანაჲსკნელ" "უკანაჲსკნელ" "უკანაჲსკნელს" "უკუანაჲს"
		 "პირველ" "პირველად" "ქუეაღ"
		 "ქუემო-კერძო" "ქუე-კერძო"
		 "შინაგან"
		 "წინაჲთ" "წინაჲსწარ"
		 "გარეგან"
		 "გუერდით"
		 "შიგან" ;; MG
		 "შემდგომად"
		 "წიაღ" ;; OG; dat + gen + inst?
		 )
	       `((cat pp)
		 (lex ,morph) 
		 (lang og)
		 ((comp case) gen)))

      ,(utp-or '("მიმართ" "მომართ")	 ;; OG, mit Aditiv
	       `((cat pp)
		 (lex ,morph) 
		 (lang og)
		 ((comp case) gen)))
      
      ,(utp-or '("გამო" "გარდამო" "კერძო" "მიერ"
		 "გამომართ" "გამოღმართ" "გარდამართ"
		 "მიმართ" "მომართ" ;; case?
		 "წიაღ"
		 )
	       `((cat pp)
		 (lang og)
		 (lex ,morph) 
		 ((comp case) inst)))

      ,(utp-or '("ვიდრე") ;; ვიდრე -დმდე
	       `((cat pp)
		 ;;(lang og)
		 (lex ,morph) 
		 ((comp case) adv)))
      
      (seq ,(utp-or '("შუა")
                    `((cat pp)
                      (lex ,morph) 
                      ((comp case) dat)))
           (? v-enclitica))
      
      (seq ,(utp-or '("შორის" "შოვრის" "აქეთ" "იქით" "ქვეშ" "მიღმა"
		      "გადაღმა" "გარეშე" "გარდა" "წინათ" "პირად")
                    `((cat pp)
                      (lex ,morph) 
                      ((comp case) dat-gen)))
           (? c-enclitica))
      (seq ,(utp-or '("ერთად" "დაკავშირებით" "შედარებით" "ახლოს")
                    `((cat pp)
                      (lex ,morph) 
                      ((comp pp) "თან")))
           (? c-enclitica))
      (seq ,(utp-or '("განსხვავებით")
                    `((cat pp)
                      (lex ,morph) 
                      ((comp pp) "გან")))
           (? c-enclitica)))
 :name 'postposition)

(precompile-u-transducer
 `(or ,(utp-or '("ვე" "ღა") `((mod-sfx ,morph)))
      (seq ,(utp "ც" `((rel-sfx "ც")))
           (? ,(utp "ა" `((long +)))))) 
 :name 'enclitica-ve-Ga-C)

(precompile-u-transducer
 `(or (seq ,(utp-or '("ვე" "ღა") `((mod-sfx ,morph)))
           (? encl-aux))   
      (seq ,(utp-or '("ემც") `((mod-sfx "მც")))
           (? encl-aux))
      (seq ,(utp "ეც" `((rel-sfx "ც")))
           (? (seq ,(utp "ა" `((long +)))
                   (? encl-aux))))
      (seq ,(utp "ა" `((long +)))
	   encl-aux))
 :name 'e-enclitica-ve-Ga-C)

(precompile-u-transducer
 `(or (seq ,(utp-or '("ვე" "ღა") `((mod-sfx ,morph)))
           (? encl-aux))
      (seq ,(utp-or '("იმც") `((mod-sfx "მც")))
           (? encl-aux))
      (seq ,(utp "იც" `((rel-sfx "ც")))
           (? (seq ,(utp "ა" `((long +)))
                   (? encl-aux))))
      (seq ,(utp "ა" `((long +)))
	   (? encl-aux))) 
 :name 'i-enclitica-ve-Ga-C)

(precompile-u-transducer
 `(or (seq ,(utp-or '("ვე" "ღა") `((mod-sfx ,morph)))
           (? encl-aux))
      (seq ,(utp-or '("ამც") `((mod-sfx "მც")))
           (? encl-aux))
      (seq ,(utp "აც" `((rel-sfx "ც")))
           (? (seq ,(utp "ა" `((long +) (lang ng)))
                   (? encl-aux))))
      (seq ,(utp "ა" `((long +)))
	   encl-aux))
 :name 'a-enclitica-ve-Ga-C)

(precompile-u-transducer
 `(? (or (? encl-aux)
	 (seq ,(utp-or '("მც") `((mod-sfx "მც")))
	      (? (seq ,(utp "ა" `((long +)))
		      (? encl-aux))))
	 (seq ,(utp "ც" `((rel-sfx "ც")))
	      (? (seq ,(utp "ა" `((long +)))
		      (? encl-aux)))))) 
 :name 'v-enclitica-C)

(precompile-u-transducer
 `(? (seq ,(utp "ა" `((long +)))
	  (or (? encl-aux)
	      (seq ,(utp "ც" `((rel-sfx "ც")))
		   (? (seq ,(utp "ა" `((long +)))
			   (? encl-aux)))))))
 :name 'c-enclitica-C)

;; FIXME: enclitica for OG
(precompile-u-transducer
 `(or (seq (or ,(utp-or '("ში" "ზე" "შუა")
                        `((case dat)
                          (pp ,morph)))
               ,(utp "სადმი" `((case gen)
			       (pp "დმი")))
	       ,(utp "დამი" `((case gen)
			      (pp "დამი")
			      (lang ng)))
	       ,(utp "დამი" `((case dir)
			      (pp "მი")
			      (lang og)))
	       ,(utp "დამო" `((case dir)
			      (pp "მო")
			      (lang og)))
	       ,(utp-or '("და" "კე")
			`((case gen)
			  (pp ,morph)
			  (lang ng)))
	       ,(utp "და" `((case dir)
			    (lang og)))
	       ,(utp "და" `((case ben)
			    (lang og)))
	       ,(utp "ამდე" `((case adv)
			      (pp "მდე"))))
           (? v-enclitica))
      (seq (or ,(utp-or '("თან" "ზედ")
                        `((case dat)
                          (pp ,morph)))
               ,(utp-or '("გან" "თვის" "თვინა" "ებრ" "ებრივ" "კენ" "წინ")
                        `((case gen)
                          (pp ,morph)))
               ,(utp-or '("თვინა")
                        `((case gen)
                          (pp "თვის")
			  (style nonstandard)))
               ,(utp "სკენ" `((case gen)
			      (pp "კენ")))
               ,(utp "სავით" `((case dat)
			       (pp "ვით")))
	       ,(utp "ამდის" `((case adv)
			       (pp "მდე"))))
           (? c-enclitica))
      (seq ,(utp "სა" '((case gen)))
	   ,(utp-e '((stem-type v)
		     (cat n)
		     (case dat)
		     (case-type full)
		     (num sg))
		   :augment 'der)
	   gen-group ;; (or gen-group dat-group)
	   )
      ,(utp "სა" '((case dat) (long +))) ;; ჩემსა და… (e.g. … კარს შუა)
      (seq ,(utp-e '((case dat))) (? ,(utp "ს")))  ;; ჩემს წინ; ჩემს შესახებ und ჩემ შესახებ
      )
 :name 'pronoun-pp)

;; more pl missing!
(precompile-u-transducer
 `(or (seq ,(utp-or '("ადმი")
		    `((case gen)
		      (pp "დმი")))
           (? v-enclitica))
      (seq (or ,(utp-or '("თან" "ზედ")
                        `((case dat)
                          (pp ,morph)))
               
	       (seq (? ,(utp "ა"))
		    ,(utp-or '("გან" "თვის" "კენ" "ებრ" "ებრივ")
			     `((case gen)
			       (pp ,morph))))
               (seq (? ,(utp "ს" '((num pl))))
                    ,(utp "ავით"
                          `((case dat)
                            (pp "ვით")))))
           (? (or c-enclitica
		  ,(utp "ა" `((long +)))))))
 :name 'det-pp)

(precompile-u-transducer
 `(or (seq (or ,(utp-or '("ში" "ზე")
                        `((case dat)
                          (pp ,morph)))
	       ,(utp-or '("ზედ")
                        `((case dat)
                          (pp "ზედ"))))
           (? v-enclitica)))
 :name 'det-dat-pp)

;; pronoun
(precompile-u-transducer
 `(or (seq ,(utp-e '((cat pron)
		     (sub-cat pers)))
	   (or
            ;; 1. & 2. pers.
            (seq ,(utp-e '((cat pron)))
		 (or (seq (or ,(utp "შენ" `((num sg)
					    (pers 2)
					    (lex "შენ")
					    (case {nom erg dat})))
			      ,(utp "ჩueნ" `((num pl)
					     (pers 1)
					     (lex "ჩuენ")
					     (case {nom erg dat})))
			      ,(utp "თქueნ" `((num pl)
					      (pers 2)
					      (lex "თქuენ") 
					      (case {nom erg dat}))))
                          (? (or enclitica-ve-Ga-C
				 ,(utp "ა" `((long +)))
				 )))
		     (seq ,(utp "მე" `((num sg)
				       (pers 1)
				       (lex "მე") 
				       (case {nom erg dat})))
			  (? enclitica-ve-Ga-C))
                     (seq (or ,(utp "ჩემ" `((num sg)
                                            (pers 1)
                                            (lex "მე")))
                              ,(utp "შენ" `((num sg)
                                            (pers 2)
                                            (lex "შენ")))
			      ,(utp "ჩueნ" `((num pl)
                                             (pers 1)
                                             (lex "ჩuენ")))
                              ,(utp "თქueნ" `((num pl)
                                              (pers 2)
					      (lex "თქuენ"))))
                          pronoun-pp)))
	    ,(utp "შე" `((num sg)
			 (pers 2)
			 (lex "შენ") 
			 (case voc)))
	    ,(utp "თქვე" `((num pl)
			   (pers 2)
			   (lex "თქვენ")
			   (lang ng)
			   (case voc)))
	    ,(utp "თქuე" `((num pl)
			   (pers 2)
			   (lex "თქუენ")
			   (lang og)
			   (case voc)))
	    ;; 3.pers
            ;; nom
            (seq (or ,(utp "იგი" `((num sg)
                                   (pers 3)
                                   (lex "იგი")
                                   (case nom)))
                     ,(utp-or '("ისინი" "ისები")
                              `((num pl)
                                (pers 3)
                                (lex "ის")
                                ;; (cat det)
                                (case nom)))
                     ,(utp "იგინი"
                           `((num pl)
                             (pers 3)
                             (lex "იგი") 
                             (case nom)))
                     ,(utp-or '("ესენი" "ესები")
                              `((num pl)
                                (pers 3)
                                (lex "ეს") 
                                (case nom)))
                     ,(utp-or '("ეგენი" "ეგები")
                              `((num pl)
                                (pers 3)
                                (lex "ეგ") 
                                (case nom))))
                 (? v-enclitica))
            (seq ,(utp-or '("ის") `((num sg)
                                    (pers 3)
                                    (lex ,morph) 
                                    (case nom)))
                 (? i-enclitica-ve-Ga-C))
            (seq ,(utp-or '("ეს") `((num sg)
                                    (pers 3)
                                    (lex ,morph)
                                    (case nom)))
                 (or (? e-enclitica-ve-Ga-C)
		     ,(utp "ა" `((long +)))))
	    (seq ,(utp-or '("ეგ") `((num sg)
                                    (pers 3)
                                    (lex ,morph) 
                                    (case nom)))
                 (? e-enclitica-ve-Ga-C))
	    ;; oblique cases ;; bound forms see determiner-ng/og 
	    (seq (or ,(utp "იმ" `((lex "ის")))
                     ,(utp "ამ" `((lex "ეს")))
                     ,(utp "მაგ" `((lex "ეგ"))))
                 ,(utp-e '((pers 3)))
		 (or (seq (or ,(utp "ან" `((case erg)
					   (num sg)))
			      ,(utp "ის" `((case gen)
					   (num sg)))
			      ,(utp "ას" `((case dat)
					   (num sg)))
			      ,(utp "ად" `((case adv)
					   (num sg)))
			      ,(utp "ათ" `((case adv)
					   (num sg)
					   (lang ng)
					   (style nonstandard)))
			      ,(utp "ით" `((case inst)
					   (num sg)))
			      ,(utp "ებს" `((case dat)
					    (num pl)))
			      ,(utp "ების" `((case gen)
					     (num pl)))
			      ,(utp "ებად" `((case adv)
					     (num pl)))
			      ,(utp "ებათ" `((case adv)
					     (num pl)
					     (lang ng)
					     (style nonstandard)))
			      ,(utp "ებით" `((case inst)
					     (num pl)))
			      ,(utp "ათ" `((case {erg dat gen})
					   (num pl))))
			  (? (or ,(utp "ა" `((long +))) ;; FIXME: add -ა for some of them
				 det-pp
				 a-enclitica-ve-Ga-C)))
		     (seq (or ,(utp "ის" `((case gen)
                                           (num sg))))
                          (? (or det-pp
				 a-enclitica-ve-Ga-C)))
		     ,(utp-or '("ისა" "ისდა")
			      `((case dir)
				(num sg)
				(lang og)))
		     (seq ,(utp "ისდა"
				`((case dir)
				  (num sg)
				  (lang og)))
			  (? ,(utp-or '("მო" "მი") ;; -დამი, -დამო; todo: add თჳსდამი
				      `((pp ,morph)))))
		     ,(utp-or '("ისდა")
			      `((case gen)
				(pp "და")
				(num sg)
				(lang ng)))
		     ,(utp-or '("ათდა")
			      `((case gen)
				(pp "და")
				(num pl)
				(lang ng)))
		     (seq (or ,(utp "ა" `((case dat)
					  (num sg))))
                          det-dat-pp)
		     (seq ,(utp "ებმა" `((case erg)
                                         (num pl)))
                          (? (or enclitica-ve-Ga-C)))))
	    (seq ,(utp "ამა" `((lex "ეს") (pers 3) (num pl)))
		 (or (seq (or ,(utp "ების" `((case gen)
					     (num pl)))
			      ,(utp "ებად" `((case adv)
					     (num pl)))
			      ,(utp "ებათ" `((case adv)
					     (num pl)
					     (lang ng)
					     (style nonstandard)))
			      ,(utp "ებით" `((case inst)
					     (num pl))))
			  (? (or ,(utp "ა" `((long +))) ;; FIXME: add -ა for some of them
				 det-pp
				 a-enclitica-ve-Ga-C)))
		     (seq ,(utp "ებმა" `((case erg)
                                         (num pl)))
                          (? (or enclitica-ve-Ga-C)))))
	    (seq ,(utp "მ" `((lex "ის") (cat pron)))
                 ,(utp-e '((pers 3)))
                 (or (seq (or ,(utp "ან" `((case erg)
                                           (num sg)))
                              ,(utp "ას" `((case dat)
                                           (num sg)))
                              ,(utp "ის" `((case gen)
                                           (num sg)
					   (lang ng)))
			      ,(utp "ად" `((case adv)
                                           (num sg)))
			      ,(utp "ით" `((case inst)
					    (num sg)))
                              ,(utp "ათ" `((case {erg dat gen})
					   (num pl))))
                          (? (or ,(utp "ა")
				 det-pp
				 det-dat-pp
				 a-enclitica-ve-Ga-C)))
		     ,(utp "ასსა" `((case dat) ;; მასსა და …
				    (num sg)
				    (long +)))
		     (seq (or ,(utp "ის" `((case gen)
                                           (num sg)
					   (lang og))))
                          (? (or det-pp
				 a-enclitica-ve-Ga-C)))
		     ,(utp-or '("ისა" "ისდა")
			      `((case dir) ;; + ben?
				(num sg)
				(lang og)))
		     (seq ,(utp "ისდა"
				`((case dir)
				  (num sg)
				  (lang og)))
			  (? ,(utp-or '("მო" "მი")
				      `((pp ,morph)))))
		     ,(utp-or '("ისდა")
			      `((case gen)
				(pp "და")
				(num sg)
				(lang ng)))
		     ,(utp-or '("ათდა")
			      `((case gen)
				(pp "და")
				(num pl)
				(lang ng)))
		     ,(utp-or '("ათა" "ათდა")
			      `((case dir)
				(num sg)
				(lang og)))
		     (seq ,(utp "ათდა"
				`((case dir)
				  (num pl)
				  (lang og)))
			  (? ,(utp-or '("მო" "მი")
				      `((pp ,morph)))))))
            (seq (or ,(utp-or '("იგი" "იმა")
                              `((num sg)
                                (pers 3)
                                (morph ,morph)
                                (lex "იგი")))
                     ,(utp-or '("ამა")
                              `((num sg)
                                (pers 3)
                                (morph ,morph)
                                (lex "ეს"))))
                 ,(utp "ვე" `((mod-sfx "ვე")))
                 (or (seq ,(utp "მ" '((case erg) ;; *igivem does not exist
                                      (morph {"იმა" "ამა"})))
                          (? ,(utp "აც" `((rel-sfx "ც")))))
                     (seq ,(utp "ს" '((case {dat gen}))) c-enclitica-C)
                     ,(utp "თი" '((case inst)))
                     (seq ,(utp "დ" '((case adv))) c-enclitica-C)
                     (seq ,(utp "თ" '((case adv)
				      (lang ng)
				      (style nonstandard)))
			  c-enclitica-C)
                     (seq ,(utp-or '("ში" "ზე")
                                   `((case dat)
                                     (pp ,morph)))
                          v-enclitica-C)
		     (seq ,(utp-or '("ზედ")
                                   `((case dat)
                                     (pp "ზედ")))
                          c-enclitica-C)))
            (seq ,(utp-or '("იგი")
                          `((pers 3)
                            (lex "იგი")))
                 ,(utp "ვე" `((mod-sfx "ვე")))
                 ,(utp "ნი" '((case nom)
			      (num pl)))
                 (? encl-aux))))
      (seq (or ,(utp "ერთმანეთ"
		     `((num sg)
		       (pers 3)
		       (lex "ერთმანეთ·ი")
		       (stem-type c)
		       (cat pron)
		       (sub-cat recip)))
	       ,(utp "ერთმანერთ"
		     `((num sg)
		       (pers 3)
		       (lex "ერთმანერთ·ი")
		       (stem-type c)
		       (cat pron)
		       (sub-cat recip)
		       (lang og)))
	       ,(utp "ერთიმეორე"
		     `((num sg)
		       (pers 3)
		       (lex "ერთიმეორ[ე]")
		       (stem-type c)
		       (cat pron)
		       (sub-cat recip))))
	   (or nom-group
	       erg-group ;; is attested!
	       (seq ,(utp-e '((case-type full)))
		    (or dat-group gen-group inst-group adv-group))
	       ;; iani
	       ))
      ,(utp-or '("ურთიერთას"
		 "ერთიერთას")
	       `((num sg)
		 (pers 3)
		 (lex ,morph)
		 (cat pron)
		 (sub-cat recip)
		 (lang og))))                
 :name 'pronoun)

(precompile-u-transducer
 `(or (seq (or (seq (or ,(utp "ა" `((cat pron+neg+anim)
				    (lang ng)
				    (lex "არაvინ")))
			,(utp "ნუ" `((cat pron+neg+anim)
				     (lang ng)
				     (lex "ნურაvინ")
				     (neg-type imp)))
			,(utp "ვე" `((cat pron+neg+anim)
				     (lang ng)
				     (lex "ვერაvინ")
				     (neg-type pot)))
			,(utp "ა" `((cat pron+neg+anim)
				    (lang og)
				    (lex "არvინ")))
			,(utp "ნუ" `((cat pron+neg+anim)
				     (lang og)
				     (lex "ნუvინ")
				     (neg-type imp)))
			,(utp "ვე" `((cat pron+neg+anim)
				     (lang og)
				     (lex "ვერvინ")
				     (neg-type pot)))
			,(utp "აღა" `((cat pron+neg+anim)
				    (lang ng)
				    (lex "აღარაvინ")))
			,(utp "ნუღა" `((cat pron+neg+anim)
				     (lang ng)
				     (lex "ნუღარაvინ")
				     (neg-type imp)))
			,(utp "ვეღა" `((cat pron+neg+anim)
				     (lang ng)
				     (lex "ვეღარაvინ")
				     (neg-type pot)))
			,(utp "აღა" `((cat pron+neg+anim)
				    (lang og)
				    (lex "აღარvინ")))
			,(utp "ნუღა" `((cat pron+neg+anim)
				     (lang og)
				     (lex "ნუღარvინ")
				     (neg-type imp)))
			,(utp "ვეღა" `((cat pron+neg+anim)
				     (lang og)
				     (lex "ვეღარvინ")
				     (neg-type pot))))
		    #+ignore
		    (? ,(utp "ღა" `((mod-sfx "")))))
	       ,(utp "არღა" `((cat pron+neg+anim)
			       (lex "აღარაvინ")
			       ;;(mod-sfx "ღა")
			       (lang og)))
	       ,(utp "ვერღა" `((cat pron+neg+anim)
			       (lex "ვეღარაvინ")
			       (neg-type pot)
			       ;;(mod-sfx "ღა")
			       (lang og))))
	   (or ,(utp "რავი" `())
	       ,(utp "რვი" `((lang og))) ;; არვინ, ვერვინ
	       ,(utp "ვი" `((lang og) (neg-type imp)))) ;; ნუვინ
	   (or (seq ,(utp "ნ" `((case {nom erg})))
		    c-enclitica
		    #+ignore
		    (? (seq ,(utp "ა" `((long +)))
			    (? encl-aux))))
	       (seq ,(utp "ს" `((case {dat gen}))) ;; კე?
		    (? (seq ,(utp-or '("გან" "თან" "ზედ" "თვის" "კენ" "ებრ" "ებრივ") ;; ???
				     `((case gen)
				       (pp ,morph)))
			    (? c-enclitica))))
	       ,(utp "სა" `((case dir)))))
      (seq (or ,(utp "არარ" `((cat pron+neg+inanim)
			      (lex "არარ[ა]·ჲ")))
	       ,(utp "ნურარ" `((cat pron+neg+inanim)
			       (lex "ნურარ[ა]·ჲ")
			       (neg-type imp)))
	       ,(utp "ვერარ" `((cat pron+neg+inanim)
			       (lex "ვერარ[ა]·ჲ")
			       (neg-type pot)))
	       ,(utp "არღარ" `((cat pron+neg+inanim)
			       (lex "არღარ[ა]·ჲ")))
	       ,(utp "ნურღარ" `((cat pron+neg+inanim)
				(lex "ნურღარ[ა]·ჲ")
				(neg-type imp)))
	       ,(utp "ვერღარ" `((cat pron+neg+inanim)
				(lex "ვერღარ[ა]·ჲ")
				(neg-type pot))))
	   (or (seq ,(utp "ა" `((case nom)
				(lang ng))))
	       (seq ,(utp "ა" `((case abs)
				(lang og))))
	       (seq ,(utp "ამ" `((case erg)
				(lang ng))))
	       (seq ,(utp "ამან" `((case erg) ;; ?
				   (lang og))))
	       (seq ,(utp "აჲ" `((case nom)
				 (lang og))))
	       (seq ,(utp "აჲთ" `((case inst)
				  (lang og))))
	       (seq ,(utp "ას" `((case dat))))
	       (seq ,(utp "ისა" `((case dir))))
	       (seq ,(utp "ის" `((case gen)))
		    (? (seq ,(utp-or '("გან" "თან" "ზედ" "თვის" "კენ" "ებრ" "ებრივ") ;; ???
				     `((pp ,morph)))
			    (? c-enclitica))))
	       (seq ,(utp "ის" `((case gen)))
		    (? (seq ,(utp-or '( "კე")
				     `((pp ,morph)))
			    (? v-enclitica)))))))
 :name 'pron-negative)

;; interrogative/relative/negative
(precompile-u-transducer
 `(or (seq (or ,(utp "ვი" `((cat pron+interr+anim)
			    (lex "vინ"))))
           (or (seq ,(utp "ნ" `((case {nom erg}))) ;; fixme: ვიღაც
                    (? (or v-enclitica-Ga-C-a
			   og-rel-enclitica)))
	       ,(utp "სა" `((case dir)))
	       (seq ,(utp "ს" `((case {dat gen})))
		    (or (? (seq ,(utp "ა" '((lang ng))) (? v-enclitica-Ga-C-a)))
                        (? og-rel-enclitica)
			(seq ,(utp-or '("გან" "თან" "ზედ" "თვის" "კენ" "ებრ" "ებრივ") ;; ???
                                      `((pp ,morph)))
                             (? c-enclitica))))
	       (seq (or ,(utp "ეთ" `((case {erg gen dat})
				     (lang og)
				     (num pl)))
			(seq ,(utp "ეთ" `((case gen)
					  (lang og)
					  (num pl)))
			     ,(utp-or '("გან" "თან" "ზედ" "თვის" "კენ" "ებრ" "ებრივ")
                                      `((pp ,morph))))
			,(utp "ეთა" `((case dir)
				      (lang og)
				      (num pl))))
		    (? og-rel-enclitica))))
      (seq (or ,(utp "ვის" `((cat poss+interr+anim)
			     (lex "vის·ი")))
	       ,(utp "არავის" `((cat poss+neg+anim)
			       (lex "არაvის·ი")))
	       ,(utp "ვერავის" `((cat poss+neg+anim)
				(lex "ვერაvის·ი")
				(neg-type pot)))
	       ,(utp "ნურავის" `((cat poss+neg+anim)
				(lex "ნურაvის·ი")
				(neg-type imp))))
           (or (seq ,(utp "ი" `((case nom) (case-type full)))
                    (? v-enclitica))
               (seq ,(utp "მა" `((case erg) (case-type full)))
                    (? v-enclitica))
               (seq ,(utp "ას" `((case dat) (case-type full)))
                    (? (seq ,(utp "ა")
                            v-enclitica)))
               (seq ,(utp "ით" `((case inst) (case-type full)))
                    (? (seq ,(utp "ა")
                            (? v-enclitica))))
               (seq (or ,(utp "ად" `((case adv) (case-type full)))
			,(utp "ათ" `((case adv) (case-type full)
				     (lang ng)
				     (style nonstandard))))
                    (? (seq ,(utp "ა") v-enclitica)))
               ,(utp "ი" `((case {nom gen inst}) (case-type reduced)))
               ,(utp "მა" `((case erg) (case-type reduced)))
               ,(utp-e `((case {dat adv}) (case-type reduced)))))
      
      
      (seq ,(utp "ვიღა" `((cat pron+interr+anim)
			  (lex "vიღა")))
           (or ,(utp-e `((case nom)))
	       ,(utp "მ" `((case erg)))
	       #+ignore
	       ,(utp "სა" `((case dir)))
	       (seq ,(utp "ს" `((case {dat gen})))
		    (or (? (seq ,(utp "ა" '((lang ng))) (? encl-aux)))
                        #+ignore(? og-rel-enclitica)
			(seq ,(utp-or '("გან" "თან" "ზედ" "თვის" "კენ" "ებრ" "ებრივ") ;; ???
                                      `((pp ,morph)))
			     (? (seq ,(utp "ა" '((lang ng))) (? encl-aux))))))
	       (seq ,(utp "ს" `((case dat) (case-type full)))
                    (? (seq ,(utp "ა") (? encl-aux))))
               (seq ,(utp "თი" `((case inst) (case-type full)))
		    (? encl-aux))
               (seq ,(utp "დ" `((case adv) (case-type full)))
                    (? (seq ,(utp "ა") (? encl-aux))))
               (seq ,(utp "ს" `((case gen))) (? (seq ,(utp "ი") (? encl-aux))))))
	   
      ;; TODO: may also be rel
      (seq (or ,(utp "რ" `((cat pron+interr+inanim)
			   (lex "რ[ა]")
			   (lang ng)))
	       ,(utp "რაღ" `((cat pron+interr+inanim)
			     (lex "რაღ[ა]")
			     (lang ng)))
	       ,(utp "რ" `((cat pron+interr+inanim)
			   (lex "რა·ჲ")
			   (lang og))))
           (or (seq ,(utp "ა" `((case nom) (lang ng)))
                    (? v-enclitica-C-a))
               (seq ,(utp "ანი" `((case nom) (lang ng) (num pl) (old-pl +)))
                    (? v-enclitica-C-a))
               (seq ,(utp "აი" `((case nom) (lang ng) (style nonstandard)))
                    (? v-enclitica-C-a))
               (seq ,(utp "ა" `((case {abs nom erg}) (lang og)))
                    (? og-rel-enclitica))
               (seq ,(utp "აჲ" `((case nom) (lang og))) (? og-rel-enclitica))
               (seq ,(utp "ამ" `((case erg) (lang ng)))
		    (? (seq ,(utp "ა") (? v-enclitica))))
	       (seq ,(utp "ამან" `((case erg) (lang og))) (? og-rel-enclitica))
	       (seq (or ,(utp "ას" `((case dat)))
			,(utp "აის" `((case dat) (style nonstandard))))
                    (or (? (seq ,(utp "ა") (? v-enclitica)))
			(? (seq (? ,(utp "ა")) og-rel-enclitica)) ;; რასა-იგი
			(seq ,(utp-or '("თან") ;; new 2011-04-30x
                                      `((pp ,morph)))
                             (? c-enclitica))))
	       #+ignore
	       (seq ,(utp "აღას" `((case dat) (lang ng)
				   (mod-sfx "ღა"))))
	       (seq (or ,(utp "ად" `((case adv)))
			,(utp "ათ" `((case adv) (style nonstandard))))
                    (? (or (seq ,(utp "ა") (? v-enclitica))
			   og-rel-enclitica)))
	       (seq ,(utp "ით" `((case inst)
				 (lang ng)))
                    (? (or (seq ,(utp "ა") (? v-enclitica))
			   (seq ,(utp "ი") (? v-enclitica)))))
	       (seq ,(utp "აჲთ" `((case inst)
				  (lang og)))
                    (? (or og-rel-enclitica
			   (seq ,(utp "ა") (? v-enclitica)) ;; ??
			   (seq ,(utp "ი") (? v-enclitica)))))
               (seq ,(utp "ის" `((case gen)
				 (lang ng)))
                    (or (? (seq ,(utp "ა") (? v-enclitica)))
                        (seq ,(utp-or '("გან" "თვის" "კენ" "ებრ" "ებრივ")
                                      `((pp ,morph)))
                             (? c-enclitica))))
               (seq ,(utp "ა" `((case dat)
				(lang ng)))
                    (or (seq ,(utp-or '("ზედ")
				      `((pp ,morph)))
			     (? c-enclitica))
			(seq ,(utp-or '("ზე" "ში")
				      `((pp ,morph)))
			     (? v-enclitica))))
	       (seq ,(utp "აჲს" `((case gen)
				  (lang og)))
                    (or (? (seq ,(utp "ა") (? v-enclitica)))
                        (? og-rel-enclitica)
			(seq ,(utp-or '("გან" "თვის" "კენ" "ებრ" "ებრივ")
                                      `((pp ,morph)))
                             (? c-enclitica))))
	       ))
      ,(utp "რა" `((cat pron+interr+inanim)
		   (lex "რ[ა]")
		   (lang ng)
		   (case {nom erg dat gen adv inst})
		   (case-type reduced)))
      (seq ,(utp "რის" `((cat poss+interr+inanim) ;; OG = ??
			 (lex "რის·ი")))
           (or (seq ,(utp "ი" `((case nom) (case-type full)))
                    (? v-enclitica))
               (seq ,(utp "მა" `((case erg) (case-type full)))
                    (? v-enclitica))
               (seq ,(utp "ას" `((case dat) (case-type full)))
                    (? (seq ,(utp "ა")
                            v-enclitica)))
               (seq ,(utp "ით" `((case inst) (case-type full)))
                    (? (seq ,(utp "ა")
                            (? v-enclitica))))
               (seq (or ,(utp "ად" `((case adv) (case-type full)))
			,(utp "ათ" `((case adv) (case-type full) (style nonstandard))))
                    (? (seq ,(utp "ა") v-enclitica)))
               ,(utp "ი" `((case {nom gen inst}) (case-type reduced)))
               ,(utp "მა" `((case erg) (case-type reduced)))
               ,(utp-e `((case {dat adv}) (case-type reduced))))))
 :name 'interrogative-relative)

;; vinme, rame, romelime, partly missing: viGaC, raGaC
;; რა:მე etc: part before colon is conjugated, მე is similar to a clitic
(precompile-u-transducer
 `(or (or (seq ,(utp "რამ" `((cat pron+sindef+inanim)
			     (lex "რამ")
			     (case nom)))
	       (? c-enclitica-C))
	  ;; არა უშავს რა
	  ,(utp "რა" `((cat pron+sindef+inanim)
		       (lex "რა")
		       (case nom)))
	  (seq (or ,(utp "ვინმე" `((cat pron+sindef+anim)
				   (lex "vინმე")))
		   ,(utp "რამე" `((cat pron+sindef+inanim)
				  (lex "რამ[ე]")
				  (case {nom erg dat gen})))
		   ,(utp "რაიმე" `((cat pron+sindef+inanim)
				  (lex "რაიმე")
				  (case {nom erg dat gen})))
		   ,(utp "რაჲმე" `((cat pron+sindef+inanim)
				   (lex "რაჲმ[ე]")
				   (lang og)))
		   ,(utp "რომელიმე" `((cat pron+sindef)
				      (lex "რომელიმე"))))
	       (or (seq ,(utp-e `((case nom)))
			(? v-enclitica-C))
		   (seq ,(utp "მ" `((case erg)))
			(? c-enclitica-C))
		   (seq ,(utp "ს" `((case {dat gen})))
			(or (? (seq ,(utp "ა") (? v-enclitica)))
			    (seq (? ,(utp "ა"))
				 ,(utp-or '("გან" "თვის" "კენ" "ებრ" "ებრივ")
					  `((case gen)
					    (pp ,morph)))
				 (? c-enclitica-C))
			    (seq ,(utp-or '("თან")
					  `((case dat)
					    (pp ,morph)))
				 (? c-enclitica-C))))
		   (seq (seq ,(utp-or '("ზე")
				      `((case dat)
					(pp ,morph)))
			     (? c-enclitica-C)))))
	  (seq (or ,(utp "რამეს" `((cat pron+sindef+inanim)
				   (lex "რამე")))
		   ,(utp "რაჲმეს" `((cat pron+sindef+inanim)
				    (lex "რაჲმე")
				    (lang og))))
	       (or (seq (? ,(utp "ა"))
			,(utp-or '("გან" "თვის" "კენ" "ებრ" "ებრივ") ;; ???
				 `((case gen)
				   (pp ,morph)))
			(? c-enclitica-C))))
	  (seq (or ,(utp "რამის" `((cat pron+sindef+inanim)
				   (lex "რამ[ე]")
				   (case gen)))
		   ,(utp "რაჲმის" `((cat pron+sindef+inanim)
				    (lex "რაჲმ[ე]")
				    (case gen)
				    (lang og))))
	       (seq (or (? (seq ,(utp "ა") (? v-enclitica)))
			(seq ,(utp-or '("გან" "თვის" "კენ" "ებრ" "ებრივ") ;; ???
				      `((case gen)
					(pp ,morph)))
			     (? c-enclitica-C)))))
	  (seq (or ,(utp "ვისმე" `((cat pron+sindef+anim) ;; ვისმეს etc. (ჭავჭავაძე)
				   (lex "vინ:მე")
				   (case dat)))
		   ,(utp "რასმე" `((cat pron+sindef+inanim)
				   (lex "რა:მე")
				   (lang ng)
				   (case dat))))
	       (? (seq ,(utp "ს")
		       (or (? c-enclitica)
			   (seq ,(utp-or '("გან" "თვის" "კენ" "ებრ" "ებრივ")
					 `((case gen)
					   (pp ,morph)))
				(? c-enclitica-C))))))
	  (seq (or ,(utp "რამესი" `((case gen)))
		   ,(utp "რამეთი" `((case inst)))
		   ,(utp "რამედ" `((case adv))))
	       ,(utp-e `((cat pron+sindef+inanim)
			 (lex "რამე")
			 (num sg)
			 (lang ng))))
	  (seq (or ,(utp "რითმე" `((case inst)))
		   ,(utp "რითიმე" `((case inst)))
		   ,(utp "რადმე" `((case adv))))
	       ,(utp-e `((cat pron+sindef+inanim)
			 (lex "რა:მე")
			 (num sg)
			 (lang ng))))
	  (seq (or ,(utp "ვის" `((cat pron+sindef+anim)
				 (lex "vინ:მე")
				 (case {dat gen})))
		   ,(utp "რას" `((cat pron+sindef+inanim)
				 (lex "რა:მე")
				 (case dat)
				 (lang ng)))
		   ,(utp "რის" `((cat pron+sindef+inanim)
				 (lex "რა:მე")
				 (case gen)
				 (lang ng)))
		   ,(utp "რომლის" `((cat pron+sindef)
				    (lex "რომ[ე]ლ·ი:მე")
				    (case gen)))
		   ,(utp "რომელს" `((cat pron+sindef)
				    (lex "რომ[ე]ლ·ი:მე")
				    (case dat))))
	       (or (seq (? ,(utp "ა")) ,(utp "მე") #+ignore(? v-enclitica))
		   (seq ,(utp-or '("გან" "თვის" "კენ" "ებრ" "ებრივ")
				 `((pp ,morph)
				   (case gen)))
			,(utp "მე")
			(? c-enclitica-C)))))
      (seq (or ,(utp "როგორმე" `((lex "როგორმე")
				 (cat adv)))
	       ,(utp-or '("როდისმე" "როდესმე" "ოდესმე")
			`((lex ,morph)
			  (cat adv)
			  (adv-type temp)))
	       ,(utp-or '("სადმე" "საიდანმე" "საიდამმე" "საიდგანმე" "საითკენმე" "საითმე")
			`((lex ,morph)
			  (cat adv)
			  (adv-type loc))))
	   ,(utp-e '((sub-cat indef-spec))))
      (seq (or ,(utp "ვიღაც" `((cat pron+indef+anim)
			       (lex "vიღაც")))
	       ,(utp "რაღაც" `((cat pron+indef+inanim)
			       (lex "რაღაც")))
	       ,(utp "ვიღაც-ვიღაც" `((cat pron+indef+anim)
				     (lex "vიღაც-vიღაც")))
	       ,(utp "რაღაც-რაღაც" `((cat pron+indef+inanim)
				     (lex "რაღაც-რაღაც")))
	       ,(utp "რომელიღაც" `((cat pron+indef+inanim)
				   (lex "რომელიღაც"))))
	   ,(utp-e '(;;(sub-cat indet)
		     (form bound)))
	   (? ,(utp "ა")))
      (seq (or ,(utp "ვიღაც" `((cat pron+indef+anim)
			       (lex "vიღაც")))
	       ,(utp "რაღაც" `((cat pron+indef+inanim)
			       (lex "რაღაც")))
	       ,(utp "ვიღაც-ვიღაც" `((cat pron+indef+anim)
				     (lex "vიღაც-vიღაც")))
	       ,(utp "რაღაც-რაღაც" `((cat pron+indef+inanim)
				     (lex "რაღაც-რაღაც"))))
	   ,(utp-e '(;;(sub-cat indet)
		     (form free)
		     (case nom)
		     (num sg)))))
 :name 'indefinite)

;; missing: რომელთაგანაც
(precompile-u-transducer
 `(seq (or ,(utp "რომელ" `((lex "რომ[ე]ლ·ი")
			   (stem-type c)
			   (sync-stem -)))
	   ,(utp "რომლ" `((lex "რომ[ე]ლ·ი")
			  (stem-type c)
			  (sync-stem +)))
	   ,(utp "რაოდენ" `((lex "რაოდენ·ი")
			    (stem-type c))))
       (or ,(utp-e '((cat pron)
		     (sub-cat int)))
	   ,(utp-e '((cat pron)
		     (sub-cat rel)
		     (lang og)))))
 :name 'romel-stem)

;; შანიძე p. 107
;; indet-stem
(precompile-u-transducer
 `(seq (or ,(utp "ვიღაც" `((cat pron+indef)
			   (sub-cat anim)
			   (form free)
			   (case-type full)
			   (lex "vიღაც")))
	   ,(utp "რაღაც" `((cat pron+indef)
			   (sub-cat inanim)
			   (form free)
			   (case-type full)
			   (lex "რაღაც")))
	   ,(utp "ვიღაც-ვიღაც" `((cat pron+indef)
				 (sub-cat anim)
				 (form free)
				 (case-type full)
				 (lex "vიღაც-vიღაც")))
	   ,(utp "რაღაც-რაღაც" `((cat pron+indef)
				 (sub-cat inanim)
				 (form free)
				 (case-type full)
				 (lex "რაღაც-რაღაც")))
	   ,(utp "რომელიღაც" `((cat pron+indef)
			       (form free)
			       (case-type full)
			       (lex "რომელიღაც"))))
       ;; now included in det-anim etc.; change name! > pron+indef ,(utp-e '((sub-cat indet)))
       (or ,(utp-e '((stem-type c)
		     (sync-stem +)))
	   ,(utp "ა" `((stem-type "ა")
		       (sync-stem -)
		       (rigid-stem +)
		       (num sg)))
	   ,(utp "ე" `((stem-type "ე")
		       (sync-stem -)
		       ;;(rigid-stem +)
		       (num pl)
		       (old-pl -)))))
 :name 'indet-stem)

;; bound forms
;; NG
(precompile-u-transducer
 `(seq (or (seq ,(utp-or '("ის" "ეს" "ეგ"
			   "ისა და ის"
			   "ესა და ეს")
			 `((lex ,morph)
			   (case nom)))
		(? ,(utp "ა" `((long +)))))
	   (seq (or ,(utp "იმ" `((lex "ის")))
		    ,(utp "ამ" `((lex "ეს")))
		    ,(utp "მაგ" `((lex "ეგ")))
		    ,(utp "იმა და იმ" `((lex "ისა და ის")))
		    ,(utp "იმადაიმ" `((lex "ისა და ის")))
		    ,(utp "ამა და ამ" `((lex "ესა და ეს")))
		    ,(utp "ამადაამ" `((lex "ესა და ეს")))
		    ,(utp "მაგ" `((lex "ეგ"))))
		(? ,(utp "ა" `((long +))))
		,(utp-e '((case {erg dat gen inst adv}))))
	   ,(utp "იგივე" `((lex "იგი")
			   (mod-sfx "ვე")
			   (case nom)))
	   ,(utp "ეგევე" `((lex "ეგ")
			   (mod-sfx "ვე")
			   (case nom)))
	   ,(utp "იმავე" `((lex "იგი")
			   (mod-sfx "ვე")
			   (case {erg dat gen inst adv})))
	   ,(utp "ამავე" `((lex "ეს")
			   (mod-sfx "ვე")
			   (case {erg dat gen inst adv})))
	   ,(utp "იმავ" `((lex "იგი")
			  (mod-sfx "ვე")
			  (case {nom erg dat gen inst adv}))))
       ,(utp-e '((cat dem)
		 (lang ng))))
 :name 'determiner-ng) 

;; OG
(precompile-u-transducer
 `(seq (or ,(utp-or '("ისი" "ესე" "ეგე" "იგი")
		    `((lex ,morph)
		      (case nom)
		      (num sg)))
	   (seq (or ,(utp "მ"   '((lex "იგი") (num sg)))
		    ,(utp "ამ"  '((lex "ესე") (num sg)))
		    ,(utp "მაგ" '((lex "ეგე") (num sg)))
		    ,(utp "იმ"  '((lex "ისი") (num sg))))
		(or ,(utp "ან"  '((case erg)))
		    ,(utp "ას"  '((case dat)))
		    ,(utp "ის"  '((case gen)))
		    ,(utp "ისა" '((case dir)))
		    ,(utp "ით"  '((case inst)))
		    ,(utp "ად"  '((case adv)))))
	   ,(utp "ესენი" '((lex "ესე") (case nom) (num pl)))
	   ,(utp "ეგენი" '((lex "ეგე") (case nom) (num pl)))
	   ,(utp "ისინი" '((lex "იგი") (case nom) (num pl)))
	   ,(utp "ამათ" '((lex "ესე") (case { erg dat gen }) (num pl)))
	   ,(utp "მაგათ" '((lex "ეგე") (case { erg dat gen }) (num pl)))
	   ,(utp "მათ" '((lex "იგი") (case { erg dat gen }) (num pl)))
	   ,(utp "იმათ" '((lex "ისი") (case { erg dat gen }) (num pl)))
	   ,(utp "ამათა" '((lex "ესე") (case dir) (num pl)))
	   ,(utp "მაგათა" '((lex "ეგე") (case gen) (num pl)))
	   ,(utp "იმათა" '((lex "ისი") (case dir) (num pl))))
       (? ,(utp "ვე" `((mod-sfx "ვე"))))
       ,(utp-e '((cat dem)
		 (lang og))))
 :name 'determiner-og)


;; missing: ramden-..., erT-erTi, ram, aranairi, araerTi, erTgvari

;; Conjunctions
(precompile-u-transducer
 `(or #+moved
      (seq ,(utp-or '("თუნდაც"
		      "მაგრამ"
		      "თითქოს")
                    `((lex ,morph)
                      (cat cj+coord)))
           (? (seq ,(utp "ა" `((long +)))
		   (? encl-aux))))
      #+moved
      (seq ,(utp-or '("მაგრა"
		      "ხოლო"
		      "თვარა"
		      "აქაოდა"
		      "თითქო"
		      "თითქოსდა")
                    `((lex ,morph)
                      (cat cj+coord)))
           (? encl-aux))
      ,(utp-or '("და"
		 "თუ"
		 "ან"
		 "ანუ"
		 "ანდა"
		 "არამედ"
		 ;;"ვიდრე"
		 "თორემ"
		 "თორემა"
		 "თვარემ"
		 "თორე"
		 ;;"აგერ"
		 ;; "აბა"
		 ;;"დიახ"
		 "დიახაც"
		 ;;"დიაღ"
		 ;;"ჰო"
		 ;;"ხო"
		 ;;"ჰოდაჰო"
		 ;; "კი"
		 ;;"ჰოდა"
		 ;;"დაე"
		 ;;"არა"
		 "არადა"
		 "თვარა"
		 ;; OG
		 "ჰე")
	       `((lex ,morph)
		 (cat cj+coord)))
      
      (seq ,(utp-or '("რომ"
		      "რომც"
		      "რო"
		      "რომე"
		      "სანამ"
		      "სანამდი"
		      "სანამდის"
		      "სანამდისინ"
		      "როდესაც"
		      "როგორც"
		      ;;"რაც"
		      "რამწამ"
		      "რაწამს"
		      "vითომ"
		      "vითომაც"
		      "vითომც"
		      "vითომდაც"
		      "vითამ" ;; Ju
		      "vითამც"
		      "მითამ" ;; Kk Ka Ps
		      )
                    `((lex ,morph)
                      (cat cj+sub)))
           (? (seq ,(utp "ა" `((long +)))
		   (? encl-aux))))
      (seq ,(utp-or '("თუ"
		      "ნუთუ"
		      "ვაითუ"
		      ;;"ნეტავ"
		      "ნეტამც"
		      "ნეტავი"
		      "თუკი"
		      "რათა"
		      "ვიდრე"
		      "როცა"
		      "თუგინდ"
		      "გინათუ"
		      "vიდრემდე" ;; OG
		      "vიდრემდის"
		      "vიდრემდენ"
		      "ვაჲთუ"
		      "vითომცდა"
		      "vითომდა"
		      "vითამდა"
		      "vითამცდა"
		      "მითამდა" ;; Kk Ka Ps
		      )
		    `((lex ,morph)
		      (cat cj+sub)))
	   (? encl-aux))
      ,(utp-or '(
		 ;;"თუმცა"
		 ;;"თუმც"
		 "vინაიდან"
		 "რადგან"
		 "ვინათგან" 
		 "რაკი"
		 ;;"რამეთუ"
		 "vითარცა"
		 "რადგანაც"
		 "რახან"
		 ;;"ოღონდ"
		 )
	       `((lex ,morph)
		 (cat cj+sub)))
      ;; OG
      ,(utp-or '("vითარცა"
		 "vითარ-იგი"
		 "vითარ-ესე"
		 "vითარ-ეგე"
		 "vითარცა-იგი"
		 "vითარცა-ესე"
		 "vითარცა-ეგე"
		 "vითარმცა"
		 "vინაჲცა"
		 "vინაჲთცა"
		 "vინაჲთგან"
		 "ოდეს-იგი"
		 ;;"რამეთუ"
		 "დაღაცათუ"
		 "დაღათუმცა"
		 "vითარმედ"
		 "რაჲთა"
		 ;;"რაჲთამცა"
		 "უკუთუ"
		 "უკუeთუ"
		 "უკუeთუმცა"
		 "უკეთუ"
		 "რაჟამს"
		 "რასჟამს"
		 "რაბამად"
		 "გარნა"
		 "ნუუკუe"
		 "გინა"
		 "რაჲ"
		 "ღათუ"
		 "რაჟამს-იგი"
		 "რაჟამს-ესე"
		 "რაჟამს-ეგე"
		 "რაჟამსღა"
		 )
	       `((lex ,morph)
		 (cat cj+sub)
		 (lang og)))
      
      )
 :name 'conjunction)

;; Adverbs
(precompile-u-transducer
 `(or (seq ,(utp-or '("არსად" "არსაიდან" "არსიდან" "არსაიდამ" "არსაიდგან" "არსაითკენ" "არსაით")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (adv-type loc)))
	   c-enclitica-C-a)
      (seq ,(utp-or '("ვერსად" "ვერსაიდან" "ვერსიდან" "ვერსაიდამ" "ვერსაიდგან" "ვერსაითკენ" "ვერსაით")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (neg-type pot)
		      (adv-type loc)))
	   c-enclitica-C-a)
      (seq ,(utp-or '("ნურსად" "ნურსაიდან" "ნურსიდან" "ნურსაიდამ" "ნურსაიდგან" "ნურსაითკენ" "ნურსაით")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (neg-type imp)
		      (adv-type loc)))
	   c-enclitica-C-a)
      (seq ,(utp-or '("აღარსად" "აღარსაიდან" "აღარსაიდამ" "აღარსაიდგან" "აღარსაითკენ" "აღარსაით")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (adv-type loc)))
	   c-enclitica-C-a)
      (seq ,(utp-or '("ვეღარსად" "ვეღარსაიდან" "ვეღარსაიდამ" "ვეღარსაიდგან" "ვეღარსაითკენ" "ვეღარსაით")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (neg-type pot)
		      (adv-type loc)))
	   c-enclitica-C-a)
      (seq ,(utp-or '("ნუღარსად" "ნუღარსაიდან" "ნუღარსაიდამ" "ნუღარსაიდგან" "ნუღარსაითკენ" "ნუღარსაით")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (neg-type imp)
		      (adv-type loc)))
	   c-enclitica-C-a)
      
      (seq ,(utp-or '("არასოდეს" "არასდროს" "არაოდეს"
		      "არაოდის") ;; ??
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (adv-type temp)))
	   c-enclitica-C-a)
      (seq ,(utp-or '("ვერასოდეს" "ვერასდროს" "ვერაოდეს")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (neg-type pot)
		      (adv-type temp)))
	   c-enclitica-C-a)
      (seq ,(utp-or '("ნურასსდროს" "ნურასოდეს" "ნურაოდეს")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (neg-type imp)
		      (adv-type temp)))
	   c-enclitica-C-a)
      (seq ,(utp-or '("აღარასოდეს" "აღარსოდეს" "აღარასდროს" "აღაროდეს")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (adv-type temp)))
	   c-enclitica-C-a)
      (seq ,(utp-or '("ვეღარასოდეს" "ვეღარასდროს" "ვეღარაოდეს")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (neg-type pot)
		      (adv-type temp)))
	   c-enclitica-C-a)
      (seq ,(utp-or '("ნუღარასოდეს" "ნუღარასდროს" "ნუღარაოდეს")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat neg)
		      (neg-type imp)
		      (adv-type temp)))
	   c-enclitica-C-a)
      
      (seq (or ,(utp-or '("როდის" "როდეს" "ოდეს" "როდემდე" "როდემდის" "როდემდინ")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat int)
		      (adv-type temp)))
	       ,(utp-or '("სად" "საიდან" "სიდან" "საიდამ" "საიდგან" "საითკენ" "საით"
			  "სადამდე" "სადამდის" "სადამდინ")
			`((lex ,morph)
			  (cat adv)
			  (sub-cat int)
			  (adv-type loc)))
	       ,(utp-or '("რატომ")
			`((lex ,morph)
			  (cat adv)
			  (sub-cat int)
			  (adv-type causal)))
	       ,(utp-or '("როგორ")
			`((lex ,morph)
			  (cat adv)
			  (sub-cat int)
			  (adv-type mann))))
	   c-enclitica-Ga-C-a)
      (seq (or ,(utp-or '("რავა")
			`((lex ,morph)
			  (cat adv)
			  (sub-cat int)
			  (adv-type mann)))
	       ,(utp-or '("რათა")
			`((lex ,morph)
			  (cat adv)
			  (sub-cat int)
			  (adv-type final))))
	   v-enclitica-Ga-C-a)
      ,(utp-or '("რათ")
	       `((lex ,morph)
		 (cat adv)
		 (sub-cat int)
		 (adv-type final)))
      (seq (or ,(utp-or '("როდისღაც" "როდესღაც" "ოდესღაც")
			`((lex ,morph)
			  (cat adv)
			  (sub-cat indet)
			  (adv-type temp)))
	       ,(utp-or '("სადღაც" "საიდანღაც" "საიდამღაც" "საიდგანღაც" "საითკენღაც" "საითღაც")
			`((lex ,morph)
			  (cat adv)
			  (sub-cat indet)
			  (adv-type loc)))
	       ,(utp-or '("რატომღაც")
			`((lex ,morph)
			  (cat adv)
			  (sub-cat indet)
			  (adv-type causal)))
	       ,(utp-or '("როგორღაც")
			`((lex ,morph)
			  (cat adv)
			  (sub-cat indet)
			  (adv-type mann))))
	   (? (seq ,(utp "ა" `((long +)))
		   (? encl-aux))))
      (seq ,(utp-or '("vითარ" "რაჲსაგან")
                    `((lex ,morph)
                      (cat adv)
		      (sub-cat int)
		      (lang og)))
	   og-rel-enclitica)
      #+ignore
      ,(utp "როგორც"
	    `((lex "როგორ")
	      (cat adv)
	      (sub-cat int)
	      (rel-sfx "ც")))
      (seq ,(utp-or '(;;"კვლავ" "კვლავად" "ისევ" "ხელახლა"
		      "პირისპირ")
                    `((lex ,morph)
                      (cat adv)))
	   c-enclitica-Ga-C-a)
      #+ignore
      (seq ,(utp-or '("სადა")
                    `((lex ,morph)
                      (cat adv)
		      (lang og)))
	   (? og-rel-enclitica))
      
      (seq ,(utp-or '("საცა")
                    `((lex "სად")
                      (cat adv)
		      (sub-cat int)
		      (rel-sfx "ც")))
           (? encl-aux))
      (seq ,(utp-or '(;;"კიდევ"
		      ;;"ამიტომ" "იმიტომ" "მაგიტომ"
		      ;; "სულ"
		      "თან" ;; "ზედ" ;;  "ზედიზედ"
		      ;; "ძალიან" "ძლიერ" "დიდად"
		      ;;"ყველგან"
		      ;;"სრულიად" 
		      "სწორად"
		      "თუნდ"
		      "ათობით" ;; and more
		      "ასობით"
		      "ათასობით"
		      "მილიონობით"
		      "ორშაბათობით" ;; etc.
		      "ჯერჯერობით"
		      "თანდათან"
		      "თანდათანობით"
		      "ზოგან"
		      "ზოგან-ზოგან"
		      )
		    `((lex ,morph)
		      (cat adv)))
	   c-enclitica-C)
      (seq ,(utp-or '(;;"ხომ" "მართლაც" "ალბათ"
		      ;; "იქნებ"
		      ;; "თურმე" "ეგებ" "განძრახ" "ძლივს"
		      ;;"ფეხდაფეხ"
		      ;;"ცხენდაცხენ"
		      ;;"პირდაპირ"
		      ;;"ამქვეყნად"
		      ;;"აქეთ-იქით"
		      ;;"აქით-იქიდან"
		      ;;"აქეთ-იქედან"
		      ;;"ამრიგად"
		      "კიდეც"
		      "უკეთ"
		      "უკეთესად"
		      "მარტოოდენ"
		      
		      ;;"ბოლოსდაბოლოს" 
		      "ბოლოცდაბოლოც" 
		      "განდაგან" 
		      ;; "გზადაგზა" 
		      "გორდაგორ" 
		      "გულდაგულ" 
		      ;;"დროდადრო" 
		      "დღედადღე" 
		      ;; "ერთიდაერთი" 
		      "ველდაველ" 
		      "ზედაზე" 
		      "ზოლდაზოლ" 
		      "ზღვადაზღვა" 
		      "თავდათავ" 
		      "თანდათან" 
		      "თვალდათვალ" 
		      "ისევდაისევ" 
		      "კანდაკან" 
		      "კარდაკარ" 
		      "კვალდაკვალ" 
		      "კვლავდაკვლავ" 
		      "კიდევდაკიდევ" 
		      ;;"მაინცდამაინც" 
		      "მერედამერე" 
		      "მთადამთა" 
		      "მხარდამხარ" 
		      "მხოლოდდამხოლოდ" 
		      "პირდაპირ" 
		      "ჟამდაჟამ" 
		      ;; "რადარა" 
		      "რიგდარიგ" 
		      "ტანდატან" 
		      "ქარდაქარ" 
		      "შიგდაშიგ" 
		      "შუქდაშუქ" 
		      "ცხენდაცხენ" 
		      "ძალდაძალ" 
		      "წამდაწამ" 
		      "წინდაწინ" 
		      "წყალდაწყალ"
		      "ხაზდახაზ" 
		      ;;"ხანდახან" 
		      "ხევდახევ" 
		      "ხელდახელ" 
		      "ხმალდახმალ"
		      "ჩუმჩუმად"
		      "თლათ" ; dial
		      )
		    `((lex ,morph) ;; should be +mann?
		      (cat adv)))
	   (? (seq ,(utp "ა" `((long +)))
		   (? encl-aux))))
      (seq ,(utp-or '("იმწამსვე" ;; "უკვე";; "კი"
		      "უფროდაუფრო" ;;"აგრეთვე"
		      "აკი"
		      "შიგადაშიგა"
		      "მაშასადამე"
		      "ტყედატყე"
		      "პირქვე"
		      "ნუუკვე" ;; OG
		      "სხუადასხუა"
		      "ესე იგი"
		      "ე. ი."
		      )
		    `((lex ,morph)
		      (cat adv)))
	   (? encl-aux))
      ,(utp-or '("ამასთანავე"
		 "vინიცობა"
		 "vინიცობაა"
		 "სადამე" ;; OG
		 "მოაქაჟამადმდე"
		 "კნინღა"
		 "კნინღადა"
		 ;;"უწინარეს"
		 "უწინარჱს"
		 "ოდენ"
		 )
	       `((lex ,morph)
		 (cat adv)))
      )
 :name 'adverb)

;; აქა, იქი, მუნ, მანდა, ზე, ქუე, ქუეშე, წინა, უკუანა, შუვა, შოვრის, გარე, გარეგან, გარეშე, მიმდე, შინაგან, ამიერ, მაგიერ, აქაჲთ, ზემოჲთ, ქუემოჲთ, წინაჲთ, უკუანაჲთ, მარჯულ, მარცხლ, მგუარდლაქა, იქი, მუნ, მანდა, ზე, ქუე, ქუეშე, წინა, უკუანა, შუვა, შოვრის, გარე, გარეგან, გარეშე, მიმდე, შინაგან, ამიერ, მაგიერ, აქაჲთ, ზემოჲთ, ქუემოჲთ, წინაჲთ, უკუანაჲთ, მარჯულ, მარცხლ, მგუარდლ

(precompile-u-transducer
 `(seq (or ,(utp "ადგილას" '((lex "ადგილი")))
	   ,(utp "ალაგას" '((lex "ალაგი")))
	   ,(utp "პირას" '((lex "პირი"))))
       ,(utp-e `((cat n)
		 (case dat)
		 (num sg)
		 (form bound) ;; ??
		 ))
       (or c-enclitica
	   ,(utp "ა")
	   (seq (or ,(utp "ავით" '((pp "ვით")))
		    ,(utp "თან" '((pp "თან"))))
		c-enclitica)))
 :name 'locative)

;; Interjection
(precompile-u-transducer
 (utp-or '("აღუ"
	   "ამპა"
	   "არიქა"
	   "გუგუ"
	   "ნწუ"
	   "ტფუ"
	   "რა"
	   "რაღა"
	   "ვიშ"
	   "გამარჯობა"
	   "გამარჯობათ"
	   "ღმერთმანი"
 	   "გეთაყვა"
 	   "გეთაყვანე"
	   "გენაცვალე"
	   "ვენაცვალე"
	   ;;"ამენ"
	   "ამჱნ"
	   "აჰა"
	   "დეე"
	   "დაე"
	   ;;"დედი"
	   ;;"დედილო"
	   "ოჰ" "უჰ" "ჰა"
	   "ჵ"
	   "ო"
	   "ოჲ"
	   "ვაჲ"
	   "ჰაბა"
	   "ვაშა"
	   "ეჰა"
	   "უიმე"
	   "ვაიმე"
	   "ვუი"
	   "ვუიმე"
	   "ჰაი"
	   "ჰაპაპაპა"
	   "აკი"
	   "დია"
	   "ურაა"
	   "დროებით"
	   "ე"
	   "ერთი"
	   "კარგი"
	   "მაგრა"
	   "მაგრად"
	   "მართლა"
	   "მშვიდობით"
	   "როგორ"
	   "როგორა"
	   "საქაფროთ"
	   "სიცოცხლე"
	   "სუ"
	   "ფრთხილად"
	   "ქვე"
	   "ძლივს"
	   "წესიერად"
	   "წინ"
	   "წიო-წიო"
	   "ხევ-ხევ"
	   "ჯერჯერობით"
	   "ჰო" "ჰე"
	   "ფუჰ"
	   "ეჰ"
	   "ვაი" "ვააი" "ვაააო"
	   "ნეტაი"
	   "ატატა-ბატატა"
	   "ლარსე"
	   "ლურსე"
	   "ცრუტ"
	   "ჭირიმე"
	   "ჭირიმეთ"
	   "კვნესამე"
	   "ქა"
	   "ჰმ"
	   "ჰოი"
	   "ჰი"
	   "ჰა" "ჰაა" "ჰააა"
	   "ჰაი-ჰუუუი"
	   "ჰუუი"
	   "ხი-ხი-ხი"
	   "იჰ"
	   "ოჰ"
	   "ოოჰ"
	   "აჰ"
	   "ააჰ"
	   "ეფფათა"
	   "ბიჭოს"
	   "ძამია"
	   "ქაა"
	   "აბა"
	   "ფუი"
	   "ჰოპლა"
	   "ჰოოპლა"
	   "შენი ჭირიმე"
	   "ტო"
	   )
	 `((lex ,morph)
	   (cat ij)))
 :name 'interjection)

#[possessed-noun-stem
  = (seq
     `,(utp-or '("მამა" "მამი" "დედა" "დედი" "ბებია" "ბიძა" "ბაბუა" "პაპა" "დეიდა"
		 "მამიდა" "ბიცოლა" "დიდედა" "ბებო" "პაპიდა" "ნათლია" "ბაბუ" "ბატონი" "გადია")
	       `((lex ,morph)))
     (or
      ["ჩემ" ((lex "ჩემ·ი")(poss-pers 1sg))]
      ["შენ" ((lex "შენ·ი")(poss-pers 2sg))]
      ["ჩვენ" ((lex "ჩვენ·ი")(poss-pers 1pl))]
      ["თქვენ" ((lex "თქვენ·ი")(poss-pers 2pl))]
      ["მის" ((lex "მის·ი")(poss-pers 3sg))]
      ["მათ" ((lex "მათ·ი")(poss-pers 3pl))]
      )
     [e ((cat n)
	 (sub-cat anim)
	 (stem-type c))])]

#[pron-poss12-stem
  = (seq (or
	  ["ჩემ" ((lex "ჩემ·ი") (poss-pers 1sg))]
	  ["შენ" ((lex "შენ·ი") (poss-pers 2sg))]
	  ["ჩვენ" ((lex "ჩვენ·ი") (lang ng) (poss-pers 1pl))]
	  ["თქვენ" ((lex "თქვენ·ი") (lang ng) (poss-pers 2pl))]
	  ["ჩუeნ" ((lex "ჩუენ·ი") (lang og) (poss-pers 1pl))]
	  ["თქუeნ" ((lex "თქუენ·ი") (lang og) (poss-pers 2pl))]
	  )
     [e ((cat pron)
	 (sub-cat poss)
	 (stem-type c))])]

#[pron-poss3-stem
  = (seq (or
	  ["მის" ((lex "მის·ი") (poss-pers 3sg))]
	  ["ამის" ((lex "ამის·ი") (poss-pers 3sg) (deixis prox))]
	  ["იმის" ((lex "იმის·ი") (poss-pers 3sg) (deixis dist))]
	  ["მაგის" ((lex "მაგის·ი") (poss-pers 3sg) (deixis med))]
	  ["თავის" ((lex "თავის·ი") (poss-pers 3sg) (refl +))] ;; ng only?
	  ["თვის" ((lex "თვის·ი") (poss-pers 3sg) (refl +) (lang og))]
	  ["მათ" ((lex "მათ·ი") (poss-pers 3pl))]
	  ["ამათ" ((lex "ამათ·ი") (poss-pers 3pl) (deixis prox))]
	  ["იმათ" ((lex "იმათ·ი") (poss-pers 3pl) (deixis dist))]
	  ["მაგათ" ((lex "მაგათ·ი") (poss-pers 3pl) (deixis med))]
	  ["თავიანთ" ((lex "თავიანთ·ი") (poss-pers 3pl) (refl +))]
	  )
     [e ((cat pron)
	 (sub-cat poss)
	 (stem-type c))])]

;; ჩემსას, მათსას, თავიანთსას, …

#[pron-poss3-stem-dat
  = (seq (or
	  ["მის" ((lex "მის·ი") (poss-pers 3sg))]
	  ["ამის" ((lex "ამის·ი") (poss-pers 3sg) (deixis prox))]
	  ["იმის" ((lex "იმის·ი") (poss-pers 3sg) (deixis dist))]
	  ["მაგის" ((lex "მაგის·ი") (poss-pers 3sg) (deixis med))]
	  ["თავის" ((lex "თავის·ი") (poss-pers 3sg) (refl +))] ;; ng only?
	  ["თვის" ((lex "თვის·ი") (poss-pers 3sg) (refl +) (lang og))]
	  ["მათს" ((lex "მათ·ი") (poss-pers 3pl))]
	  ["ამათს" ((lex "ამათ·ი") (poss-pers 3pl) (deixis prox))]
	  ["იმათს" ((lex "იმათ·ი") (poss-pers 3pl) (deixis dist))]
	  ["მაგათს" ((lex "მაგათ·ი") (poss-pers 3pl) (deixis med))]
	  ["თავიანთს" ((lex "თავიანთ·ი") (poss-pers 3pl) (refl +))]
	  )
     [e ((cat pron)
	 (sub-cat poss)
	 (stem-type c))])]

#[pron-refl-stem
  = (or
     ["თავ" ((lex "თავ·ი")
	     (cat pron)
	     (num sg)
	     (refl +)
	     (stem-type c)
	     (lang ng))]
     ["თავ" ((lex "თ[ა]ვ·ი")
	    (cat pron)
	    (num sg)
	    (refl +)
	    (sync-stem -)
	    (stem-type c)
	    (lang og))]
     ["თვ" ((lex "თ[ა]ვ·ი")
	    (cat pron)
	    (num sg)
	    (refl +)
	    (sync-stem +)
	    (stem-type c)
	    (lang og))]
     ["თავის-თავ" ((lex "თავის-თავ·ი")
		   (cat pron)
		   (num sg)
		   (refl +)
		   (stem-type c))])]

#[pron-neg-stem
  = (or
     (seq (or
	   ["არაფერ" ((lex "არაფ[ე]რ·ი")(sync-stem -))]
	   ["ვერაფერ" ((lex "ვერაფ[ე]რ·ი") (sync-stem -)(neg-type pot))]
	   ["ნურაფერ" ((lex "ნურაფ[ე]რ·ი") (sync-stem -)(neg-type imp))]
	   ["აღარაფერ" ((lex "აღარაფ[ე]რ·ი") (sync-stem -))]
	   ["ვეღარაფერ" ((lex "ვეღარაფ[ე]რ·ი") (sync-stem -) (neg-type pot))]
	   ["ნუღარაფერ" ((lex "ნუღარაფ[ე]რ·ი") (sync-stem -) (neg-type imp))]
	   ["არაფრ" ((lex "არაფ[ე]რ·ი")(sync-stem +))]
	   ["ვერაფრ" ((lex "ვერაფ[ე]რ·ი") (sync-stem +)(neg-type pot))]
	   ["ნურაფრ" ((lex "ნურაფ[ე]რ·ი") (sync-stem +)(neg-type imp))]
	   ["აღარაფრ" ((lex "აღარაფ[ე]რ·ი") (sync-stem +))]
	   ["ვეღარაფრ" ((lex "ვეღარაფ[ე]რ·ი") (sync-stem +) (neg-type pot))]
	   ["ნუღარაფრ" ((lex "ნუღარაფ[ე]რ·ი") (sync-stem +) (neg-type imp))]
	   )
      [e ((cat {quant pron+neg+inanim})
	  (stem-type c))])
     (seq (or
	   ["არა" ((lex "არა"))]
	   ["ვერა" ((lex "ვერა")(neg-type pot))]
	   ["ნურა" ((lex "ნურა")(neg-type imp))]
	   ["აღარა" ((lex "აღარა"))]
	   ["ვეღარა" ((lex "ვეღარა") (neg-type pot))]
	   ["ნუღარა" ((lex "ნუღარა") (neg-type imp))]
	   )
      [e ((cat pron+neg+inanim)
	  (stem-type "ა")
	  (rigid-stem +)
	  (sync-stem -))]))]

#[negation
  = (seq (or
	  (seq
	   (or
	    ["არ" ((lex "არ"))]
	    ["როდი" ((lex "როდი"))]
	    ["ვერ" ((lex "ვერ")
		    (neg-type pot))]
	    ["აღარ" ((lex "აღარ"))]
	    ["არღარ" ((lex "არღარ")
		      (lang og))]
	    ["ვეღარ" ((lex "ვეღარ")
		      (neg-type pot))]
	    ["ვერღარ" ((lex "ვერღარ")
		       (neg-type pot)
		       (lang og))])
	   (? (or ["ა" ((long +))]
		  (seq ["ა" ((long +) (lang ng))]
		       encl-aux)
		  (seq ["ც" ((rel-sfx "ც") (lang ng))]
		       (? (seq ["ა" ((long +))]
			       (? encl-aux))))
		  ["ცა" ((rel-sfx "ცა") (lang og))])))
	  ["ნუ" ((lex "ნუ")
		 (neg-type imp))]
	  (seq (or ["ნუღარ" ((lex "ნუღარ")
			     (neg-type imp))]
		   ["ნურც" ((lex "ნუ")
			    (neg-type imp)
			    (rel-sfx "ც"))]
		   ["ნუღარც" ((lex "ნუღარ")
			      (neg-type imp)
			      (rel-sfx "ც"))])
	       (? ["ა" ((long +))])))
     [e ((cat neg))])]

(defparameter *noun-fst*
  (compile-u-transducer
   `(or (seq (or #-fst ,(utp '$)
		 #+fst stem
		 possessed-noun-stem
		 ;; pron-poss3-stem
		 pron-refl-stem
		 pron-neg-stem
		 indet-stem)
             (or nom-group ;; contains
		 erg-group ;; adjectives
                 voc-group
		 (seq ,(utp-e '((case-type full)))
                      (or dat-group gen-group inst-group adv-group
                          old-plural))))
	#+ignore
	(seq (or #-fst ,(utp '$)
		 #+fst deriv-stem)
             iani1)
	(seq romel-stem
             (or nom-group
		 erg-group
		 (seq ,(utp-e '((case-type full)))
                      (or dat-group gen-group inst-group adv-group
                          old-plural)))
	     (? og-rel-enclitica)) ;; TODO: exclude other enclitics
	(seq (or pron-poss3-stem
		 pron-poss12-stem)
             (or nom-group
		 erg-group
                 voc-group
		 ;; ჩემს, შენს, მისს, თავიანთს etc.
		 (seq ,(utp "ს" '((case-type reduced)
				  (case { dat adv }))))
		 ;; ჩემის ხელის
		 (seq ,(utp "ის" '((case-type reduced)
				  (case gen))))
		 (seq ,(utp-e '((case-type full)))
                      (or ;; dat-group gen-group
			  inst-group adv-group
                          old-plural))
		 ;; iani
		 ))
	;; ჩემსას, მისას, თავიანთსას
	(seq (or pron-poss3-stem-dat
		 (seq pron-poss12-stem ,(utp "ს")))
	     (seq ,(utp "ას" '((case-type full)
			       (case dat)))))
	locative
        pronoun
        determiner-ng
	determiner-og
        postposition
        interjection
        adverb
	;;(seq cardinal ,(utp-or '("ჯერ" "გზის") '((cat adv) (repet +)))) 
        interrogative-relative
	indefinite
        pron-negative
	;;number
	conjunction
	negation
	)
   :name 'noun
   :dfa-class 'georgian-dfa))

#+test
(setf *fst*
  (compile-u-transducer
   `(seq (or noun finite-verb)
         (? indirect-speech))
   :dfa-class 'georgian-dfa))

:eof
