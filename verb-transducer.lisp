;;;-*- Mode: Lisp; Package: TRANSDUCER; Readtable: fst-readtable -*-
;;;
;;; Georgian verb mophological parser using a finite state 
;;; transducer with (untyped) feature structure unification
;;; (C) Paul Meurer 1999-2013

;; Bugs:

;; გავუვლო?
;; 2032-130/1/2, 140/141, 156/157, 160/161/162, 165/6 merge
;; იარიან (<- იარენ)
;; გადაივლება etc.
;; 2206-38 perfect

;; ჰრქუა
;; მოკითხვაჲ
;; ყო


(in-package :fst)

#+copy
(defclass georgian-dfa (extended-dfa)
  ())


;; "მი|მო|მიმო|და|აღ|ა|შე|ჩა|წარ|წა|წინა|გადა|გარდა|გან|გა|უკუ|უარ|შთა|ძალ|თანა|იავარ"

; preverb, used in preverb-group
(precompile-u-transducer 
 `(or ,(utp-or '("მი" "მო" "მიმო" "და" "აღ" "ა" "შე" "ჩა" "წარ" "წა"
                 "წინა" ;; წინავგრძნობ
                 "გადა" "გარდა" "გან" "გა" "უკუ" "უარ" "შთა" "ძალ" "თანა" "ზარ" "ზედა"
                 "გარემო" "გარშემო" "გარსშემო"
                 "წინააღ" "ზეწამო" "მიმოგან"
                 "იავარ" "იძულებულ" "ნათელ" "სრულ" "უგულებელ" "უვნებელ" "უზრუნველ" "უკვდავ" "უჩინარ" "ურთიერთ"
                 "ღაღად" "შეურაცხ" "ცხად" "ხელ" "ჴელ" ;; for Kmna/qoPa
		 "§" ;; placeholder for OG compound verb prefixes
		 "*") ;; placeholder for directional preverbs, to be implemented
               `((pv ,morph) (c-pv ,morph))) 
      ,(utp "გად" '((pv "გად") (c-pv "გადა")))
      ,(utp "გარდ" '((pv "გარდ") (c-pv "გარდა"))))
 :name 'pv)

; preverbs used only with root ყოფ1 (?) ;; fix!
#[qopna-pv
  = [("იავარ" "იძულებულ" "ნათელ" "სრულ" "უგულებელ" "უზრუნველ" "უვნებელ"
      "უკვდავ" "უჩინარ" "ღაღად" "შეურაცხ" "ცხად" "ხელ")
     ((pv ?morph) (c-pv ?morph) (c-root "ყოფ1"))]]

#[preverb-group
  = (or
     [e ((pv -) (c-pv -))]
     (seq qopna-pv ["#"])
     (seq (or pv
	      ["დამო" ((pv "დამო") (c-pv "და") (dir-pv "მო"))]
	      ["აღმო" ((pv "აღმო") (c-pv "აღ") (dir-pv "მო"))]
	      ["ამო" ((pv "ამო") (c-pv "ა") (dir-pv "მო"))]
	      ["შემო" ((pv "შემო") (c-pv "შე") (dir-pv "მო"))]
	      ["ჩამო" ((pv "ჩამო") (c-pv "ჩა") (dir-pv "მო"))]
	      ["წარმო" ((pv "წარმო") (c-pv "წარ") (dir-pv "მო"))]
	      ["წამო" ((pv "წამო") (c-pv "წა") (dir-pv "მო"))]
	      ["ზეწამო" ((pv "ზეწამო") (c-pv "წა") (dir-pv "მო"))]
	      ["გადმო" ((pv "გადმო") (c-pv "გადა") (dir-pv "მო"))]
	      ["გარდმო" ((pv "გარდმო") (c-pv "გარდა") (dir-pv "მო"))]
	      ["გამო" ((pv "გამო") (c-pv "გა") (dir-pv "მო"))]
	      ["შთამო" ((pv "შთამო") (c-pv "შთა") (dir-pv "მო"))])
      (? ["%" ((dialect +))]) ;; special marker, see phonotactics.regex
      ["#"]))]		      ;; preverb marker

; version vowels
(make-utp-or '("ა" "ი" "უ" "ე") `((vv ,morph))
             :name 'vv-aiue)

;; version vowel "უ" is realized as "ი" after 1./2. object prefix
#[vv-aie = (or [("ა" "ე") ((vv = ?morph))]
                ["ი" ((vv = {"ი" "უ"}))])]

; root
#-fst
(precompile-u-transducer 
 (utp '$ '((root)))
 :name 'root)

; roots that can reduplicate
#-fst
(precompile-u-transducer 
 (utp '$ '((root)))
 :name 'reduplication-root)

#[obj-1sg = ["მ" ((obj pers = 1)
		  (obj num = sg)
		  (subj pers = {2 3})
		  (lang ng))]]

;; fixme: but not MG
#[obj-1-og = ["მ" ((obj pers = 1)
		   (subj pers = {2 3})
		   (lang og))]]

#[obj-1pl = ["გვ" ((obj pers = 1)
		   (obj num = pl)
		   (obj incl-1pl = +) ;; needed for OG
		   (subj pers = {2 3}))]]

#[obj-2 = ["გ" ((obj pers = 2)
		(subj pers = {1 3}))]]

#[obj-3-h = (or
	     ["H" ((obj pers = 3)
		   (type root = h-root)
		   ;; (subj pers = {2 3}) ;; Feb. 2015
		   (type obj-3-pfx = {++ + +- x})
		   (lang = ng))]
	     ["H" ((obj pers = 3)
		   (type root = h-root)
		   ;;(subj pers = {2 3}) ;; Feb. 2015
		   (subnorm = +)
		   (type obj-3-pfx = -)
		   (lang = ng))]
	     ["H" ((obj pers = 3)
		   (type root = h-root)
		   (subj pers = 1)
		   (type obj-3-pfx = {++ +})
		   (subnorm = +)
		   (lang = ng))])]

#[obj-3-s = (or
	     ["H" ((obj pers = 3)
		   (type root = s-root)
		   (type obj-3-pfx = {++ + +- x})
		   (lang = ng))]
	     ["H" ((obj pers = 3)
		   (type root = s-root)
		   (subnorm = +)
		   (type obj-3-pfx = -)
		   (lang = ng))])]

;; #[obj-3 = ["H" ((obj pers = 3))]]

#[obj-3-null = (or
		[e ((type root = { s-root x-root null-root })
		    (type obj-3-pfx = {-- - +- x})
		    (lang = ng))]
		[e ((subnorm = +)
		    (type root = { s-root x-root null-root })
		    (type obj-3-pfx = +)
		    (lang = ng))]
		;; h-root 1 subj
		[e ((type root = h-root)
		    ((subj pers) 1)
		    (type obj-3-pfx = {-- - +- + x})
		    (lang = ng))]
		[e ((type root = h-root)
		    ((subj pers) 1)
		    (subnorm = +)
		    (type obj-3-pfx = ++)
		    (lang = ng))]
		;; h-root 2/3 subj
		[e ((type root = h-root)
		    ((subj pers) {2 3})
		    (type obj-3-pfx = {-- - +- x})
		    (lang = ng))]
		[e ((type root = h-root)
		    ((subj pers) {2 3})
		    (subnorm = +)
		    (type obj-3-pfx = +)
		    (lang = ng))])]

#[obj-3 =  (or
	    (seq
	     (or
	      ["H" ((obj pers = 3)
		    (lang = og)
		    (type obj-3-pfx = {++ + +- -- - x}))]
	      ["X" ((obj pers = 3) ;; დაჰბადა etc., I-61
		    (lang = og) ;; fixme: should be active
		    (type obj-3-pfx = x))])
	     (utp-not '((vv = "ი")
			(class = pass))))
	    ["X" ((obj pers = 3) ;; ხანმეტი ი-passives: ხიყო etc.
		  (vv = "ი")
		  (class = pass) ;; class equals Genus flag
		  (lang = og))])]

(make-utp 'subj-1 "ვ" '(((subj pers) 1)))

;; TODO: make obsolete by putting into phonotactics.regex
(make-utp 'subj-2-x "ხ" '(((subj pers) 2)
			  ((obj pers) 3)
			  (type subj2-pfx = +)
			  (vv -)))

#[subj-2 = ["h" ((subj pers = 2)
		 (obj pers = {1 3}))]]

#[prefix-group
  = (or
     ;; obj pers 3 
     (seq
      (or subj-1
	  subj-2-x
	  subj-2
	  [e ((subj pers = 3))])
      ;; ჭავჭავაძე: გაჰხდა, ჰყოფილხარ etc.
      (? ["ჰ" ((subj pers = {2 3})
	       (obj pers = 3)
	       (vv = -)
	       (subnorm +)
	       (lang ng))])
      (seq (or obj-3   ;; OG
	       obj-3-s ;; NG
	       obj-3-h
	       obj-3-null)
	   [e ((obj pers = 3))])
      (or vv-aiue [e ((vv = -))])
      [e ((obj pers = 3))])
     ;; obj pers 1/2
     (seq
      (or (seq (or subj-2 [e ((subj pers = {1 3}))])
	       (or obj-1sg
		   obj-1-og
		   obj-1pl
		   obj-2))
	  obj-2)
      (or vv-aie
	  [e ((vv -))])))]

#+fstxx
(precompile-u-transducer
 `(seq ,(utp "-" '((reduplication +) (dir-pv -)))
       (or (seq pv
                ,(utp-not '((c-pv {"მი" "გადა" "გარდა"})))
                ,(utp "მო" #+ignore'((red-dir-pv "მო"))))
           ,(utp "მო" '((c-pv "მი") #+ignore(red-dir-pv "მო")))
           ,(utp "გადმო" '((c-pv "გადა") #+ignore(red-dir-pv "მო")))
           ,(utp "გარდმო" '((c-pv "გარდა") #+ignore(red-dir-pv "მო")))
           ;; ა…-და… type reduplication
           ,(utp-or '("და" "ჩა" "ჩამო")
		    `((c-pv "ა")
		      #+ignore(red-dir-pv ,morph)))) ;; formulate differently?
       ,(utp "#") ;; preverb marker
       prefix-group
       )
 :name 'reduplication-prefix-group)

#-fst
(precompile-u-transducer
 `(seq (or ,(utp "-" '((reduplication +) (dir-pv -)))
           #-fst ,(utp "-" '((parsing +) (reduplication +) (dir-pv -)))) ;; "-" For Tschenkeli parsing
       (or (seq pv
                ,(utp-not '((c-pv {"მი" "გადა" "გარდა"})))
                ,(utp "მო" '((red-dir-pv "მო"))))
           ,(utp "მო" '((c-pv "მი") (red-dir-pv "მო")))
           ,(utp "გადმო" '((c-pv "გადა") (red-dir-pv "მო")))
           ,(utp "გარდმო" '((c-pv "გარდა") (red-dir-pv "მო")))
           ;; ა…-და… type reduplication
           ,(utp-or '("და" "ჩა" "ჩამო")
		    `((c-pv "ა")
		      (red-dir-pv ,morph)))) ;; formulate differently?
       ,(utp "#") ;; preverb marker
       prefix-group
       )
 :name 'reduplication-prefix-group)

(precompile-u-transducer
 (utp "თ" '(((subj pers) {1 2}) ((subj num) pl)))
 :name 'subj-12-pl-t)

(precompile-u-transducer
 `(or subj-12-pl-t
      ,(utp-e '(((subj pers) {1 2})
                ((subj num) sg))))
 :name 'subj-12-pl-t?)

;; not used??
(precompile-u-transducer
 (utp "თ" '(((obj pers) 2)
	    ((obj num) pl)
	    (lang ng)))
 :name 'obj-2-pl-t)

;; not used??
(precompile-u-transducer
 `(or obj-2-pl-t 
      ,(utp-e '(((obj pers) 2)
                ((obj num) sg)
		(lang ng)))
      ,(utp-e '(((obj pers) 2) ;; obj pl is not marked in OG
                (lang og)))
      ,(utp-e '(((obj pers) {1 3}))))
 :name 'obj-2-pl-t?)

;; (plu)perfect
(precompile-u-transducer
 `(or ,(utp "თ" '(((obj pers) {2 3})
		  ((obj num) pl)
		  (lang ng)))
      ,(utp "თ" '(((subj pers) {1 2}) ;; new
		  ((subj num) pl))))
 :name 'obj-23-pl-t)

;; (plu)perfect
(precompile-u-transducer
 `(or obj-23-pl-t
      ,(utp-e '(((obj pers) {2 3})
                ((obj num) sg)
		((subj num) sg)
		(lang ng)))
      ,(utp-e '(((obj pers) 2)
                ((subj num) sg)
		(lang og)))
      ,(utp-e '(((subj pers) 3)
		(lang og)))
      ,(utp-e '(((obj pers) 1)
		((subj num) sg)
		)))
 :name 'obj-23-pl-t?)

;; for perfect and conj-perfect where თ overrides ს
(precompile-u-transducer
 `(or obj-23-pl-t
      ,(utp "ს" '(((obj pers) {2 3})
                  ((subj pers) 3)
                  ((obj num) sg)
		  (lang ng)))
      ,(utp "ს" '(((obj pers) {2 3})
                  ((subj pers) 3)
                  (lang og)))
      ,(utp "ს" '(((obj pers) 1)
                  ((subj pers) 3))))
 :name 'obj-23-pl-t/s)

(precompile-u-transducer
 (utp "თ" '(((subj num) pl)))
 :name 'subj-pl-t)

(precompile-u-transducer
 `(or subj-pl-t ,(utp-e '(((subj num) sg))))
 :name 'subj-pl-t?)

#[d-st-ext
  = ["დ" ((pr-st-ext = "დ")
	  (tense = {imperfect conditional conj-present conj-future
			      iter-imperfect imperative-present})
	  (type pr-st-ext = "დ"))]]	; only relevant for medium

#[od-st-ext
  = ["ოდ" ((pr-st-ext = "ოდ")
	   (tense = {imperfect conditional conj-present conj-future
			       iter-imperfect imperative-present})
	   (type pr-st-ext = "ოდ"))]]

;; personal ending after d
#+ignore
(precompile-u-transducer
 `(or ,(utp "ი" '((pers-sfx "ი")
                  ((subj pers) {1 2})
                  (tense {imperfect conditional})))
      ,(utp "ა" '((pers-sfx "ა")
                  ((subj pers) 3)
                  ((subj num) sg)
                  (tense {imperfect conditional})))
      ,(utp "ე" '((pers-sfx "ე")
                  (tense {conj-present conj-future}))))
 :name 'ps-d-vowel)

#+old
#[subj-12-t?
  = (or (seq ["თ"]
             (or [e (((subj num) pl))]
                 [e (((subj num) sg)
                     ((obj pers) 2) ; not გვ-
                     ((obj num) pl))]
                 [e (((subj num) sg)
                     ((subj pers) 3) ;; new
                     ((obj pers) 3)
                     ((obj num) pl)
                     (inverted +)
                     )]))
        ;; no -T
        (seq [e (((subj num) sg))]
             (or [e (((obj pers) {2 3})
                     ((obj num) sg))]
                 [e (((obj pers) 1))]
                 [e (((subj num) sg)
                     ((obj pers) 3)
                     ((obj num) pl)
                     (inverted -))]
                 ;; new, but see ??? hierarchy, Boeder?
                 [e (((subj num) sg)
                     ((subj pers) {1 2})
                     ((obj pers) 3)
                     ((obj num) pl)
                     (inverted +))])))]

#[subj-12-t?
  = (or (seq ["თ"]
             (or [e (((subj num) pl))] ;; always subj pl in OG
                 [e (((subj num) sg)
                     ((obj pers) 2) ; not გვ-
                     ((obj num) pl)
		     (lang ng))]
                 [e (((subj num) sg)
                     ((subj pers) 3) ;; new
                     ((obj pers) {2 3})
                     ((obj num) pl)
		     (lang ng))]))
        ;; no -თ
        (seq [e (((subj num) sg))]
             (or [e (((obj pers) {2 3})
                     ((obj num) sg))]
                 [e (((obj pers) 1)
		     (lang ng))]
                 [e ((lang og))]
                 ;; new, but see ??? hierarchy, Boeder?
                 [e (((subj num) sg)
                     ((subj pers) {1 2})
                     ((obj pers) 3)
                     ((obj num) pl)
                     (inverted +))])))]

;; ???
#+not-used
#[subj-12v-t?
  = (or (seq ["თ"]
             (or [e (((subj num) pl))]
                 [e (((subj num) sg)
                     ((obj pers) 2) ; not გვ-
                     ((obj num) pl)
		     (lang ng))]
                 [e (((subj num) sg)
                     ((subj pers) 3) ;; new
                     ((obj pers) 3)
                     ((obj num) pl)
		     (lang ng))]))
        ;; no -თ
        (seq [e (((subj num) sg))]
             (or [e (((obj pers) {2 3})
                     ((obj num) sg))]
                 [e (((obj pers) 1))]
                 ;; new, but see ??? hierarchy, Boeder?
                 [e (((subj num) sg)
                     ((subj pers) {1 2})
                     ((obj pers) 3)
                     ((obj num) pl)
                     (inverted +))])))]

#[subj-3sg-t
  = (seq ["თ"]
         (or [e (((subj num) sg))]
             [e ((inverted +))])
         (or [e (((obj pers) 2) ; not გვ-
                 ((obj num) pl)
		 (lang ng))]
             [e (((obj pers) 3)
                 ((obj num) pl)
		 (lang ng)
                 ;;;;;??(inverted +)
                 )]))] ; inversion

;; წერეთელი :
;; + ჰქონდესთ subj 3sg obj 3pl
;; + აუკრავსთ subj 3sg obj 3pl
;; + გთხოვსთ  subj 3sg obj 2pl

#[subj-3sg-t?
  = (or subj-3sg-t
        ;; no -თ
        (seq [e (((subj num) sg))]
             (or [e (((obj pers) 1)
		     (lang ng))]
                 [e (((obj pers) {2 3})
                     ((obj num) sg)
		     (lang ng))]
                 [e ((lang og))]
		 #+test
                 [e (((obj num) pl) ;; მიხილნა, გიხილნა, იხილნა
		     ;;(lang og)
                     (inverted -))])))]

#[subj-3sg-s?
  = (or
     [e ((subj pers = 3)
	 (subj num = sg)
	 (inverted -)
	 ;;(obj num = sg) ; August 2016
	 )]
     [e ((subj pers = 3)
	 (subj num = sg)
	 (inverted = -) ;; ??
	 (obj pers = 2) ;; August 2016, was: (obj pers = {2 3})
	 (obj num = pl) ;; xxxxxxxx
	 )]
     [e ((subj pers = 3)
	 ;;(subj num = pl)
	 (inverted +)
	 (obj num = sg))]
     #+ignore ; August 2016
     [e ((subj pers = 3) ;; იხილნეს
	 (inverted -)
	 (obj num = pl)
	 ;;(lang og)
	 )]
     #+test
     [e ((subj pers = 3)
	 (subj num = sg)
	 (inverted +)
	 (obj num = pl))]
     [e ((subj pers = 3)
	 (subj num = sg)
	 (obj pers = 1)
	 (obj num = pl))]
     [e ((subj pers = 3)
	 (subj num = pl)
	 (obj pers = 1)
	 (obj num = pl)
	 (inverted = +))])]

#[ps-d-pers-sfx
  = (or (seq
	 (or ["ი" ((pers-sfx = "ი")
		   (subj pers = {1 2})
		   (tense = iter-imperfect )
		   (lang og))]
	     ["i" ((pers-sfx = "ი")
		   (subj pers = {1 2})
		   (tense = {imperfect conditional}))]
	     ["i" ((pers-sfx = "ი")
		   (subj pers = 2)
		   (tense = imperative-present)
		   (lang og))]
	     ["ე" ((pers-sfx = "ე")
		   (subj pers = {1 2})
		   (tense = {conj-present conj-future}))]
	     ["ე" ((pers-sfx = "ე")
		   (subj pers = 3)
		   (obj pers = {2 3})
		   (obj num = pl)
		   (inverted = +)
		   (tense = {conj-present conj-future}))])
	 subj-12-t?)
     ;; 3. singular
     (seq ["ა" ((pers-sfx = "ა")
		(subj pers = 3)
		(subj num = sg)
		(inverted = -)
		(tense = {imperfect conditional}))]
      subj-3sg-t?)
     (seq ["ა" ((pers-sfx = "ა")
		(subj pers = 3)
		(inverted = +)
		(tense = {imperfect conditional}))]
      subj-3sg-t?)
     ;; xxx
     (seq ["ეს" ((pers-sfx = "ეს")
		 (tense = {conj-present conj-future}))]
      subj-3sg-s?)
     ["ესთ" ((pers-sfx = "ეს")
	     (tense = {conj-present conj-future})
	     (subj pers = 3)
	     (subj num = sg)
	     ;; (inverted +)
	     ((obj pers) = {2 3} )
	     ((obj num) = pl)
	     (subnorm = +)
	     (lang = ng))]
     (seq ["ის" ((pers-sfx = "ის")
		 (tense = iter-imperfect)
		 (lang og))]
      ;; ??
      subj-3sg-s?)
     (seq ["iნ" ((pers-sfx = "ინ") ;; წერდინ
		 (tense = imperative-present)
		 (lang og))]
      ;; ??
      subj-3sg-s?)
     ;; 3. plural
     ["ნენ" ((pers-sfx = "ნენ")
	     (subj pers = 3)
	     (subj num = pl)
	     (inverted -)
	     (tense = {imperfect conditional conj-present conj-future})
	     (lang ng))]
     ["ენ" ((pers-sfx = "ნენ")
	    (subj pers = 3)
	    (subj num = pl)
	    (inverted -)
	    (tense = {imperfect conditional conj-present conj-future})
	    (subnorm +)
	    (lang ng))]
     ["ეს" ((pers-sfx = "ეს") ;; წერდეს
	    (subj pers = 3)
	    (subj num = pl)
	    (inverted -)
	    (tense = {imperfect conditional})
	    (lang og))]
     ["ენ" ((pers-sfx = "ენ") ;; წერდენ
	    (subj pers = 3)
	    (subj num = pl)
	    (inverted -)
	    (tense = {conj-present conj-future})
	    (lang og))]
     ["იან" ((pers-sfx = "იან") ;; წერდენ
	     (subj pers = 3)
	     (subj num = pl)
	     (inverted -)
	     (tense = iter-imperfect)
	     (lang og))]
     ["ედ" ((pers-sfx = "ედ") ;; წერდედ
	    (subj pers = 3)
	    (subj num = pl)
	    (inverted -)
	    (tense = imperative-present)
	    (lang og))])]

#[ps-od-pers-sfx
  = (or
     (seq [e ((lang ng))] ps-d-pers-sfx)
     (seq
      [e ((lang og))]
      (or (seq (or ["ი" ((pers-sfx = "ი")
			 (subj pers = {1 2})
			 (tense = {iter-imperfect conj-present conj-future}))]
		   ["ე" ((pers-sfx = "ე")
			 (subj pers = {1 2})
			 (tense = {imperfect conditional}))]
		   ["ე" ((pers-sfx = "ე")
			 (subj pers = 2)
			 (tense = imperative-present)
			 (lang og))]
		   #+ignore
		   ["ე" ((pers-sfx = "ე")
			 (subj pers = 3)
			 (obj pers = {2 3})
			 (obj num = pl)
			 (inverted = +)
			 (tense = {conj-present conj-future}))])
	       subj-12-t?)
	  ;; 3. singular
	  (seq ["ა" ((pers-sfx = "ა")
		     (subj pers = 3)
		     (subj num = sg)
		     (inverted = -)
		     (tense = {imperfect conditional}))]
	       subj-3sg-t?)
	  ;; ??
	  (seq ["ა" ((pers-sfx = "ა")
		     (subj pers = 3)
		     (inverted = +)
		     (tense = {imperfect conditional}))]
	       subj-3sg-t?)
	  (seq ["ის" ((pers-sfx = "ის")
		      (tense = {iter-imperfect conj-present conj-future}))]
	       ;; ??
	       subj-3sg-s?)
     
	  (seq ["ენ" ((pers-sfx = "ენ") ;; წერდინ
		      (tense = imperative-present)
		      (lang og))]
	       ;; ??
	       subj-3sg-s?)
	  ;; 3. plural
	  ["ეს" ((pers-sfx = "ეს")
		 (subj pers = 3)
		 (subj num = pl)
		 (inverted -)
		 (tense = {imperfect conditional}))]
	  ["ესთ" ((pers-sfx = "ეს")
		  (subj pers = 3)
		  (subj num = pl)
		  ((obj pers) {2 3} )
		  ((obj num) pl)
		  (subnorm +)
		  (inverted -)
		  (tense = {imperfect conditional}))]
	  ["იან" ((pers-sfx = "იან") ;; დაიწერებოდიან
		  (subj pers = 3)
		  (subj num = pl)
		  (inverted -)
		  (tense = {iter-imperfect conj-present conj-future}))]
	  ["ინ" ((pers-sfx = "ინ") ;; დაიწერებოდინ
		 (subj pers = 3)
		 (subj num = pl)
		 (inverted -)
		 (tense = {conj-present conj-future}))]
	  ["ედ" ((pers-sfx = "ედ") ;; დაიწერებოდედ
		 (subj pers = 3)
		 (subj num = pl)
		 (inverted -)
		 (tense = imperative-present)
		 (lang og))])))]

;; subj-obj-sfx-t?
(precompile-u-transducer
 `(or (seq ,(utp "თ" ())
           (or ,(utp-e '(((type subj12-sfx) -) ;; OG: only this
                         ((subj pers) {1 2})
                         ((subj num) pl)))
               ,(utp-e '(((type subj12-sfx) -)
                         ((subj pers) 1)
                         ((subj num) sg)
                         ((obj pers) 2)
                         ((obj num) pl)
			 (lang ng)))
               ,(utp-e '(((type subj3-sfx) {"ს" "ის"}) ;; collapse to "ს" everywhere??
                         ((subj pers) 3)
                         ((subj num) sg)
                         ((obj pers) 2) ; not გვ-
                         ((obj num) pl)
			 (lang ng)))
               ,(utp-e '(((type subj3-sfx) {"ს" "ის"})
                         ((subj pers) 3)
                         ((subj num) sg)
                         ((obj pers) 3)
                         ((obj num) pl)
			 (inverted +)
                         (lang ng)))
               ,(utp-e '(((type subj3-sfx) {"ს" "ის"})
                         ((subj pers) 3)
                         ((subj num) pl)
                         ((obj pers) {2 3})
                         ((obj num) pl)
                         (inverted +) ;; inversion only
			 (lang ng)))))
      ;; no -თ
      (seq ,(utp-e '(((type subj12-sfx) {"ა" -}) ;; OG: only this
                     ((subj num) sg)
                     ((subj pers) {1 2})))
           (or ,(utp-e '(((obj pers) {1 3})
			 (lang ng)))
               ,(utp-e '(((obj pers) 2)
                         ((obj num) sg)
			 (lang ng)))
	       ,(utp-e '((lang og))))))
 :name 'subj-obj-sfx-t?)

; present-series-active-suffix-group
(precompile-u-transducer
 `(seq
   ,(utp-e '((morph-type active)))
   (or (seq (or ,(utp-or '("ამ" "ავ" "ოფ" "ი")
			 `((sf ,morph)
			   ;; new (vv {- "ა" "ი" "უ"})
			   )) 
                ,(utp-or '("ევ" "ვ" "ემ" "მ" "ებ" "ობ")
			 `((sf ,morph)))
		,(utp-e '((sf -)))
		,(utp "ებ" '((vv "ე")
			     (sf "ებ")
			     (root "ძი")))
		,(utp "ი" '((vv "ე")
			    (sf "ი")
			    (root "ძახ")))
		,(utp "ი" '((vv "ე")
			    (sf "ი")
			    (root "ლ")))
		,(utp-e '((vv "ე")
			  (sf -)
			  (root "ძახ"))))
            (or (or (seq (or ,(utp "ს" '(((subj pers) 3)
					 ((type subj3-sfx) "ს")
					 (tense { present future })))
			     ,(utp "ნ" '(((subj pers) 3)
					 ((type subj3-sfx) "ს")
					 (tense iter-present))))
			 (or ,(utp-e '(((subj num) sg)
				       (inverted -)))
			     ,(utp-e '((inverted +)
				       (lang ng)))
			     ,(utp-e '(((subj num) sg)
				       (inverted +)
				       (lang og))))
			 (or ;; still wrong: გაუადვილდეს
			  ;; NG
			  (or ,(utp-e '(((obj pers) 1) ; გვ-
					(lang ng)))
			      ,(utp-e '(((obj pers) 2)
					((obj num) sg)
					(lang ng)))
			      ,(utp-e '(((obj pers) 3)
					(inverted -)
					(lang ng)))
			      ,(utp-e '(((obj pers) 3)
					(inverted +)
					((obj num) sg)
					(lang ng))))
			  ;; OG
			  ,(utp-e '((lang og)))))
		    ;; -სთ (e.g., წერეთელი)
		    ,(utp "სთ" '(((subj pers) 3)
				 ((type subj3-sfx) "ს")
				 (tense { present future })
				 ((subj num) sg)
				 ((obj pers) {2 3} )
				 ((obj num) pl)
				 (subnorm +)
				 (lang ng)))
		    (seq ,(utp "ა" '(((subj pers) 3)
				     (vv {- "უ" "ა"}) ;; აცვივა
				     (sf -)	      ; ** ??
				     ((type subj3-sfx) "ა")
				     (tense present))) ;; not future?
			 ,(utp-not '((root "ჯობ")))
			 subj-3sg-t?)
		    (seq ,(utp "ია" '(((subj pers) 3)
				      (root {"ვარგ" "ჯობ"}) ;; FIXME: doesn't work in fst? (e.g., წერია)
				      (tense present)))
			 subj-3sg-t?)
		    (seq ,(utp-not '((sf "ი")))
			 (or (seq ,(utp "ენ" '(((subj pers) 3)
					       ((subj num) pl)
					       (inverted -)
					       (tense {present future})))
				  ,(utp-not '((root {"გuან" "დუმ" "სხედ"
						     "სხენ" ;; OG
						     "ჩან" "წუხ"
						     "ვარგ" "ზი" "დი" "ჩადი" "დგა" "წვ"})
					      (sf -))))
			     ,(utp "ან" '(((subj pers) 3)
					  ((subj num) pl)
					  (inverted -)
					  (tense present)
					  (root {"გuან" "დუმ" "სხედ" "სხენ" "ვარგ"
						 "ჩან" "წუხ" "ახლ" "ზი" "დი" "ჩადი"})))
			     ,(utp "ან" '(((subj pers) 3)
					  ((subj num) pl)
					  (inverted +)
					  (tense present)
					  (root {"ქონ" "სხენ"})
					  (lang og)))
			     ,(utp "ნან" '(((subj pers) 3)
					   ((subj num) pl)
					   (inverted -)
					   (root "დგა")
					   (tense present)))
			     ,(utp "იან" '(((subj pers) 3)
					   ((subj num) pl)
					   (inverted -)
					   (root "ვარგ")
					   (tense present)))
			     ,(utp "ანან" '(((subj pers) 3)
					    ((subj num) pl)
					    (inverted -)
					    (root {"წვ" "ვარგ"})
					    (tense present)))
			     ,(utp "ანან" '(((subj pers) 3)
					    ((subj num) pl)
					    (inverted +)
					    (root {"რწმ"})
					    (tense present)
					    (lang og)))))
		    ,(utp "ან" '(((subj pers) 3)
				 ((subj num) pl)
				 (inverted -)
				 (sf "ი")
				 (tense {present future})))
		    ,(utp "ედ" '(((subj pers) 3)
				 ((subj num) pl)
				 (inverted -)
				 (tense iter-present)))
		    (seq (or ,(utp "ი" '(((type subj3-sfx) "ა"))) ; მინდიხარ
			     ,(utp-e '((root {"ვარგ"})
				       ((type subj3-sfx) "ა"))) ; ვვარგვარ
			     ,(utp-e '(((type subj3-sfx) "ს")))
			     ,(utp-e '((sf "ი"))))
			 (or ,(utp "ვარ" '(((subj pers) 1)
					   (sf {- "ი"})
					   ((type subj12-sfx) "ვარ/ხარ")
					   (lang ng)))
			     ,(utp "ხარ" '(((subj pers) 2)
					   (sf {- "ი"})
					   ((type subj12-sfx) "ვარ/ხარ")
					   (lang ng)))
			     ,(utp-e '(((subj pers) {1 2}) ;; I-101
				       (lang og))))
			 ,(utp-e '((tense present)))
			 subj-12-t?)
		    (seq subj-obj-sfx-t? ,(utp-e '((tense {present future})))))
                (seq (or ,(utp-e '((sf "ი")))
                         ,(utp-not '((sf "ი"))))
                     d-st-ext ps-d-pers-sfx)))
      (seq od-st-ext
           ,(utp-e '((sf "ი"))) ;; ტიროდა
           ps-od-pers-sfx)))
 :name 'present-series-active-suffix-group)

; present-series-causative-suffix-group
(precompile-u-transducer
 `(seq 
   ,(utp-e '((morph-type causative)))
   (or 
    ;; -ineb 
    (seq (or ,(utp-e '((sf -)
                       (vv {- "ა" "ი" "უ"})))
             ,(utp-or '("ევ" "ვ" "ემ" "მ" "ებ" "ობ")
                      `((vv {- "ა" "ი" "უ"}) 
                        (sf ,morph))))
         (or ,(utp "ინ" '((vv {"ა" "ი" "უ"}) ; vurbenineb (only one?)
			  ))
	     ,(utp "ი" '((vv {"ა" "ი" "უ"}) ; vurbenineb (only one?)
			 (subnorm +))))
         ,(utp "ებ" '((caus-sf "ინ"))))
    (seq (or ,(utp "ე" `((vv {- "ა" "ი" "უ"}) 
			 (sf "ევ"))))
         ,(utp "ინ" '((vv {"ა" "ი" "უ"})))
         ,(utp "ებ" '((caus-sf "ინ"))))
    ;; -mev
    (seq ,(utp "მ" `((sf "მ")
		     (vv "ა"))) ;; e.g. vasmev
         ,(utp "ევ" '(((type root-vowel) -)
                      (caus-sf "ევ"))))
    ;; -evineb
    (seq (or ,(utp-e   '((sf -))) ; ??
             ,(utp "ვ" `((sf "ვ")))
             ,(utp "მ" `((sf "მ"))))
         (or ,(utp "ევ" '(((type root-vowel) -)))
	     ,(utp "ე" '(((type root-vowel) -)
			 (subnorm +))))
         ,(utp "ინ" '((vv "ა")))
         ,(utp "ებ" '((caus-sf "ევინ")))))
   (or (seq (or (seq ,(utp "ს" '(((subj pers) 3)
                                 ((subj num) sg)))
                     (or ,(utp-e '(((obj num) sg)))
                         ,(utp-e '(((obj pers) 1) ; გვ-
                                   ((obj num) pl)))))
                ,(utp "ენ" '(((subj pers) 3)
                             ((subj num) pl))) 
                subj-obj-sfx-t?)
            ,(utp-e '((tense {present future}))))
       (seq d-st-ext ps-d-pers-sfx)))
 :name 'present-series-causative-suffix-group)

; present-series-passive-suffix-group
#[present-series-passive-suffix-group ;; erXis, vescraPvi ??
  = (seq
     (utp-not '((root "ვალ")
		(c-root "სვლ")))
     (utp-not '((c-root "სვლ")
		(root "ვლ"))) ;; but: -ivleba, -mevleba! fix!
     (or (seq
          (or (seq ["D" ((passive-sfx "დ+ებ"))]
		   ["ებ"])
              [("ებ" "ობ" "ოფ") ((passive-sfx ?morph)
				 (sf -)
				 (vv {"ა" "ი" "ე" "უ"})
				 )]
              ["ებ" ((passive-sfx "ებ") 
                     (sf -)
                     (vv -))]
              ["ვებ" ((sf "ვ")
                      (passive-sfx "ებ")
                      (vv {"ა" "ი" "ე"}))]
	      ;; ვეჩაგვრინები
              (seq  [e ((vv "ე"))]
                    (or [("ებ" "ევ") ((sf ?morph))]
                        [e ((sf -))])
                    ["ინებ" ((caus-sf "ინ")
                             (passive-sfx "ებ"))])
              ["მებ" ((sf "მ")
                      (passive-sfx "ებ")
                      (vv {"ა" "ი" "ე"}))]
              ["ემ" ((sf -)
                     (passive-sfx "ემ") ; e.g. iCema
                     (vv {"ი" "ე"}))])
          (or ["იან" (((subj pers) 3)
                      ((subj num) pl)
                      ;;(inverted -) ;; მაგონდებიან
                      ;;(obj num = sg)
                      (tense {present future}))]
              (seq ["ის" ((subj pers = 3)
			  (subj num = sg)
			  (inverted = -)
			  (tense {present future})
			  (lang og)
			  ((type subj3-sfx) "ა"))]
                   subj-3sg-t?)
              (seq ["ინ" ((subj pers = 3)
			  (subj num = sg)
			  (inverted = -)
			  (tense iter-present)
			  ;;(lang og)
			  ;; why?
			  ((type subj3-sfx) "ა")
			  )]
                   subj-3sg-t?)
	      ["იედ" ((subj pers = 3)
		      (subj num = pl)
		      (inverted = -)
		      (tense = iter-present))]
	      (seq ["ა" ((subj pers = 3)
                         (subj num = sg)
                         (inverted = -)
                         (tense {present future})
                         (lang ng)
			 ((type subj3-sfx) "ა"))]
                   subj-3sg-t?)
	      (seq ["ა" ((subj pers = 3)
                         (inverted = +)
			 (lang ng)
                         (tense {present future})
                         ((type subj3-sfx) "ა"))]
		   subj-3sg-t?)
	      (seq ["ის" ((subj pers = 3)
			  (inverted = +)
			  (lang og)
			  (tense {present future})
			  ((type subj3-sfx) "ა"))]
                   subj-3sg-t?)
	      ;; new August 2016
	      (seq ["ინ" ((subj pers = 3)
			  (inverted = +)
			  (lang og)
			  (tense iter-present)
			  ;;((type subj3-sfx) "ა")
			  )]
                   subj-3sg-t?)
              ["ის" (((subj pers) 3)
                     ((subj num) sg)
                     (tense {present future})
                     ((type subj3-sfx) "ის"))] ; memravlebis
              (seq ["ი" ( ;;((subj pers) {1 2}) ;; ??
                         ;;((type subj3-sfx) "ის") ;; ??
                         (tense {present future}))]
                   subj-obj-sfx-t?)
              (seq ["ოდ" ((pr-st-ext "ოდ"))]
                   ps-od-pers-sfx))
          (or [e ((morph-type passive))]
              [e ((vv "ე")
                  (tense {future conditional conj-future})
		  (lang ng)
                  (morph-type stative-passive))]))
      (seq
       (or ["ევ" ((sf -)
		  (passive-sfx "ევ")	; e.g. irbeva
		  (vv {"ი" "ე"}))]
	   ["მევ" ((sf "მ")
		   (passive-sfx "ევ")	; e.g. ismeva
		   (vv {"ი" "ე"}))])
       (or ["იან" (((subj pers) 3)
		   ((subj num) pl)
		   (inverted -)
		   ;;(obj num = sg)
		   (tense {present future}))]
	   ["ის" (((subj pers) 3)
		  ((subj num) sg)
		  (tense {present future})
		  (morph-type passive)
		  (lang og))]
	   (seq ["ა" ((subj pers = 3)
		      (subj num = sg)
		      (inverted = -)
		      (tense {present future})
		      (lang ng))]
		subj-3sg-t?)
	   (seq ["ა" ((subj pers = 3)
		      (inverted = +)
		      (lang ng) ;; ??
		      (tense {present future}))]
		subj-3sg-t?)
	   (seq ["ი" (((subj pers) {1 2})
		      (tense {present future}))]
		subj-obj-sfx-t?))
       [e ((morph-type { passive stative-passive } ))])
      (seq
       ["ი" ((passive-sfx "ი") ; e.g. ismis, esmis, eSinia, vemduri, aklia, mgonia, mekuTvni? or stative passive
	     (morph-type passive) ;; {passive stative-passive?}
	     )]
       (or (seq ["ა" (((subj pers) 3)
		      ;;((subj num) sg)
		      (tense {present future}) ;; only present for stative-passive
		      ((type subj3-sfx) "ა"))]
		(or subj-3sg-t?
		    (seq ["ნ" ((lang ng) (subnorm +))]
			 subj-3sg-t?)))
	   (seq ["ეს" (((subj pers) 3) ;; ჰგონიეს
		       (tense {present future})
		       ((type subj3-sfx) "ა")
		       (lang og))]
		subj-3sg-t?)
	   (seq [e (((subj pers) 3)
		    (root "გონ")		;; მგონი
		    (tense {present future}) ;; only present for stative-passive
		    ((type subj3-sfx) "ა")
		    (lang ng))]
		subj-3sg-t?)
	   (seq ["ს" (((subj pers) 3)
		      ((subj num) sg)
		      (tense {present future})
		      (morph-type passive)
		      ((type subj3-sfx) "ის"))]	; *** more exact: connect to sf/vv
		subj-3sg-s?) ;; ????****@@@@
	   ;; new: ესმით
	   (seq [e (((subj pers) 3)
		    ((subj num) sg)
		    (tense {present future})
		    (morph-type passive)
		    ((type subj3-sfx) "ის"))]	; *** more exact: connect to sf/vv
		subj-3sg-t)
	   (seq ["ნ" (((subj pers) 3)
		      ((subj num) sg)
		      (tense iter-present)
		      (morph-type passive)
		      ((type subj3-sfx) "ის"))]
		subj-3sg-s?)
	   ["ან" (((subj pers) 3)
		  ((subj num) pl)
		  ;;(obj num = sg)
		  (tense {present future})
		  #+ignore?
		  ((type subj3-sfx) "ის"))]
	   (seq (or [e ((subj pers = {1 2}) ; e.g. vemduri
			(type subj12-sfx = -)
			(morph-type passive)
			((type subj3-sfx) "ის")
			(tense = {present future}))]
		    ["ვარ" ((subj pers = 1)
			    (type subj12-sfx = "ვარ/ხარ")
			    (tense = {present future})
			    (lang ng))]
		    ["ხარ" ((subj pers = 2)
			    (type subj12-sfx = "ვარ/ხარ")
			    (tense = {present future})
			    (lang ng))] ;; ((type subj3-sfx) "ა") only?
		    ["ე" ((subj pers = {1 2})
			  (type subj12-sfx = "ვარ/ხარ")
			  (tense = {present future})
			  (lang og))])
		subj-12-t?
		;;subj-obj-sfx-t? 
		)
	   (seq ["ნ" ((subj pers = 3)
		      (subj num = sg)
		      (inverted = -)
		      (tense iter-present))]
		subj-3sg-t?)
	   ["ედ" ((subj pers = 3)
		  (subj num = pl)
		  (inverted = -)
		  (tense = iter-present))]))
      
      (seq ["ოდ" ((pr-st-ext "ოდ") ;; vetqodi
		  (vv {"ი" "ე"})
		  (passive-sfx -))]
	   (or [e ((morph-type passive))]
	       [e ((vv "ე")
		   (morph-type stative-passive))])
	   ps-od-pers-sfx)
      (seq
       (or [("მ" "ვ") ((passive-sfx ?morph) ; e.g. ebmis, meboxvis
		       (vv {"ი" "ე"})
		       (morph-type passive))])
       (or ["იან" (((subj pers) 3) 
		   ((subj num) pl)
		   ;;(obj num = sg)
		   (tense {present future}))]
	   ["ის" (((subj pers) 3) ((subj num) sg)
		  (tense {present future}))]
	   (seq ["ი" (((subj pers) {1 2})
		      (tense {present future}))]
		;; subj-12-pl-t?
		subj-obj-sfx-t?)
	   (seq ["ოდ" ((pr-st-ext "ოდ"))]
		ps-od-pers-sfx)))
      (seq
       (or ["ე" ((passive-sfx "ევ")	; e.g. irbeoda
		 (vv {"ი" "ე"}))]
	   ["ევ" ((passive-sfx "ევ")	; e.g. irbeoda
		  (vv {"ი" "ე"}))]
	   ["მე" ((sf "მ")
		  (passive-sfx "ევ")	; e.g. ismeoda
		  (vv {"ი" "ე"}))]
	   ["ი" ((passive-sfx "ი")	; e.g. hKvioda
		 (vv {- "უ"}))]
	   [e ((passive-sfx -)	; e.g. ჰრცხვენოდა
	       (vv {- "უ"}))])
       (seq ["ოდ" ((pr-st-ext "ოდ"))]
	    (or [e ((morph-type {passive stative-passive}))]) ;; {passive stative-passive?}
	    ps-od-pers-sfx))))]

#+ignore
(defun c-sf (sf)
  (cond ;((string= sf "ინ") "ენ") ; e.g. gaaPina
        ((string= sf "ი") "ევ") ; e.g. daHia
        ((string= sf "ე") "ევ")
        ((string= sf "ივ") "ევ")
        ((string= sf "ვ") "ევ") ; ??
        ((string= sf "მ") "ამ")
        (t sf)))

#[en-plural-sfx? =
  (or
   [e ((lang ng))]
   (seq
    [e ((lang og)
	(morph-type {active causative}))]
    (or ["N" (((dir-obj num) pl))]
	[e (((dir-obj num) sg))]))
   (seq
    [e ((lang og)
	(gv {"P1" "RP1" "RP1 (OR)" "ZP1" "ZP2" "ZP3"})
	(morph-type { passive stative-passive }))]
    (or ["n" (((subj num) pl))]
	[e (((subj num) sg))]))
   (seq
    (utp-not '((gv {"P1" "RP1" "RP1 (OR)" "ZP1" "ZP2" "ZP3"})))
    [e ((morph-type passive)
	(lang og))]))]

; aorist-series-suffix-group
(precompile-u-transducer
 `(seq (or ,(utp-e '(;; new (vv {"ი" "უ" -})
                     (passive-sfx -) (sf -)
                     (morph-type {active passive})))
           ,(utp "ი" `((sf "ევ")
		       (morph-type active)))
           ,(utp "ევ" `((sf "ევ")
			(morph-type active)
			(lang og)))
	   ,(utp "ივ" `((sf "ევ")
			(morph-type active)
			((dir-obj num) pl)
			((obj pers) 3)
			(lang og)))
	   ,(utp-e '((vv "ა") ;; e.g. vasvi
                     (passive-sfx -) (sf -)
                     (morph-type {active passive})))
           ,(utp-e '((vv "ე")
                     (sf -)
                     (root "ძებ") ;; c-root?
                     (morph-type active)))
           ,(utp-e '((vv "ე")
                     (sf -)
                     (root "ძი")
                     (morph-type active)))
           ,(utp-e '((vv "ე")
                     (sf -)
                     (root "ც")
                     (c-root "ცემ")
                     (morph-type active)))
           ;; causative
           (seq (or ,(utp-e   '((sf -)))
                    ,(utp "ვ" `((sf "ვ")))
                    ,(utp "მ" `((sf "მ"))))
                ,(utp "ევ" '(((type root-vowel) -)))
                (or ,(utp "ინ" '((vv "ა")
				 (caus-sf "ევინ")
				 (morph-type causative)))
		    ,(utp "ი" '((vv "ა")
				(caus-sf "ევინ")
				(morph-type causative)
				(subnorm +)))))
	   (seq (or ,(utp-e   '((sf -)))
                    ,(utp "ვ" `((sf "ვ")))
                    ,(utp "მ" `((sf "მ"))))
                ,(utp "ე" '(((type root-vowel) -)))
                ,(utp "ინ" '((vv "ა")
			     (caus-sf "ევინ")
			     (morph-type causative))))
           (seq (or ,(utp-or '("ევ" "ვ" "ემ" "მ" "ებ" "ობ")
                             `((sf ,morph)))
                    ,(utp-e   '((sf -))))
                (or ,(utp "ინ" '((vv {"ა" "ი" "უ"}) ; avurbenine
                                 (caus-sf "ინ")
                                 (morph-type causative)))
                    ,(utp "ინ" '((vv "ე") ; aveGebine
                                 (caus-sf "ინ")
                                 (morph-type causative)))
		    ,(utp "ი" '((vv {"ა" "ი" "უ"}) ; avurbenine
				(caus-sf "ინ")
				(morph-type causative)
				(subnorm +)))
                    ,(utp "ი" '((vv "ე") ; aveGebine
				(caus-sf "ინ")
				(morph-type causative)
				(subnorm +)))))
           ;; passive
           (seq ,(utp-e '((passive-sfx "ევ")
                          (sf -)
                          (morph-type { passive stative-passive })))
                (or
                 ;; aorist
                 ,(utp "ი" '(((subj pers) {1 2})
                             (tense aorist)
                             ((type aorist) weak)))
                 ,(utp-e `(((subj pers) {1 2}) ;; Tsch. p. 268
                           (tense aorist)
                           ((type aorist) strong)))
		 ,(utp-e `((tense { iter-aorist imperative-aorist })
			   (lang og))) ;; ??
                 ,(utp "ი" '(((subj pers) 3)
                             ((subj num) sg)
                             (tense aorist)
                             ((type aorist) weak)
                             ((type ev-sfx) -)))
                 ,(utp-e '(((subj pers) 3)
                           ;((subj num) sg)
                           (tense aorist)
                           ((type aorist) weak)
                           ((type ev-sfx) mixed)))
                 ,(utp-e '(((subj pers) 3)
                           ((subj num) sg)
                           (tense aorist)
                           ((type aorist) strong)))
                 ,(utp "ივ" `(((subj pers) 3)
                              ((subj num) pl)
                              (tense aorist)
                              ((type aorist) weak)))
                 ,(utp-e `(((subj pers) 3)
                           ((subj num) pl)
                           (tense aorist)
                           ((type aorist) strong)))
                 ;; optative
                 ,(utp "ი" '((tense optative)
                             ((type optative) "ო")))
                 ,(utp-e '((tense optative)
			   ((type optative) { "ე" "ა" } ) ;; "ა" is new, experimental mai 2009 ;; daerkvas
                           ))))
           ,(utp "D" '((passive-sfx "დ+ებ")
		       (sf -)
		       ((type aorist) strong) ; ?
		       ((type aorist-3sg) "ა")
		       (morph-type passive)))
           ,(utp-e '((vv "ე")
                     (passive-sfx -) ;; ??
                     (sf -)
                     (morph-type {passive stative-passive}))))
       (or
        ;; 1. + 2. person 
        (seq (or (seq (or ,(utp "ე" `((screeve-sign "ე")
				      (tense aorist)
				      ((type aorist) weak)
				      ((subj pers) {1 2})))
			  ,(utp "ე" `((screeve-sign "ე")
				      (tense imperative-aorist)
				      ((type aorist) weak)
				      ((subj pers) 2)
				      (lang og))))
		      en-plural-sfx?)
		 (seq en-plural-sfx?
		      (or ,(utp "ი" `(;;(screeve-sign "ი")
				      (tense iter-aorist)
				      ((subj pers) {1 2})
				      (lang og)))
			  ,(utp "i" `((screeve-sign "ი")
				      (tense aorist)
				      ((type aorist) strong)
				      ((subj pers) {1 2})))
			  ,(utp "i" `((screeve-sign "ი")
				      (tense imperative-aorist)
				      ((type aorist) strong)
				      ((subj pers) 2)
				      (lang og)))
			  ,(utp "o" `(((type optative) "ო")
				      (tense optative)
				      ((type ev-sfx) -)
				      ((subj pers) {1 2})))
			  (seq ,(utp "a" `(((type optative) "ა")
					   (tense optative)
					   (morph-type active)
					   #+aorist-type((type aorist) strong)
					   ((subj pers) {1 2})))
			       ,(utp-not `((root "ც") (c-root "ცემ"))))
			  ,(utp "ე" `((root "ც") (c-root "ცემ")
				      ((type optative) "ე")
				      (tense optative)
				      (morph-type active)
				      ((subj pers) {1 2})))
			  ,(utp "ე" `(((type optative) "ე")
				      (tense optative)
				      ;;(morph-type passive)
				      ((subj pers) {1 2})))
			  ,(utp "a" `(((type optative) "ა")
				      (tense optative)
				      (morph-type {passive stative-passive #+ignore causative})
				      ((subj pers) {1 2}))))))
	     subj-obj-sfx-t?)
        ;; 3. person
        (seq en-plural-sfx?
	     (or (seq (or ,(utp "ა" `((screeve-sign "ა")
				      (tense aorist)
				      ((type aorist-3sg) "ა")
				      ((subj pers) 3)
				      ((subj num) sg)))
			  ,(utp "O" `((screeve-sign "ო")
				      (tense aorist)
				      ((type aorist-3sg) "ო")
				      ((subj pers) 3)
				      ((subj num) sg))))
		      subj-3sg-t?)
		 (seq (or ,(utp "N" `((screeve-sign "ნ") ;; BUT: იყავნ <- ი<ყAV>N ?
				      (tense imperative-aorist)
				      ;;((type aorist-3sg) "ა")
				      ((subj pers) 3)
				      ((subj num) sg)
				      (lang og))))
		      ;; ??
		      subj-3sg-t?)
		 (seq (or ,(utp "oს" `(((type optative) "ო") ;; valid for obj num = pl, pers = 2??
				       (tense optative)
				       ((type ev-sfx) -)
				       ((subj pers) 3)
				       ((subj num) sg)))
                 
			  ,(utp "oსთ" `(((type optative) "ო")
					(tense optative)
					((type ev-sfx) -)
					((subj pers) 3)
					((subj num) sg)
					((obj pers) = {2 3} )
					((obj num) = pl)
					(subnorm = +)
					))
			  ,(utp "ეს" `(((type optative) "ე")
				       (tense optative)
				       ;;(morph-type passive)
				       ((subj pers) 3)
				       ((subj num) sg)))
			  ,(utp "ესთ" `(((type optative) "ე")
					(tense optative)
					;;(morph-type passive)
					((subj pers) 3)
					((subj num) sg)
					((obj pers) = {2 3} )
					((obj num) = pl)
					(subnorm = +)))
			  
			  ,(utp "aს" `(((type optative) "ა")
				       (tense optative)
				       (morph-type { passive stative-passive })
				       ((subj pers) 3)
				       ((subj num) sg)))
			  
			  ,(utp "aსთ" `(((type optative) "ა")
					(tense optative)
					(morph-type { passive stative-passive })
					((subj pers) 3)
					((subj num) sg)
					((obj pers) = {2 3} )
					((obj num) = pl)
					(subnorm = +)))
			  ;; special for მისცე etc.
			  (seq ,(utp "aს" `(((type optative) "ა") ;; *1*
					    (tense optative)
					    (morph-type active)
					    ((subj pers) 3)
					    ((subj num) sg)))
			       ,(utp-not `((root "ც") (c-root "ცემ"))))
			  ,(utp "ეს" `((root "ც")
				       (c-root "ცემ") ;; *2*
				       ((type optative) "ე")
				       (tense optative)
				       (morph-type active)
				       ((subj pers) 3)
				       ((subj num) sg)))
			  ,(utp "ის" `((tense iter-aorist) ;; დაწერის
				       ((type ev-sfx) -)   ;; ??
				       ((subj pers) 3)
				       ((subj num) sg)
				       (lang og))))
		      subj-3sg-s?)
		 ;; <- moved to (*1*, *2*) ->
		 ;; 3. plural
		 ,(utp "ეს" `((screeve-sign "ეს")
			      (tense aorist)
			      (morph-type {active causative})
			      ((subj pers) 3)
			      ((subj num) pl)))
		 ,(utp "ედ" `((screeve-sign "ედ")
			      (tense imperative-aorist)
			      (morph-type {active causative}) ;; ??
			      ((subj pers) 3)
			      ((subj num) pl)
			      (lang og)))
		 ,(utp "იან" `((screeve-sign "იან")
			       (tense iter-aorist)
			       (morph-type {active causative}) ;; ??
			       ((subj pers) 3)
			       ((subj num) pl)
			       (lang og)))
		 (seq (or ,(utp "o" `(((type optative) "ო")
				      (tense optative)
				      ((subj pers) 3)
				      ((subj num) sg)))
			  (seq ,(utp "ა" `(((type optative) "ა")
					   (tense optative)
					   (morph-type active)
					   ((subj pers) 3)
					   ((subj num) sg)))
			       ,(utp-not `((root "ც") (c-root "ცემ"))))
                 
			  ;; special for misCe etc.
			  ,(utp "ე" `((root "ც") (c-root "ცემ")
				      ((type optative) "ე")
				      (tense optative)
				      (morph-type active)
				      ((subj pers) 3)
				      ((subj num) sg)))
                 
			  ,(utp "ა" `(((type optative) "ა")
				      (tense optative)
				      (morph-type passive)
				      ((subj pers) 3)
				      ((subj num) sg)))
                 
			  ,(utp "ე" `(((type optative) "ე")
				      (tense optative)
				      (morph-type {passive #+ignore causative})
				      ((subj pers) 3)
				      ((subj num) sg))))
		      subj-3sg-t) ;; xxxxx
		 
		 ,(utp "oნ" `(((type optative) "ო")
			      (tense optative)
			      (morph-type {active passive stative-passive causative})
			      #+aorist-type((type aorist) weak)
			      ((type ev-sfx) -)
			      ((subj pers) 3)
			      ((subj num) pl)
			      #+nein(obj num = sg)))
		 
		 (seq ,(utp "aნ" `(((type optative) "ა")
				   (tense optative)
				   (morph-type active)
				   #+aorist-type((type aorist) strong)
				   ((subj pers) 3)
				   ((subj num) pl)
				   #+nein(obj num = sg)))
		      ,(utp-not `((root "ც") (c-root "ცემ"))))
        
		 ;; special for მისცე etc.
		 ,(utp "ენ" `((root "Eც") (c-root "ცემ")
			      ((type optative) "ე")
			      (tense optative)
			      (morph-type active)
			      #+aorist-type((type aorist) strong)
			      ((subj pers) 3)
			      ((subj num) pl)
			      #+nein(obj num = sg)))
		 ,(utp "aნ" `(((type optative) "ა")
			      (tense optative)
			      (morph-type { passive stative-passive } )
			      ((subj pers) 3)
			      ((subj num) pl)))
		 ,(utp "ენ" `(((type optative) "ე")
			      (tense optative)
			      ;;(morph-type { passive stative-passive } )
			      ((subj pers) 3)
			      ((subj num) pl)
			      (lang og)))
		 ,(utp "ეს" `((tense aorist)
			      (morph-type { passive stative-passive } )
			      ((subj pers) 3)
			      ((subj num) pl)
			      (lang og)))
		 ,(utp "იან" `((tense iter-aorist)
			       (morph-type { passive stative-passive } )
			       ((subj pers) 3)
			       ((subj num) pl)
			       (lang og)))
		 ,(utp "ედ" `((screeve-sign "ედ")
			      (tense imperative-aorist)
			      (morph-type { passive stative-passive }) ;; ??
			      ((subj pers) 3)
			      ((subj num) pl)
			      (lang og)))))
	,(utp "ნენ" `(((type optative) "ე")
                      (tense optative)
                      (morph-type passive)
                      ((subj pers) 3)
                      ((subj num) pl)
		      (inverted -)
		      (lang ng)))
	,(utp "ნენ" `((screeve-sign "ნენ")
                      ;;(tense {aorist optative})
                      (tense aorist)
                      (morph-type {passive stative-passive}) ;;(morph-type passive) ;; causative??
                      (subj pers = 3)
                      (subj num = pl)
                      (obj num = sg)
		      (inverted -)
		      (lang ng)))
	,(utp "ენ" `(((type optative) "ე")
		     (tense optative)
		     (morph-type passive)
		     ((subj pers) 3)
		     ((subj num) pl)
		     (inverted -)
		     (subnorm +)
		     (lang ng)))
	,(utp "ენ" `((screeve-sign "ნენ")
		     ;;(tense {aorist optative})
		     (tense aorist)
		     (morph-type {passive stative-passive}) ;;(morph-type passive) ;; causative??
		     (subj pers = 3)
		     (subj num = pl)
		     (obj num = sg)
		     (inverted -)
		     (subnorm +)
		     (lang ng)))))
 :name 'aorist-series-suffix-group)

;; pluperfect-suffix-group
(precompile-u-transducer
 `(seq ,(utp-e '((vv "ე")))
       (or ,(utp "ი" `((sf {"ი" "ევ"})
		       (morph-type active))) 
           (seq ,(utp "ებ" `((sf "ებ") ; denominativa: gamekeTeb(ineb)ina
                             (lang ng)))
                (or ,(utp-e '((morph-type active)))
                    (seq (or ,(utp "ინ" '((caus-sf "ინ")
                                      (morph-type causative)))
			     ,(utp "ი" '((caus-sf "ინ")
					 (morph-type causative)
					 (subnorm +))))
                         ,(utp "ებ" '((ppf-vv "ებ"))))) ; name??
                (or ,(utp "ინ" '((ppf-infix "ინ")))
		    ,(utp "ი" '((ppf-infix "ინ") ;; წერეთელი
				(subnorm +)))))
	   
	   ;; OG has no -ებინ- for transitives. What about causatives?
	   (seq ,(utp-e `((sf "ებ") ; denominativa: gamekeTeb(ineb)ina
			  (lang og)))
		(or ,(utp-e '((morph-type active)))
		    #+disabled ;; ??
		    (seq ,(utp "ინ" '((caus-sf "ინ")
                                      (morph-type causative)))
                         ,(utp "ებ" '((ppf-vv "ებ"))))))
           ,(utp-e `((sf "ებ") ;; მოესაზრათ
		     (subnorm +)
		     (morph-type active)))
           (seq (or ,(utp-e   '((sf -)))
                    ,(utp "ვ" `((sf "ვ")))
                    ,(utp "მ" `((sf "მ"))))
                ,(utp "ევ" '(((type root-vowel) -)))
                (or ,(utp "ინ" '((caus-sf "ევინ")
				 (morph-type causative)))
		    ,(utp "ი" '((caus-sf "ევინ")
				(morph-type causative)
				(subnorm +))))
                ,(utp "ებ" '((ppf-vv "ებ"))) ; name??
                (or ,(utp "ინ" '((ppf-infix "ინ")))
		    ,(utp "ი" '((ppf-infix "ინ") ;; წერეთელი
				(subnorm +))))) ; carmomeTKmevinebina
           
           (seq (or ,(utp-or '("ევ" "ვ" "ემ" "მ" "ობ" "ენ") 
                             `((sf ,morph)))
                    ,(utp-e  '((sf -))))
                (or ,(utp "ინ" '((caus-sf "ინ")
				 (morph-type causative)))
		    ,(utp "ი" '((caus-sf "ინ")
				(morph-type causative)
				(subnorm +))))
                ,(utp "ებ" '((ppf-vv "ებ"))) ; name??
                #+orig,(utp "ინ" '((ppf-infix "ინ")))
		(or ,(utp "ინ" '((ppf-infix "ინ")))
		    ,(utp "ი" '((ppf-infix "ინ") ;; წერეთელი
				(subnorm +)))))
           (or ,(utp-e '((sf -)
                         (morph-type active)))
               ,(utp "ნ" '((nasal-infix "ნ")
                           (morph-type active)))))
       (or
        (seq (or 
	      ;; 1. + 2. person object
	      (seq (or ,(utp-e '((lang ng)))
		       (seq ,(utp-e '((lang og)))
			    (or ,(utp "N" '(((dir-obj num) pl)))
				,(utp-e '(((dir-obj num) sg))))))
		   (or ,(utp "ე" `((screeve-sign "ე")
				   (tense pluperfect)
				   ((type aorist) weak)
				   ((subj pers) {1 2})))
		       ,(utp "i" `((screeve-sign "ი")
				   (tense pluperfect)
				   ((type aorist) strong)
				   ((subj pers) {1 2})))
		       ,(utp "ო" `((screeve-sign "ო")
				   (tense conj-perfect)
				   ((type aorist) weak)
				   ((subj pers) {1 2})))
		       (seq ,(utp "ა" `((screeve-sign "ა")
					(tense conj-perfect)
					((type aorist) strong)
					((subj pers) {1 2})))
			    ,(utp-not `((root "ც") (c-root "ცემ"))))
		       ,(utp "ე" `((screeve-sign "ე")
				   (root "ც") (c-root "ცემ")
				   ((type optative) "ე")
				   (tense conj-perfect)
				   (morph-type active)
				   ((type aorist) strong)
				   ((subj pers) {1 2})))
		       ,(utp "ი" `((screeve-sign "ი")
				   (tense iter-perfect)
				   ((subj pers) {1 2})
				   (lang og)))))	      
	      ;; 3. person object
	      (or (seq (? ,(utp "ნ" '((sf -) (style old))))
		       ,(utp "ა" `((screeve-sign "ა")
				   (tense pluperfect)
				   ((type aorist-3sg) "ა")
				   ((subj pers) 3)
				   ((dir-obj num) sg))))
		  ,(utp "ო" `((screeve-sign "ო")
			      (tense pluperfect)
			      ((type aorist-3sg) "ო")
			      ((subj pers) 3)
			      ((dir-obj num) sg)))
		  ,(utp "ნეს" `((screeve-sign "ა")
				(tense pluperfect)
				((subj pers) 3)
				((dir-obj num) pl)))))
             obj-23-pl-t?)
	;; 3. person subject
        (seq
         (or ,(utp "o" `((screeve-sign "ოს")
			 (tense conj-perfect)
			 ((type optative) "ო")
			 #+aorist-type((type aorist) weak)
			 ((subj pers) 3)
			 ((dir-obj num) sg)))
             (seq ,(utp "ა" `((screeve-sign "ას")
                              (tense conj-perfect)
                              ((type optative) "ა")
                              #+aorist-type((type aorist) strong)
                              ((subj pers) 3)
                              ((dir-obj num) sg)))
                  ,(utp-not `((root "ც") (c-root "ცემ"))))
             ,(utp "ე" `((screeve-sign "ეს")
                         (root "ც") (c-root "ცემ")
                         ((type optative) "ე")
                         (tense conj-perfect)
                         (morph-type active)
                         #+aorist-type((type aorist) strong)
                         ((subj pers) 3)
                         ((dir-obj num) sg)))
	     ,(utp "ი" `((screeve-sign "ი")
			 (tense iter-perfect)
			 ((subj pers) 3)
			 ((dir-obj num) sg)
			 (lang og))))
         obj-23-pl-t/s)
	;; obj pl
	(seq
         (or ,(utp "ნენ" `((screeve-sign "ოს")
			   (tense conj-perfect)
			   ((subj pers) 3)
			   ((dir-obj num) pl)))
	     ,(utp "ნიან" `((screeve-sign "ი")
			    (tense iter-perfect)
			    ((subj pers) 3)
			    ((dir-obj num) pl)
			    (lang og))))
         ;; obj-23-pl-t/s
	 ))
       )
 :name 'pluperfect-suffix-group)

; perfect-suffix-group
(precompile-u-transducer
 `(seq ,(utp-e '((vv "უ")))
       (or (seq (or 
                 ;; active, medium
                 (or ,(utp-or '("ებ" "ევ") `((sf ,morph)
                                             (morph-type active)))
                     (seq ,(utp-e '((morph-type active)
                                    (sf -)))
                          (? ,(utp "ნ" '((nasal-infix "ნ"))))))
                 ;; causative
                 (seq (or ,(utp-or '("ევ" "ვ" "ემ" "მ" "ებ" "ობ" #+ignore "ენ")
                                   `((sf ,morph)))
                          ,(utp-e   '((sf -))))
                      (or ,(utp "ინ" '((morph-type causative)))
			  ,(utp "ი" '((morph-type causative)
				      (subnorm +))))
                      ,(utp "ებ" '((caus-sf "ინ"))))
                 (seq (or ,(utp-e   '((sf -)))
                          ,(utp "ვ" `((sf "ვ")))
                          ,(utp "მ" `((sf "მ")))) ; carmomiTKmevinebia
                      ,(utp "ევ" '(((type root-vowel) -)))
		      (or ,(utp "ინ" '((morph-type causative)))
			  ,(utp "ი" '((morph-type causative)
				      (subnorm +))))
                      ,(utp "ებ" '((caus-sf "ევინ"))))
                 ;; e.g. შემისმევია
                 ,(utp "მევ" `((sf "მ")
                               (caus-sf "ევ")
                               (morph-type causative)))
		 ;; გაუღეჭია
		 ,(utp-e `((sf "ავ")
			   (morph-type active)
			   (lang ng)
			   #+disabled(subnorm +)
			   )))
                ,(utp "ი" '((i-infix "ი")))
                (or ,(utp "ა" '((pers-sfx "ა")
                                ((subj pers) 3)
				(tense perfect)
				(lang ng)))
                    ,(utp "ვარ" '((pers-sfx "ვარ")
                                  ((subj pers) 1)
				  (tense perfect)
				  (lang ng)))
                    ,(utp "ხარ" '((pers-sfx "ხარ")
                                  ((subj pers) 2)
				  (tense perfect)
				  (lang ng)))
		    ;; OG
		    ,(utp "ეს" '((pers-sfx "ა")
				 ((subj pers) 3)
				 ((dir-obj num) sg)
				 (tense perfect)
				 (lang og)))
		    ,(utp "ან" '((pers-sfx "ა")
				 ((subj pers) 3)
				 ((dir-obj num) pl)
				 (tense perfect)
				 (lang og)))
		    ,(utp "ენ" '((pers-sfx "ა") ;; დაუწერიენ
				 ((subj pers) 3)
				 ((dir-obj num) sg)
				 (tense iter-perfect1)
				 (lang og)))
		    ,(utp "ედ" '((pers-sfx "ა") ;; განმიბნევიედ
				 ((subj pers) 3)
				 ((dir-obj num) pl)
				 (tense iter-perfect1)
				 (lang og)))
                    ,(utp "ე" '((pers-sfx "ვარ")
				((subj pers) 1)
				(tense perfect)
				(lang og)))
                    ,(utp "ე" '((pers-sfx "ხარ")
				((subj pers) 2)
				(tense perfect)
				(lang og))))
                obj-23-pl-t?)
           ;; ავ, ამ
	   (seq ,(utp-or '("ავ" "ამ")
			 `((sf ,morph)
			   (morph-type active)
			   (tense perfect)
			   (lang ng)))
                obj-23-pl-t/s)
	   (seq ,(utp-or '("ავ" "ამ")
			 `((sf ,morph)
			   (morph-type active)
			   (lang og)))
                (or ,(utp "ს" '(((subj pers) 3)
				((dir-obj num) sg)
				(tense perfect)))
		    ,(utp "ნ" '(((subj pers) 3)
				((dir-obj num) sg)
				(tense iter-perfect1)))
		    ,(utp "ან" '(((subj pers) 3)
				 ((dir-obj num) pl)
				 (tense perfect)))
		    ,(utp "ედ" '(((subj pers) 3)
				 ((dir-obj num) pl)
				 (tense iter-perfect1)))))
	   ;; 1/2 pers
           (seq (or (seq (or ,(utp "ვ" `((sf "ავ") (lang ng))) ; ?? ***
                             ,(utp-e `((sf "ავ") (lang ng)))
                             (seq ,(utp "მ" `((sf "ამ") (lang ng)))
                                  ,(utp-not '((root "სვ"))))  ; დამისმიხარ (<- დამისვმიხარ)!
                             (seq ,(utp-e `((sf "ამ") (lang ng)))
                                  ,(utp-not '((root "ს")))))
                         ,(utp "ი" '((i-infix "ი")
                                     (morph-type active))))
                    ,(utp "ავ" `((sf "ავ") (morph-type active)))
                    (seq ,(utp "ამ" `((sf "ამ") (morph-type active)))
                         ,(utp-not '((root "ს")))))
                (or ,(utp "ვარ" '((pers-sfx "ვარ")
                                  ((subj pers) 1)
				  (tense perfect)
				  (lang ng)))
                    ,(utp "ხარ" '((pers-sfx "ხარ")
                                  ((subj pers) 2)
				  (tense perfect)
				  (lang ng)))
		    ,(utp-e '((pers-sfx "ვარ")
			      ((subj pers) 1)
			      (tense perfect)
			      (lang og)))
                    ,(utp-e '((pers-sfx "ხარ")
			      ((subj pers) 2)
			      (tense perfect)
			      (lang og))))
                obj-23-pl-t?)))
 :name 'perfect-suffix-group)

; relative-passive-perfect-sf
(precompile-u-transducer
 `(or ,(utp-or '("ევ" "ვ" "ე" "მ" "ებ" "ობ")
               `((sf ,morph)))
      (seq (or ,(utp-or '("ევ" "ებ") `((sf ,morph)))
               ,(utp-e '((sf -))))
           ,(utp "ინებ" '((caus-sf "ინ"))))
      ,(utp-e `((sf -))))
 :name 'relative-passive-perfect-sf)

(precompile-u-transducer
 `(or ,(utp-or '("ვ" "ე" "მ" "ებ" "ობ")
               `((sf ,morph)))
      ,(utp "ე" '((sf "ევ")))
      (seq (or ,(utp-or '("ევ" "ებ") `((sf ,morph)))
               ,(utp-e '((sf -))))
           ,(utp "ინებ" '((caus-sf "ინ"))))
      ,(utp-e `((sf -))))
 :name 'relative-passive-pluperfect-sf)

; relative-passive-perfect-suffix-group
(precompile-u-transducer
 `(seq ,(utp-e '((vv -)
                 (relation relative)
                 (morph-type { passive stative-passive } )))
       (or 
        ;; perfect
        (seq (or relative-passive-perfect-sf
                 ,(utp "ნ" '((nasal-infix "ნ")
                             (sf -))))
             ,(utp "ი" '((i-infix "ი")
                         (tense perfect))) 
             (or (seq ,(utp "ა" '((pers-sfx "ა")
                                  ((subj pers) 3)
                                  ((subj num) sg)))
                      subj-3sg-t?)
                 (seq (or ,(utp "ვარ" '((pers-sfx "ვარ")
                                        ((subj pers) 1)))
                          ,(utp "ხარ" '((pers-sfx "ხარ")
                                        ((subj pers) 2))))
                      subj-12-t?)
                 ,(utp "ან" '((pers-sfx "ან")
                              ((subj pers) 3)
                              ((subj num) pl)
                              ((obj num) sg)
			      #+ignore(lang og)))))
        ;; pluperfect
        (seq relative-passive-pluperfect-sf
             ,(utp "ოდ" '((st-ext "ოდ")
                           (tense pluperfect))) 
             (or (seq ,(utp "ი" '((pers-sfx "ი")
                                  ((subj pers) {1 2})))
                      subj-12-t?)
                 (seq ,(utp "ა" '((pers-sfx "ა")
                                  ((subj pers) 3)
                                  ((subj num) sg)))
                      subj-3sg-t?)
                 ;; subj-pl-t?
                 ,(utp "ნენ" '((pers-sfx "ნენ")
                               ((subj pers) 3)
                               ((subj num) pl)
                               ((obj num) sg)
			       #+ignore(lang og)))))
        ;; conjunctive perfect
        (seq relative-passive-pluperfect-sf
             ,(utp "ოდ" '((st-ext "ოდ")
                           (tense conj-perfect))) 
             (or (seq ,(utp "ე" '((pers-sfx "ე")
                                  ((subj pers) {1 2})))
                      subj-12-t?)
                 ,(utp "ეს" `((pers-sfx "ეს")
                              ((subj pers) 3)
                              ((subj num) sg)
                              ((obj num) sg)))
                 ,(utp "ეს" `((pers-sfx "ეს")
                              ((subj pers) 3)
                              ((subj num) sg)
                              ((obj pers) 1)
                              ((obj num) pl)))
                 (seq ,(utp "ე" `((pers-sfx "ეს")
                                  ((subj pers) 3)
                                  ((subj num) sg)))
                      subj-3sg-t)
                 ,(utp "ნენ" `((pers-sfx "ნენ")
                               ((subj pers) 3)
                               ((subj num) pl)
                               ((obj num) sg)
			       #+ignore(lang og)
			       ))))))
 :name 'relative-passive-perfect-suffix-group)

;; not complete
#[stative-passive-present-suffix-group
  = (seq [e (;;(pv -) ;; may have pv in OG
             (morph-type stative-passive))]
     (or (seq (or [("ენ" "ევ") ((sf ?morph))]
		  [e ((sf -))])
	      (utp-not '((root {"დ" "ძ" })))
	      ["ი" ((passive-sfx "ი"))]
	      (or (seq ["ა" ((pers-sfx "ა")
			     ((subj pers) 3)
			     ((subj num) sg)
			     ;; OG also: see Shanidze p. 134
			     (tense present))]
		       subj-3sg-t?)
		  (seq ["ეს" ((pers-sfx "ა")
			      ((subj pers) 3)
			      ((subj num) sg)
			      (tense present)
			      (lang og))]
		       subj-3sg-t?)
		  (seq (or ["ვარ" (((subj pers) 1)
				   (tense present)
				   (lang ng))]
			   ["ხარ" (((subj pers) 2)
				   (tense present)
				   (lang ng))])
		       subj-12-t?)
		  (seq ["ე" ((subj pers = {1 2})
			     (tense present))]
		       subj-12-t?)
		  (seq (or ["ე" (((subj pers) {1 2}) ;; (pers-sfx "ა") only?
				 (subnorm +)	     ;; examples??
				 (tense present)
				 (lang ng))])
		       subj-12-t?)
		  ["ან" ((pers-sfx "ან")
			 ((subj pers) 3)
			 ((subj num) pl)
			 (tense present))]
		  (seq ["ენ" ((subj pers = 3)
			      (subj num = sg)
			      ;;(inverted = -)
			      (tense iter-present)
			      (lang og))]
		       ;; subj-3sg-t?
		       )
		  ["ედ" ((subj pers = 3)
			 (subj num = pl)
			 ;;(inverted = -)
			 (tense = iter-present)
			 (lang og))]))
      (seq			   ;; stative passives in -ს
       (or ["ავ" ((sf "ავ")	   ; akravs
		  ;;(vv "ა")
		  ;;(root "კრ")
		  )]
	   ["ევ" ((sf "ევ")		; sdevs, sjevs
		  ((subj pers) {2 3})
		  (root {"დ" "ძ"}))]
	   ["ე" ((sf "ევ")		; sdevar, sjevar
		 ((subj pers) 1)
		 (root {"დ" "ძ"}))]
	   [e ((sf -)			; aXns, ujs
	       (root {"ჩნ" "ძ"}))] ; todo: add subj3-sfx
	   [e ((sf -)			; og აბს,…
	       (type subj3-sfx = "ს"))])
       (or ["ს" ((subj pers = 3)
		 (subj num = sg)
		 ;(type subj3-sfx = "ს")
		 (tense present))]
	   ["ენ" (;;(pers-sfx "ენ")
		  ((subj pers) 3)
		  ((subj num) pl)
		  (tense present)
		  (lang ng))]
	   ["ან" (;;(pers-sfx "ან")
		  ((subj pers) 3)
		  ((subj num) pl)
		  (tense present)
		  (lang og))]
	   ["ნ" (((subj pers) 3)
		 ((subj num) sg)
		 (tense iter-present)
		 (lang og))]
	   ["ედ" (((subj pers) 3)
		  ((subj num) pl)
		  (tense iter-present)
		  (lang og))]
	   (seq (or ["ვარ" (((subj pers) 1)
			    (tense present)
			    (lang ng))]
		    ["ხარ" (((subj pers) 2)
			    (tense present)
			    (lang ng))])
		subj-12-t?)
	   (seq (utp-e '((lang og)
			 (tense present)
			 ((subj pers) {1 2})))
		subj-12-t?))
       [e ((morph-type stative-passive))])))]

;; past-participle-stem for perfect
(precompile-u-transducer
 `(seq (or ,(utp "მ" '((part-pfx "მ")))
           #+ignore ;; should not occur in perfect series?
           ,(utp "ნა" '((part-pfx "ნა")))
           ,(utp-e '((part-pfx -))))
       ,(utp "<" '())
       root
       ,(utp ">" '())
       (or (seq (or ,(utp-e '((sf -)))
                    ,(utp-or '("ენ" "ობ") `((sf ,morph))))
                ,(utp "ილ" '((part-pfx -)
                              (part-sfx "ილ"))))
           (seq (or ,(utp-e '((sf -)))
                    ,(utp-or '("ებ" "ე" "მ") `((sf ,morph)))) 
                ,(utp "ულ" '((part-pfx -)
                              (part-sfx "ულ"))))
           ;; prefix m
           (seq ,(utp-e '((sf -)))
                (or ,(utp "არ" '((part-pfx "მ")
                                  (part-sfx "არ")))
                    ,(utp "ალ" '((part-pfx "მ")
                                  (part-sfx "ალ")))))
           ;; prefix na
           #+ignore ;; should not occur in perfect series?
           (seq ,(utp-e `((part-pfx "ნა")
                          ;;(sf -)
                          ))
                (or ,(utp-or '("ენ" "ებ" "ობ" "ავ" "ევ")
                             `((part-pfx "ნა")
                               (sf ,morph)
                               ))
                    ,(utp "არ" '((part-sfx "არ")
                                  (sf -)))
                    ,(utp "ალ" '((part-sfx "ალ")
                                  (sf -)))))))
 :name 'past-participle-stem)

;; past-participle-stem for participle, no more used
(precompile-u-transducer
 `(seq (or ,(utp "მ" '((part-pfx "მ")))
           ,(utp "მა" '((part-pfx "მა")))
           ,(utp "ნა" '((part-pfx "ნა")))
           ,(utp-e '((part-pfx -))))
       ,(utp "<" '())
       #-fst root #+fst past-participle-root
       ,(utp ">" '())
       (or (seq (or ,(utp-e '((sf -)))
                    ,(utp-or '("ენ" "ობ")
			     `((sf ,morph))))
                ,(utp "ილ" '((part-pfx -)
                              (part-sfx "ილ"))))
           (seq (or ,(utp-e '((sf -)))
                    ,(utp-or '("ებ" "ე" "მ") `((sf ,morph)))
                    ,(utp "ე" `((sf "ევ")))
                    ,(utp "მ" `((sf "ამ")))) 
                ,(utp "ულ" '((part-pfx -)
                              (part-sfx "ულ"))))
           ;; prefix m/ma
           (seq (or ,(utp-e '((sf -)
                              (part-pfx "მ")))
                    ,(utp-e '((sf -)
                              (part-pfx "მა")))) ;; gamajGari
                (or ,(utp "არ" '((part-sfx "არ")))
                    ,(utp "ალ" '((part-sfx "ალ")))))
           ;; prefix na
           (seq ,(utp-e `((part-pfx "ნა")))
                (or ,(utp-or '("ენ" "ებ" "ობ" "ავ" "ევ" "ამ")
                             `((sf ,morph)
                               (part-sfx -)))
                    ,(utp-e '((sf -)
                              (part-sfx -)))
                    ,(utp-or '("არ" "ალ" "ევ")
                             `((part-sfx ,morph)
                               (sf -)))))))
 :name 'past-participle-stem1)

;; future-participle-stem
(precompile-u-transducer
 `(seq ,(utp "სა" '((part-fut-pfx "სა")))
       (or ,(utp "მ" '((part-pfx "მ"))) ;; samqoPi
           ,(utp-e '((part-pfx -))))
       ,(utp "<" '())
       #-fst root #+fst future-participle-root
       ,(utp ">" '())
       (or ;; sfx -el
        (seq (or ,(utp-e '((sf -))) ;; root verbs (rare) or i-verbs
                 ,(utp-or '("მ" "ებ" "ობ")
                          `((sf ,morph)))
                 ,(utp "მ" `((sf "ამ")))
                 ,(utp "ვ" `((sf "ავ"))))
             (or ,(utp "ელ" '((part-sfx "ელ")
			      (stem-type c)
			      (sync-stem -)
			      (lang ng)))
                 ,(utp "ელ" '((part-sfx "ელ")
			      (stem-type c)
			      (lang og))) ;; syncope not oblig. in OG
                 ,(utp "ლ" '((part-sfx "ელ")
                              (stem-type c)
                              (sync-stem +)))))
        ;; no sfx
        (seq (or ,(utp-e '((sf -))) ;; root verbs or i-verbs
                 ,(utp-or '("ენ" "ევ" "ავ" "ებ" "ობ" "ამ")
                          `((sf ,morph))))
             ,(utp-e '((part-sfx -)
                       (stem-type c))))
        ;; sfx -al, -ar, -o
        (seq (or ,(utp-e '((sf -))) ;; root verbs (rare) or i-verbs
                 ,(utp-or '("მ" "ებ" "ობ")
                          `((sf ,morph)))
                 ,(utp "ა" '((sf "ავ")))) ;; saCekvao
             (or ,(utp-or '("ალ" "არ")
                          `((part-sfx ,morph)
                            (stem-type c)))
                 ,(utp "ო" `((part-sfx "ო")
                              (stem-type v)))
                 ,(utp "ე" `((part-sfx "ე") ;; sajile
                              (stem-type v)))))))
 :name 'future-participle-stem)

;; present-participle-stem
(precompile-u-transducer
 `(seq ,(utp-or '("მ" "მა" "მო" "მე") `((part-pfx ,morph)))
       (or ,(utp-e '((vv -))) ;; cf. Hewitt p. 430 for interpretation of this a as vv, take this out!
           ,(utp "ა" `((vv "ა"))))
       ,(utp "<" '())
       #-fst root #+fst present-participle-root
       ,(utp ">" '())
       (or ;; sfx -el
        (seq (or ,(utp-e '((sf -))) ;; root verbs (rare) or i-verbs
                 ,(utp-or '("მ" "ებ" "ობ")
                          `((sf ,morph)))
                 ,(utp "ვ" '((sf "ავ")))
                 ,(utp "მ" `((sf "ამ")))
		 ,(utp "ევ" `((sf "ევ"))))
             (or ,(utp "ელ" '((part-sfx "ელ")
			      (stem-type c)
			      (sync-stem -)
			      (lang ng)))
                 ,(utp "ელ" '((part-sfx "ელ")
			      (stem-type c)
			      (lang og))) ;; syncope not oblig. in OG
                 ,(utp "ლ" '((part-sfx "ელ")
                              (stem-type c)
                              (sync-stem +)))))
        ;; no sfx
        (seq (or ,(utp-e '((sf -))) ;; root verbs or i-verbs
                 ,(utp-or '("ენ" "ევ" "ავ" "ებ" "ობ")
                          `((sf ,morph))))
             ,(utp-e '((part-sfx -)
                       (stem-type c))))
        ;; sfx -al, -ar
        (seq (or ,(utp-e '((sf -))) ;; root verbs (rare) or i-verbs
                 ,(utp-or '("მ" "ებ" "ობ" "ევ")
                          `((sf ,morph))))
             (or ,(utp-or '("ალ" "არ")
                          `((part-sfx ,morph)
                            (stem-type c)))))
        ;; sfx -e
        (seq (or ,(utp-e '((sf -)))
                 ,(utp-or '("ავ")
                          `((sf ,morph))))
             ,(utp "ე" `((part-sfx "ე")
        
                         (stem-type v))))
        ;; sfx -iar-e
        (seq (or ,(utp-e '((sf -)))
                 ,(utp-or '("ობ")
                          `((sf ,morph))))
             ,(utp "იარე" `((part-sfx "იარე")
                         (stem-type v))))
        ;; sfx -ar-e
        (seq (or ,(utp-e '((sf -)))
		 ,(utp "ვ" '((sf "ავ") ;; "მშფოთვარე"
			     )))
             ,(utp "არე" `((part-sfx "არე")
                           (stem-type v))))))
 :name 'present-participle-stem)

;; negative-participle-stem
(precompile-u-transducer
 `(seq ,(utp "უ" '((part-pfx "უ")))
       ,(utp "<" '())
       #-fst root #+fst negative-participle-root
       ,(utp ">" '())
       (or ;; sfx -el
        (seq (or ,(utp-e '((sf -))) ;; root verbs (rare) or i-verbs
                 ,(utp-or '("მ" "ებ" "ობ" "ევ")
                          `((sf ,morph)))
                 ,(utp "ვ" `((sf "ავ")))
                 ,(utp "მ" `((sf "ამ"))))
             (or ,(utp "ელ" '((part-sfx "ელ")
			      (stem-type c)
			      (sync-stem -)
			      (lang ng)))
                 ,(utp "ელ" '((part-sfx "ელ")
			      (stem-type c)
			      (lang og))) ;; syncope not oblig. in OG
                 ,(utp "ლ" '((part-sfx "ელ")
			     (stem-type c)
			     (sync-stem +)))))
        ;; no sfx
        (seq (or ,(utp-e '((sf -))) ;; root verbs or i-verbs
                 ,(utp-or '("ენ" "ევ" "ავ" "ებ" "ობ")
                          `((sf ,morph))))
             ,(utp-e '((part-sfx -)
                       (stem-type c))))
        ;; sfx -al, -ar
        (seq (or ,(utp-e '((sf -))) ;; root verbs (rare) or i-verbs
                 ,(utp-or '("მ" "ებ" "ობ")
                          `((sf ,morph))))
             ,(utp-or '("ალ" "არ")
                      `((part-sfx ,morph)
                        (stem-type c))))
	;; sfx -o (is this really a neg.part. or the negative circumfix? (e.g. umojrao
        (seq (or ,(utp-e '((sf -))) ;; root verbs?
                 )
             ,(utp "ო" `((part-sfx "ო")
			 (stem-type v))))))
 :name 'negative-participle-stem)

;; add root etc. for fst!
;; absolute-passive-perfect-verb
(precompile-u-transducer
 `(seq (or ,(utp "ვ" '(((subj pers) 1)))
           ,(utp-e '(((subj pers) {2 3}))))
       (? ,(utp "ჰ" '((subj pers = {2 3}) ;;ჭავჭავაძე: ჰყოფილხარ; todo: exclude vowel roots
		(subnorm +)
		(lang ng))))
       past-participle-stem
       ,(utp-e '((relation absolute)
                 ((obj pers) 3)
                 ((obj num) sg)
                 (morph-type { passive stative-passive } )))
       (or (seq (or ,(utp "ვარ" '((pers-sfx "ვარ") ((subj pers) 1)
                                  (tense perfect)))
                    ,(utp "ხარ" '((pers-sfx "ხარ") ((subj pers) 2)
                                  (tense perfect)))
                    ,(utp "იყავი" '((pers-sfx "იყავი") ((subj pers) {1 2}) 
                                    (tense pluperfect)))
                    ,(utp "იყო" '((pers-sfx "იყო") ((subj pers) {1 2})
                                  (tense conj-perfect))))
                subj-pl-t?)
           ,(utp "ა" '((pers-sfx "ა")
                       ((subj pers) 3) ((subj num) sg)
                       (tense perfect)))
           ,(utp "ან" '((pers-sfx "ან")
                        ((subj pers) 3) ((subj num) pl) 
                        (tense perfect)))
           ,(utp "იყო" '((pers-sfx "იყო")
                        ((subj pers) 3) ((subj num) sg)
                        (tense pluperfect)))
           ,(utp "იყვნენ" '((pers-sfx "იყვნენ")
                            ((subj pers) 3) ((subj num) pl)
                            (tense pluperfect)))
	   ,(utp "იყვენ" '((pers-sfx "იყვნენ")
			   ((subj pers) 3) ((subj num) pl)
			   (tense pluperfect)
			   (subnorm = +)))
           ,(utp "იყოს" '((pers-sfx "იყოს")
                          ((subj pers) 3) ((subj num) sg)
                          (tense conj-perfect)))
           ,(utp "იყონ" '((pers-sfx "იყონ") 
                          ((subj pers) 3) ((subj num) pl)
                          (tense conj-perfect)))
           ,(utp "იყავ" '((pers-sfx "იყავი")
                          ((subj pers) {1 2})
                          ((subj num) sg)
                          (subnorm +)
                          (tense pluperfect)))
           ,(utp "იყვნენ" '((pers-sfx "იყვნენ")
                            ((subj pers) 3) ((subj num) pl)
                            (tense conj-perfect)
                            (style old)))
           ,(utp "იყვენით" '((pers-sfx "იყვენი")
                             ((subj pers) {1 2})
                             ((subj num) pl)
                             (tense pluperfect)
                             (style old)))
           ,(utp "იყვნეთ" '((pers-sfx "იყვნე")
                             ((subj pers) {1 2})
                             ((subj num) pl)
                             (tense conj-perfect)
                             (style old)))))
 :name 'absolute-passive-perfect-verb)

;; mival etc.

#+obsolete??
#[present-series-svla-suffix-group
  = (seq (or (seq [e ((subj pers = {1 2})
                      (root = {"ვალ" "ვAლ"})
		      (c-root "სვლ")
                      (tense = {present future}))]
                  subj-pl-t?)
	  ["ს" ((subj pers = 3) ;; "ვალს"
		(subj num = sg)
		(root = {"ვალ" "ვAლ"})
		(c-root "სვლ")
		(tense = present))]
	  ["ს" ((subj pers = 3) ;; "ვალს"
		(subj num = sg)
		(root = "ვAლ")
		(c-root "სვლ")
		(tense = future)
		;;(lang = og)
		)]
	  [e ((subj pers = 3)
	      (subj num = sg)
	      (root = "ვა")
	      (tense = {present future}))]
	  ["ენ" ((subj pers = 3)
		 (subj num = pl)
		 (root = {"ვლ" "ვAლ"})
		 (c-root "სვლ")
		 (tense = {present future}))]
	  ["ენან" ((subj pers = 3)
		   (subj num = pl)
		   (root = "ვAლ")
		   (c-root "სვლ")
		   (tense = {present future})
		   (lang = og))]
	  ["ნ" ((subj pers = 3)
		(subj num = sg)
		(root = "ვAლ")
		(tense = iter-present))]
	  ["ედ5" ((subj pers = 3)
		 (subj num = pl)
		 (root = "ვAლ")
		 (tense iter-present))]
	  ["ენედ" ((subj pers = 3)
		   (subj num = pl)
		   (root = "ვAლ")
		   (tense iter-present))])
     [e ((morph-type passive))])]

#+obsolete
#[aorist-series/conditional-svla-suffix-group
  = (seq (or (seq ["ი" ((subj pers = {1 2})
                        (root = "ვედ")
                        (tense = aorist))]
                  subj-pl-t?)
	  [e ((subj pers = {1 2}) ;; წარვედ
	      (root = "ვედ")
	      (tense = aorist)
	      (subj num = sg)
	      (lang = og))]
	  [e ((subj pers = 2) ;; წარვედ
	      (root = "ვედ")
	      (tense = imperative-aorist)
	      (subj num = sg)
	      (lang = og))]
	  (seq ["ი" ((subj pers = 2)
		     (num pl)
		     (root = "ვედ")
		     (tense = imperative-aorist))]
	       subj-pl-t?)
	  ["ინ" ((subj pers = 3) ;; წარვედინ
		 (subj num = sg)
		 (root = "ვედ")
		 (tense = imperative-aorist)
		 (lang = og))]
	  ["ედ" ((subj pers = 3) ;; წარვედინ
		 (subj num = pl)
		 (root = "ვიდ")
		 (tense = imperative-aorist)
		 (lang = og))]
	  (seq ["ე" ((subj pers = {1 2})
		     (root = "ვიდ")
		     (tense = optative))]
	       subj-pl-t?)
	  ["ა" ((subj pers = 3)
		(subj num = sg)
		(root = "ვიდ")
		(tense = aorist))]
	  ["ეს" ((subj pers = 3)
		 (subj num = sg)
		 (root = "ვიდ")
		 (tense = optative))]
	  ["ნენ" ((subj pers = 3)
		  (subj num = pl)
		  (root = "ვიდ")
		  (tense = {aorist optative}))]
	  ["ენ" ((subj pers = 3)
		 (subj num = pl)
		 (root = "ვიდ")
		 (subnorm +)
		 (tense = {aorist optative}))]
	  
	  (seq ["ი" ((subj pers = {1 2})
		     ;;(subj num = sg)
		     (root = "ვიდ")
		     (tense = iter-aorist))]
	       subj-pl-t?)
	  ["ის" ((subj pers = 3)
		 (subj num = sg)
		 (root = "ვიდ")
		 (tense = iter-aorist))]
	  ["იან" ((subj pers = 3)
		  (subj num = pl)
		  (root = "ვიდ")
		  (tense = iter-aorist))]
	  
	  (seq ["ოდ" ((root = "ვიდ")
		      (pr-st-ext "ოდ")
		      (tense = {conditional conj-future iter-present
					    iter-imperfect imperative-present}))]
	       ps-od-pers-sfx))
         ;;#+fst root
         [e ((morph-type passive))])] ;; ??

;; auxiliary verb

#+obsolete?
#[aux-present
  = (or ["არის" (((subj pers) 3)
                 ((subj num) sg)
                 (tense present))] 
        ["არიან" (((subj pers) 3)
                  ((subj num) pl)
                  (tense present))]
        (seq (? (or ["ვარ" (((subj pers) 1)
                            (tense present))]
                    ["ხარ" (((subj pers) 2)
                            (tense present))]))
             subj-pl-t?))]

#+obsolete?
#[aux-non-present
  = (or (seq (or subj-1 [e (((subj pers) 2))])
	 (or ["იქნები" ((tense future))]
	     ["იყავი" ((tense {aorist imperfect}))]
	     [("იყო" "იყვე") ((tense {optative conj-present}))]
	     ["იქნე" ((tense {optative-future conj-future}))]
	     ["ყოფილიყავი" ((tense pluperfect))]
	     ["ყოფილიყო" ((tense conj-perfect))])
	 subj-pl-t?)
     ["ვყოფილვარ" ((tense perfect)
		   ((subj pers) 1))]
     ["ყოფილხარ" ((tense perfect)
		  ((subj pers) 2))]
     ;; 3. person
     ["იქნება" (((subj num) sg)
		((subj pers) 3)
		(tense future))]
     ["იქნებიან" (((subj num) pl)
		  ((subj pers) 3)
		  (tense future))]
     ["იქნეს" (((subj pers) 3) ;; what about pl?
	       (tense {optative-future conj-future}))]
     ["იყო" ((tense {aorist imperfect})
	     ((subj pers) 3)
	     ((subj num) sg))]
     [("იყოს" "იყვეს") ((tense optative)
			((subj pers) 3)
			((subj num) sg))]
     ["იყვნენ" ((screeve-sign "ნენ")
		(tense {aorist imperfect optative conj-present})
		((subj pers) 3)
		((subj num) pl))]
     ["იყვენ" ((screeve-sign "ნენ")
	       (tense {aorist imperfect optative conj-present})
	       ((subj pers) 3)
	       ((subj num) pl)
	       (subnorm +))]
     ["იყონ" ((tense {optative conj-present})
	      ((subj pers) 3)
	      ((subj num) pl))]
     ["ყოფილა" ((tense perfect)
		((subj pers) 3)
		((subj num) sg))]
     ["ყოფილან" ((tense perfect)
		 ((subj pers) 3)
		 ((subj num) pl))]
     ["ყოფილიყო" ((tense pluperfect)
		  ((subj pers) 3)
		  ((subj num) sg))]
     ["ყოფილიყვნენ" ((tense pluperfect)
		     ((subj pers) 3)
		     ((subj num) pl))]
     ["ყოფილიყოს" ((tense conj-perfect)
		   ((subj pers) 3)
		   ((subj num) sg))]
     ["ყოფილიყონ" ((tense conj-perfect)
		   ((subj pers) 3)
		   ((subj num) pl))])]

#+obsolete?
#[auxiliary-verb
  = (seq (or aux-present aux-non-present)
         [e ((cat v)
             (subcat aux)
             (c-root "ყოფნ1"))])]


;;,(utp-e '((morph-type causative)))

(precompile-u-transducer
 `(seq (or ,(utp-e '((part-pfx -)))
           ,(utp "სი" '((part-pfx "სი")))
           ,(utp "მ" '((part-pfx "მ")))) ; msgavseba
       ,(utp "<" '())
       #-fst root #+fst masdar-root
       ,(utp ">" '())
       (or (seq ,(utp-e '((morph-type non-causative)))
                (or ,(utp-e '((sf -))) ;; root verbs or i-verbs
                    ,(utp-or '("ენ" "ევ" #+ignore"ავ" "ებ" "ობ")
                             `((sf ,morph)))
                    ,(utp "ვ" `((sf "ავ")))
                    ,(utp "მ" `((sf "ამ"))))
                ,(utp "ა" '((part-pfx {- "მ"})
                            (part-sfx -)
                            (stem-type v))))
           (seq ,(utp-e '((morph-type causative)))
                (or
                 ;; -ineb 
                 (seq (or ,(utp-e '((sf -)))
                          ,(utp-or '("ევ" "ვ" "ემ" "მ" "ებ" "ობ")
                                   `((sf ,morph))))
                      ,(utp "ინებ" '((caus-sf "ინ"))))
                 ;; -mev
                 (seq ,(utp "მ" `((sf "მ"))) ;; e.g. vasmev
                      ,(utp "ევ" '(((type root-vowel) -)
                                   (caus-sf "ევ"))))
                 ;; -evineb
                 (seq (or ,(utp-e   '((sf -))) ; ??
                          ,(utp "ვ" `((sf "ვ")))
                          ,(utp "მ" `((sf "მ"))))
                      ,(utp "ევინებ" '((caus-sf "ევინ")))))
                ,(utp "ა" '((part-pfx -)
                            (part-sfx -)
                            (stem-type v))))
           (seq ,(utp-e '((morph-type non-causative)))
                ,(utp-or '("ილ" "ულ") `((part-sfx ,morph) ;; tirili
                                         (sf -)
                                         (stem-type c))))
           ,(utp-e '((part-pfx -)
                     (part-sfx -)
                     (sf -)
                     (stem-type c)))))
 :name 'masdar)

;; tmesis; deprecated, treated programmatically because of complexity
#+ignore
#[og-infix
  = (seq (or 
	  [("-რაჲმე-") ((infix-cat pron+indet+inanim)
			(infix-lex "რაჲმე"))]
	  [("-ვინმე-") ((infix-cat pron+indet+anim)
			(infix-lex "ვინმე"))]
	  [("-მცა-") ((infix-cat cj)
		      (infix-lex "მცა"))]
	  [("-ხოლო-") ((infix-cat cj)
		       (infix-lex "ხოლო"))]
	  )
     (utp-not `((pv -)))
     [e ((lang = og))])]

#-fst
#[finite-verb
  = (seq preverb-group
     ;;#+fst(? og-infix)
     [e ((finite +))]
     ;; #-fst(? ["Ä"]) ;; for Tschenkeli parsing
     (or (seq prefix-group
	      ["<"] ;; root start marker
	      (or (seq root
		       [">"] ;; root end marker
		       (or [e ((reduplication -))]
			   ;; todo: cleanup
			   (seq (or (utp "ევ" `((sf "ევ")))
				    (seq (utp "ი" `((sf {"ი"})
						    (root "ვლ")
						    (tense {future iter-present conditional conj-future})))
					 (utp-not '((root "ვლ"))))
				    (utp-e `((sf {"ი"}) ;; more general??
					     (root "ვლ") ;; ??
					     (tense {future iter-present conditional conj-future})))
				    (utp-e `((sf {"ი"})
					     (tense {aorist optative pluperfect conj-perfect})))
				    (utp-not '((sf {"ი"}))))
				(seq (or (seq (utp-e '(((type subj12-sfx) "ვარ/ხარ")))
					      present-series-active-suffix-group)
					 (utp-not '(((type subj12-sfx) "ვარ/ხარ")))))
				reduplication-prefix-group
				["<"]
				reduplication-root
				[">"]
				))
		       (or present-series-active-suffix-group
			   present-series-causative-suffix-group
			   present-series-passive-suffix-group
			   aorist-series-suffix-group
			   perfect-suffix-group
			   pluperfect-suffix-group
			   relative-passive-perfect-suffix-group
			   stative-passive-present-suffix-group))))
      absolute-passive-perfect-verb)
     [e ((cat v))])]

;; reduplication first segment
#+fst
#[reduplication
  = (seq
     ;; (utp-not '((pv = "-"))) ;; there are reduplications without pv, but differing roots
     (or
      (utp "ევ" `((sf "ევ")))
      (seq (utp "ი" `((sf {"ი"})
		      (root "ვლ")
		      (tense {future iter-present conditional conj-future})))
	   (utp-not '((root "ვლ")))) ;, ???
      (utp-e `((sf {"ი"})  ;; more general??
	       (root "ვლ") ;; ??
	       (tense {future iter-present conditional conj-future})))
      (utp-e `((sf {"ი"})
	       (tense {aorist optative pluperfect conj-perfect})))
      (utp-not '((sf {"ი"}))))
     (seq (or (seq (utp-e '(((type subj12-sfx) "ვარ/ხარ")))
		   present-series-active-suffix-group)
	      (utp-not '(((type subj12-sfx) "ვარ/ხარ")))))
     ["-" ((redup = "+"))])]

#+fst
#[finite-verb
  = (seq preverb-group
     ;;#+fst(? og-infix)
     [e ((finite +))]
     (or (seq prefix-group
	      ["<"] ;; root start marker
	      (seq root
		   [">"] ;; root end marker
		   (or reduplication ;; first part of reduplicated verb only
		       present-series-active-suffix-group
		       present-series-causative-suffix-group
		       present-series-passive-suffix-group
		       aorist-series-suffix-group
		       perfect-suffix-group
		       pluperfect-suffix-group
		       relative-passive-perfect-suffix-group
		       stative-passive-present-suffix-group)))
      absolute-passive-perfect-verb) ;; fixme: reduplication missing
     [e ((cat v))])]

#[participle
  = (or (seq preverb-group
             (or (seq [e ((finite -)
                          (stem-type c)
                          (tense past-part)
                          (subcat past-part))]
                      past-participle-stem1)
                 (seq [e ((finite -)
                          (tense future-part)
                          (subcat future-part))]
                      future-participle-stem)
                 (seq [e ((finite -)
                          (tense present-part)
                          (subcat present-part))]
                      present-participle-stem)
                 (seq [e ((finite -)
                          (tense negative-part)
                          (subcat negative-part))]
                      negative-participle-stem)
                 (seq [e ((finite -)
                          (tense masdar)
                          (case-type full) ;; ??
                          (subcat masdar))]
                      masdar))
             (or ["ი" ((cat v)
                       (stem-type c)
                       (parsing -))]
                 [e ((cat v)
                     (stem-type v)
                     ;;(tense {future-part present-part past-part negative-part masdar})
                     (parsing -))]
                 (seq [e ((parsing +))]
                      (or nom-group erg-group ;; contains adjectives
                          (seq [e ((case-type full))]
                               (or dat-group gen-group inst-group adv-group
                                   old-plural)))))))]

#-fst
(defparameter *verb-fst*
  (compile-u-transducer '(seq finite-verb) :dfa-class 'georgian-dfa))

;; used in paradigms.lisp
#-fst
(defparameter *full-verb-fst*
  (compile-u-transducer '(seq (or finite-verb participle)) :dfa-class 'georgian-dfa))

#-fst
(defparameter *fst*
  (compile-u-transducer
   `(seq (or noun
             finite-verb
             participle
             pronoun
             determiner
             postposition
             adverb-conjunction-interjection)
         (? indirect-speech))
   :dfa-class 'georgian-dfa))

; caval

;(fsa::fsa-print *fst*)

(defun remove-root-number (root)
  (if (find (char root (1- (length root))) "1234567¸¹°")
    (subseq root 0 (1- (length root)))
    root))

(defmethod attribute-list ((dfa extended-dfa))
  (let ((attributes ()))
    (relation-map
     (lambda (state relation)
       (declare (ignore state))
       (relation-map
        (lambda (symbol dest)
          (declare (ignore symbol))
          ;;(print (list symbol dest))
          (dolist (trans dest)
            (labels ((get-path (dg path)
                       (if (atom (cdr dg))
                         (pushnew (reverse path) attributes :test #'equal)
                         (dolist (sub-dg (cdr dg))
                           (assert (null (cddr sub-dg)))
                           (get-path (cadr sub-dg) (cons (car sub-dg) path))))))
              (get-path (cadr trans) ()))))
        relation))
     (fsa-delta dfa))
    attributes))

(defmethod attribute-values-list ((dfa extended-dfa))
  (let ((attribute-table (make-hash-table :test #'equal)))
    (relation-map
     (lambda (state relation)
       (declare (ignore state))
       (relation-map
        (lambda (symbol dest)
          (declare (ignore symbol))
          ;;(print (list symbol dest))
          (dolist (trans dest)
            (labels ((get-path (dg path)
                       (cond ((parser::extended-list-p (cdr dg))
                              (dolist (val (parser::extended-list-form (cdr dg)))
                                (pushnew val (gethash (reverse path) attribute-table) :test #'equal)))
                             ((atom (cdr dg))
                              (pushnew (cdr dg) (gethash (reverse path) attribute-table) :test #'equal))
                             (t
                              (dolist (sub-dg (cdr dg))
                                (assert (null (cddr sub-dg)))
                                (get-path (cadr sub-dg) (cons (car sub-dg) path)))))))
              (get-path (cadr trans) ()))))
        relation))
     (fsa-delta dfa))
    (collecting
      (maphash (lambda (path values)
                 (collect (append path values)))
               attribute-table))))

:eof
