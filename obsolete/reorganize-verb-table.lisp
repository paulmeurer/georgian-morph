;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10 -*-

;; paul.meurer@aksis.uib.no
;; Aksis, Unifob, University of Bergen.
;; http://www.uni.no/

;; bugs: დავსდუდუნე, მივფანტ-მოვთანტავm დავიფრთხიალებ… (perf)

(in-package :fst)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf clsql-sys::*original-readtable* nil)
  (enable-sql-reader-syntax))

(with-database-connection ()
  (dolist (row (select [count-distinct [vn]] [c-root] [id] [sub-id]
		       :from [morph verb-features]
		       :distinct t
		       :group-by '([id] [sub-id] [c-root])
		       :order-by '([id] [sub-id]))
		       )
    (when (> (car row) 1)
      (print row))
    ))

(with-database-connection ()
  (let ((vn-list ()))
    (dolist (row (select [count-distinct [vn]] [c-root] [id] [sub-id]
			 :from [morph verb-features]
			 :distinct t
			 :group-by '([id] [sub-id] [c-root])
			 :order-by '([id] [sub-id])))
      (when (> (car row) 1)
	(destructuring-bind (count c-root id sub-id) row
	  (pushnew (list* c-root sub-id
			 (remove-duplicates
			  (mapcar (lambda (vn) (string-trim "[]" vn))
				  (select [vn]
					  :from [morph verb-features]
					  :distinct t :flatp t
					  :where [and [= [id] ?id] [= [sub-id] ?sub-id]]
					  :order-by [vn]))
			  :test #'string=))
		   vn-list
		   :test #'equal))))
    (dolist (root+vns vn-list)
      (when (cdddr root+vns)
	(print root+vns)))
    ))

(("ამო" "აღმო" NIL)
 ("გა" "გამო")
 ("ა" "ცხად" NIL)
 ("გად" "გადმო" NIL)
 ("შე" "შემო")
 ("მი" "მიმო" NIL)
 ("გად" "გადა" NIL)
 ("გა" "გან")
 ("მი" "მიმო")
 ("მი" "წა")
 ("გამო" "და")
 ("მი" "მო")
 ("და" "დამო" NIL)
 ("წა" "წამო" NIL)
 ("ჩა" "ჩამო" NIL)
 ("ა" "ამო" NIL)
 ("შე" "შემო" NIL)
 ("გა" "გამო" NIL)
 ("მი" "მო" NIL))



(2 "ნარცხ" 1604 7) 
(2 "ნარცხ" 1604 9) 
(2 "ნარცხ" 1604 6) 
(2 "ქონ1" 2565 2) 
(2 "ნარცხ" 1604 8) 
(2 "ნარცხ" 1604 19) 
(2 "მბობ" 1266 43) 
(2 "კითხულ" 961 19) 
(3 "ცოდნ" 3117 1) 

(defun redup-pv (pv red)
  (if (or (null red) (equal red "მო")) 
      (cond ((equal pv "მი")
	     "მო")
	    ((equal pv "გადა")
	     "გადმო")
	    ((equal pv "გარდა")
	     "გარდმო")	
	    (t
	     (concat pv "მო")))
      red))

(defun insert-paradigm (id sub-id)
  (macrolet ((add0 (x y) `(progn (when (and ,y ,x (string/= ,x ,y))
				   (print (list :different ,x ,y)))
				 (when ,x (setf ,y ,x))))
	     (add  (x y) `(pushnew ,x ,y :test #'string=))
	     (add1 (x y) `(when ,x (pushnew ,x ,y :test #'string=))))
    (let (c-root vn tsch-class pf-pv impf-pv red-dir-pv)
      (do-query ((%c-root %vn %tsch-class %pv %red-dir-pv %reduplication)
		 [select [c-root] [vn] [tsch-class]
			 [pv] [red-dir-pv] [reduplication]
			 :from [morph verb-features]
			 :distinct t
			 :where [and [= [id] ?id] [= [sub-id] ?sub-id]
				     [or [like [tense] "%|present|%"]
					 [like [tense] "%|imprefect|%"]
					 [like [tense] "%|conj-present|%"]]]
			 :order-by [vn]])
	(add0 %c-root c-root)
	(add1 %vn vn)
	(add0 %tsch-class tsch-class)
	(add %pv impf-pv)
	(add0 (when %reduplication (redup-pv %pv %red-dir-pv)) red-dir-pv))
      (do-query ((%c-root %vn %tsch-class %pv %red-dir-pv %reduplication)
		 [select [c-root] [vn] [tsch-class]
			 [pv] [red-dir-pv] [reduplication]
			 :from [morph verb-features]
			 :distinct t
			 :where [and [= [id] ?id] [= [sub-id] ?sub-id]
				     [or [like [tense] "%|future|%"]
					 [like [tense] "%|conditional|%"]
					 [like [tense] "%|conj-future|%"]
					 [like [tense] "%|aorist|%"]
					 [like [tense] "%|optative|%"]
					 [like [tense] "%|perfect|%"]
					 [like [tense] "%|conj-perfect|%"]
					 ]]
			 :order-by [vn]])
	(add0 %c-root c-root)
	(add1 %vn vn)
	(add0 %tsch-class tsch-class)
	(add %pv pf-pv)
	(add0 (when %reduplication (redup-pv %pv %red-dir-pv)) red-dir-pv))
      (dolist (pf (or pf-pv (list nil)))
	(dolist (impf (or impf-pv (list nil)))
	  (make-instance 'verb-paradigm
			 :id id
			 :sub-id sub-id
			 :c-root c-root
			 :vn (car vn)
			 :tsch-class tsch-class
			 :pf-pv (or pf "-")
			 :impf-pv (or impf "-")
			 :red-dir-pv red-dir-pv
			 :date (get-universal-time))))
      (print (list id sub-id c-root vn tsch-class pf-pv impf-pv :red red-dir-pv)))))

#+main
(let ((id+subid-list (select [id] [sub-id]
			     :distinct t
			     :from [morph verb-features]
			     :order-by '([id] [sub-id]))))
  (loop for (id sub-id) in id+subid-list
       do (insert-paradigm id sub-id)))

#+main
(let ((rows (select [id] [sub-id] [link-sub-id] [base-sub-id] [participle-sub-id] [pv] [derived-type]
		    :from [morph verb-translation]
		    :distinct t)))
  (loop for (id sub-id link-sub-id base-sub-id participle-sub-id pv derived-type)
     in rows
     do (update-records [morph verb-paradigm]
			:attributes '([link-sub-id] [base-sub-id] [participle-sub-id] [pv] [derived-type])
			:values (list link-sub-id base-sub-id participle-sub-id (or pv "-") derived-type)
			:where [and [= [id] ?id] [= [sub-id] ?sub-id]])))

#+test
(update-records [morph verb-paradigm]
		:av-pairs '(([vn] "გორება"))
		:where [and [= [id] 392]
			    [= [vn] "გორვა (|| გორაობა)"]
			    [<> [tsch-class] "KT"]])

#+main
(let ((rows (select [verb-paradigm id]
		    [verb-paradigm sub-id]
		    [verb-features caus-sf]
		    [verb-paradigm vn]
		    :from [morph verb-paradigm]
		    :left-join [morph verb-features]
		    :on [and [= [verb-paradigm id] [verb-features id]]
			     [= [verb-paradigm sub-id] [verb-features sub-id]]]
		    :where [and ;; [like [verb-paradigm vn] "%ვრა-ქთ"]
				[like [verb-paradigm vn] "%-ქთ"]
				[like [verb-features tense] "%|aorist|%"]]
		    :distinct t)))
  (loop for (id sub-id caus-sf vn)
     in rows
     do #+test(update-records [morph verb-paradigm]
			:attributes '([vn])
			:values (list (concat (subseq vn 0 (- (length vn) 4))
					      (or caus-sf "ინ")
					      "ება"))
			:where [and [= [id] ?id] [= [sub-id] ?sub-id]])
       (print (list id sub-id caus-sf vn
		    (concat (subseq vn 0 (- (length vn) 4))
			    (or caus-sf "ინ")
			    "ება")))))

#+main
(let ((rows (select [verb-paradigm id]
		    [verb-paradigm sub-id]
		    [verb-features caus-sf]
		    [verb-paradigm vn]
		    :from [morph verb-paradigm]
		    :left-join [morph verb-features]
		    :on [and [= [verb-paradigm id] [verb-features id]]
			     [= [verb-paradigm sub-id] [verb-features sub-id]]]
		    :where [and ;; [like [verb-paradigm vn] "%ვრა-ქთ"]
			    [like [verb-paradigm vn] "%-ქთ"]
			    [like [verb-features tense] "%|aorist|%"]]
		    :distinct t)))
  (setf rows (sort rows #'< :key #'car))
  (loop for (id sub-id caus-sf vn)
     in rows
     for kt = (let ((vn (delete #\( (delete #\) (delete #\[ (delete #\] vn))))))
		(concat (subseq vn 0 (- (length vn) 4))
			(or caus-sf "ინ")
			"ება"))
     do (print (list id sub-id caus-sf vn kt))
       #+test(update-records [morph verb-paradigm]
		       :attributes '([vn])
		     :values (list kt)
		     :where [and [= [id] ?id] [= [sub-id] ?sub-id]])))


#+test
(dolist (row (select [id] [sub-id] [c-root]
		     [vn]
		     :from [morph verb-paradigm]
		     :where [like [vn] "%-%"]
		     :order-by '([id] [sub-id])
		     :distinct t))
  (destructuring-bind (id sub-id c-root vn) row
    (print (list id sub-id c-root vn))
    #+test
    (update-records [morph verb-paradigm]
		    :attributes '([vn])
		    :values (list 
			     (if (or (null vn-b-count)
				     (and vn-a-count (>= vn-a-count vn-b-count)))
				 vn-a
				 vn-b))
		    :where [and [= [id] ?id] [= [sub-id] ?sub-id]])
    ))

#+once
(prognxx
  (defparameter *mi-mo-12-table* (make-hash-table :test #'equal))
  (defparameter *mi-mo-3-table* (make-hash-table :test #'equal))

  (do-query ((c-root class id sub-id pv)
	     [select [c-root] [tsch-class] [id] [sub-id] [pv]
		     :from [morph verb-features]
		     :where [and [or [= [obj-pers] "1|2"] [= [subj-pers] "1|2"]]
				 [or [like [tense] "%|future|%"]
				     ;;[like [tense] "%|aorist|%"]
				     ]]
		     :order-by '([id] [sub-id])
		     :distinct t])
    (push pv (gethash (list c-root class id sub-id) *mi-mo-12-table*)))
  (do-query ((c-root class id sub-id pv)
	     [select [c-root] [tsch-class][id] [sub-id] [pv]
		     :from [morph verb-features]
		     :where [and [or [= [obj-pers] "3"] [= [subj-pers] "3"]]
				 [or [like [tense] "%|future|%"]
				     ;;[like [tense] "%|aorist|%"]
				     ]]
		     :order-by '([id] [sub-id])
		     :distinct t])
    (push pv (gethash (list c-root class id sub-id) *mi-mo-3-table*)))

  (maphash (lambda (id-subid 12-pvs)
	     (let ((3-pvs (gethash id-subid *mi-mo-3-table*)))
	       (unless (equal 3-pvs 12-pvs)
		 (print (list id-subid (car 3-pvs) (car 12-pvs))))))
	   *mi-mo-12-table*))

#+once
(with-transactionxx ()
  (loop for ((c-root class id sub-id) 3-pv 12-pv)
     in '((("ნდ1" "RP1" 1633 14) "მი" "მო") 
	  (("შველ" "T5" 2850 5) "მი" "მო") 
	  (("ძალ" "RP1" 3171 10) "მი" "მო") 
	  (("ახლ2" "RP1" 70 1) "მი" "მო") 
	  (("ვლ2" "T5" 591 22) "წა" "წამო") 
	  (("ქნევ2" "T3" 2559 67) "გადა" "გადმო") 
	  (("ყოფ1" "T3" 2793 100) "გადა" "გადმო") 
	  (("ხლ2" "T4" 3648 13) "მი" "მო") 
	  (("ცემ" "T5" 3068 18) "მი" "მო") 
	  (("შვერ1" "T4" 2852 31) "მი" "მო") 
	  (("ფურთხ" "T4 (nur mit i.O.)" 2427 13) "მი" "მო") 
	  (("ზელ" "T4" 624 17) "მი" "მო") 
	  (("ლოცულ" "T3" 1187 11) "მი" "მო") 
	  (("სალმ" "RP1" 1964 4) "მი" "მო") 
	  (("გზავნ" "RP1 (OR)" 362 35) "ა" "ამო") 
	  (("შველ" "RP1" 2850 14) "ა" "ამო") 
	  (("ფიც" "T5" 2371 8) "შე" "შემო") 
	  (("ზღ" "RP1 (OR)" 675 6) "მი" "მო") 
	  (("გებ" "T5" 339 3) "მი" "მო") 
	  (("გირავ" "T5" 369 5) "მი" "მო") 
	  (("დგომ" "RP5" 457 61) "მი" "მო") 
	  (("ძლევ1" "T5" 3209 3) "შე" "შემო") 
	  (("წყევლ" "T4" 3392 3) "მი" "მო") 
	  (("ყენ" "T4" 2745 38) "მი" "მო") 
	  (("ბარ1" "T5" 101 10) "მი" "მო") 
	  (("სვენ" "RP1" 2031 33) "მი" "მო") 
	  (("შველ" "RP1" 2850 22) "შე" "შემო") 
	  (("ყივილ" "RM2" 2765 20) "შე" "შემო") 
	  (("ყვირილ" "T5" 2760 29) "მი" "მო") 
	  (("ხვეწნ" "RP1" 3619 6) "შე" "შემო") 
	  (("კუთვნ" "T5" 1069 5) "მი" "მო") 
	  (("ხტომ" "RP6" 3718 34) "შე" "შემო") 
	  (("ლოცულ" "RP1 (OR)" 1187 15) "გადა" "გადმო") 
	  (("ჩეჩ2" "T5" 2950 3) "შე" "შემო") 
	  (("ძღვნ" "T3" 3233 3) "მი" "მო") 
	  (("ჩემ" "T5" 2943 9) "მი" "მო") 
	  (("წვდენ" "T3" 3268 16) "გა" "გამო") 
	  (("ნათ" "T5" 1578 28) "მი" "მო") 
	  (("დ" "T4" 432 26) "მი" "მო") 
	  (("ბნევ1" "T5" 194 19) "მი" "მო") 
	  (("თხოვნ" "T5" 825 23) "მი" "მო") 
	  (("ბოლ" "T5" 204 14) "მი" "მო") 
	  (("ჩერ" "RP4" 2947 21) "მი" "მო") 
	  (("ბრალ" "RP2 (OR)" 226 9) "გადა" "გადმო") 
	  (("პყარ" "T3" 1792 13) "მი" "მო") 
	  (("ჯდომ" "RP7" 3781 39) "და" "დამო") 
	  (("პყარ" "T5" 1792 15) "მი" "მო") 
	  (("ლაქუც" "RM4" 1130 3) "მი" "მო") 
	  (("კითხულ" "RM3" 961 17) "მი" "მო") 
	  (("ჩერ" "RP4" 2947 23) "შე" "შემო") 
	  (("ცვივნ" "RP3" 3080 49) "გადა" "გადმო") 
	  (("ქვავ" "T4" 2522 6) "მი" "მო") 
	  (("გზავნ" "T3" 362 24) "მი" "მო") 
	  (("ზღ" "T3" 675 3) "მი" "მო") 
	  (("ჩენ" "T3" 2944 27) "შე" "შემო") 
	  (("სამძიმრ" "T3 (nur mit i.O.)" 1970 1) "მი" "მო") 
	  (("ბაძ" "T5 (nur mit i.O.)" 121 1) "მი" "მო") 
	  (("ყრდენ" "RP1" 2803 11) "მი" "მო") 
	  (("გ" "T3" 338 21) "მი" "მო") 
	  (("ყრ" "T4" 2801 55) "მი" "მო") 
	  (("ბნევ2" "T5" 195 13) "მი" "მო") 
	  (("ვაჭრ" "RP1" 574 9) "შე" "შემო") 
	  (("სახლ" "RP3" 2012 24) "შე" "შემო") 
	  (("ალერს" "RM2" 29 3) "მი" "მო") 
	  (("კედლ" "RP1" 903 4) "შე" "შემო") 
	  (("შტერ" "T4" 2895 9) "შე" "შემო") 
	  (("ყრ" "T5" 2801 67) "შე" "შემო") 
	  (("ცვივნ" "RP3" 3080 45) "ა" "ამო") 
	  (("ხმიან" "RM4" 3664 5) "ჩა" "ჩამო") 
	  (("ყენ" "T3" 2745 67) NIL "წამო") 
	  (("ტმასნ" "RP1" 2191 2) "მი" "მო") 
	  (("ჭყეტ" "T4" 3509 14) "მი" "მო") 
	  (("კერძ" "T5" 921 1) "მი" "მო") 
	  (("ცემ" "KT" 3068 29) "მი" "მო") 
	  (("შვერ1" "T3" 2852 21) "ა" "ამო") 
	  (("ჩეჩ2" "T5" 2950 1) "მი" "მო") 
	  (("შველ" "T5" 2850 10) "ჩა" "ჩამო") 
	  (("ტირ" "T3" 2164 24) "მი" "მო") 
	  (("წყ3" "T3" 3387 8) "მი" "მო") 
	  (("კონ" "RP1" 1005 9) "მი" "მო") 
	  (("ჩრ" "T4" 2994 11) "შე" "შემო") 
	  (("ჭყეტ" "T4" 3509 16) "შე" "შემო") 
	  (("ძლევ1" "RP1 (OR)" 3209 13) "შე" "შემო") 
	  (("ბერ2" "RP4" 150 8) "შე" "შემო") 
	  (("წვდენ" "T3" 3268 59) "გა" "გამო") 
	  (("ცქერ" "RP4" 3146 25) "შე" "შემო") 
	  (("ბედ1" "T5" 135 8) "შე" "შემო") 
	  (("ცვივნ" "RP3" 3080 51) "მი" "მო") 
	  (("თვლ" "T3" 771 22) "შე" "შემო") 
	  (("ყენ" "T3" 2745 69) NIL "წარმო") 
	  (("შვერ1" "T3" 2852 23) "გა" "გამო") 
	  (("შტერ" "T4" 2895 7) "მი" "მო") 
	  (("ხტომ" "RP6" 3718 32) "მი" "მო") 
	  (("კედლ" "RP1" 903 2) "მი" "მო") 
	  (("ხმარ" "RP1" 3660 19) "წა" "წამო") 
	  (("ფეთ" "T5" 2336 3) "შე" "შემო") 
	  (("დგომ" "RP7 (OR)" 457 59) "მი" "მო") 
	  (("ხტომ" "RP6" 3718 36) "ჩა" "ჩამო") 
	  (("ჩრ" "T5" 2994 15) "შე" "შემო") 
	  (("ქნევ2" "T3" 2559 35) "გა" "გამო") 
	  (("ზიარ" "T5" 637 7) "მი" "მო") 
	  (("ბრუნ" "RP3" 243 52) "შე" "შემო") 
	  (("ზომ" "T3" 658 15) "მი" "მო") 
	  (("წერ" "T5" 3260 49) "მი" "მო") 
	  (("ნდ1" "T5" 1633 8) "მი" "მო") 
	  (("ცვივნ" "RP3" 3080 55) "ჩა" "ჩამო") 
	  (("ჩერ" "T4" 2947 8) "მი" "მო") 
	  (("ცივ" "RP4" 3089 19) "მი" "მო") 
	  (("ბრუნ" "T3" 243 27) "გა" "გამო") 
	  (("ვარდნ" "RP5 (OR)" 566 21) "მი" "მო") 
	  (("ხტომ" "RP7" 3718 46) "მი" "მო") 
	  (("სისიან" "T3" 2050 1) "მი" "მო") 
	  (("ნიჭ" "RP1 (OR)" 1664 4) "მი" "მო") 
	  (("გნ" "T5" 384 6) "მი" "მო") 
	  (("ბრუნ" "RP3" 243 50) "მი" "მო") 
	  (("წევ1" "T5" 3258 131) "შე" "შემო") 
	  (("ნათ" "T3" 1578 22) "მი" "მო") 
	  (("ცივ" "T4" 3089 17) "მი" "მო") 
	  (("შავ" "T4" 2828 10) "მი" "მო") 
	  (("ძახ1" "T5" 3179 55) "მი" "მო") 
	  (("ლაქუც" "T3 (nur mit i.O.)" 1130 7) "მი" "მო") 
	  (("დგომ" "RP6" 457 34) "მი" "მო") 
	  (("ძღვნ" "T5" 3233 7) "შე" "შემო") 
	  (("ხედ" "T5" 3581 46) "ჩა" "ჩამო") 
	  (("ზომ" "T4" 658 25) "მი" "მო") 
	  (("ხლ2" "T4" 3648 30) NIL "გადმო") 
	  (("შუქ" "T4" 2906 15) "მი" "მო") 
	  (("ნიჭ" "T5" 1664 1) "მი" "მო") 
	  (("ახლოვ" "T3" 71 7) "მი" "მო") 
	  (("ყრ" "T4" 2801 57) "შე" "შემო") 
	  (("ხმაურ" "RM4" 3662 18) "ჩა" "ჩამო") 
	  (("ჩენ" "T3" 2944 25) "მი" "მო") 
	  (("ტყუ" "T5" 2240 23) "შე" "შემო") 
	  (("ქცევ1" "T5" 2606 33) "მი" "მო") 
	  (("ხმიან" "RM4" 3664 3) "შე" "შემო") 
	  (("შველ" "T5" 2850 12) "წა" "წამო") 
	  (("წვდენ" "T3" 3268 45) "გა" "გამო") 
	  (("ჩერ" "T4" 2947 10) "შე" "შემო") 
	  (("მართ1" "T5" 1228 15) "მი" "მო") 
	  (("ქცევ1" "T5" 2606 35) "შე" "შემო") 
	  (("ხედ" "T5" 3581 44) "შე" "შემო") 
	  (("შველ" "RP1" 2850 19) "მი" "მო") 
	  (("თვლ" "T4" 771 26) "მი" "მო") 
	  (("კარ2" "T2" 885 3) "მი" "მო") 
	  (("შტერ" "RP4" 2895 17) "მი" "მო") 
	  (("ყურ2" "T5 (nur mit i.O.)" 2818 2) "მი" "მო") 
	  (("შხეფ" "T4" 2916 4) "მი" "მო") 
	  (("კარ2" "T5" 885 8) "მი" "მო") 
	  (("თვლ" "T3" 771 20) "და" "დამო") 
	  (("ვარდნ" "RP6 (OR)" 566 34) "შე" "შემო") 
	  (("კივილ" "T5" 959 18) "მი" "მო") 
	  (("ცვივნ" "RP3" 3080 53) "შე" "შემო") 
	  (("თოვ" "RM3" 784 10) "მი" "მო") 
	  (("სევ" "RP1" 2019 19) "მი" "მო") 
	  (("ხარ1" "T5" 3555 13) "მი" "მო") 
	  (("შტერ" "RP4" 2895 19) "შე" "შემო") 
	  (("ხმარ" "T5" 3660 10) "წა" "წამო") 
	  (("ზომ" "RP1 (OR)" 658 19) "მი" "მო") 
	  (("სიყვარულ" "RP1" 2059 1) "მი" "მო") 
	  (("მხრ" "RP1" 1563 2) "მი" "მო") 
	  (("ტევ2" "T3" 2150 21) "მი" "მო") 
	  (("პარ" "RP1" 1721 26) "მი" "მო") 
	  (("ჯიბრ" "RP1" 3793 6) "შე" "შემო") 
	  (("ვარდნ" "RP6 (OR)" 566 32) "მი" "მო") 
	  (("ყვედრ" "T5" 2753 2) "წა" "წამო") 
	  (("ყიდულ" "T5" 2764 15) "მი" "მო") 
	  (("ძალ" "T5" 3171 5) "მი" "მო") 
	  (("ფურთხ" "T4 (nur mit i.O.)" 2427 15) "შე" "შემო") 
	  (("ტორღიალ" "RP1" 2199 1) "ა" "ამო") 
	  (("ჯახუნ" "T3" 3775 6) "მი" "მო") 
	  (("გვრ" "T5" 360 6) "მი" "მო") 
	  (("სახლ" "RP1" 2012 29) "ჩა" "ჩამო") 
	  (("ცქერ" "RP4" 3146 23) "მი" "მო") 
	  (("გზავნ" "RP1 (OR)" 362 37) "გა" "გამო") 
	  (("შუქ" "T3" 2906 12) "მი" "მო") 
	  (("გებ" "RP1" 339 9) "მი" "მო") 
	  (("ჩენ" "RP3" 2944 40) "შე" "შემო") 
	  (("ალერს" "RM4" 29 6) "მი" "მო") 
	  (("მაგრ" "T4" 1207 14) "მი" "მო") 
	  (("კარ2" "RP1" 885 15) "მი" "მო") 
	  (("ახლოვ" "RP3" 71 12) "მი" "მო") 
	  (("ხმარ" "T5" 3660 12) "წა" "წამო") 
	  (("ჩვევ" "RP1" 2953 16) "მი" "მო") 
	  (("ქირავ" "T5" 2537 4) "მი" "მო") 
	  (("კითხულ" "RP1" 961 34) "შე" "შემო") 
	  (("ხმაურ" "RM4" 3662 16) "შე" "შემო") 
	  (("გ" "T5" 338 27) "მი" "მო"))
     do (update-records [morph verb-paradigm]
			:av-pairs `(([pf-12-pv] ,12-pv))
			:where [and [= [id] ?id]
				    [= [sub-id] ?sub-id]
				    [= [pf-pv] ?3-pv]])))


;; Reduplication

#+test
(do-query ((id sub-id impf-pv pf-pv vn red-dir-pv)
	   [select [id] [sub-id] [impf-pv] [pf-pv] [vn] [red-dir-pv]
		   :from [morph verb-paradigm]
		   :where [not [null [red-dir-pv]]]
		   :order-by '([id] [sub-id])])
  (print (list id sub-id impf-pv pf-pv
	       (format nil "~a~a-~a~a"
		       pf-pv (subseq vn 0 (1- (length vn)))
		       red-dir-pv vn))))
  

#+once
(with-transactionxx ()
  (loop for (id sub-id impf-pv pf-pv vn)
     in '((194 10 "-" "მი" "მიბნევ-მობნევა") 
	  (243 46 "მი" "მი" "მიბრუნ-მობრუნება") 
	  (392 33 "-" "გა" "გაგორ-გამოგორება") 
	  (432 39 "-" "მი" "მიდებ-მოდება") 
	  (457 16 "-" "მი" "მიდგომ-მოდგომა") 
	  (468 13 "-" "ა" "ადევნ-დადევნა") 
	  (468 14 "-" "ა" "ადევნ-ჩადევნა") 
	  (566 2 "-" "ა" "ავარდნ-დავარდნა") 
	  (566 5 "-" "გა" "გავარდნ-გამოვარდნა") 
	  (714 26 "-" "გა" "გათამაშ-გამოთამაშება") 
	  (760 2 "-" "ა" "ათვალიერ-ჩათვალიერება") 
	  (760 4 "-" "გადა" "გადათვალიერ-გადმოთვალიერება") 
	  (760 7 "-" "მი" "მითვალიერ-მოთვალიერება") 
	  (961 6 "-" "მი" "მიკითხვ-მოკითხვა") 
	  (1436 8 "-" "მი" "მიმტვრევ-მომტვრევა") 
	  (1436 65 "-" "მი" "მიმტვრევ-მომტვრევა") 
	  (1632 9 "-" "მი" "მინგრევ-მონგრევა") 
	  (1632 22 "-" "მი" "მინგრევ-მონგრევა") 
	  (1632 42 "-" "მი" "მინგრევ-მონგრევინება") 
	  (1632 55 "-" "მი" "მინგრევ-მონგრევა") 
	  (1844 21 "გა" "გა" "გარბენ-გამორბენა") 
	  (1853 2 "-" "ა" "არევ-დარევა") 
	  (2029 7 "-" "მი" "მისმა-მოსმა") 
	  (2029 12 "-" "წა" "წასმა-წამოსმა") 
	  (2029 14 "-" "ა" "ასმა-ჩასმა") 
	  (2029 20 "-" "მი" "მისმა-მოსმა") 
	  (2032 2 "მი" "-" "მისვლა-მოსვლა") 
	  (2032 35 "ა" "ა" "ასვლა-ჩასვლა") 
	  (2032 42 "მი" "მი" "მისვლა-მოსვლა") 
	  (2032 78 "ა" "ა" "ასვლა-ჩასვლა") 
	  (2032 125 "-" "ა" "ავლა-ჩავლა") 
	  (2032 128 "გა" "გა" "გავლა-გამოვლა") 
	  (2032 136 "-" "მი" "მივლა-მოვლა") 
	  (2032 136 "-" "მიმო" "მიმოვლა-მოვლა") 
	  (2032 147 "ა" "ა" "ავლა-ჩავლა") 
	  (2032 151 "-" "გა" "გავლა-გამოვლა") 
	  (2032 158 "-" "მი" "მივლა-მოვლა") 
	  (2032 193 "მი" "-" "მისვლა-მოსვლა") 
	  (2033 3 "-" "მი" "მისვლეპ-მოსვლეპა") 
	  (2033 12 "-" "მი" "მისვლეპ-მოსვლეპა") 
	  (2138 9 "მი" "მი" "მიტან-მოტანა") 
	  (2141 8 "-" "ა" "ატარ-ჩამოტარება") 
	  (2141 11 "-" "გა" "გატარ-გამოტარება") 
	  (2141 16 "-" "მი" "მიტარ-მოტარება") 
	  (2141 16 "-" "მიმო" "მიმოტარ-მოტარება") 
	  (2141 69 "-" "მიმო" "მიმოტარ-მოტარება") 
	  (2141 69 "-" "მი" "მიტარ-მოტარება") 
	  (2206 11 "-" "გადა" "გადატრიალ-გადმოტრიალება") 
	  (2206 15 "-" "მი" "მიტრიალ-მოტრიალება") 
	  (2206 38 "-" "გადა" "გადატრიალ-გადმოტრიალება") 
	  (2206 42 "-" "მი" "მიტრიალ-მოტრიალება") 
	  (2606 41 "-" "გა" "გაქცევ-გამოქცევა") 
	  (2606 96 "-" "გა" "გაქცევ-გამოქცევა") 
	  (2611 2 "-" "ა" "აღებ-დაღება") 
	  (2751 5 "-" "გა" "გაყვან-გამოყვანა") 
	  (2751 8 "-" "გადა" "გადაყვან-გადმოყვანა") 
	  (2751 12 "-" "მი" "მიყვან-მოყვანა") 
	  (2751 19 "-" "წა" "წაყვან-წამოყვანა") 
	  (2764 12 "-" "მი" "მიყიდვ-მოყიდვა") 
	  (2786 9 "გა" "გა" "გაყოლ-გამოყოლა") 
	  (2786 12 "გადა" "გადა" "გადაყოლ-გადმოყოლა") 
	  (2786 16 "მი" "მი" "მიყოლ-მოყოლა") 
	  (2787 53 "ა" "ა" "აყოლ-ჩაყოლა") 
	  (2801 5 "-" "გადა" "გადაყრა-გადმოყრა") 
	  (2801 9 "-" "მიმო" "მიმოყრა-მოყრა") 
	  (2801 9 "-" "მი" "მიყრა-მოყრა") 
	  (2801 139 "-" "მი" "მიყრა-მოყრა") 
	  ;; (2801 139 "-" "მიმო" "მიმოყრ-მოყრა") 
	  (2873 8 "-" "მი" "მიშლა-მოშლა") 
	  (2873 93 "-" "მი" "მიშლა-მოშლა") 
	  (2873 94 "-" "მი" "მიშლა-მოშლა") 
	  (3258 4 "-" "გა" "გაწევ-გამოწევა") 
	  (3258 10 "-" "მი" "მიწევ-მოწევა") 
	  (3258 17 "-" "წა" "წაწევ-წამოწევა") 
	  (3258 23 "-" "ა" "აწევ-დაწევა") 
	  (3258 26 "-" "გა" "გაწევ-გამოწევა") 
	  (3258 140 "-" "მი" "მიწევ-მოწევა") 
	  (3258 143 "-" "წა" "წაწევ-წამოწევა") 
	  (3258 144 "-" "მი" "მიწევ-მოწევა") 
	  (3258 145 "-" "წა" "წაწევ-წამოწევა") 
	  (3581 8 "-" "გა" "გახედვ-გამოხედვა") 
	  (3581 15 "-" "გა" "გახედვ-გამოხედვა") 
	  (3581 33 "-" "ა" "ახედვ-დახედვა") 
	  (3581 38 "-" "გადა" "გადახედვ-გადმოხედვა") 
	  (3688 8 "-" "მი" "მიხრა-მოხრა") 
	  (3688 61 "-" "მი" "მიხრა-მოხრა"))
     do (unless (equal impf-pv "-") 
	  (update-records [morph verb-paradigm]
			  :av-pairs `(([impf-vn] ,vn))
			  :where [and [= [id] ?id]
				      [= [sub-id] ?sub-id]
				      [= [impf-pv] ?impf-pv]
				      [= [pf-pv] ?pf-pv]]))
     (unless (equal pf-pv "-") 
       (update-records [morph verb-paradigm]
		       :av-pairs `(([pf-vn] ,vn))
		       :where [and [= [id] ?id]
				   [= [sub-id] ?sub-id]
				   [= [impf-pv] ?impf-pv]
				   [= [pf-pv] ?pf-pv]]))))

;; participles

(defparameter *participle-table* (make-hash-table :test #'equal))

(do-query ((participle pos)
	   [select [stem] [pos]
		   :from [morph noun-features]
		   :where [and [in [pos] '("p.p." "p.a." "p.f." "p.n.")]
			       [not [= [comment] "wrong-pos"]]]])
  (let ((pos (cond ((equal pos "p.p.") :past-part)
		   ((equal pos "p.a.") :present-part)
		   ((equal pos "p.f.") :future-part)
		   ((equal pos "p.n.") :negative-part))))
    (print (list participle pos))
    (pushnew pos (gethash participle *participle-table*))))

(defparameter *participle-stem-table* (make-hash-table :test #'equal))

(dolist (pos '(:past-part :present-part :future-part :negative-part))
  (debug pos)
  (do-query ((id sub-id &rest morphemes)
	     [select [id] [sub-id] [pv] [part-pfx] [root] [sf] [part-sfx]
		     :from [morph verb-features]
		     :where [like [tense] (format nil "%|~(~a~)|%" pos)]
		     :order-by '([id] [sub-id])
		     ;;:limit 100
		     ])
    (setf morphemes (mapcar (lambda (m) (if (or (null m) (equal m "-")) "" m)) morphemes))
    (destructuring-bind (pv part-pfx root sf part-sfx) morphemes
      (case pos
	(:present-part (when (equal part-pfx "") (setf part-pfx "მ")))
	(:future-part (when (equal part-pfx "") (setf part-pfx "სა")))
	(:negative-part (when (equal part-pfx "") (setf part-pfx "უ"))))
      (setf root (substitute #\ე #\E (substitute #\ა #\A root)))
      (let* ((stem (format nil "~a~a~a~a~a"
			   pv
			   part-pfx
			   root
			   sf
			   part-sfx
			   ))
	     (star-stem (format nil "~a~a~a~a~a"
				(if (equal pv "") "" "*")
				part-pfx
				root
				sf
				part-sfx
				))
	     (noun-class (cond ((equal part-sfx "ევ") :A)
			       ((equal part-sfx "იარე") :M)
			       ((equal part-sfx "ო") :O)
			       ((equal part-sfx "ულ") :A)
			       ((equal part-sfx "ალ") :B)
			       ((equal part-sfx "არე") :M)
			       ((equal part-sfx "ე") :M)
			       ((equal part-sfx "ილ") :A)
			       ((equal part-sfx "ელ") :B)
			       ((equal part-sfx "არ") :B)
			       (t :A)))
	     (participle (substitute #\ყ #\ჴ (substitute #\ე #\ჱ stem)))
	     (attested (when (find pos (gethash participle *participle-table*)) t))
	     (nom (if (or (equal part-sfx "")
			  (not (find (last-char part-sfx) "ეაოუიჱჳ")))
		      "ი"
		      ""))
	     (participle (concat participle nom))
	     )
	(pushnew (list pos star-stem noun-class (or (fullform-count participle) attested))
		 (gethash (list id sub-id) *participle-stem-table*)
		 :test #'equal)))))

#+test
(Print (gethash (list 2895 7) *participle-stem-table*))

#+main
(with-database-connection ()
  (with-transaction ()
    (maphash (lambda (id+subid values)
	       (dolist (value values)
		 (destructuring-bind (pos stem noun-class attested) value
		   (let ((row (select [attested]
				      :from [morph verb-participle]
				      :flatp t
				      :where [and [= [id] (car id+subid)]
						  [= [sub-id] (cadr id+subid)]
						  [= [type] ?pos]
						  [= [stem] ?stem]])))
		     (cond ((null row)
			    (insert-records :into [morph verb-participle]
					    :av-pairs
					    `(([id] ,(car id+subid))
					      ([sub-id] ,(cadr id+subid))
					      ([type] ,pos)
					      ([stem] ,stem)
					      ([code] ,noun-class)
					      ([attested] ,(when attested t)))))
			   ((and attested (null (car row)))
			    (update-records [morph verb-participle]
					    :av-pairs '(([attested] t))
					    :where [and [= [id] (car id+subid)]
							[= [sub-id] (cadr id+subid)]
							[= [type] ?pos]
							[= [stem] ?stem]]
					    )))))))
	     *participle-stem-table*)))

;; masdars

(defparameter *masdar-table* (make-hash-table :test #'equal))

(do-query ((id sub-id pf-pv vn pf-vn)
	   [select [verb-paradigm id] [verb-paradigm sub-id]
		   [pf-pv] [verb-paradigm vn] [pf-vn]
		   :distinct t
		   :from [morph verb-paradigm]
		   :left-join [morph verb-features]
		   :on [and [= [verb-paradigm id] [verb-features id]]
			    [= [verb-paradigm sub-id] [verb-features sub-id]]]
		   :where [or [like [tense] "%|aorist|%"] [like [tense] "%|future|%"]]
		   :order-by '([verb-paradigm id] [verb-paradigm sub-id])
		   ;;:limit 100
		   ])
  (unless (equal (last-char vn) #\*)
    (let* ((lemma (or pf-vn
		      (if (equal pf-pv "-")
			  vn
			  (u:concat pf-pv "-" vn))))
	   (stem (or pf-vn
		      (if (equal pf-pv "-")
			  vn
			  (u:concat "*" vn))))
	   (code (ecase (last-char lemma)
		   (#\ი :A)
		   (#\ა :I)))
	   (stem (ecase (last-char stem)
		   (#\ი (subseq stem 0 (1- (length stem))))
		   (#\ა stem))))
      ;;(print (list id sub-id stem code))
      (pushnew (print (list lemma stem code :perf t))
	       (gethash (list id sub-id) *masdar-table*)
	       :test #'equal))))

(do-query ((id sub-id pf-pv impf-pv vn impf-vn)
	   [select [verb-paradigm id] [verb-paradigm sub-id]
		   [pf-pv] [impf-pv] [verb-paradigm vn] [impf-vn]
		   :distinct t
		   :from [morph verb-paradigm]
		   :left-join [morph verb-features]
		   :on [and [= [verb-paradigm id] [verb-features id]]
			    [= [verb-paradigm sub-id] [verb-features sub-id]]]
		   :where [like [tense] "%|present|%"]
		   :order-by '([verb-paradigm id] [verb-paradigm sub-id])
		   ;;:limit 100
		   ])
  (unless (equal (last-char vn) #\*)
    (let* ((attested t)
	   (lemma (or impf-vn
		      (let ((pv-list (gethash (list id vn) *vn-pv-table*)))
			(cond ((not (equal impf-pv "-"))
			       (u:concat impf-pv "-" vn))
			      ((and (null (cdr pv-list))
				    (equal pf-pv "-"))
			       vn)
			      (t
			       (setf attested nil)
			       vn)))))
	   (stem (or impf-vn
		     (let ((pv-list (gethash (list id vn) *vn-pv-table*)))
		       (cond ((not (equal impf-pv "-"))
			      (u:concat "*" vn))
			     (t #+ignore(and (null (cdr pv-list))
					     (equal pf-pv "-"))
			      vn))))))
      (when lemma
	(let* ((code (ecase (last-char lemma)
		       (#\ი :A)
		       (#\ა :I)
		       (#\ო :O)))
	       (stem (ecase (last-char stem)
		       (#\ი (subseq stem 0 (1- (length stem))))
		       ((#\ა #\ო) stem))))
	  (print (list id sub-id stem code))
	  (pushnew (print (list lemma stem code :imperf attested))
		   (gethash (list id sub-id) *masdar-table*)
		   :test #'equal))))))


#+main
(let ((pos :masdar))
  (with-database-connection ()
    (with-transaction ()
      (maphash (lambda (id+subid values)
		 (dolist (value values)
		   (destructuring-bind (lemma stem noun-class aspect attested) value
		     (let ((row (select [attested]
					:from [morph participle]
					:flatp t
					:where [and [= [id] (car id+subid)]
						    [= [sub-id] (cadr id+subid)]
						    [= [type] ?pos]
						    [= [stem] ?stem]])))
		       (cond ((null row)
			      (insert-records :into [morph participle]
					      :av-pairs
					      `(([id] ,(car id+subid))
						([sub-id] ,(cadr id+subid))
						([type] ,pos)
						([stem] ,stem)
						([code] ,noun-class)
						([aspect] ,aspect)
						([attested] ,(when attested t)))))
			     ((and attested (null (car row)))
			      (update-records [morph participle]
					      :av-pairs '(([attested] t))
					      :where [and [= [id] (car id+subid)]
							  [= [sub-id] (cadr id+subid)]
							  [= [type] ?pos]
							  [= [stem] ?stem]]
					      )))))))
	       *masdar-table*))))

;; ხ/ჴ

(defparameter *q-lemma-hash* (make-hash-table :test #'equal))
(defparameter *x-lemma-hash* (make-hash-table :test #'equal))

#+test
(u:with-file-fields ((lemma trans) "~/lisp/projects/georgian-morph/wordlists/oge-deu.tsv")
  (when (and (> (length lemma) 2)
	     (or (find #\ხ lemma)
		 (find #\ჴ lemma)))
    ;(when (find #\ხ lemma) (print lemma))
    (let* ((og-stem (subseq lemma 0
			   (or (search "-ჲ" lemma :start2 (- (length lemma) 2))
			       (search "-ი" lemma :start2 (- (length lemma) 2)))))
	   (ng-stem (substitute #\ხ #\ჴ og-stem)))
      (if (find #\ჴ lemma)
	  (setf (gethash ng-stem *q-lemma-hash*) og-stem)
	  (setf (gethash ng-stem *x-lemma-hash*) og-stem)))))

#+test
(do-query ((stem)
	   [select [stem] :from [morph noun-features]
		   :where [and [like [stem] "%ხ%"]
			       [or [null [lang]] [<> [lang] "og"]]
			       [or [null [features]] [not [like [features] "%Name%"]]]]
		   :distinct t
		   :flatp t
		   :order-by [stem]])
  (let ((og-q-seg nil)
	(og-x-seg nil)
	(ng-seg nil))
    (maphash (lambda (q-lemma og-stem)
	       (when (and (search q-lemma stem)
			  (or (null og-q-seg)
			      (> (length q-lemma) (length og-q-seg))))
		 (setf og-q-seg og-stem ng-seg q-lemma)))
	     *q-lemma-hash*)
    (maphash (lambda (q-lemma og-stem)
	       (when (and (search q-lemma stem)
			  (or (null og-x-seg)
			      (> (length q-lemma) (length og-x-seg))))
		 (setf og-x-seg og-stem)))
	     *x-lemma-hash*)
    (when (and og-q-seg
	      (or (null og-x-seg)
		  (< (length og-x-seg) (length og-q-seg)))
	      (< (length og-q-seg) (length stem)))
      (let ((start (search ng-seg stem)))
	(print (replace stem og-q-seg :start1 start))))))

#+test
(dolist (stem )
  (update-records [morph noun-features]
		  :av-pairs `(([stem] ,stem))
		  :where [= [stem] (substitute #\ხ #\ჴ stem)]))


(update-records [morph participle]
		:av-pairs '(([type] "MASDAR-DISABLED"))
		:where [= [type] :masdar])

(update-records [morph participle]
		:av-pairs '(([main-form] t))
		:where [= [type] :masdar])

#+test
(delete-recordsx :from [morph participle]
		:where [= [type] :masdar])

(with-transaction ()
  (let ((table (make-hash-table :test #'equal))
	(masdars
	 (u:collecting
	   (do-query ((id sub-id vn impf-vn pf-vn)
		      [select [id] [sub-id] [vn] [impf-vn] [pf-vn]
			      :from [morph verb-paradigm]
			      :distinct t
			      :order-by '([id] [sub-id])])
	     (cond ((and (null impf-vn) (null pf-vn))
		    (u:collect (list id sub-id (u:concat "*" vn) nil)))
		   ((null pf-vn)
		    (u:collect (list id sub-id impf-vn :impf))
		    (u:collect (list id sub-id (u:concat "*" vn) :pf)))
		   ((null impf-vn)
		    (u:collect (list id sub-id (u:concat "*" vn) :impf))
		    (u:collect (list id sub-id pf-vn :pf)))
		   (t
		    (u:collect (list id sub-id impf-vn :impf))
		    (u:collect (list id sub-id pf-vn :pf))))))))
    (loop for (id sub-id vn aspect) in masdars
       for code = (ecase (last-char vn)
		    (#\* nil)
		    (#\ი :A)
		    (#\ა :I)
		    (#\ო :O))
       for stem = (case (last-char vn)
		    (#\ი (subseq vn 0 (1- (length vn))))
		    (otherwise vn))
       unless (gethash (list id sub-id stem) table)
       do (insert-records :into [morph participle]
			  :attributes '([id] [sub-id] [type] [stem] [code] [aspect] [date] [main-form])
			  :values (list id sub-id :masdar stem code aspect (get-universal-time) t))
	 (setf (gethash (list id sub-id stem) table) t))))

:eof