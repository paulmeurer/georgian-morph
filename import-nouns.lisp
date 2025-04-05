;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10 -*-

;; paul.meurer@uib.no
;; https://clarino.uib.no/

;; below code for importing excerpted names

;; Extracted morph:



;; write-fst-participle-stems-sql()
;; write-fst-noun-stems-sql()

(in-package :fst)

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

#+test ;; strange error
(debug (select [stem] [code] [pos] [features] [style-features] [lang] [template] [comment]
	       :from [morph noun-features]
	       :where [and [like [stem] "სეფე-წულ"]]))

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



;;(print (select [pos] :distinct t :flatp t :from [noun-features] :order-by `([pos])))
;;(print (select [code] :distinct t :flatp t :from [noun-features] :order-by `([code])))


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

#+test
(u:with-file-fields ((name class type &optional morph) "projects:georgian-morph;regex;geonames.txt"
		     :separator #\+ :comment #\#)
  (print (list name class type morph)))

#+test
(write-fst-geo-stems)


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



#+test
(with-file-lines (line "~/lisp/projects/web-dictionary/data/rayfield-utf8.txt")
  (unless (char= (char line 0) #\#)
    (extract-headwords line)))

#+test
(print (select [*] :from [noun-features] :where [like [code] "%O2%"])) 

;;(print (extract-headwords "ოშიყანა რაბა <i>n Ju</i> four species (Sukkot ritual)"))


#+test
(print (parse-rayfield-line "მეფე <i>n</i> 1 king; monarch, sovereign; მეფეთ(ა) ~ <i>†</i> suzerain, overlord: მნათობთა ~ <i>ﬂ</i> sun; ყვავილთა ~ <i>ﬂ</i> rose; ბოზბაში – წვნიანების მეფეა, როგორც თართი – თევზებისა Mutton soup is the king of soups as sturgeon is the king of fish: წიგნი პირველი/მეორე მეფეთა Book of Samuel I/II; წიგნი მესამე/მეოთხე მეფეთა Book of Kings I/II; 2 <i>†</i> groom (<i>at wedding</i>); 3 <i>chess</i> king; 4 <i>N</i> Queen (<i>Tamar only</i>)"))

#+test
(print (collect-features "king; monarch, sovereign; მეფეთ(ა) ~ <i>†</i> suzerain, overlord: მნათობთა ~ <i>ﬂ</i> sun; ყვავილთა ~ <i>ﬂ</i> rose; ბოზბაში – წვნიანების მეფეა, როგორც თართი – თევზებისა Mutton soup is the king of soups as sturgeon is the king of fish: წიგნი პირველი/მეორე მეფეთა Book of Samuel I/II; წიგნი მესამე/მეოთხე მეფეთა Book of Kings I/II;"))
#+test
(print (collect-features "<i>†</i> groom (<i>at wedding</i>); 3 <i>chess</i> king; 4 <i>N</i> Queen (<i>Tamar only</i>)"))


;;(execute-command "alter table NOUN_FEATURES add STYLE_FEATURES varchar(255) after FEATURES;")

;; pa np fp vn



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

#+test
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

;; from noun-sql.lisp

#+once
(dolist (class '(noun-features))
  ;;(drop-table 'noun-features-xx)
  (unless (table-exists-p (view-table (find-class class)))
    (create-view-from-class class)
    (when (eq class 'noun-features)
      (execute-command "alter table NOUN_FEATURES modify STEM VARCHAR(255) character set utf8 collate utf8_bin;"))))

#+test
(let ((date (get-universal-time))
      (sub-id 1)
      (prev-stem "")
      (prev-pos "")
      (prev-code nil)
      (stem-pos-code ()))
  (with-transaction ()
    (with-file-lines (line "projects:georgian-morph;georgian-nouns.txt")
      (let ((line (string-trim " " (subseq line 0 (position #\# line)))))
	(unless (string= line "")
	  (destructuring-bind (stem codes) (split line #\:)
	    (destructuring-bind (code &rest readings) (split codes #\space)
	      (dolist (reading readings)
		(destructuring-bind (pos &optional features) (split reading #\+ 2)
		  (print (list (fst::convert stem) code pos features))
		  (unless (find (list stem pos code) stem-pos-code :test #'equal)
		    (cond ((and (string= stem prev-stem)
				(string= pos prev-pos)
				(equal code prev-code))
			   (incf sub-id))
			  (t (setf sub-id 1)))
		    (unless (string= stem prev-stem)
		      (setf stem-pos-code ()))
		    (push (list stem pos code) stem-pos-code)
		    (setf prev-stem stem prev-pos pos prev-code code)
		    (insert-records :into [noun-features]
				    :attributes `([stem] [code] [pos] [sub-id] [features] [template] [author] [date])
				    :values (list (fst::convert stem) code pos sub-id (when features (concat "+" features)) nil "Tsch" date))))))))))))

#+test
(let ((date (get-universal-time)))
  (with-transaction ()
    (with-file-lines (name "projects:georgian-morph;first-names-tmp.txt")
      (when (> (length name) 0)
	(let* ((conjugation (case (char name (1- (length name)))
			      (#\a :K)
			      (#\e :M)
			      (#\o :O)
			      (#\i :X)
			      (#\u :O)
			      (otherwise :Z)))
	       (code (string-upcase conjugation))
	       (sub-id (or (car (select [max [sub-id]]
					:from [noun-features]
					:flatp t
					:where [and [= [stem] name]
						    [= [code] code]
						    [= [pos] "n"]]))
			   0)))
	  (insert-records :into [noun-features]
			  :attributes `([stem] [code] [pos] [sub-id] [features] [template] [author] [date])
			  :values (print (list name code "n" (1+ sub-id) "+N+Prop+Name+FirstName" nil "Opentext" date))))))))


#+test
(with-transaction ()
  (update-records [noun-features]
		  :attributes '([features])
		  :values (list "+Anim")
		  :where [like [stem] "%log"]))

#+test
(let ((tree (dat:make-string-tree)))
  (u:with-file-lines (line "projects:georgian-morph;wordlists;soplebi-list.txt")
    (let* ((paren-start (position #\( line))
	   (word (subseq line 0 paren-start)))
      (setf word (string-trim " .	1234567890" word))
      (cond ((= (length word) 1)
	     nil)
	    (t
	     (setf (dat:string-tree-get tree (nreverse word)) t)))))
  (dat:do-string-tree (lemma ignore tree)
    (setf lemma (reverse lemma))
    (let ((space-pos (position #\space lemma)))
      (when (and space-pos (char= (char lemma (1- space-pos)) #\ი))
	(setf (char lemma (1- space-pos)) #\=)))
    (format t "~a+~a+City~%"
	    lemma
	    (case (char lemma (1- (length lemma)))
	      (#\ი "A")
	      (otherwise "O")))))

(in-package :korpuskel)

#+test
(let ((date (get-universal-time)))
  (with-database-connection ("localhost/gnc/gnc/kartuli" :database-type :postgresql)
    (with-transactionx ()
      (u:with-file-lines (name "projects:gnc;data;tina;names-first-unknown.txt")
        (when (and (> (length name) 0)
                   (char/= (char name 0) #\#))
	  (let* ((conjugation (case (char name (1- (length name)))
			        (#\ა :K)
			        (#\ე :M)
			        (#\ო :O)
			        (#\ი :X)
			        (#\უ :O)
			        (otherwise :Z)))
	         (code (string-upcase conjugation))
	         (sub-id (or (car (select [max [sub-id]]
					  :from [morph noun-features]
					  :flatp t
					  :where [and [= [stem] name]
						      [= [code] code]
						      [= [pos] "n"]
                                                      ;; [= [features] "+Prop+Name+FirstName"]
                                                      ]))
			     0)))
            (when (> sub-id 0)
              (print name))
            #-test
	    (insert-records :into [morph noun-features]
			    :attributes `([stem] [code] [pos] [sub-id] [features] [template] [author] [date])
			    :values (print (list name code "n" (1+ sub-id) "+Prop+Name+FirstName"
                                                 nil "Tina" date)))))))))

#+test
(let ((date (get-universal-time)))
  (with-database-connection ("localhost/gnc/gnc/kartuli" :database-type :postgresql)
    (with-transaction ()
      (u:with-file-lines (name "projects:gnc;data;tina;names-last-unknown.txt")
        (when (and (> (length name) 0)
                   (char/= (char name 0) #\#))
	  (let* ((conjugation
                  (cond ((find #\+ name)
                         (prog1 (intern (string-upcase (string (char name (1- (length name))))))
                           (setf name (subseq name 0 (- (length name) 2)))))
                        (t
                         (case (char name (1- (length name)))
			   (#\ა :K)
			   (#\ე :M)
			   (#\ო :O)
			   (#\ი
                            (setf name (subseq name 0 (- (length name) 1)))
                            :A)
			   (#\უ :O)
			   (otherwise :Z)))))
	         (code (string-upcase conjugation))
                 (found (car (select [max [sub-id]]
					  :from [morph noun-features]
					  :flatp t
					  :where [and [= [stem] name]
						      [= [code] code]
						      [= [pos] "n"]
                                                      [= [features] "+Prop+Name+LastName"]
                                                      ])))
	         (sub-id (unless found
                           (or (car (select [max [sub-id]]
					    :from [morph noun-features]
					    :flatp t
					    :where [and [= [stem] name]
						        [= [code] code]
						        [= [pos] "n"]
                                                        ]))
			       0))))
            (if found
              (print name)
              #-test
              (print (list name code "n" (1+ sub-id) "+Prop+Name+LastName"
                           nil "Tina" date)))
            #-test
            (unless found
	      (insert-records :into [morph noun-features]
			      :attributes `([stem] [code] [pos] [sub-id] [features] [template] [author] [date])
			      :values (list name code "n" (1+ sub-id) "+Prop+Name+LastName"
                                            nil "Tina" date)))))))))


#+test
(let ((date (get-universal-time)))
  (with-database-connection ("localhost/gnc/gnc/kartuli" :database-type :postgresql)
    (with-transaction ()
      (u:with-file-lines (name "projects:gnc;data;tina;names-pers-unknown.txt")
        (when (and (> (length name) 0)
                   (char/= (char name 0) #\#))
	  (let* ((name-start (position #\space name))
                 (type (intern (string-upcase (subseq name 0 name-start)) :keyword))
                 (name (subseq name (1+ name-start)))
                 (conjugation
                  (cond ((find #\+ name)
                         (prog1 (intern (string-upcase (string (char name (1- (length name))))))
                           (setf name (subseq name 0 (- (length name) 2)))))
                        (t
                         (case (char name (1- (length name)))
			   (#\ა :K)
			   (#\ე :M)
			   (#\ო :O)
			   (#\ი
                            (setf name (subseq name 0 (- (length name) 1)))
                            :A)
			   (#\უ :O)
			   (otherwise :Z)))))
	         (code (string-upcase conjugation))
                 (found (car (select [max [sub-id]]
					  :from [morph noun-features]
					  :flatp t
					  :where [and [= [stem] name]
						      [= [code] code]
						      [= [pos] "n"]
                                                      [like [features] "+Prop+Name%"]
                                                      ])))
	         (sub-id (unless found
                           (or (car (select [max [sub-id]]
					    :from [morph noun-features]
					    :flatp t
					    :where [and [= [stem] name]
						        [= [code] code]
						        [= [pos] "n"]
                                                        ]))
			       0))))
            (cond (found
                   (print name))
                  ((find type '(:adj :e :p))
                   nil)
                  (t
                   #-test
                   (print (list name code "n" (1+ sub-id)
                                (ecase type
                                  (:a "+Prop+Name")
                                  ((:m :ma) "+Prop+Name+Myth")
                                  (:w "+Prop+Title"))
                                nil "Tina" date))))
            #+test
            (unless (or found (find type '(:adj :e :p)))
	      (insert-records :into [morph noun-features]
			      :attributes `([stem] [code] [pos] [sub-id] [features] [template] [author] [date])
			      :values (list name code "n" (1+ sub-id)
                                            (ecase type
                                              (:a "+Prop+Name")
                                              ((:m :ma) "+Prop+Name+Myth")
                                              (:w "+Prop+Title"))
                                            nil "Tina" date)))))))))


#+test
(let ((date (get-universal-time)))
  (with-database-connection ("localhost/gnc/gnc/kartuli" :database-type :postgresql)
    (with-transaction ()
      (u:with-file-lines (name "projects:georgian-morph;wordlists;guess-tmp1.txt")
        (when (and (> (length name) 0)
                   (char/= (char name 0) #\#)
                   (eql (string< "f " name) 2)
                   )
	  (let* ((name (subseq name 2))
                 (conjugation (case (char name (1- (length name)))
			        (#\ა :K)
			        (#\ე :M)
			        (#\ო :O)
			        (#\ი :X)
			        (#\უ :O)
			        (otherwise :Z)))
	         (code (string-upcase conjugation))
	         (sub-id (or (car (select [max [sub-id]]
					  :from [morph noun-features]
					  :flatp t
					  :where [and [= [stem] name]
						      [= [code] code]
						      [= [pos] "n"]
                                                      [= [features] "+Prop+Name+FirstName"]
                                                      ]))
			     0)))
            ;; (Print name)
            (if (> sub-id 0)
                (print name)
                #-test
	        (insert-records :into [morph noun-features]
			        :attributes `([stem] [code] [pos] [sub-id] [features] [template] [author] [date])
			        :values (print (list name code "n" (1+ sub-id) "+Prop+Name+FirstName"
                                                     nil "TRANS" date))))))))))

#+test
(let ((date (get-universal-time)))
  (with-database-connection ("localhost/gnc/gnc/kartuli" :database-type :postgresql)
    (with-transaction ()
      (u:with-file-lines (name "projects:georgian-morph;wordlists;guess-tmp1.txt")
        (when (and (eql (string< "l " name) 2))
	  (let* ((name (subseq name 2))
                 (conjugation
                  (cond ((find #\+ name)
                         (prog1 (intern (string-upcase (string (char name (1- (length name))))))
                           (setf name (subseq name 0 (- (length name) 2)))))
                        (t
                         (case (char name (1- (length name)))
			   (#\ა :K)
			   (#\ე :M)
			   (#\ო :O)
			   (#\ი
                            (setf name (subseq name 0 (- (length name) 1)))
                            :A)
			   (#\უ :O)
			   (otherwise :Z)))))
	         (code (string-upcase conjugation))
                 (found (car (select [max [sub-id]]
					  :from [morph noun-features]
					  :flatp t
					  :where [and [= [stem] name]
						      [= [code] code]
						      [= [pos] "n"]
                                                      [= [features] "+Prop+Name+LastName"]
                                                      ])))
	         (sub-id (unless found
                           (or (car (select [max [sub-id]]
					    :from [morph noun-features]
					    :flatp t
					    :where [and [= [stem] name]
						        [= [code] code]
						        [= [pos] "n"]
                                                        ]))
			       0))))
            (if found
              (print name)
              #-test
              (print (list name code "n" (1+ sub-id) "+Prop+Name+LastName"
                           nil "Tina" date)))
            #-test
            (unless found
	      (insert-records :into [morph noun-features]
			      :attributes `([stem] [code] [pos] [sub-id] [features] [template] [author] [date])
			      :values (list name code "n" (1+ sub-id) "+Prop+Name+LastName"
                                            nil "TRANS" date)))))))))


(in-package :fst)

#+test
(let ((date (get-universal-time)))
  (with-database-connection ("localhost/gnc/gnc/kartuli" :database-type :postgresql)
    (with-transaction ()
      (u:with-file-lines (name "projects:georgian-morph;lists;guessed-rt.txt")
        (let* ((features (ecase (intern (string-upcase (char name 0)) :keyword)
                           (:f "+Prop+Name+FirstName")
                           (:l "+Prop+Name+LastName")
                           (:o "+Prop+Name+Org")))
               (name (subseq name 2))
               (conjugation
                (cond ((find #\+ name)
                       (prog1 (intern (string-upcase (string (char name (1- (length name))))))
                         (setf name (subseq name 0 (- (length name) 2)))))
                      (t
                       (case (char name (1- (length name)))
			 (#\ა :K)
			 (#\ე :M)
			 (#\ო :O)
			 (#\ი
                          (cond ((char= (char name (- (length name) 2)) #\·)
                                 (setf name (subseq name 0 (- (length name) 2)))
                                 :A)
                                (t
                                 :O)))
			 (#\უ :O)
			 (otherwise :Z)))))
	       (code (string-upcase conjugation))
               (found (car (select [max [sub-id]]
				   :from [morph noun-features]
				   :flatp t
				   :where [and [= [stem] name]
					       [= [code] code]
					       [= [pos] "n"]
                                               [= [features] features]
                                               ])))
	       (sub-id (unless found
                         (or (car (select [max [sub-id]]
					  :from [morph noun-features]
					  :flatp t
					  :where [and [= [stem] name]
						      [= [code] code]
						      [= [pos] "n"]
                                                      ]))
			     0))))
          (if found
              (print name)
              #+test
              (print (list name code "n" (1+ sub-id) "+Prop+Name+LastName"
                           nil "RT" date)))
          (unless found
            #-test
	    (insert-records :into [morph noun-features]
			    :attributes `([stem] [code] [pos] [sub-id] [features] [template] [author] [date])
			    :values (list name code "n" (1+ sub-id) features
                                          nil "RT" date))))))))

:eof
