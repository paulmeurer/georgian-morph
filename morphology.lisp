;;;-*- Mode: Lisp; Package: TRANSDUCER -*-

(in-package :fst)

;; comments: # means changed/added, + confirmed by corpus, ? error, ++ found in corpus
;; ## unchecked; # t from Tschenkeli.


#||

A    კაცი - კაცის - კაცები
B/C  კედელი - კედლის - კედლები
P/Q  ნიორი - ნივრის - ნივრები
U/V  ხუცესი - ხუცის - ხუცები
D/E  ქვეყანა - ქვეყნის - ქვეყნები
F    მხარე - მხრის - მხრები ;; ?? missing
I/J  ძმა - ძმის - ძმები
K/L  გოგონა - გოგონას - გოგონები
M/N  ხე - ხის -ხეები
O    სოკო - სოკოს - სოკოები
R/S  ხატვა - ხატვის - 0 (masdari)
X/Y  ტრამვაი - ტრამვაის - ტრამვაები
H    აშშ - აშშ-ს
Z    დავით - დავითის
ZA   გიორგი - გიორგის

||#

#+wrong
(defun syncope-stem (stem)
  (let ((length (length stem)))
    (concat (subseq stem 0 (- length 2)) (string (last-char stem)))))

#+test
(print (syncope-stem "ამბავ" t))

(defun syncope-stem (stem &optional mark)
  (let ((last-vowel (position-if (lambda (c) (find c "აეოუ")) stem :from-end t)))
    (if mark
	(concat (subseq stem 0 last-vowel)
		"[" (subseq stem last-vowel (1+ last-vowel)) "]"
		(subseq stem (1+ last-vowel)))
	(concat (subseq stem 0 last-vowel) (subseq stem (1+ last-vowel))))))

(defun ablaut-stem (stem char left-pos &optional mark)
  (if mark
      (concat (subseq stem 0 (- (length stem) left-pos 1))
	      "["
	      (subseq stem (- (length stem) left-pos 1) (- (length stem) left-pos))
	      "]"
	      (subseq stem (- (length stem) left-pos)))
      (let ((copy (copy-seq stem)))
	(setf (char copy (- (length stem) left-pos 1)) char)
	copy)))

(defun truncate-stem (stem &optional (left-pos 1) mark)
  (if mark
      (u:concat (subseq stem 0 (- (length stem) left-pos))
		"[" (subseq stem (- (length stem) left-pos)) "]")
      (subseq stem 0 (- (length stem) left-pos))))

(defun mwe-stem (stem &optional mark)
  (if mark
      (subst-substrings stem '("=" "·ი"))
      (delete #\= stem)))

(defun mwe-truncate-stem (stem &optional keep-i mark)
  (if mark
      (let ((stem (subst-substrings stem '("=" "·ი"))))
	(u:concat (subseq stem 0 (- (length stem) 1))
		  "[" (subseq stem (- (length stem) 1)) "]"))
      (let ((stem (if keep-i
		      (substitute #\ი #\= stem)
		      (delete #\= stem))))
	(subseq stem 0 (- (length stem) 1)))))

#+test
(print (mwe-truncate-stem "შავ= ზღვა" 1 t))
#+test
(print (truncate-syncope-stem "ქვეყანა" t))

(defun truncate-syncope-stem (stem &optional mark)
  (let ((length (length stem)))
    (if mark
	(concat (subseq stem 0 (- length 3))
		"[" (subseq stem (- length 3) (- length 2)) "]"
		(string (last-char stem 2))
		"[" (subseq stem (- length 1)) "]")
	(concat (subseq stem 0 (- length 3)) (string (last-char stem 2))))))

(defun conjugation-features (conj-class stem lemma word-class &optional sub-class style lang personp)
  `((stem ,stem)
    (lex ,lemma)
    (cat ,word-class)
    ,@(when sub-class
        `((sub-cat ,sub-class)))
    ,@(when style
        `((style ,style)))
    ,@(when lang
        `((lang ,lang)))
    ,@(ecase conj-class
        ((:a) ;; კაც
         `((stem-type c)))
        (:a1 ;; ღვინ
         `((stem-type "ო") ;; ??
           (num sg)
           (case {gen dir inst})))
        (:a2 ;; ხბო/ხბორები
         `((stem-type c) ;; ??
           (num pl)
	   (old-pl -)))
	(:aa ;;
         `((stem-type c) ;; MWE: დიდი მიტარბი
           (case {nom gen inst})))
	(:aa1 ;;
         `((stem-type c)
           (case {dat adv})))
	(:z ;; ნუგზარ
	 `((stem-type bare)))
        ((:b :b1) ;; კედელ: კედლის
         `((stem-type c)
           (sync-stem -)))
	(:bx ;; კედელ: კედელის, subnorm of :B, but not of :B1
         `((stem-type c)
           (sync-stem +)
	   (style subnorm)))
        (:c ;; კედლ
         `((stem-type c)
           (sync-stem +)))
        (:p ;; ნიორ
         `((stem-type c)
           (sync-stem -)))
	(:px ;; ნიორ 
         `((stem-type c)
           (sync-stem +)
	   (style subnorm)))
        (:q ;; ნივრ
         `((stem-type c)
           (sync-stem +)))
        (:u ;; ხუცეს
         `((stem-type c)
           (sync-stem -)))
	(:ux ;; ხუცეს-ის, ამბავ-ის
         `((stem-type c)
           (sync-stem +)
	   (style subnorm)))
        (:v ;; ხუც
         `((stem-type c)
           (sync-stem +)))
        ((:d :d1) ;; ქვეყანა
         `((stem-type "ა")
	   (rigid-stem -)
           (sync-stem -)))
	(:e ;; ქვეყნ
         `((stem-type "ა")
	   (rigid-stem -)
           (sync-stem +)))
	(:dx ;;
         `((stem-type "ა")
	   (rigid-stem -)
           (sync-stem +)))
        (:f ;; "მხარე"
         `((stem-type "ე")
           (pl-stem -)
           (sync-stem -)))
        
        (:g ;; "მხრ"
         `((stem-type "ე")
           (sync-stem +)))
        (:h ;; "aSS"
         `((stem-type abbr)))
        (:i ;; "jma"
         `((stem-type "ა")
           (rigid-stem -)
           (sync-stem -)))
        (:j ;; "jm"
         `((stem-type "ა")
           (sync-stem +)
           (rigid-stem -)))
	(:ia ;; "შავი ზღვა"
         `((stem-type "ა")
           (rigid-stem -)
           (sync-stem -)
	   (case nom)))
        (:ja1 ;; "შავ ზღვას"
         `((stem-type "ა")
           (rigid-stem -)
           (sync-stem -)
	   (case {dat adv})))
	(:ia1 ;; "შავი ზღვის"
         `((stem-type "ა")
           (sync-stem +)
           (rigid-stem -)
	   (case {gen inst})))
        (:ja nil)
        (:k ;; "gogona"
         `((stem-type "ა")
           (rigid-stem +)
           (sync-stem -)))
        (:l ;; "gogon"
         `((stem-type "ა")
           (rigid-stem +)
           (sync-stem +)))
        (:m ;; "ხე"
         `((stem-type "ე")
           (sync-stem -)))
	(:m1 ;; "დღე/დღეის"
         `((stem-type c)
           (num sg)
           (case {gen dir inst})))
        (:n ;; "ხ"
         `((stem-type "ე")
           (pl-stem -)
           (sync-stem +)))
        ((:o :on :za) ;; "სოკო"
	 (cond ((not personp)
		`((stem-type v)))
	       ((char= (last-char stem) #\ე)
		`((stem-type prop-ე))) ;; prop-ე is needed b/o OG alternative gen in -ეს
	       (t
		`((stem-type propv)) ;; სოფო, გიორგი, ვიტალი
		)))
        (:o1 ;; "ღვინო"
         `((stem-type v) (case {abs nom erg dat adv}) (num sg)))
        (:o2 ;; "ღვინო"
         `((stem-type v) (num pl)))
        (:o3 ;; "დრო/დროის"
         `((stem-type c)
           (num sg)
           (case {gen dir inst})))
	(:oa ;;
         `((stem-type v) ;; MWE: ძველი აბაშა
           (case {nom gen inst})))
	(:oa1 ;;
         `((stem-type v)
           (case {dat adv})))

        (:r ;; "Hatv" masdar
         `((stem-type "ა")
           (rigid-stem -)))
        (:s ;; "Hatv"
         `((stem-type "ა")
           (sync-stem +)))
        (:x ;; "tramvai"
         `((stem-type "ი")))
	(:-
         `((stem-type "-")))))) 

#+obsolete
(defun load-root-table ()
  (clrhash *root-table*)
  (with-file-lines (line "projects:georgian-morph;root-lists.txt")
    (destructuring-bind (c-root . roots) (split (inv-convert line) #\space)
      (dolist (root roots)
        (pushnew c-root (gethash root *root-table*) :test #'string=)))))

;; overridden in verb-feature-table-sql.lisp
;;(load-root-table)

(defmethod match-stems ((dfa georgian-dfa) string pos)
  "Returns a list of pairs of stems matching string starting with pos and stem dgs."
  (collecting
    (loop for i from (1+ pos) to (length string)
          for stem = (subseq string pos i)
          do (dolist (features (gethash stem *noun-table*))
               #+debug(print (list stem features))
               (destructuring-bind (lemma conjugation comment . pos-list) features
                 (declare (ignore comment))
                 (dolist (pos pos-list)
                   (let ((pos (cond ((eq pos 'masd)
                                     'n)
                                    ((consp pos)
                                     (car pos))
                                    (t
                                     pos)))
                         (sub-cat (cond ((eq pos 'masd)
                                         'masd)
                                        ((consp pos)
                                         (cdr pos))
                                        (t
                                         nil))))
                     #+debug(print (list pos sub-cat))
		     (let ((cf (conjugation-features conjugation stem lemma pos sub-cat)))
		       (collect (cons (list-to-dg cf) i))
		       (when (and (find '(sync-stem -) cf :test #'equal)
				  (not (find '(rigid-stem -) cf :test #'equal)))
			 (collect (cons (list-to-dg 
					 `((sync-stem +)
					   ,@(unless (find 'style cf :test #'car)
						     (list '(style subnorm)))
					   ,@(remove '(sync-stem -) cf :test #'equal)))
					i)))))))))))

(defmethod match-roots ((dfa georgian-dfa) string pos)
  "Returns a list of pairs of roots matching string starting with pos and root dgs."
  (collecting
    (loop for i from (1+ pos) to (length string)
          for root = (subseq string pos i)
          do (when (gethash root *root-table*)
               (collect (cons (list-to-dg `((root ,root))) i))))))

#||
(pprint (u-transduce "mivdivar" *fst*))

(print (gethash (inv-convert "val") *root-table*))

(print (match-roots *fst* (inv-convert "cer") 0))

(pprint (u-transduce (inv-convert "vacer") *fst*))
(pprint (u-transduce "damecera" *fst*))

||#

;; no alternatives
(defun path-value (dg &rest path)
  (assert (null (car dg)))
  (cond ((null dg)
         nil)
        ((null path)
         (cdr dg))
        (t
         (apply #'path-value (cadr (find (car path) (cdr dg) :key #'car)) (cdr path)))))

(defmethod feature-string ((f string) &optional path plusp)
  (let* ((str (cond ((georgian-char-p (char f 0))
                    (string-capitalize (convert f)))
                   ((char= (char f 0) #\+)
                    (subseq f 1))
                   (t
                    (string-capitalize f))))
         (str (cond ((and (= (length path) 3)
                          (eq (car path) 'der)
                          (eq (cadr path) 'der))
                     (concat "DD" str))
                    ((eq (car path) 'der)
                     (concat "D" str))
                    (t
                     str))))
    (if plusp (concat "+" str) str)))

(defmethod feature-string ((f symbol) &optional path plusp)
  ;;(print (list f path))
  (let ((str (cond ((eq f '+)
                    (delete #\- (string-capitalize (car (last path)))))
                   ((eq f '-)
                    nil)
                   ((and (= (length path) 3)
                         (eq (car path) 'der)
                         (eq (cadr path) 'der))
                    (concat "DD" (string-capitalize f)))
                   ((eq (car path) 'der)
                    (concat "D" (string-capitalize f)))
                   (t
                    (string-capitalize f)))))
    (if plusp (concat "+" str) str)))

(defmethod feature-string ((f number) &optional path plusp)
  (declare (ignore path))
  (format nil "~a~d" (if plusp "+" "") f))

(defmethod feature-string ((f parser::extended-list) &optional path plusp)
  (declare (ignore path))
  (format nil "~a~{~a~}" (if plusp "+" "") (mapcar #'string-capitalize (parser::extended-list-form f))))

(defun xle-features (dg-list) ;;(print dg-list)
  (let ((paths '((case) (case-type) (pers) (num) (der num) (der der num) (old-pl)
                 (artic) (der case) (der der case) (pp) (der pp) (der der pp) (der-type) (mod-sfx) (rel-sfx))))
    (delete-duplicates
     (collecting
       (dolist (dg dg-list)
         (let ((features
                (collecting
                  (collect (cons '(cat) (or (apply #'path-value dg '(sub-cat))
                                            (apply #'path-value dg '(der sub-cat))
                                            (apply #'path-value dg '(der der sub-cat))
                                            (apply #'path-value dg '(cat)))))
                  (dolist (path paths)
                    (let ((value (apply #'path-value dg path)))
                      (when value (collect (cons path value))))))))
           (collect (list* (convert (or (path-value dg 'lex)
                                        (path-value dg 'der 'lex)
                                        (path-value dg 'der 'der 'lex)))
                           (format nil "~{+~a~}" (collecting
                                                   (mapc (lambda (path.feature)
                                                           (destructuring-bind (path . feature) path.feature
                                                             (let ((str (feature-string feature path)))
                                                               (when str (collect str)))))
                                                         features)))
                           nil
                           (when (path-value dg 'encl-qopna) t))))))
     :test #'equal)))

;; KARTULI-MASDARS is preliminary; they should be derived from the verb entries

(defun write-xle-noun-lexicon (stream &key part masdarp)
  (cond (part
         (format stream "STANDARD KARTULI-NOUNS-~d LEXICON (1.0)" part))
        (masdarp
         (format stream "STANDARD KARTULI-MASDARS LEXICON (1.0)"))
        (t
         (format stream "STANDARD KARTULI-NOUNS LEXICON (1.0)")))
  (write-char #\Linefeed stream)
  (write-char #\Linefeed stream)
  (let ((count 0))
    (with-file-lines (line "projects:georgian-morph;georgian-nouns.txt")
      (when (or (null part)
                masdarp
                (and (= part 1)
                     (<= count 20000))
                (and (= part 2)
                     (> count 20000)
                     (<= count 40000))
                (and (= part 3)
                     (> count 40000)
                     (<= count 60000))
                (and (= part 4)
                     (> count 60000)))
        (let* ((comment-start (position  #\# line))
               ;;(comment (when comment-start (subseq line comment-start)))
               (line (subseq line 0 comment-start)))
          (unless (eql comment-start 0)
            (destructuring-bind (stem conjugation . features) (string-parse line :whitespace ": ")
              (when (or (and masdarp (find-if (lambda (f) (search "masd" f)) features))
                        (and (not masdarp) (not (search "masd" features))))
                (destructuring-bind (stem &optional lemma) (split stem #\/)
                  (let* ((conjugation (intern (string-upcase conjugation) :keyword))
                         (nom-ending
                          (ecase conjugation
                            ((:A :A1 :B :B1 :P :U) "ი")
                            ((:C :D :F :I :K :M :M1 :O :O1 :O2 :R :X) "")))
                         (lemma (or lemma (concat stem nom-ending))) ;; lemma is nominative form
                         (ascii-lemma (convert lemma))
                         (foundp nil))
                    (unless (find conjugation '(:O2)) ; fix for Gvino 
                      (loop for (pos+features) on features with first = t
                            for pos = (subseq pos+features 0 (position #\+ pos+features))
                            when (find pos '("n" "a" "masd") :test #'string-equal)
                            do
                            (setf foundp t)
                            (unless first (write-char #\Linefeed stream))
                            (format stream "~a ~a XLE @(~a ~a);"
                                    (if first ascii-lemma "    ")
                                    ;; XLE lexical class
                                    (cond ((string-equal pos "n")
                                           "N")
                                          ((string-equal pos "masd")
                                           "N")
                                          ((string-equal pos "a")
                                           "A")
                                          (t
                                           (error "POS ~s not implemented." pos)))
                                    ;; template
                                    (cond ((string-equal pos "n")
                                           "NOUN")
                                          ((string-equal pos "masd")
                                           "MASDAR")
                                          ((string-equal pos "a")
                                           "ADJECTIVE")
                                          (t
                                           (error "POS ~s not implemented." pos)))
                                    ascii-lemma)
                            (setf first nil)
                            finally
                            (when foundp
                              (write-string " ETC." stream)
                              (write-char #\Linefeed stream)))))))))))
      (incf count)))
  (format stream "~c----~c" #\Linefeed #\Linefeed)
  nil)



;; main forms:
#+test
(with-open-file (stream "projects:xle;grammars;kartuli;kartuli-masdar-lex.lfg"
                        :direction :output :if-exists :supersede)
  (write-xle-noun-lexicon stream :masdarp t))
#+test
(progn
  (delete-file "projects:xle;grammars;kartuli;kartuli-noun-lex-1.lfg")
  (delete-file "projects:xle;grammars;kartuli;kartuli-noun-lex-2.lfg")
  (delete-file "projects:xle;grammars;kartuli;kartuli-noun-lex-3.lfg")
  (delete-file "projects:xle;grammars;kartuli;kartuli-noun-lex-4.lfg")
  (with-open-file (stream "projects:xle;grammars;kartuli;kartuli-noun-lex-1.lfg"
                          :direction :output :if-exists :supersede)
    (write-xle-noun-lexicon stream :part 1))
  (with-open-file (stream "projects:xle;grammars;kartuli;kartuli-noun-lex-2.lfg"
                          :direction :output :if-exists :supersede)
    (write-xle-noun-lexicon stream :part 2))
  (with-open-file (stream "projects:xle;grammars;kartuli;kartuli-noun-lex-3.lfg"
                          :direction :output :if-exists :supersede)
    (write-xle-noun-lexicon stream :part 3))
  (with-open-file (stream "projects:xle;grammars;kartuli;kartuli-noun-lex-4.lfg"
                          :direction :output :if-exists :supersede)
    (write-xle-noun-lexicon stream :part 4)))


:eof
