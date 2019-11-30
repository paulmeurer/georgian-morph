;;;-*- Mode: Lisp; Package: GNC.TEXT -*-

(in-package :gnc.text)

#+test
(cl-fst:fst-lookup *og-analyzer*
		   ;;"ველთაჲ"
		   "შუშანიკ"
		   (lambda (w l+f)
		     (print (list w l+f))))


(u:with-file-lines (part "projects:georgian-morph;lists;missing-past-part.txt")
  (unless (or (zerop (length part)) (char= (char part 0) #\#))
    (unless (find (u:last-char part) "აეიოუ")
      (setf part (u:concat part "ი")))
    (let ((part-ana nil))
      (cl-fst:fst-lookup *ng-analyzer*
			 part
			 (lambda (w l+f)
			   (declare (ignore w))
			   (dolist (ana (u:split l+f #\newline))
			     (when (search "+PastPart+Nom+Sg" ana)
			       (setf part-ana ana)))))
      (write-line (or part-ana part))
      )))


(let ((table (dat:make-string-tree)))
  (u:with-file-lines (part "~/lisp/projects/georgian-morph/lists/missing-past-part-analyzed.txt")
    (setf (dat:string-tree-get table (reverse part)) t))
  (dat:do-string-tree (part val table)
    (write-line (reverse part))))

;;; OLD STUFF

(in-package :fst)

#[future-participle
  = (seq preverb-group
         future-participle-stem
         (or [e ((stem-type c)
                     (case abs))]
             ["i" ((case nom)
                   (stem-type c))]
             ["×Ú" ((case adv)
                     (stem-type c))]
             (or [e ((stem-type v)
                     (case nom))]
                 ["Ú" ((case adv)
                        (stem-type v))])))]

(defparameter *future-participle-fst*
  (compile-u-transducer '(seq future-participle) :dfa-class 'georgian-dfa))

;; extract future participles

(defparameter *fut-part-fst*
  (compile-u-transducer
   `(seq preverb-group
         ,(utp-e '((finite -)
                   (stem-type c)
                   (tense fut-part)
                   (case-type full)
                   (subcat fut-part)))
         future-participle-stem)
   :dfa-class 'georgian-dfa))

#+test
(u-transduce "gasagebi" *future-participle-fst*)
#+test
(progn
  (with-file-lines (line "projects:georgian-morph;georgian-nouns.txt")
    (let* ((comment-start (position  #\# line))
           (line (subseq line 0 comment-start)))
      (unless (eql comment-start 0)
        (destructuring-bind (stem conjugation . features) (string-parse line :whitespace ": ")
          (when (u-transduce stem *future-participle-fst*)
            (print stem))))))
  (with-file-lines (line "projects:georgian-morph;part-fut-pass.txt")
    (let* ((comment-start (position  #\# line))
           (form (inv-convert (cadr (split line #\tab)))))
      (when (u-transduce form *future-participle-fst*)
        (print form)))))

;; output is in "filtered-part-fut-pass.txt"

(defparameter *count* nil)

(defun add-future-participles (&key (printp t))
  (let ((*count* (cons 0 0)))
    (dat:do-string-tree (c-root paradigm-list *parsed-verb-table*)
      (declare (ignore paradigm-list))
      (add-root-future-participles c-root :printp printp))
    *count*))

(defun add-root-future-participles (c-root &key printp)
  (let ((paradigm-list (dat:string-tree-get *parsed-verb-table* c-root))) 
    (dolist (paradigm (cdr paradigm-list))
      (unless (getf paradigm :ignore)
        (let* ((id (getf paradigm :id))
               (paradigm-features (cdr (paradigm-features id nil))))
          (when paradigm-features
            (destructuring-bind (root gv id pv . features) (car paradigm-features)
              (declare (ignore root pv features))
              (unless (find id '("2794-1") :test #'string=)
                (cond ((eq (char gv 0) #\T)
                       (build-fp-features paradigm-features :trans :printp printp))
                      ((or (eq 0 (search "MV" gv)) (eq 0 (search "RM" gv)))
                       (build-fp-features paradigm-features :medium :printp printp))
                      (t
                       nil))))))))))

(defparameter *filtered-part-fut-pass-table* (dat::make-string-tree))

#+test
(with-file-lines (line "projects:georgian-morph;filtered-part-fut-pass.txt")
  (setf (dat::string-tree-get *filtered-part-fut-pass-table* line) t))

;; (paradigm-features "2118-6")

#+test ;; main
(add-future-participles :printp nil)

; (2770 . 9185)
; (3552 . 12293)
; (3140 . 12705)
; (3650 . 12195)
; (3872 . 11973)
; (3956 . 11889)
; (4308 . 11534)

(defun check-attested-part-fut-pass (str)
  (let ((foundp (dat::string-tree-get *filtered-part-fut-pass-table* str)))
    (when foundp
      (setf (dat::string-tree-get *filtered-part-fut-pass-table* str) :found))
    foundp))

#+test
(dat::do-string-tree (string value *filtered-part-fut-pass-table*)
  (when (and (eq value t) (not (or (eq (search "×Ú" string :from-end t) (- (length string) 2))
                                   (eq (search "äÚ" string :from-end t) (- (length string) 2)))))
    (print string)))

(defun build-fp-features (paradigm-features class &key (printp t))
  (let* ((tense (ecase class
                  (:trans 'future)
                  (:medium 'present)))
         (fut/pres-features
          (remove-if-not (lambda (list)
                           (find-if (lambda (avp)
                                      (and (eq (car avp) 'tense)
                                           (or (eq (cadr avp) tense)
                                               (and (parser::extended-list-p (cadr avp))
                                                    (find tense (parser::extended-list-form (cadr avp)))))))
                                    (nthcdr 4 list)))
                         paradigm-features))
         (foundp nil)
         (participles
          (collecting
            (dolist (features-list fut/pres-features)
              (let* ((features (nthcdr 4 features-list))
                     (sf (get-feature-value 'sf features '-))
                     (sf (if (eq sf '-) "" sf))
                     (reduced-sf (cond ((string= sf "ß")
                                        "")
                                       ((string= sf "×â")
                                        "â")
                                       ((string= sf "×Ü")
                                        "")
                                       (t
                                        sf)))
                     (pv (get-feature-value 'pv features '-))
                     (pv (if (eq pv '-) "" pv))
                     (vn (get-feature-value 'vn features '-))
                     (root (car features-list))
                     (root (cond ((and (string= sf "×â")
                                       (char= (last-char root) #\Ü))
                                  (subseq root 0 (1- (length root))))
                                 (t
                                  root))))
                (flet ((sa-i () (concat pv "è×" root reduced-sf "ß"))
                       (sa-ad () (concat pv "è×" root reduced-sf "×Ú"))
                       (sa-av-i () (concat pv "è×" root "×Ü" "ß"))
                       (sa-av-ad () (concat pv "è×" root "×Ü" "×Ú"))
                       (sa-eli () (concat pv "è×" root reduced-sf "Ûáß"))
                       (sa-lad () (concat pv "è×" root reduced-sf "á×Ú"))
                       (sa-o () (concat pv "è×" root "ä"))
                       (sa-od () (concat pv "è×" root "äÚ"))
                       (sa-ari () (concat pv "è×" root "×çß"))
                       (sa-rad () (concat pv "è×" root "ç×Ú"))
                       (sa-ali () (concat pv "è×" root "×áß")))
                  (let ((sa-i (and (not (and (eq class :medium) (string= sf "äØ")))
                                   (not (string= sf "×â"))
                                   (or (check-attested-part-fut-pass (sa-i))
                                       (check-attested-part-fut-pass (sa-ad)))))
                        (sa-av-i (and (string= sf "×Ü")
                                      (or (check-attested-part-fut-pass (sa-av-i))
                                          (check-attested-part-fut-pass (sa-av-ad)))))
                        (sa-eli (and (find sf '("×â" "ÛØ" "äØ" "ß") :test #'string=)
                                     (not (and (eq class :medium) (string= sf "äØ")))
                                     (or (check-attested-part-fut-pass (sa-eli))
                                         (check-attested-part-fut-pass (sa-lad)))))
                        (sa-o (and (eq class :medium)
                                   (find sf '("äØ") :test #'string=)
                                   (or (check-attested-part-fut-pass (sa-o))
                                       (check-attested-part-fut-pass (sa-od)))))
                        (sa-ari (and (eq class :medium)
                                     (find sf '("ß") :test #'string=)
                                     (not (find #\ç root))
                                     (or (check-attested-part-fut-pass (sa-ari))
                                         (check-attested-part-fut-pass (sa-rad)))))
                        (sa-ali (and (eq class :medium)
                                     (find sf '("ß") :test #'string=)
                                     (find #\ç root)
                                     (or (check-attested-part-fut-pass (sa-ali))
                                         (check-attested-part-fut-pass (sa-lad))))))
                    (setf foundp (or sa-i sa-av-i sa-eli sa-o sa-ari sa-ali))
                    ;; collect possible forms;
                    ;; t : attested + probable
                    ;; 't- attested + improbable
                    ;; '+ unattested + probable
                    ;; '- unattested + improbable
                    (when (not (and (eq class :medium) (string= sf "äØ")))
                      (collect (list (sa-i) class
                                     (if (or (and (string= sf "ß")
                                                  (not (find-if (lambda (c) (find c root)) "×Ûßäê")))
                                             #+test
                                             (and (string= sf "ÛØ")
                                                  (find-if (lambda (c) (find c root)) "×Ûßäê"))
                                             #+test
                                             (string= sf "×Ü"))
                                       (if sa-i 't- '-)
                                       (if sa-i t '+))
                                     :sf reduced-sf :vn vn :pf-sfx '- :pv pv :root root)))
                    (when (string= sf "×Ü")
                      (collect (list (sa-av-i) class (or sa-av-i '+) :sf sf :vn vn :pf-sfx '- #+ignore "×Ü" :pv pv :root root)))
                    (when (and (find sf '("×â" "ÛØ" "äØ" "ß") :test #'string=)
                               (not (and (eq class :medium) (string= sf "äØ"))))
                      (collect (list (sa-eli) class
                                     (if (or (find sf '("" "äØ") :test #'string=)
                                             (and (string= sf "ß")
                                                  (find-if (lambda (c) (find c root)) "×Ûßäê"))
                                             (and (string= sf "ÛØ")
                                                  (not (find-if (lambda (c) (find c root)) "×Ûßäê"))))
                                       (if sa-eli 't- '-)
                                       (if sa-eli t '+))
                                     :sf reduced-sf :vn vn :pf-sfx "Ûá" :pv pv :root root)))
                    (when (and (eq class :medium)
                               (find sf '("äØ") :test #'string=))
                      (collect (list (sa-o) class (or sa-o '+) :sf "" :vn vn :pf-sfx "ä" :pv pv :root root)))
                    (when (and (eq class :medium)
                               (find sf '("ß") :test #'string=)
                               (not (find #\ç root)))
                      (collect (list (sa-ari) class (or sa-ari '+) :sf "" :vn vn :pf-sfx "×ç" :pv pv :root root)))
                    (when (and (eq class :medium)
                               (find sf '("ß") :test #'string=)
                               (find #\ç root))
                      (collect (list (sa-ali) class (or sa-ali '+) :sf "" :vn vn :pf-sfx "×á" :pv pv :root root))))))))))
      (when (and printp participles)
        (format t "(~{~s~^ ~})~%" (delete-duplicates participles :test #'equal)))
      (if foundp
        (incf (car *count*))
        (incf (cdr *count*)))
      (destructuring-bind (root gv id pv . features) (car paradigm-features)
        (declare (ignore features))
        (when t ;; (eq (char gv 0) #\T) ;; only transitives have periphrastic perfects ;; oh no
          ;;(print paradigm)
          (dolist (participle-list (delete-duplicates participles :test #'equal))
            (pushnew (list gv id pv
                           '(tense future-part)
                           `(vn ,(getf (cdr participle-list) :vn))
                           `(pv ,(let ((pv (getf (cdr participle-list) :pv)))
                                   (if (equal pv "") '- pv)))
                           ;;,@(when pv-p `((pv ,(get-feature-value 'pv features '-))))
                           `(pf-sfx ,(getf (cdr participle-list) :pf-sfx))
                           `(root ,root)
                           `(sf ,(let ((sf (getf (cdr participle-list) :sf)))
                                   (if (equal sf "") '- sf))))
                     (gethash root *features-table*)
                     :test #'override-features-equal))))))

;;; masdar

;; extract masdars

(defparameter *masdar-fst*
  (compile-u-transducer
   `(seq preverb-group
         ,(utp-e '((finite -)
                   (tense masdar)
                   (subcat masdar)))
         masdar
         (or ,(utp "ß" '((stem-type c)))
             ,(utp-e '((stem-type v)))))
   :dfa-class 'georgian-dfa))

#+test
(u-transduce "âäàÜÚäâ×" *masdar-fst*)
#+test
(u-transduce "óÛç×" *masdar-fst*)
#+test-copy
(progn
  (with-file-lines (line "projects:georgian-morph;georgian-nouns.txt")
    (let* ((comment-start (position  #\# line))
           (line (subseq line 0 comment-start)))
      (unless (eql comment-start 0)
        (destructuring-bind (stem conjugation . features) (string-parse line :whitespace ": ")
          (when (u-transduce stem *future-participle-fst*)
            (print stem))))))
  (with-file-lines (line "projects:georgian-morph;part-fut-pass.txt")
    (let* ((comment-start (position  #\# line))
           (form (inv-convert (cadr (split line #\tab)))))
      (when (u-transduce form *future-participle-fst*)
        (print form)))))

(defparameter *count* nil)

(defparameter *missing-masdars* ())

#+result
("×èå×çÛÝäØ×" "×ïßàäØ×" "×õá" "×ö×" "Ø×Ý×ÝäØ×" "Ø×ãÚ" "â×ã×ÚßéÛØ×" "Ø×ç" "Ø×çÙ"
 "Ø×çÙ×ãß" "Ø×ìß" "Ø×íãäØ×" "Ø×óÜç×" "ØÙäáÜ×" "ØÚÜßãÜ×" "ØÛØàÜ×" "ØÛÙÜç×"
 "ØÛÚß×ãÛØß" "ØÛç" "ØÛçßà×ñäØß" "ØÛçéî" "ØÝê" "Øßç" "Øá×çÚã×" "Øçò" "ØçôäØ×" "Øê"
 "ØêâØÜá×" "ØêçòÙÜá×" "Øí×Üßáß" "Øíêßáß" "ÙÛèÜá×" "ÙÜ×ç" "ÙÜÛâ×" "ÙÜßçÙÜßãÛØ×"
 "Ùâßç" "Ú×ëÜã×" "ÚÙ" "ÚÛÚ" "ÚçêÝÜ×" "Úêâßáß" "ÚêçÞÜ×" "Úêíßáß" "Úí×Øã×" "ÚíáÛÝ×"
 "ÚíáêÜÛØ×" "Ûã×äØ×" "Ü×ãÛØ×" "Ü×èõÛØ×" "ÜÛÝßçäØ×" "ÜÛçñõÜá×" "ÜÛïÛØ×" "ÜáéäáÜ×"
 "ÜçñäØ×" "Ý×ç×ëäØ×" "Ý×õßáß" "ÝÛÜÛØ×" "Ýâêßáß" "ÝäçÜ×" "ÝêØÜç×" "ÝêÞõÜ×" "Ýêßáß"
 "Þ×ÜßèóßãÛØ×" "Þ×ÞìêãÛØ×" "Þ×ãÙÜ×" "Þ×çÙâ×ãÛØ×" "Þ×çèÜ×" "Þ×ëÜá×" "Þ×ìÛØ×" "ÞÛÞìÜ×"
 "ÞÛèÜá×" "ÞãÛÜ×" "ÞçßâÜá×" "ÞêÛØ×" "ÞêÞõã×" "ÞõÛÜ×" "ÞõÜç×" "ÞõáÛï×" "ÞõêãÜ×" "ß×"
 "à×çàÜá×" "àÛÞßáäØ×" "àÛãóÜá×" "àÛç×éÛØ×" "àÜ×âÜá×" "àÜ×ç×õôßãÛØ×" "àÜá×ÚÛØ×"
 "àßÜßáß" "àßÞõÜßãÛØ×" "àßçñõÜá×" "àßçóîÜá×" "àáÞäáÜ×" "àã×Üßáß" "àäãóß×áÛØ×"
 "àäèÛØ×" "àçàäá×" "àêßáß" "àêåÜç×" "àêçñõÜá×" "àêëõÜá×" "á×á×äØ×" "á×ïìÜç×"
 "áßÝíÛØ×" "áäÞßëäÞäØ×" "áäåÜç×" "áéäØ×" "áéäáÜ×" "â×çÙÜá×" "â×çÞáêáäØ×" "âßã×çÛäØ×"
 "âÛóîÜç×" "âßÝÜã×" "âêçÚÜá×" "âêçéÜá×" "âêõÜá×" "âðßÜãÛØ×" "âñçäØ×" "ã×ÞÜá×"
 "ã×âìÜç×" "ã×çðêãÛØ×" "ã×éÜç×" "ã×ñÜç×" "ã×óîÛãÛØ×" "ãßäçóîáÛØ×" "ãßõÜç×" "ãäâÜç×"
 "ãìçÛÜ×" "äçÞìÜá×" "äìÜç×" "äõÜç×" "å×çôîÜ×" "åÛåÜá×" "åßçêéîÜÛØ×" "åá×àÜ×"
 "åáêéäØ×" "åçäóß×áß" "âçäóß×áÛØ×" "åêÚÜç×" "æßÜßáß" "æßãæíÜá×" "æí×Üßáß" "æíßÜßáß"
 "çØÛãßãÛØ×" "çÛàÜá×" "çßà" "çßãÛØ×" "çö×" "è×ÙçäØ×" "è×Ü×ãÛØ×" "è×ãÙÜç×" "è×ãÞÜá×"
 "è×ãèÜá×" "è×åÜã×" "è×çéîÜá×" "è×êØ×çß" "è×ñäÚ×ÜÛØ×" "è×õÜç×" "è×õéÛØ×" "èß×çêáß"
 "èßÜßáß" "èàßãéÜá×" "èêÚÜç×" "èêãèÜá×" "èêèÜã×" "èó×ÜáÛØßãÛØ×" "èõ×åêãÛØ×" "èõÜá×"
 "é×éî×ãß" "éÜßëÜç×" "éßçßáß" "éßõÜç×" "éáÛà×" "éáäïã×" "éäçéÜá×" "éçßÝ×ÜÛØ×"
 "éîÜÛÜã×" "éîá×çôÜ×" "êñßáäØáäØ×" "ë×çÙÜá×" "ë×éÜç×" "ë×ïÜç×" "ëßáéÜç×" "ëßìÜá×"
 "ëßñÜç×" "ëßóÜá×" "ëäàêèäØ×" "ëêßáß" "ëêçñÜá×" "ëêíÜç×" "ëïÜ×" "ëõêßáß" "ì×ÚãÛØ×"
 "ì×ç×ÙâÛØ×" "ìßãÚÜç×" "ìßèÜ×" "ìáß×ÜÛØ×" "ìäìÜá×" "ìäïßïÛØ×" "ìêçöÜ×" "ìêçöã×"
 "ìêèÜá×" "ìêëÜç×" "ìêñã×" "ìêõßáß" "ìïêßáß" "íÜ×çèÜá×" "íÜçÛô×" "íâêçòÜá×"
 "íã×Üßáß" "íêßáß" "ÚêïÜ×" "î×Üî×Üß" "î×Üî×ÜÛØ×" "î×Ý×õäØ×" "î×ÞÜç×" "î×î×ãß"
 "î×î×ãÛØ×" "î×öÜç×" "îÜßçßáß" "îßÜßáß" "îâêßáß" "îêçîêéß" "îêçîêéÛØ×" "îêðßáß"
 "ï×ãïÜá×" "ï×ìÜç×" "ïßÙÜã×" "ïßëÜç×" "ïâêïÜã×" "ïêßáß" "ïõßÜßáß" "ð×ðõ×ãäØ×"
 "ðÜßãÞÜç×" "ðßÜßáß" "ðßçÞßëßçÞäØ×" "ðêØ×ðäØ×" "ðêâ×ÞÛØ×" "ðõ×Üßáß" "ðõßÜßáß"
 "ðõßàã×" "ðõßçàÛÚÛá×äØ×" "ñÛñõÜá×" "ñßØêç×äØ×" "ñßãñÜá×" "ñçÛâÜá×" "ñçêåÛãéÛá×äØ×"
 "ò×ØÜç×" "ò×ÙÜç×" "òäÜã×" "ó×åã×" "óÛÜã×" "óßÚÜã×" "óßÜßáß" "óßãóàÜá×" "óßõÜá×"
 "óà×Üßáß" "óàâêßáß" "óàã×Üßáß" "óàêâåÜá×" "óÜá×" "óâ×èã×" "óäã×èóäçÛØ" "óêçÞÜã×"
 "óîÛÜá×" "óîáÜ×" "ôÜ×çéÜá×" "ôßÜßáß" "ôßàã×" "ôßãôÜç×" "ôßçõÜá×" "ôãäØ×" "ôî×Üßáß"
 "ôîÛç×" "ôîÜßçßáß" "ôîßÜßáß" "ôîáêçéÜ×" "ôîêçéÜ×" "õ×Üßáß" "õ×ÝâêÝß×ãäØ×" "õ×ãõÜá×"
 "õ×ãöÜá×" "õ×èäØ×" "õÛáâçêÚÛäØ×" "õÜßÜßáß" "õßÜßáß" "õßâ×ãÛØ×" "õáêçðêãÛØ×" "õÜã×"
 "õäéÜç×" "õÜç×" "õçÛà×" "õêÞÜç×" "õêßáß" "õêãéçêñäØ×" "ö×Øã×" "ö×ØãÛØ×" "ö×ÙÜ×"
 "ö×ÙÜã×" "ö×ãÚÜç×" "ö×ãöáäØ×" "ö×ãöáÛØ×" "ö×ïêïäØß" "ö×öí×ãß" "öÛàÜ×" "öâ×" "öáÛõ×"
 "öí×Üßáß")

(defun add-masdars (&key (printp t))
  (setf *missing-masdars* ())
  (let ((*count* (cons 0 0)))
    (dat:do-string-tree (c-root paradigm-list *parsed-verb-table*)
      (declare (ignore paradigm-list))
      (add-root-masdars c-root :printp printp))
    *count*))

#+test
(u-transduce "Ø×ìß×äØ×" *masdar-fst*)
#+test
(u-transduce "ÜØ×ìß×äØ" *fst*)

(defun add-root-masdars (c-root &key printp)
  (let ((paradigm-list (dat:string-tree-get *parsed-verb-table* c-root))) 
    (dolist (paradigm (cdr paradigm-list))
      (unless (getf paradigm :ignore)
        (let* ((id (getf paradigm :id))
               (paradigm-features (cdr (paradigm-features id nil))))
          (when paradigm-features
            (destructuring-bind (root gv id pv . features) (car paradigm-features)
              (declare (ignore root pv features))
              (unless (find id '("2794-1") :test #'string=)
                (build-masdar-features paradigm-features id :printp printp)
                #+ignore
                (cond ((eq (char gv 0) #\T)
                       (build-masdar-features paradigm-features id :trans :printp printp))
                      ((or (eq 0 (search "MV" gv)) (eq 0 (search "RM" gv)))
                       (build-masdar-features paradigm-features id :medium :printp printp))
                      (t
                       nil))))))))))

#+test
(add-masdars :printp nil)

;; to do: add perfective/imperfective
(defun build-masdar-features (paradigm-features id &key (printp t))
  (let ((preverbs ())
        (masdar nil)
        (gv nil))
    (dolist (tense-features paradigm-features)
      (destructuring-bind (root genus-verbi id pv . features) tense-features
        (declare (ignore root id pv))
        (setf masdar (cadr (assoc 'vn features))
              gv genus-verbi)
        (pushnew (or (cadr (assoc 'pv features)) '-) preverbs :test #'equal)
        #+test(print (list root id masdar preverbs))))
    (unless (find #\# masdar)
      (let ((features (car (u-transduce masdar *masdar-fst*))))
        (unless features
          (pushnew masdar *missing-masdars* :test #'equal)
          #+debug(print masdar)
          #+debug(print paradigm-features))
        (when printp (print (cons masdar features)))
        (dolist (pv preverbs)
          (let ((root (cdr (get-feature-value 'root features '(nil . -))))
                (sf (cdr (get-feature-value 'sf features '(nil . -))))
                (stem-type (cdr (get-feature-value 'stem-type features '(nil . c)))))
            (pushnew (list gv id pv
                           '(tense masdar)
                           `(vn ,masdar)
                           `(pv ,pv)
                           `(root ,root)
                           `(sf ,sf)
                           `(stem-type ,stem-type))
                     (gethash root *features-table*)
                     :test #'override-features-equal)))))))

:eof