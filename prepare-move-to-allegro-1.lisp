;;;-*- Mode: Lisp; Package: FSTN -*-
;;;

;; needs (mk :georgian-parser)

(in-package :fstn)

;;; transducers: fsa:georgian-parser;noun-morphology.lisp, fsa:georgian-parser;verb-morphology.lisp

(defconstant +newline+ #\Return #+ignore #\Linefeed) ;; #\Return

(defmethod write-one-entry-to-stream ((w dictionary-window) &key id stream (goto-next t))
  (let (entry 
        language
        (enter-view (enter-view w)))
    (when goto-next
      (loop do
            (multiple-value-setq
              (entry language)
              (display-tree-entry w 'dict:get-next-entry :update nil))
            when (string= (car entry) "÷ê÷!")
            do (return-from write-one-entry-to-stream)
            until (and (find #\­ (car entry))
                       (not (dialect/alias-entry-p enter-view)))))
    (dict::set-text (view-named 'search w) (car entry) language)
    (replace-fake-hyphens-and-slashes enter-view)
    (remove-braces enter-view)
    (expand-infinitives enter-view)
    ;(v-expand-abbreviations enter-view)
    (set-mark (fred-buffer enter-view) 0)
    (format stream "---------------- ~d ----------------~%" id)
    (write-line (car entry) stream) (terpri stream)
    (write-line (buffer-substring (fred-buffer enter-view) (buffer-size (fred-buffer enter-view))) stream)
    #+disabled(parse-dict-entry w)
    (fred-update enter-view)
    (car entry)))

#+once-only
(with-open-file (stream "projects:georgian-morph;verb-entries.txt" :direction :output :if-exists :supersede)
  (loop for i from 1
        while (write-one-entry-to-stream (find-if #'(lambda (w) (string= (window-title w) "Tschenkli"))
                                                  (windows :class 'dictionary-window))
                                         :stream stream :id i)))



(defclass string-buffer ()
  ((string :initform nil :initarg :string :reader buffer-string)
   (pos :initform 0 :initarg :pos :accessor buffer-pos)))

(defun get-verb-entry (&key (file "projects:georgian-morph;verb-entries.txt") file-pos root entry-nr)
  (with-open-file (stream file)
    (cond (file-pos
           (file-position stream file-pos)
           (when (zerop file-pos)
             (read-line stream nil nil))
           (read-line stream nil nil)
           (read-line stream nil nil))
          (root
           (loop for line = (read-line stream nil nil)
                 with prev-line = ""
                 while line
                 until (and (search "--------" prev-line)
                            (string= line root))
                 do (setf prev-line line))
           (read-line stream nil nil))
          (entry-nr
           (loop for line = (read-line stream nil nil)
                 while line
                 until (zerop entry-nr)
                 when (search "--------" line)
                 do (decf entry-nr))
           (read-line stream nil nil)))
    (let ((entry (with-output-to-string (string-stream)
                        (loop for line = (read-line stream nil nil)
                              while line
                              until (search "--------" line)
                              do (write-line line string-stream)))))
      (values (make-instance 'string-buffer :string entry)
              (file-position stream)
              entry))))

#+test
(get-verb-entry :file-pos 187)

#+test
(let ((mark (make-instance 'string-buffer :string "asdfasdf
asdfasdf
asdfasdff")))
  (%buffer-line-end mark 5 3))

(defmethod %buffer-size ((mark string-buffer))
  (length (buffer-string mark)))

(defmethod %set-mark ((mark string-buffer) pos)
  (setf (buffer-pos mark) pos))

(defmethod %move-mark ((mark string-buffer) &optional (dist 1))
  (incf (buffer-pos mark) dist))

(defmethod %buffer-char ((mark string-buffer) &optional pos)
  (char (buffer-string mark) (%buffer-position mark pos)))

(defmethod %buffer-substring ((mark string-buffer) &optional (left 0) right)
  (if (< left right)
    (subseq (buffer-string mark) left right)
    (subseq (buffer-string mark) right left)))

(defmethod %buffer-position ((mark string-buffer) &optional pos)
  (cond ((integerp pos)
         (assert (<= 0 pos (%buffer-size mark)))
         pos)
        ((eq pos t)
         (%buffer-size mark))
        ;; 'pos is a mark' is missing
        ((null pos)
         (buffer-pos mark))))

(defmethod %buffer-line-end ((buf string-buffer) &optional startpos (count 0))
  (multiple-value-bind (pos left) (%buffer-line-start buf startpos (1+ count))
    ;;(print (list pos left startpos count)) 
    (if (null left)
        (if (zerop pos) (setq left 1) (setq pos (- pos 1)))
        (setq left (+ left (if (minusp count) 1 -1))))
    (values pos (if (eq left 0) nil left))))

(defmacro while (test &body body)
  (let ((testlab (gensym))
        (toplab (gensym)))
    `(tagbody
       (go ,testlab)
      ,toplab
      (progn ,@body)
      ,testlab
      (when ,test (go ,toplab)))))

(defmethod %buffer-line-start ((mark string-buffer) &optional start count)
  (with-slots (string) mark
    (if (null count) (setq count 0)
        (require-type count 'fixnum))
    (setq start (%buffer-position mark start))
    (let* ((end (length string))
           (pos start))
      (declare (fixnum count start end))
      (cond ((<= count 0)
             (setq end start start 0)
             (while (<= count 0) 
               (setq pos 
                     (position +newline+ string :start start :end (max (1- end) 0) :from-end t)
                     #+ignore
                     (%buffer-backward-find-char/not buf +newline+ start end nil))
               (when (not pos)
                 (return-from %buffer-line-start
                   (values 0 (if (zerop count) nil count))))
               (setq count (1+ count))
               (setq end pos))
             (values (1+ end) nil))
            (t (block nil
                 (while (> count 0)
                   (setq pos (position +newline+ string :start start :end end)
                         #+ignore(%buffer-forward-find-char/not buf +newline+ start end nil))
                   (when (not pos)(setq start end)(return))
                   (setq count (1- count))
                   (setq start (1+ pos))))
               (values start (if (zerop count) nil count)))))))

#+copy
(defun buffer-string-pos (buf string &key start end from-end &aux pos)
  (setq start (buffer-position buf (or start (if from-end 0)))
        end (buffer-position buf (or end (if (not from-end) t))))
     (cond ((> start end) (error "start > end: ~S ~S" start end))
         (from-end ;search backwards
          (buffer-backward-search buf string start end))
         (t ;search forward
           (setq pos (buffer-forward-search buf string start end))
           (if pos 
               (- pos (if (characterp string) 1 (length (string string))))))))

#+copy
(defun buffer-char-pos (buf char-or-string &key start end from-end &aux pos)
  (setq start (buffer-position buf (or start (if from-end 0)))
        end (buffer-position buf (or end (if (not from-end) t))))
    (cond ((> start end) (error "start > end: ~S ~S" start end))
         (from-end ;search backwards
          (buffer-backward-find-char buf char-or-string start end))
         (t ;search forward
           (setq pos (buffer-forward-find-char buf char-or-string start end))
           (if pos 
               (- pos 1)))))

(defmethod %buffer-string-pos ((buf string-buffer) str &key start end from-end)
  (with-slots (string) buf
    (setq start (%buffer-position buf (or start (if from-end 0)))
          end (%buffer-position buf (or end (if (not from-end) t))))
    (cond ((> start end) (error "start > end: ~S ~S" start end))
          (from-end ;search backwards
           (search (string str) string :start2 start :end2 end :from-end t))
          (t
           (search (string str) string :start2 start :end2 end)))))

(defmethod %buffer-char-pos ((buf string-buffer) char-or-string &key start end from-end)
  (with-slots (string) buf
    (setq start (%buffer-position buf (or start (if from-end 0)))
          end (%buffer-position buf (or end (if (not from-end) t))))
    (cond ((> start end) (error "start > end: ~S ~S" start end))
          (from-end ;search backwards
           (position-if (lambda (c)
                          (find c (string char-or-string)))
                        string :start start :end end :from-end t)
           #+ignore
           (buffer-backward-find-char buf char-or-string start end))
          (t ;search forward
           (position-if (lambda (c)
                          (find c (string char-or-string)))
                        string :start start :end end)))))

(defmethod %buffer-not-char-pos ((buf string-buffer) char-or-string &key start end from-end)
  (with-slots (string) buf
    (setq start (%buffer-position buf (or start (if from-end 0)))
          end (%buffer-position buf (or end (if (not from-end) t))))
    #+debug(print (list :%buffer-not-char-pos char-or-string start end from-end))
    (cond ((> start end) (error "start > end: ~S ~S" start end))
          (from-end ;search backwards
           (position-if (lambda (c)
                          (not (find c (string char-or-string))))
                        string :start start :end end :from-end t)
           #+ignore
           (buffer-backward-find-char buf char-or-string start end))
          (t ;search forward
           (or (position-if (lambda (c)
                              (not (find c (string char-or-string))))
                            string :start start :end end)
               #+ignore(1+ end))))))

(defconstant $space-and-return (coerce (list #\Space #\Newline #\Linefeed) 'string)) ;; #.(format nil " ~%"))

(defmacro do-%buffer-words ((mark left right &key from to boundary) &body body)
  (let ((start (gensym))
        (end (gensym))
        (b (gensym)))
    `(let* ((,b (or ,boundary $space-and-return))
            (,start (min (or (%buffer-not-char-pos  
                              ,mark ,b :start (or ,from (%buffer-position ,mark)))
                             (%buffer-size ,mark))
                         (or ,to (%buffer-size ,mark))))
            (,end (1+ (or (%buffer-not-char-pos ,mark ,b
                                                :end (or ,to (%buffer-size ,mark))
                                                :start ,start :from-end t)
                          (1- (or ,to (%buffer-size ,mark)))))) 
            (,left ,start))
       (%set-mark ,mark ,left)
       (let ((,right ,start))
         (loop 
           until (= ,right ,end)
           do (setf ,right (or (%buffer-char-pos ,mark ,b
                                                 :start (min ,left ,end) :end ,end) 
                               ,end))
           (%set-mark ,mark (min (1+ ,right) ,end)) 
           ,@body
           do
           (setf 
            ,end (min ,end (%buffer-size ,mark)) ; new 12.12.95
            ,left (or (%buffer-not-char-pos ,mark ,b 
                                            :start ,right :end ,end) ,end)))))))


(defmacro do-%buffer-lines ((mark left right &optional size &key from to) &body body)
  (let ((size (or size (gensym))) 
        (stop (gensym)))
    `(let ((,size (%buffer-size ,mark))
           (,left (%buffer-position ,mark)))
       ,(when from 
          `(progn (%set-mark ,mark ,from)
                  (setf ,left ,from)))
       (let (,right (,stop nil))
         (loop 
           until (or ,stop (>= (1+ ,left) ,size))
           do
           (setf ,stop (>= (setf ,right (min (%buffer-line-end ,mark (1+ ,left))
                                             (or ,to ,size)))
                           (or ,to ,size)))
           (%set-mark ,mark (min ,right ,size))
           ,@body
           do
           (setf ,left ,right))))))

(defmethod get-global-annotations ((mark string-buffer))
  (let* ((line-end (%buffer-line-end mark 0))
         (right-brace (%buffer-char-pos mark #\) :end line-end :from-end t))
         (left-brace (when right-brace 
                       (%buffer-char-pos mark #\( :end right-brace :from-end t)))
         (slash (%buffer-char-pos mark #\/ :end line-end :from-end t))
         (ancient (%buffer-char-pos mark #\  :start (or slash 0) :end line-end))
         annotations)
    (when left-brace
      (do-%buffer-words (mark left right :from left-brace :to (1+ right-brace))
        (let ((ann (%buffer-substring mark left right))) 
          (unless (find-if #'dict:georgian-char-p ann)
            (push ann annotations)))))
    (when (%buffer-string-pos mark " s. " :start 0 :end line-end)
      (push "s." annotations))
    (when ancient (push " " annotations))
    (nreverse annotations)))

#+test
(get-global-annotations (get-verb-entry :root "×Ü×Úâîäë­")) ;:entry-nr 1190))

(defmethod buffer-next-word ((mark string-buffer) &key 
                                (start (%buffer-position mark))
                                (boundary-chars $space-and-return))
  (let* ((word-start
          (or (%buffer-not-char-pos mark boundary-chars :start start)
              (%buffer-size mark)))
         (word-end 
          (or (%buffer-char-pos mark boundary-chars :start word-start)
              (%buffer-size mark)))
         (second-word-start 
          (or (%buffer-not-char-pos mark boundary-chars :start word-end)
              (%buffer-size mark))))
    (values (%buffer-substring mark word-start word-end)
            second-word-start)))

(defmethod buffer-georgian-start ((mark string-buffer) &key (start (%buffer-position mark)) end)
  (let ((georgian-start
         (%buffer-char-pos mark $georgian-characters+tilde :start start :end end)))
    (when (and georgian-start 
               (< start georgian-start)
               (find (%buffer-char mark (1- georgian-start)) "()[]"))
      (decf georgian-start))
    georgian-start))

(defmethod buffer-get-genus-verbi ((mark string-buffer) start) 
  ;(format t "genus-verbi-start ~a~%" (buffer-substring mark start (+ start 25)))
  (let (gv str stop end (first t))
    (do-%buffer-words (mark left right :from start)
      (setf str (%buffer-substring mark left right))
      (cond ((or first
                 (find (string-trim "()" str) '("OR" "ohne" "i.O." "nur" "mit" "FR")
                       :test 'string=)) 
             (push str gv))
            ((find str '("a)" "b)" "c)" "d)" "e)") :test 'string=)
             nil)
            (t (setf stop t)))
      (setf first nil)
      until stop
      finally (setf end left))
    (values (concatenate-word-list (reverse gv)) end)))

(defmethod find-closing-brace ((mark string-buffer) &optional 
                                   (start (%buffer-position mark)) (end (%buffer-size mark))
                                   (niveau 0))
  "finds the right closing brace <niveau> bracing niveaus down; e.g. if <niveau> = 1, the method will
find the brace after b in < (a) b) c)>"
  (%set-mark mark start)
  (loop with count = niveau
        do
        (case (%buffer-char mark)
          (#\( (incf count))
          (#\) (decf count))
          (otherwise nil))
        (when (= end (%buffer-position mark)) (return-from find-closing-brace)) 
        until (zerop count)
        do (%move-mark mark))
  (%buffer-position mark))

(defmethod find-opening-brace ((mark string-buffer) &optional 
                                   (start 0) (end (%buffer-position mark)) (niveau 0))
  "finds the left opening brace <niveau> bracing niveaus down; e.g. if <niveau> = 1, the method will
find the brace before b in <(a (b (c> starting from the right side"
  (set-mark mark end)
  (loop with count = niveau
        do
        (case (%buffer-char mark)
          (#\) (incf count))
          (#\( (decf count))
          (otherwise nil))
        (when (= start (%buffer-position mark)) (return-from find-opening-brace)) 
        until (zerop count)
        do (%move-mark mark -1))
  (%buffer-position mark))

(defmethod buffer-georgian-end ((mark string-buffer) &key (start (%buffer-position mark))
                                    end (additional-chars " ª[]~ Â-!?"))
  (%set-mark mark (%buffer-not-char-pos mark (if additional-chars
                                               (concatenate 'string 
                                                            dict::$georgian-characters
                                                            additional-chars)
                                               dict::$georgian-characters) :start start))
  (let ((g-end (%buffer-position mark))
        brace-end)
    (if (and (char= #\( (%buffer-char mark))
             (%buffer-char-pos
              mark dict::$georgian-characters 
              :start g-end
              :end (setf brace-end (find-closing-brace mark g-end))) 
             (not (%buffer-char-pos
                   mark "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" ; 
                   :start g-end
                   :end brace-end)))
      (buffer-georgian-end mark :start (1+ brace-end) :end end)
      (if (char= #\, (%buffer-char mark g-end))
        (+ 2 g-end)
        g-end))))

(defmethod extract-verb ((mark string-buffer) roots start end)
  (when (string-member (%buffer-substring mark start end) '("Â " "Â"))
    (return-from extract-verb "Â"))
  (let (verb-start verb-end)
    (do-%buffer-words (mark left right :from start :to end)
      (loop for root in roots with trimmed-root
            do (setf trimmed-root (string-right-trim "±²³´µ¶·¸¹" root))
            (when (%buffer-string-pos mark trimmed-root :start left :end right)
              (setf verb-start (or verb-start left)
                    verb-end (if verb-end (max verb-end right) right)))
            (when (and (%buffer-char-pos mark #\( :start left :end right) 
                       (search trimmed-root
                               (remove #\( (remove #\) (%buffer-substring mark left right)))))
              (setf verb-start (or verb-start left)
                    verb-end (if verb-end (max verb-end right) right))))
      finally (return (%buffer-substring mark (or verb-start left) (or verb-end right))))))

(defmethod %tenses-only ((mark string-buffer) &key start end)
  (not (%buffer-char-pos
        mark "beghijklmnqsvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" ; allows fut, aor, pf, opt, cd
        :start start :end end)))

(defmethod %aor-only ((mark string-buffer) &key start end)
  (and (> end (+ start 4))
       (not (%buffer-char-pos  
             mark "bcdeghijklmnqstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
             :start start :end end))
       (not (%buffer-string-pos mark "pr" :start start :end end)))) ; no pr either

(defmethod %fut-only ((mark string-buffer) &key start end)
  (not (%buffer-char-pos  
        mark "bcdeghijklmnqsvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" ; allows aor and pf
        :start start :end end)))

(defmethod extract-aor-pf-impf-opt-cond ((mark string-buffer) &key start end)
  (format t "~%all-extract: ~a ~a ~s~%" start end (%buffer-substring mark start end))
  (let* ((aor-start (list (%buffer-string-pos mark "aor " :start start :end end) :aor))
         (impf-start (list (%buffer-string-pos mark "impf " :start start :end end) :impf))
         (pf-start (list (%buffer-string-pos mark " pf " :start start :end end) :pf))
         (fut-start (list (%buffer-string-pos mark "fut " :start start :end end) :fut))
         (opt-start (list (%buffer-string-pos mark " opt " :start start :end end) :opt))
         (cond-start (list (%buffer-string-pos mark " cd " :start start :end end) :cond))
         (conj-pr-start (list (%buffer-string-pos mark " cj.pr " :start start :end end) :conj-pr))
         start-list 
         tempora-end
         result
         (lengths '(:aor 4 :impf 5 :pf 4 :fut 4 :opt 5 :cond 4 :conj-pr 7))
         )
    (setf start-list
          (sort (remove-if #'null (list aor-start impf-start pf-start opt-start cond-start fut-start
                                               conj-pr-start)
                           :key 'car) '< :key 'car)
          tempora-end
          (let ((t-end
                 (%buffer-not-char-pos mark (concatenate 'string dict::$georgian-characters " (ª)-")
                                       :start (+ (caar (last start-list)) 5) :end end)))
            (cond ((null t-end)
                   end)
                  ((char= (%buffer-char mark (1- t-end)) #\()
                   (1- t-end))
                  (t t-end))))
    #+debug(print (list :start-list start-list :tempora-end tempora-end))
    (loop for sublist on start-list
          do
          (push (string-trim " " (%buffer-substring mark (+ (caar sublist) 
                                                            (getf lengths (cadar sublist)))
                                                    (or (caadr sublist) tempora-end))) result)
          (push (second (first sublist)) result))
    (values result tempora-end)))

(defmethod buffer-current-exp-end ((mark string-buffer) &optional 
                                       (start (%buffer-position mark))
                                       (end (%buffer-size mark)))
  (when (= start end) (return-from buffer-current-exp-end end)) 
  (let ((first-char (%buffer-char mark start)))
    (case first-char
      (#\(
       (setf start (buffer-find-closing-paren mark :start start :end end)))
      (#\"
       (setf start (%buffer-char-pos mark #\" :start (1+ start) :end end)))) 
    (or (%buffer-char-pos mark $space-and-return :start start :end end)
        end)))

(defmethod buffer-find-closing-paren ((mark string-buffer) 
                                           &key
                                           (start (%buffer-position mark))
                                           (end (%buffer-size mark)))
  (loop with paren-count = 1
        do
        (setf start (%buffer-char-pos mark "()" :start (1+ start) :end end))
        while start
        do
        (if (char= (%buffer-char mark start) #\()
          (incf paren-count)
          (decf paren-count))
        until (= paren-count 0))
  start)

(defmethod buffer-word-pos ((mark string-buffer) str &key start end)
  (let ((pos (%buffer-string-pos mark str :start start :end end)))
    (cond ((not pos)
           nil)
          ((= pos start)
           start)
          ((and (find (%buffer-char mark (1- pos)) " ")
                (find (%buffer-char mark (+ pos (length str))) " ,"))
           pos)
          (t
           (buffer-word-pos mark str :start (+ pos (length str)) :end end)))))

(defmethod trans-end-mark-pos ((mark string-buffer) key
                                  &key 
                                  (start (%buffer-position mark)) 
                                  (end (%buffer-size mark)))
  "finds the end of translation and begin of the next verb form"
  (unless (find #\~ key) (setf key "~")) ; new!!!!!!!!!
  (setf start (%buffer-not-char-pos mark #.(format nil " ~%") :start start :end end))
  (let ((pos nil)
        next-georgian-end)
    (do-%buffer-words (mark l r :from start :to end)
      while (< l end)
      do (setf r (buffer-current-exp-end mark l end)
               next-georgian-end (buffer-georgian-end 
                                  mark :start r
                                  :additional-chars " ª[]~ ,Â-!?/"))
      until (when (and (char= (%buffer-char mark (1- r)) #\;)
                       (extended-georgian-char-p (%buffer-char mark (1+ r)))
                       (if (not (dict:georgian-char-p (%buffer-char mark (1+ r))))
                         (dict:georgian-char-p (%buffer-char mark (+ 2 r)))
                         t)
                       (not (buffer-word-pos mark key :start r :end next-georgian-end)))
              (setf pos (1- r))))
    pos))

(defmethod buffer-search-words ((mark string-buffer) list-of-words
                                    &key (start (%buffer-position mark))
                                    (end (%buffer-size mark)))
  (do-%buffer-words (mark l r :boundary " ,;()" :from start :to end)
    thereis
    (let ((substr (%buffer-substring mark l r)))
      (unless (string= substr "")
        (loop for word in list-of-words
              thereis (string= word substr))))))

(defmethod v-get-annotation ((mark string-buffer)
                                &optional (start (%buffer-position mark)))
  (let* ((annotations nil) annotation-end)
    (loop with exp-end and word and exp
          do
          (setf exp-end (buffer-current-exp-end mark start))
          (setf exp (%buffer-substring mark start exp-end)
                word (buffer-next-word mark :start start))
          while (or
                 (and
                  (string/= (%buffer-substring mark start exp-end) "a)")
                  (buffer-search-words mark $v-annotations :start start :end exp-end)) 
                 (search-all '("(mit " "(ohne " "(od. ") word)
                 (and  (string-member exp '("mit" "ohne"))
                       (string-member (buffer-next-word mark :start exp-end) 
                                      '("S" "G" "D/A")))
                 (and  (string-member exp '("nur"))
                       (string-member (buffer-next-word mark :start exp-end) 
                                      '("PR")))
                 (and (string= word "bei")
                      (string= (first annotations) "s."))
                 (and (string= (first annotations) "=")
                      (georgian-word-p word))) 
          do
          (push exp annotations)
          (setf start (1+ exp-end))
          finally (setf annotation-end start))
    ;(format t "~%annotations: ~a" annotations)
    (values (reverse annotations) annotation-end)))

(defmethod parse-unnumbered-subentry ((mark string-buffer) common-key replace-key
                                           annotation start &key
                                           (end (%buffer-size mark))
                                           grammar-info)
  (setf start (if start (min start end) end))
  ;(print "parse-unnumbered-subentry")
  ;(print (%buffer-substring mark start end))
  (loop
    with subentry-end ; and tilde-pos
    and annotation-end = start and new-annotation ; = (v-get-annotation mark start)
    do
    (%set-mark mark annotation-end)
    (setf ;tilde-pos (buffer-next-tilde-pos mark annotation-end end)
     subentry-end (min (or (trans-end-mark-pos mark replace-key
                                               :start annotation-end
                                               :end end) 
                           end)))
    do
    ;(print (buffer-substring mark annotation-end subentry-end))
    (v-parse-subentry mark common-key replace-key 
                         (append annotation new-annotation) grammar-info
                         annotation-end subentry-end)
    while (< subentry-end end)
    do
    (let (new-replace-key)
      (multiple-value-setq (new-replace-key new-annotation annotation-end)
        (v-get-replace-keyword mark (+ subentry-end 2)))
      ;*(format t "~%new-replace-key: ~a" new-replace-key) 
      (unless (string= new-replace-key "")
        (setf replace-key new-replace-key
              annotation (append 
                          (remove-if-not (lambda (str) 
                                           (string-member 
                                            (string-trim "()," str)
                                            $common-annotations))
                                         annotation)))
        ;(format t "~%unnumbered annotation: ~a" annotation)
        ))))

(defmethod v-parse-subentry ((mark string-buffer) c-key r-key annotation
                                grammar-info start end)
  ;; determine here which replace keys to jump over
  #+debug
  (format t "~%c-key: ~a, r-key: ~a, annotation: ~a, grammar-info: ~a"
          c-key r-key annotation grammar-info)
  (if (or (and (find #\Space r-key) 
               (not (find #\( r-key))
               (< (count #\~ r-key) 2)))
    nil ;*(format t "~%jumped over: c-key: ~a, r-key: ~a" c-key r-key)
    (progn
      (when (and (< end (%buffer-size mark)) (find (%buffer-char mark end) "!?"))
        (incf end)
        (when (and (< end (%buffer-size mark)) (find (%buffer-char mark end) "!?"))
          (incf end))) ; twice to include cases like "waaas?!"
      (setf end (1+ (%buffer-not-char-pos mark #.(format nil " ~%") :end end :from-end t)))
      (let (g-ann ;(preverb-marker :fut)
            (roots (getf grammar-info :roots)))
        (when (and (member (getf grammar-info :key-t) '(:comma :fut-trans :fut-pv-trans))
                   (not (zerop (length r-key))))
          ;(format t "~%r-key: ~a" r-key)
          (multiple-value-bind (aor pf key)
                               (extract-aor-pf r-key :roots roots)
            ;(format t "~%aor: ~a, pf: ~a" aor pf)
            (when aor (push aor grammar-info) (push :aor grammar-info))
            (when pf (push pf grammar-info) (push :pf grammar-info))
            (setf r-key key))) 
        (when (getf-and-push :inf grammar-info g-ann) (push :vn g-ann))
        (unless (find "(nur PR)" annotation :test 'string=)
          (when (getf-and-push :pf grammar-info g-ann) (push :pf g-ann))
          (when (getf-and-push :aor grammar-info g-ann) (push :aor g-ann)))
        (when (getf-and-push :opt grammar-info g-ann) (push :opt g-ann))
        (when (getf-and-push :impf grammar-info g-ann) (push :impf g-ann))
        (when (getf-and-push :cond grammar-info g-ann) (push :cond g-ann))
        (when (getf-and-push :gv grammar-info g-ann) (push :gv g-ann))
        (let ((key-t (getf grammar-info :key-t)))
          ;(format t "~%key-t: ~a" key-t)
          ;(print key-t) (print c-key) (print r-key)
          (cond ((eq :fut-trans key-t) ;; Ùäç
                 (cond 
                  ((and (not (find #\~ r-key)) (find #\Space r-key))
                   (let* ((words (u:parse-string r-key :whitespace " "))
                          (verb (find-if (lambda (w) 
                                           (contains-root-p w roots))
                                         words)))
                     
                     ;*(format t "~%~%words: ~a, verb: ~a, r-key: ~A~%" words verb r-key)
                     (setf g-ann
                           (append 
                            g-ann
                            (list 
                             :pr 
                             (mark-root (getf grammar-info :pr) roots)
                             :fut (mark-root (or verb r-key) roots)))
                           r-key (getf grammar-info :pr))))
                  ((not (find #\~ r-key))
                   (setf g-ann (append g-ann
                                       (list :pr 
                                             (mark-root (getf grammar-info :pr)
                                                        roots)
                                             :fut (mark-root r-key roots)))
                         r-key (getf grammar-info :pr)))
                  ((string/= (getf grammar-info :pr) c-key)
                   (setf g-ann
                         (append g-ann
                                 (list :pr 
                                       (mark-root (getf grammar-info :pr) roots)
                                       :fut (mark-root 
                                             (subst-substrings r-key 
                                                               (list "~" c-key))
                                             roots))))) 
                  (t
                   (setf g-ann 
                         (append g-ann
                                 (list :pr ; :fut mistake!?!? 
                                       (mark-root
                                        (v-concat-common-and-rest-key 
                                         (getf grammar-info :pr) r-key :pr)    
                                        roots))))
                   ;(format t "fut-with-pv: ~a~%" g-ann )
                   )))
                ((and (eq :generic key-t)
                      (find #\~ (getf grammar-info :fut)))
                 ; replace c-key in r-key by "~"
                 ;(format t "~%c-key: ~a, r-key: ~a~%" c-key r-key)
                 (setf g-ann (append g-ann
                                     (list :pr ; *** 
                                           (if (find #\~ (getf grammar-info :fut))
                                             (mark-root (subst-substrings r-key (list "~" c-key)) 
                                                        roots)
                                             r-key)
                                           :fut (getf grammar-info :fut)
                                           ))))
                ((and (eq :generic key-t)
                      (find #\~ r-key)
                      (getf grammar-info :fut)
                      (find #\Â (getf grammar-info :fut))
                      ) ; new!!!!!!!! 
                 (setf g-ann (append g-ann
                                     (list :pr 
                                           (mark-root 
                                            (v-concat-common-and-rest-key c-key r-key :pr)
                                            roots)
                                           :fut
                                           (mark-root 
                                            (insert-preverb r-key (getf grammar-info :fut))
                                            roots)))))
                ((and (eq :generic key-t)
                      (find #\~ r-key)) ; almost new!!!!!!!! 
                 (setf g-ann (append g-ann
                                     (list :pr 
                                           (mark-root 
                                            (v-concat-common-and-rest-key c-key r-key :pr)
                                            roots)))))
                ((and (eq :generic key-t)
                      (not (getf grammar-info :fut))) 
                 (setf g-ann (append g-ann (list :pr (mark-root r-key roots)))
                       r-key (getf grammar-info :pr)))
                ((eq :generic key-t)
                 (setf g-ann (append g-ann
                                     (list :pr (mark-root (getf grammar-info :pr) roots)
                                           :fut (mark-root (getf grammar-info :fut) roots)))
                       r-key (getf grammar-info :pr)))
                ((and (eq :pr-trans key-t) (getf grammar-info :fut))
                 (setf g-ann (append g-ann (list :fut (getf grammar-info :fut)
                                          :pr (getf grammar-info :pr)
                                                 ))))
                ((eq :pr-trans key-t)
                 (setf g-ann (append g-ann (list :pr (getf grammar-info :pr)))))
                ((find key-t '(:comma :fut-pv-trans))
                 ;(print (%buffer-substring mark start end))
                 (if (and (not (find #\~ r-key)) (find #\Space r-key))
                   (let* ((words (u:parse-string r-key :whitespace " "))
                          (verb (find-if (lambda (w) 
                                           (contains-root-p w roots))
                                         words)))
                     (setf g-ann
                           (append g-ann
                                   (list :pr (mark-root (getf grammar-info :pr) roots)
                                         :fut (mark-root (or verb r-key) roots)))
                           r-key (getf grammar-info :pr)))
                   
                   (let ((marked-fut (mark-root r-key (getf grammar-info :roots))))
                     (setf g-ann (append g-ann
                                         (list :pr (mark-root c-key roots)
                                               :fut marked-fut))))))
                (t nil))) 
        (let (verb-subentry
              (trans (%buffer-substring mark start end)))
          (unless (zerop (length trans))
            (push trans verb-subentry)
            (push :tr verb-subentry)
            (when annotation
              (let ((ann-str ""))
                (dolist (ann annotation)
                  (ncat ann-str (format nil "~a " ann)))
                (push ann-str verb-subentry))
              (push :ann verb-subentry))
            (dolist (field (reverse $fields))
              (let ((val (getf g-ann field)))
                (when val 
                  (push val verb-subentry)
                  (push field verb-subentry))))
            (push verb-subentry *result-list*)))
        ;(format t "~%gr: ~s~%" grammar-info)
        ;(format t "~%~a ~a~%" c-key r-key)
        ))))

(defmethod extract-aor-pf ((mark string-buffer) &key start end)
  #+debug(format t "~%extract: ~s~%" (%buffer-substring mark start end))
  (let* ((aor-start (%buffer-string-pos mark "aor" :start start :end end))
         (pf-start (when aor-start (%buffer-string-pos mark "pf" :start aor-start :end end))))
    #+debug(print (list :a aor-start :p pf-start))
    (if aor-start
      (values (string-trim " " (%buffer-substring mark (+ aor-start 4) pf-start))
              (string-trim " " (%buffer-substring mark (+ pf-start 3) end)))
      ;; comma divided list
      (let* ((str (%buffer-substring mark start end))
             (comma-pos (or (position #\, str) (position #\; str))))
        (values (subseq str 0 comma-pos)
                (subseq str (+ comma-pos 2)))))))

(defmethod parse-gv-subentry ((mark string-buffer) roots inf gv &key start
                                 (end (%buffer-size mark)))
  (let (str first-word info grammar-info lines starts common-keys future aor/pf)
    (do-%buffer-words (mark left right :from start :to end)
      (setf start left)
      (when (char= #\( (%buffer-char mark left))
        (setf right (find-closing-brace mark left end))) 
      (setf str (%buffer-substring mark left right))
      until (georgian-word-p str)
      do 
      (push str info)) ; info contains what is between gv and the verb
    #+debug(format t "info: ~a~%" info)
    #+debug(format t "r: ~d " roots)
    (let* ((first-georgian-end (buffer-georgian-end mark :start start :end end)) 
           present
           (present0 (extract-verb mark roots start first-georgian-end))
           (first-georgian (string-trim 
                            " ," (%buffer-substring mark start first-georgian-end))))
      #+debug(format t "~%present: ~a" present0)
      #+debug(format t "~%first-georgian: ~a" first-georgian)
      (let (present-string) ; extract present substitute
        (when (and (zerop (length present0))
                   (setf present-string 
                         (find-if (lambda (str) (search "pr fehlt; dafŸr:" str)) info)))
          (setf present0 (subseq present-string
                                 (+ 7 (search "dafŸr:" present-string)))))) 
      (do-%buffer-lines (mark left right size :from first-georgian-end :to end)
        #+debug(print (list :substr (%buffer-substring mark left right)))
        (setf first-word (buffer-next-word mark :start left)
              present present0) ; ?? what do we need present0 for?
        #+debug(print (list :first-word first-word))
        ;(print (%buffer-substring mark left right))
        (cond ((= 1 (- right left)) t) 
              ((string= (%buffer-substring mark left right) "s. fut")
               (push :goto-fut lines) 
               (push nil starts)
               (push nil common-keys)
               (push nil aor/pf))
              ((and (string= first-word "fut")
                    (%fut-only mark :start left :end right))
               (multiple-value-bind (aorist perfect f)
                                    (extract-aor-pf (%buffer-substring 
                                                     mark (+ 5 left) right) :roots roots)
                 (setf future f)
                 (setf (first aor/pf) (list :aor aorist :pf perfect))))
              ((and (string= first-word "fut")
                    (%tenses-only mark :start left :end right))
               (setf grammar-info (extract-aor-pf-impf-opt-cond mark :start (1- left) :end right))) 
              ;; ******
              ((or (string= first-word "fut")
                   (and (string-member first-word '("a)" "b)" "c)" "d)"))
                        (string= (buffer-next-word mark :start (+ left 4)) "fut")))
               (push nil aor/pf)
               (let* ((g-start (buffer-georgian-start mark :start left :end right))
                      (g-end (1- (buffer-georgian-end mark :start g-start :end right)))
                      #+debug(quaquack (print (list :left left :gstart g-start :end right)))
                      (g-text (%buffer-substring mark g-start g-end)))
                 #-old(push (+ left (if (string= first-word "fut") 5 8)) starts) 
                 #+new(push g-start starts)
                 #+ignore
                 (let ((behind-fut/alphanum-pos 
                        (+ left (if (string= first-word "fut") 5 8))))
                   (when (< behind-fut/alphanum-pos g-start)
                     (let ((ann-text (%buffer-substring mark behind-fut/alphanum-pos g-start)))
                       
                       (format t "~%ann-text: ~a" ann-text))
                     ))
                 ;(format t "~%g-text: ~a" g-text)
                 (cond ((%buffer-char-pos mark #\~ :start g-start :end g-end) 
                        (push present common-keys)
                        (push :fut-pv-trans lines))
                       ((find #\Space g-text) ; only one of the words is the verb form
                        (let* ((words (u:parse-string g-text :whitespace " "))
                               (verb (find-if (lambda (w) (contains-root-p w roots)) words)))
                          (cond (verb
                                 (push present common-keys)
                                 (if (fst::has-pv-p verb roots 'fst::future)
                                   (push :fut-pv-trans lines)
                                   (push :fut-trans lines)))
                                (t
                                 (error "could not find verb in ~a." g-text)))))
                       (t
                        (push g-text common-keys)
                        (push :fut-trans lines)))))
              ((char= #\, (%buffer-char mark (- left 2)))
               (push :comma lines)
               (push left starts)
               ;(format t "~%common-keys: ~a" first-georgian) 
               (push first-georgian common-keys)
               (push nil aor/pf)
               (setf present nil))
              ((and (string= first-word "aor")
                    (%aor-only mark :start (1+ left) :end right))
               (multiple-value-bind (aorist perfect)
                                    (extract-aor-pf mark :start (1+ left) :end right)
                 (setf (first aor/pf) (list :aor aorist :pf perfect)))) 
              ((find first-word '("impf" "opt" "aor") :test 'string=)
               (push :pr-trans lines)
               (let (transl-start) ; position after tempora
                 (multiple-value-setq (grammar-info transl-start)
                   (extract-aor-pf-impf-opt-cond mark :start (1- left) :end right))
                 (push nil aor/pf)
                 (let* ((g-start (buffer-georgian-start mark :start start :end right))
                        (g-end (1- (buffer-georgian-end mark :start g-start :end right))))
                   (push transl-start starts)
                   (push (extract-verb mark roots g-start g-end) common-keys)
                   ;(format t "pr-trans: ~a~%" (extract-verb mark roots g-start g-end))
                   )))
              (t
               #+debug(print (list :generic first-word :start start :end right))
               (push :generic lines)
               (push nil aor/pf)
               (let* ((g-start (buffer-georgian-start mark :start start :end right))
                      (g-end (1- (buffer-georgian-end mark :start g-start :end right))))
                 (push g-start starts)
                 #+debug(print (list :generic first-word :start start :end right :g-start g-start :g-end g-end))
                 (if (%buffer-char-pos mark #\~ :start g-start :end g-end)
                   (progn (push present common-keys)
                          (setf present nil))
                   (progn (push (extract-verb mark roots g-start g-end) common-keys)
                          ;(format t "generic: ~a~%" (extract-verb mark roots g-start g-end))
                          ))))))
      (when info (push info grammar-info) (push :info grammar-info))
      (push gv grammar-info) (push :gv grammar-info)
      (push inf grammar-info) (push :inf grammar-info)
      (push roots grammar-info) (push :roots grammar-info) 
      (when present (push present grammar-info) (push :pr grammar-info))
      (when future (push future grammar-info) (push :fut grammar-info)) 
      ;*(format t "~%grammar-info: ~a" grammar-info)
      (setf lines (reverse lines) aor/pf (reverse aor/pf)
            starts (reverse starts) common-keys (reverse common-keys))
      ;*(format t "~%lines: ~s~%, grammar-info: ~s" lines grammar-info)
      ; processing the translations
      (loop for
            line in lines and line-start in starts and common-key in common-keys
            and aor+pf in aor/pf
            with
            parse-flag and line-end ; and next-word 
            and replace-key and annotations 
            and gr-info
            do
            (setf parse-flag nil)
            (case line
              (:goto-fut nil)
              (:comma (setf parse-flag t))
              (:fut-trans (setf parse-flag t))
              (:fut-pv-trans (setf parse-flag t))
              (:pr (setf parse-flag t)) 
              (:pr-trans (setf parse-flag t)) 
              (:generic (setf parse-flag t))) 
            (when parse-flag
              (when (and common-key (find #\[ common-key))
                (setf common-key (delete #\[ common-key)
                      common-key (delete #\] common-key)))
              ;(Print line)
              (let ((aor+pf* ()) (rest-string nil)) 
                (multiple-value-setq (replace-key annotations line-start aor+pf* rest-string)
                  (v-get-replace-keyword mark line-start))
                ;*(print (list line replace-key line-start aor+pf* rest-string))
                (when aor+pf*
                  (when aor+pf (warn "aor+pf: ~a, aor+pf*: ~a" aor+pf aor+pf*))
                  (setf aor+pf aor+pf*))
                ;(print (list replace-key annotations))
                (setf annotations (append *global-annotations* annotations) 
                                 ;next-word (buffer-next-word mark :start line-start)
                                 line-end (%buffer-line-end mark line-start))
                (cond ((and rest-string
                            (char= #\[ (char replace-key 0))
                            (find #\~ rest-string))
                       (let ((preverbs (u:string-parse rest-string :whitespace ", "))
                             (common-key (string-trim "[]" replace-key))
                             (aor (getf aor+pf :aor))
                             (pf (getf aor+pf :pf))) 
                         (warn "rest-string parsed: ~a" preverbs)
                         (dolist (pv preverbs)
                           (let ((gr-info (append (list :key-t line)
                                                  ;aor+pf
                                                  (list :aor (insert-preverb pv aor)
                                                        :pf (insert-preverb pv pf))
                                                  grammar-info)))
                             (parse-unnumbered-subentry mark common-key pv annotations
                                                        line-start :end line-end
                                                        :grammar-info gr-info)))))
                      (t
                       (when rest-string
                         (warn "unconsumed rest-string: ~a" rest-string))
                       (setf gr-info (append (list :key-t line) aor+pf grammar-info))
                       (parse-unnumbered-subentry mark common-key replace-key annotations
                                                  line-start :end line-end
                                                  :grammar-info gr-info))
                      )))))))

;; analyses ann?, key, ann?, rest
(defmethod v-get-replace-keyword ((mark string-buffer)
                                      &optional (start (%buffer-position mark)))
  (setf start (%buffer-not-char-pos mark #\Space :start start))
  (let* ((g-start (buffer-georgian-start mark :start start)) 
         (key-end (buffer-georgian-end mark :start g-start :additional-chars " ª[]~ ,Â-!?/"))) 
    ;*(format t"~%g-start: ~a" g-start)
    (multiple-value-bind 
      (annotations annotation-end)
      (v-get-annotation mark (if (char= #\Space (%buffer-char mark key-end))
                               (1+ key-end) key-end))
      ;*(print (list annotations annotation-end start g-start))
      #+ignore
      (when (< annotation-end start) 
        (print (%buffer-substring mark annotation-end start))) 
      (when (and g-start (< start g-start))
        (push (string-trim " " (%buffer-substring mark start g-start)) annotations))
      (when g-start
        (let ((replace-key (string-trim " " (%buffer-substring mark g-start key-end)))
              (open-paren-pos nil)
              (close-paren-pos nil))
          ;; partial-key is only for debugging purposes
          #+debug(format t "~%partial-key: ~s" (%buffer-substring mark start (+ start 20)))
          #+debug(format t "~%replace-key: ~s" replace-key)
          #+debug(format t "~%annotations: ~s" annotations)
          (cond ((and (find #\Space replace-key)
                      (= 1 (count #\~ replace-key))
                      ; heuristics to avoid (aor, pf)-pairs:
                      (not (aor-pf-pair-p replace-key)))
                 (let* ((tilde-pos (position #\~ replace-key))
                        (key-start (or (position-if (lambda (c) (char= c #\Space)) 
                                                    replace-key
                                                    :end tilde-pos :from-end t)
                                       0))
                        (key-end (position-if (lambda (c) (find c " ,")) replace-key :start tilde-pos)))
                   ;(print "first cond chosen:")
                   (values (subseq replace-key key-start key-end)
                           annotations
                           g-start)))
                ; a) fut ~ (ÜÛá×å×ç×àÛ, âßá×å×ç×àãß× â×èŞ×ã)
                ((let ((last-tilde-pos (position #\~ replace-key :from-end t)))
                   ;; start = 1 to avoid problems with "(Ú×)ÜßØİêÜáÛØ ((Ú×)ÜßØİêÜáÛ, (Ú×)âßØİêÜáãß×)" etc.
                   (setf open-paren-pos (and (> (length replace-key) 1)
                                             (position #\( replace-key :start (or last-tilde-pos 1)))
                         close-paren-pos
                         (when open-paren-pos (position #\) replace-key :from-end t)))
                   #+debug(format t "~%open: ~d, close: ~d, replace-key: ~s" open-paren-pos close-paren-pos replace-key)
                   (and close-paren-pos
                        (> close-paren-pos (+ open-paren-pos 3))
                        (or (find #\, replace-key :start open-paren-pos)
                            ;; ~ (1. Üê×âØÛ 3. ê×âØä; âß×âØãß×)
                            (and (find #\1 replace-key :start open-paren-pos)
                                 (find #\; replace-key :start open-paren-pos)))
                        (not (search ".." replace-key))))
                 (multiple-value-bind (aorist perfect)
                                      (extract-aor-pf mark :start (+ g-start open-paren-pos 1)
                                                      :end (+ g-start close-paren-pos))
                   #+debug(format t "~%aorist+perfect: ~s, ~s" aorist perfect)
                   (values (subseq replace-key 0 (1- open-paren-pos))
                           annotations
                           (+ g-start close-paren-pos)
                           (list :aor aorist :pf perfect))))
                ((progn
                   (setf open-paren-pos (%buffer-char-pos mark #\( :start key-end)
                         close-paren-pos
                         (when open-paren-pos
                           (find-closing-brace mark open-paren-pos)
                           ;(%buffer-char-pos mark #\) :start open-paren-pos)
                           ))
                   #+debug(print (list :start start :key-end key-end :op open-paren-pos :cl close-paren-pos))
                   (and close-paren-pos
                        (> close-paren-pos (+ open-paren-pos 3)) ; e.g.
                        (%aor-only mark :start key-end :end close-paren-pos)
                        ;*(print "Changed here")
                        (not (%buffer-string-pos mark "aor" :start start :end open-paren-pos))))
                 ;; e.g. fut Â (aor Ü×óîáßÛ pf âßóîáÛÜß×) ;; hŠ??
                 (if (or (string= "(ªª " (%buffer-substring mark open-paren-pos (+ open-paren-pos 4)))
                         (string-member (%buffer-substring mark open-paren-pos (+ open-paren-pos 5))
                                        '("(â×è " "(âßè " "(é×ãè"))
                         (string-member (%buffer-substring mark open-paren-pos (+ open-paren-pos 6))
                                        '("(Ü×Ú×è")))
                   ;; fut [âéàßÜÚÛØ×]; ×~ (ªª ×âéàßÜ×) ;; preliminary: discard "(ªª ×âéàßÜ×)" ***
                   (values replace-key
                           annotations
                           close-paren-pos
                           nil
                           (%buffer-substring mark (1+ key-end) open-paren-pos))
                   (multiple-value-bind (aorist perfect)
                                        (extract-aor-pf mark :start (1+ open-paren-pos)
                                                        :end close-paren-pos)
                     ;(format t "~%aorist+perfect: ~a, ~a" aorist perfect)
                     (format t "~%replace-key: ~s, ???: ~s" 
                             replace-key (%buffer-substring mark (1+ key-end) open-paren-pos)) 
                     ;*(print "second cond chosen") 
                     (values replace-key
                             annotations
                             close-paren-pos
                             (list :aor aorist :pf perfect)
                             (%buffer-substring mark (1+ key-end) open-paren-pos)))))
                (t
                 ;*(format t "~%replace-key chosen: ~a" replace-key) 
                 (values replace-key annotations annotation-end))))))))

(defmethod parse-inf-subentry ((mark string-buffer) roots inf &key start
                                     (end (%buffer-size mark)))
  (loop 
    with next-nl-pos and gv and start-pos
    do
    (multiple-value-setq (gv start-pos) (buffer-get-genus-verbi mark start)) 
    (setf next-nl-pos (1+ start))
    (loop
      do
      (setf next-nl-pos (%buffer-line-end mark (1+ next-nl-pos)))
      until
      (or (= next-nl-pos end) 
          (string-member (buffer-next-word mark :start next-nl-pos)
                         $genera-verbi)))
    (parse-gv-subentry mark roots inf gv :start start-pos :end next-nl-pos)
    until (= next-nl-pos end)
    do (setf start next-nl-pos)))

(defmethod get-roots ((mark string-buffer))
  "extracts the different roots in the first line from a verb entry and sets mark
to the end of the line"
  (let* ((end (%buffer-char-pos mark +newline+ :start 0))
         (root-sign-pos (%buffer-char-pos mark #\­ :start 0)) 
         (char-after-root-sign (%buffer-char mark (1+ root-sign-pos)))
         roots
         verbal-noun)
    #+debug(print (list :end end :rsp root-sign-pos :char-after-root-sign char-after-root-sign)) 
    (cond ((or (dict:georgian-char-p char-after-root-sign) ; keyword is VN
               (char= #\( char-after-root-sign))
           (when (%buffer-char-pos mark #\/ :start 0 :end end)
             (error "root string inconsistency"))
           (setf roots (resolve-root-braces (%buffer-substring mark 0 root-sign-pos))
                 verbal-noun 
                 (remove #\­ (%buffer-substring mark 0 
                                                (%buffer-char-pos mark #.(format nil " ~%")
                                                                  :start 0)))))
          (t 
           (do-%buffer-words (mark left right :from 0 :to end :boundary #\/)
             (let ((str (%buffer-substring mark left right)))
               (setf roots (append roots (resolve-root-braces str)))))))
    (%set-mark mark (1+ end))
    (let ((first-root (car roots)))
      (format t "~%roots: ~a, first-root: ~a" roots first-root)
      (values (sort roots '> :key 'length) verbal-noun (%buffer-position mark) first-root))))

(defmethod parse-dict-entry ((mark string-buffer))
  (setf *global-annotations* (get-global-annotations mark))
  (unless (%buffer-string-pos mark " s. " :start 0 :end (%buffer-line-end mark 0))
    (let ((size (%buffer-size mark)))
      (multiple-value-bind (roots inf roots-end key-root)
                           (get-roots mark)
        (setf *result-list* (list (cons roots key-root)))
        (multiple-value-bind (next-word start-pos)
                             (buffer-next-word mark :start roots-end)
          #+debug(print (list :next-word next-word :start-pos start-pos))
          (cond
           ((or (string= next-word "Inf.")
                (and (string= next-word "1.")
                     (string= (buffer-next-word mark :start start-pos) "Inf.")))
            (decf start-pos) ; better not to erase the - at Inf. -
            (loop 
              with next-nl-pos
              do
              (setf next-nl-pos (%buffer-line-end mark start-pos)
                    inf (%buffer-substring mark
                                           (or (buffer-georgian-start 
                                                mark :start start-pos :end next-nl-pos) start-pos)
                                           next-nl-pos)
                    start-pos next-nl-pos)
              (loop
                do 
                (setf next-nl-pos (%buffer-line-end mark (1+ next-nl-pos)))
                (setf next-word (buffer-next-word mark :start next-nl-pos))
                until
                (or (= next-nl-pos size)
                    (string= next-word "Inf.")
                    (and (find next-word '("2." "3." "4." "5." "6.") :test 'string=)
                         (string= (buffer-next-word mark :start (+ next-nl-pos 3)) "Inf."))))
              (parse-inf-subentry mark roots inf :start start-pos :end next-nl-pos)
              until (= next-nl-pos size)
              do (setf start-pos (1+ next-nl-pos))))
           (t (parse-inf-subentry mark roots inf :start roots-end))))))
    *result-list*))

#+test
(get-roots (get-verb-entry :root "×Ü×Úâîäë­"))

;; write parsed entries to file
#+test
(with-open-file (stream "projects:georgian-morph;parsed-verb-entries.txt" :direction :output :if-exists :supersede)
  (loop with entry and pos = 0 #+test(nth-value 1 (get-verb-entry :root "õá²­"))
        do (multiple-value-setq (entry pos) (get-verb-entry :file-pos pos))
        while (and entry (> (%buffer-size entry) 0))
        do (pprint (nreverse (parse-dict-entry entry)) stream)))


#+test
(loop with entry and pos = (nth-value 1 (get-verb-entry :root "öí×ã­"))
      while pos
      do (multiple-value-setq (entry pos) (get-verb-entry :file-pos pos))
      while (and entry (> (%buffer-size entry) 0))
      do (pprint (parse-dict-entry entry)))

#+test
(pprint (parse-dict-entry (get-verb-entry :file-pos 2626777)))

#+test
(format-entry (parse-dict-entry (get-verb-entry :root "×õáä(Ü)­")))


;; use "projects:fsa;georgian-parser;tschenkeli-verb-parser.lisp"

#+test
(let ((*package* (find-package :fst)))
  (loop (fst::make-root-verb-filter)))

#+test
(get-verb-entry :root "×õáä(Ü)­")

;; did not go through
#+only-once
(with-open-file (stream "projects:georgian-morph;verb-dgs.txt" :direction :output :if-exists :supersede)
  (loop with entry and pos = 0 #+test(nth-value 1 (get-verb-entry :root "õá²­"))
        do (multiple-value-setq (entry pos) (get-verb-entry :file-pos pos))
        while (and entry (> (%buffer-size entry) 0))
        do 
        (let ((*package* (find-package :fst))
              (*print-circle* nil))
          (pprint (get-root-dg (parse-dict-entry entry)) stream)
          (terpri stream))))

(defun get-root-dg (result-list)
  (let* (;;(result-list (nreverse result-list))
         (root-info (car result-list))
         (c-root (car root-info))         
         ;;(c-root (cdr root-info)) for "parsed-verb-entries.txt"     
         (roots (fst::change-ev-roots root-info
                                      (mapcar #'fst::remove-root-number root-info)))
         (disjunctive-features))
    ;(format t "~%c-root: ~a, root-info: ~a" c-root root-info)
    (dolist (form (cdr result-list))
      ;(format t "~%getting expanded: ~s" form)
      (dolist (form (fst::expand-alternatives form roots))
        ;(format t "~%expanded form: ~s" form)
        (let ((root-feature-list (fst::build-root-feature-list form roots c-root)))
          (dolist (root-features root-feature-list)
            (destructuring-bind (root . features) (print root-features)
              (let ((root+rest (member root disjunctive-features :test #'equal)))
                (if root+rest
                  (setf (cadr root+rest)
                        (append (cadr root+rest) features))
                  (setf disjunctive-features
                        (list* root features disjunctive-features)))))))))
    ;; merge features for each root
    (loop for (root . rest) on disjunctive-features by #'cddr
          do root ;; avoids warning; better idea?
          (setf (car rest) 
                (merge-disjunctive-feature-lists (reverse (car rest)))))
    (if nil;t;print
      (let ((*package* (find-package :fst))
            (*print-circle* nil))
        (loop for (root features . rest) on disjunctive-features by #'cddr
              do rest ;; avoids warning
              (format t "~%root: ~a" root)
              (pprint features))
        (values))
      (values disjunctive-features c-root))))

#+test
(let ((*package* (find-package :fst)))
  (pprint (get-root-dg
           '(("åäÜã" "åäêá" "åäÜ" "åä")
             (:vn "åäÜã×" :gv "MV" :pr "ÜåäêáäØ" :aor "ÜßåäÜ(ã)Û" :pf "âßåäÜ(ã)ß×" :fut
                  "ÜßåäÜ(ã)ß" :tr "jÂn/et. finden")
             (:vn "åäÜã×" :gv "T²" :pr "ÜßåäÜ(ã)ß" :aor "ÜßåäÜ(ã)Û" :pf "âßåäÜ(ã)ß×" :fut
                  "~" :tr "jÂn/et. finden")
             (:vn "åäÜã×" :gv "T³" :pr "ÜêåäÜ(ã)ß" :aor "ÜêåäÜ(ã)Û" :pf "âßåäÜ(ã)ß×" :fut
                  "~" :tr
                  "jÂm/fŸr jÂn jÂn od. et. finden; bei jÂm od. an eÂm Gegenstand et. finden, entdecken; ~ â×è Ùêáè \"jÂm das Herz wieder finden\", d.h. jÂn wieder zu KrŠften bringen/beleben; jÂs E§lust wecken")
             (:vn "åäÜãßãÛØ×" :gv "KT" :pr "Ü×åäÜãßãÛØ" :aor "Ü×åäÜãßãÛ" :pf "âßåäÜãßãÛØß×"
                  :fut "~" :tr "jÂn jÂn/et. finden lassen, jÂm helfen jÂn/et. zu finden")
             (:vn "åäÜÛØ× (ªª åäÛØ×)" :gv "T±" :pr "ÜåäÜÛØ (ªª ÜåäÛØ)" :aor "ÜåäÜÛ" :pf
                  "âßåäÜß×" :fut "~" :tr "finden")
             (:vn "åäÜÛØ× (ªª åäÛØ×)" :gv "T²" :pr "ÜßåäÜÛØ" :aor "âäÜßåäÜÛ" :pf
                  "âäâßåäÜÛØß×" :fut "âä~" :tr
                  "sich et. (z.B. Ruhm) erwerben, jÂn/et. finden, et. erlangen/erringen/erreichen (z.B. Anerkennung, Sieg usw.)")
             (:vn "åäÜÛØ× (ªª åäÛØ×)" :gv "T³" :pr "ÜêåäÜÛØ" :aor "âäÜêåäÜÛ" :pf
                  "âäâßåäÜÛØß×" :fut "âä~" :tr
                  "jÂm/fŸr jÂn et. erwerben, erlangen, erringen, jÂm/fŸr jÂn jÂn od. et. finden")
             (:vn "åäÜÛØ× (ªª åäÛØ×)" :gv "P±" :pr "âäßåäÜÛØ× (ªª ßåäÜÛØ×)" :ann
                  "(nur PR) " :tr "man findet, es ist zu finden, es gibt/hat")
             (:vn "åäÜÛØ× (ªª åäÛØ×)" :gv "RP± (OR)" :pr "âäÛåäÜÛØ× (ªª ÛåäÜÛØ×)" :ann
                  "(nur PR) " :tr
                  "bei jÂm ist et. zu finden od. vorhanden, jÂd hat/besitzt jÂn od. et.; auf/fŸr et. gibt es et. (z.B. eÂe Antwort auf eÂe Frage)")
             (:vn "åäÜÛØßãÛØ×" :gv "KT" :pr "Ü×åäÜÛØßãÛØ" :aor "âäÜ×åäÜÛØßãÛ" :pf
                  "âäâßåäÜÛØßãÛØß×" :fut "âä~" :tr
                  "jÂn et. erlangen/erringen lassen, jÂn jÂn/et. finden lassen, jÂm helfen jÂn/et. zu finden")))))

;; nouns
#+only-once
(with-open-file (stream "projects:georgian-morph;noun-codes.txt" :direction :output :if-exists :supersede)
  (wood:p-do-btree (word pointer) ((dict::dictionary-btree *stem-dict*))
    (write-string (dict::unmark-word word) stream)
    (let ((entry (wood:p-load pointer)))
      (when entry
        (write-char #\Space stream)
        (write-string entry stream))
      (terpri stream))))

#+test
(fstn-parse-sentence "ÙäÙäÛØßè á×â×İß õØäçÛØß ôçÛáÛØß× Ú× ØßôÛØßè× ï×ÜÛØß.")
#+test
(fstn-parse-sentence "Ûè íÜßãä Úò×áß×ã âæ×ÜÛ×.")
#+test
(fstn-parse "ØßôÛØßè×ŞÜßè××")
#+test
(fstn-parse "ÙäÙäÛØßè×ŞÜßè")

#||
(fst::u-transduce "ÙäÙäÛØßè×ŞÜßè" fst::*fst*)
(fst::u-transduce "ÙäÙä" fst::*fst*)
(fst::u-transduce "ÙäÙäè×ŞÜßè" fst::*noun-fst*)
||#

;; see make-verb-filter() and display-verb-parses() in tschenkeli-verb-parser.lisp

;; load-noun-table() in georgian-parser:tschenkeli.lisp

(defun load-noun-table ()
  (clrhash fst::*noun-table*)
  (with-file-lines (base-form "projects:georgian-parser;georgian-nouns.txt")
    (unless (char= (char base-form 0) #\#)
      (destructuring-bind (stem+conj . pos+flags) (split base-form #\space)
        (declare (ignore pos+flags))
        (destructuring-bind (stem conjugation) (split stem+conj #\:)
          (let* ((conjugation (intern (string-upcase conjugation) :keyword))
                 (nom-ending
                  (case conjugation
                    ((:a :b :p :u) "ß")
                    ((:d :f :i :k :m :o :o1 :r :x) "")))
                 (nom (concat stem nom-ending)))
            (push (fst::list-to-dg
                   (if (eq conjugation :r)
                     `((fst::stem ,stem)
                       (fst::lex ,nom)
                       (fst::conj ,conjugation)
                       (fst::class fst::masd))
                     `((fst::stem ,stem)
                       (fst::lex ,nom)
                       (fst::conj ,conjugation)
                       (fst::class fst::n)))) 
                  (gethash stem fst::*noun-table*))))))))

#+test
(load-noun-table)

#||
(fst::u-transduce "ÙäÙä" fst::*noun-fst*)

(fst::match-roots "Ù" 0)

(fst::u-transduce "ÙäÙäÛØßè×ŞÜßè" fst::*noun-fst*)
(fst::match-stems "ÙäÙäÛØßè×ŞÜßè" 0)
||#

:eof