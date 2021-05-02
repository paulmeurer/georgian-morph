;;;-*- Mode: Lisp; Package: TRANSDUCER -*-

(in-package :fst)

(defun parse-xle-verb-lexicon ()
  (setf *xle-template-table* (dat:make-string-tree))
  (dolist (file '(#+disabled "projects:xle;grammars;georgian;georgian-verb-lex.lfg"
                  ;;"projects:xle;grammars;georgian;kartuli-verb-lex-2.lfg"
                  ;;"projects:xle;grammars;georgian;kartuli-verb-lex-3.lfg"
                  ;; this has to be put back!
                  #+disabled"projects:xle;grammars;kartuli;kartuli-verb-lex-add.lfg"))
    (let ((entry "")
          (in-comment-p nil))
      (with-file-lines (line file)
        (multiple-value-setq (line in-comment-p)
          (remove-xle-comments line in-comment-p))
        (setf line (string-trim '(#\space #\tab) line))
        (cond ((string= "" line)
               nil)
              ((eq (search "STANDARD" line) 0)
               nil)
              ((char= (last-char line) #\.)
               (setf entry (concat entry line))
               (parse-xle-verb-entry entry)
               (setf entry ""))
              (t
               (setf entry (concat entry line))))))))

(defun remove-xle-comments (str &optional in-comment-p (start 0))
  (let ((quote-pos (position #\" str :start start)))
    (cond ((null quote-pos)
           (values (if in-comment-p "" (subseq str start))
                   in-comment-p))
          ((and (> quote-pos start) (char= (char str (1- quote-pos)) #\`))
           (multiple-value-bind (rxc icp) (remove-xle-comments str in-comment-p (1+ quote-pos))
             (values (concat (if in-comment-p "" (subseq str start (1+ quote-pos))) rxc)
                     icp)))
          (t
           (multiple-value-bind (rxc icp) (remove-xle-comments str (not in-comment-p) (1+ quote-pos))
             (values (concat (if in-comment-p "" (subseq str start quote-pos)) rxc)
                     icp))))))


(defun write-xle-verb-lexicon-entry (id &optional (stream *standard-output*))
  (let ((templates (dat:string-tree-get *xle-template-table* id)))
    (destructuring-bind (masdar . templates) templates
      (let (#+ascii(masdar (convert-encoding masdar :amirani :unicode))
	    (templates (mapcar (lambda (template)
				 (destructuring-bind (xle-tmpl msd pv . rest) template
				   (list* xle-tmpl
					  msd #+ascii(convert-encoding msd :amirani :unicode)
					  pv #+ascii(convert-encoding pv :amirani :unicode)
					  rest)))
			       templates)))
	(format stream "~a-~a V XLE ~:[~;~%  { ~]~{@(~{~a~^ ~})~^ ~%  | ~}~:[~; }~];"
		masdar
		id
		(cdr templates) templates (cdr templates))
	(when (eq (char (nth 3 (car templates)) 0) #\T)	;; gv
	  (write-char #\linefeed stream)
	  (format stream " Vpart XLE ~:[~;~%  { ~]~{@(~{~a~^ ~})~^ ~%  | ~}~:[~; }~];"
		  (cdr templates) templates (cdr templates)))
	(write-string " ETC." stream)
	(write-char #\linefeed stream))
      nil)))

(defun write-xle-verb-lexicon (&optional (file "projects:xle;grammars;georgian;georgian-verb-lex.lfg"))
  #+allegro(copy-file file "projects:xle;grammars;georgian;georgian-verb-lex.lfg~")
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (write-string "STANDARD KARTULI-VERBS LEXICON (1.0)" stream)
    (write-char #\linefeed stream)
    (write-char #\linefeed stream)
    (dat:do-string-tree  (verb-id val *xle-template-table*)
      (declare (ignore val))
      (write-xle-verb-lexicon-entry verb-id stream))
    (write-char #\linefeed stream)
    (write-char #\linefeed stream)
    (write-string "----" stream)
    (write-char #\linefeed stream)))

;; this assumes that the first entry is the V entry
(defun parse-xle-verb-entry (entry)
  (let ((entry-list (split entry #\;)))
    #+disabled
    (assert (and (null (cddr entry-list))
                 (or (null (cdr entry-list))
                     (string= (string-trim '(#\space #\tab) (cadr entry-list)) "ETC."))))
    (let ((entry (car entry-list)))
      (destructuring-bind (masdar+verb-id pos xle templates)
                          (split (nsubstitute #\space #\tab entry) #\space 4)  ;; should remove duplicate spaces?
        (declare (ignore xle))
        (let* ((split-pos (position-if (lambda (c) (find c "1234567890")) masdar+verb-id :start 1))
               (verb-id (subseq masdar+verb-id split-pos))
               (masdar (subseq masdar+verb-id 0 (1- split-pos))))
          (when (or (not (char= (char pos 0) #\+))
                    (null (dat:string-tree-get *xle-template-table* verb-id)))
            (setf (dat:string-tree-get *xle-template-table* verb-id)
                  (list masdar)))
          (let ((sub-entries (split (string-trim '(#\space #\tab #\{ #\}) templates) #\|)))
            (dolist (sub-entry sub-entries)
              (destructuring-bind (template vn pv gv id) (split (string-trim "@() " sub-entry) #\space)
                ;;(declare (ignore vn pv gv))
                (assert (string= id verb-id :start1 1))
                (pushnew (list template vn pv gv id)
                         (cdr (dat:string-tree-get *xle-template-table* verb-id)
                              #+old(gethash verb-id *xle-template-table*)) :test #'equal)))))))))

