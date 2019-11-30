;;;-*- Mode: Lisp; Package: GNC.TEXT -*-

(in-package :gnc.text)

(defun load-names (type)
  (let ((tree (dat:make-string-tree)))
    (u:with-file-fields ((word %type docs) "projects:gnc;data;names.tsv")
      (setf word (delete-if (lambda (c) (find c "@[](){}|,/\\+?:.“”„")) word))
      (unless (name-p word (ecase type
			     (:person "+Prop+Name")
			     (:place "+Prop+Geo")
			     (:people "+Prop+Name")))
	(when (string-equal type %type)
	  (let ((doc-list (u:split docs #\,)))
	    (dolist (doc doc-list)
	      (pushnew (intern (string-trim " " doc) :keyword)
		       (dat:string-tree-get tree word)))))))
    tree))

(defun name-p (word &optional (features "+Prop"))
  (cl-fst:fst-lookup
   (car *analyzer*)
   word
   (lambda (w l+f)
     (declare (ignore w))
     (return-from name-p (and (search features l+f) t)))))

#+test
(print (name-p "შუშანიკისი"))
#+test
(print (name-p "ათინელი"))

(progn
  (defparameter *place-names* (load-names :place))
  (defparameter *person-names* (load-names :person))
  (defparameter *people-names* (load-names :people)))

(defparameter *suffixes*
  '("ის" "ისა" "ისაჲ" "ისაო" "ისად" "ისათჳს" "ისი" "ისნი" "ისამან" "სისთგან" "ს" "სა" "საგან" "-ცა"
    "-ო" "ისამან" "მან" "სა" "ჲსგან" "ჲსსა" "სგან" "ნი" "ჲს" "თა" "ჲსი" "ჲსნი" "ისთა" "ს"
    "ცა" "ისათა" "სსა" "სცა" "ისათვის" "ოსადმდე" "ისთვის" "-ა" "ით"
    "ისასა" "თანა" "ისათა" "მ" "სი" "ისსა" "თურთ" "სთჳს" "სთა"
    "ელი" "ელთა" "ელთაგან" "ელმან" "ელსა" "ისაკენ"
    "ისანი" "ში" "ზედ" "ობით" "დ" "ჲ" "ისასა" "ისამან" "ს" "ისა" "ზედა"
    "ისგან" "ისაგან" "ისსა" "ისთასა" "თგან" "ისამან" "ჲსთჳს" "სნი""სამან" 
    "ელსა" "ელმან" "თათვის" "თაგან" "ელისა" "თაკენ" "თასა" "თანი"
    "თან" "ისანი" "ისმან"))


(defun is-suffix-p (suffix name)
  (and (>= (length name) (length suffix))
       (string= name suffix :start1 (- (length name) (length suffix)))))

(defun find-base-forms (tree &optional drop-ი-p)
  (setf *suffixes* (sort *suffixes* #'string>))
  (let ((stem-tree (dat:make-string-tree)))
    (dat:do-string-tree (name docs tree)
      (let ((foundp nil))
	(loop for suffix in *suffixes*
	   until (and (is-suffix-p suffix name)
		      (or (and (dat:string-tree-get
				tree
				(subseq name 0 (- (length name) (length suffix))))
			       (push name (dat:string-tree-get
					   stem-tree
					   (subseq name 0 (- (length name) (length suffix))))))
			  (and drop-ი-p
			       (dat:string-tree-get
				tree
				(u:concat (subseq name 0 (- (length name) (length suffix))) "ი"))
			       (push name (dat:string-tree-get
					   stem-tree
					   (u:concat (subseq name 0 (- (length name) (length suffix))) "ი")))))
		      (setf foundp t)))
	(unless foundp
	  (push name (dat:string-tree-get stem-tree name)))))
    (print (car stem-tree))
    stem-tree))

(progn
  (defparameter *person-stems* (find-base-forms *person-names* t))
  (defparameter *place-stems* (find-base-forms *place-names* t))
  (defparameter *people-stems* (find-base-forms *people-names* t)))

(with-open-file (stream "projects:gnc;data;person-names.tsv" :direction :output :if-exists :supersede)
  (dat:do-string-tree (stem names *person-stems*)
    (let ((doc-list ()))
      (dolist (name names)
	(dolist (doc (dat:string-tree-get *person-names* name))
	  (pushnew doc doc-list :test #'string=)))
      (format stream "~a	~{~a~^, ~}	~{~a~^, ~}~%" stem names (sort doc-list #'string<)))))

(with-open-file (stream "projects:gnc;data;place-names.tsv" :direction :output :if-exists :supersede)
  (dat:do-string-tree (stem names *place-stems*)
    (let ((doc-list ()))
      (dolist (name names)
	(dolist (doc (dat:string-tree-get *place-names* name))
	  (pushnew doc doc-list :test #'string=)))
      (format stream "~a	~{~a~^, ~}	~{~a~^, ~}~%" stem names (sort doc-list #'string<)))))

(with-open-file (stream "projects:gnc;data;people-names.tsv" :direction :output :if-exists :supersede)
  (dat:do-string-tree (stem names *people-stems*)
    (let ((doc-list ()))
      (dolist (name names)
	(dolist (doc (dat:string-tree-get *people-names* name))
	  (pushnew doc doc-list :test #'string=)))
      (format stream "~a	~{~a~^, ~}	~{~a~^, ~}~%" stem names (sort doc-list #'string<)))))

:eof
