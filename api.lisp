;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: FST; Base: 10; Readtable: augmented-readtable -*-
;;
;; JSON API for the Georgian Morphology noun/verb feature editor.
;; Replaces the XML/XSLT-based UI with endpoints consumed by a Vue 3 SPA.

(in-package :fst)

;; st-json is loaded via :depends-on in kartuli-paradigm.asd

(defvar *in-json* nil)

(defmacro json (&rest args)
  `(cond (*in-json*
          (st-json:jso ,@args))
         (t
          (let ((*in-json* t))
            (st-json:write-json
             (st-json:jso ,@args)
             stream)))))

(defun json-array (list stream)
  (write-char #\[ stream)
  (loop for (item . rest) on list
        do (st-json:write-json item stream)
        when rest do (write-char #\, stream))
  (write-char #\] stream))

;;; ---- Noun Features API ----

(define-url-function api/noun-features-json
    (request (search-stem search-code search-pos search-lang search-features search-style-features)
             :type :json
             :path (concatenate 'string "/" *url-base* "/api/noun-features")
             :headers '(("Access-Control-Allow-Origin" . "*")))
  (with-database-connection ()
    (let* ((stem-clause
            (cond ((or (null search-stem) (equal search-stem "")) nil)
                  ((and (> (length search-stem) 0)
                        (char= (char search-stem (1- (length search-stem))) #\#))
                   [like [stem] (substitute #\% #\* (subseq search-stem 0 (1- (length search-stem))))])
                  (t [like [stem] (u:concat (substitute #\% #\* search-stem) "%")])))
           (code-clause
            (when (and search-code (not (equal search-code "")))
              [= [code] ?search-code]))
           (pos-clause
            (when (and search-pos (not (equal search-pos "")))
              [= [pos] ?search-pos]))
           (lang-clause
            (when (and search-lang (not (equal search-lang "")))
              [= [lang] ?search-lang]))
           (features-clause
            (when (and search-features (not (equal search-features "")))
              [like [features] (u:concat "%" search-features "%")]))
           (style-features-clause
            (when (and search-style-features (not (equal search-style-features "")))
              [like [style-features] (u:concat "%" search-style-features "%")]))
           (where-clauses (remove nil (list stem-clause code-clause pos-clause
                                            lang-clause features-clause style-features-clause)))
           (noun-features-list
            (when where-clauses
              (collecting
                (let ((max-rows 1000))
                  (block query
                    (do-query ((&rest row)
                               [select [*]
                                       :from [morph noun-features]
                                       :where (if (cdr where-clauses)
                                                  (apply #'clsql-sys:sql-and where-clauses)
                                                  (car where-clauses))
                                       :order-by [stem]
                                       :limit 100])
                      (when (= (decf max-rows) -1)
                        (return-from query))
                      (collect (copy-seq row))))))))
           (*in-json* t)
           (items
            (mapcar (lambda (row)
                      (destructuring-bind (stem code pos sub-id features style-features template
                                                comment author translation date lang) row
                        (st-json:jso
                         "stem" (or stem :null)
                         "code" (or code :null)
                         "pos" (or pos :null)
                         "subId" (or sub-id :null)
                         "features" (or features :null)
                         "styleFeatures" (or style-features :null)
                         "lang" (or lang :null)
                         "template" (or template :null)
                         "comment" (or comment :null)
                         "author" (or author :null)
                         "translation" (or translation :null)
                         "date" (if date (u::format-universal-time date nil) :null)
                         "stemKey" (format nil "~a@~a@~a@~a" stem code pos sub-id))))
                    noun-features-list)))
      (st-json:write-json
       (st-json:jso "count" (length items)
                     "nouns" items)
       stream))))

;;; ---- Noun Feature Choices API ----

(define-url-function api/noun-feature-choices-json
    (request (type)
             :type :json
             :path (concatenate 'string "/" *url-base* "/api/noun-feature-choices")
             :headers '(("Access-Control-Allow-Origin" . "*")))
  (with-database-connection ()
    (let ((*in-json* t)
          (values
           (cond
             ((string-equal type "code")
              (mapcar (lambda (pair)
                        (destructuring-bind (code &optional example) pair
                          (st-json:jso "value" code "example" (or example :null))))
                      '(("A" "კაცი - კაცის - კაცები")
                        ("A1")
                        ("B" "კედელი - კედლის - კედლები")
                        ("C")
                        ("D" "ქვეყანა - ქვეყნის - ქვეყნები")
                        ("D1" "ქვეყანა - ქვეყ(ა)ნის - ქვეყნები")
                        ("F" "მხარე - მხრის - მხრები")
                        ("I" "ძმა - ძმის - ძმები")
                        ("K" "გოგონა - გოგონას - გოგონები")
                        ("M" "ხე - ხის - ხეები")
                        ("M1" "დღე - დღეის")
                        ("O" "სოკო - სოკოს - სოკოები")
                        ("O1" "ღვინო")
                        ("O2" "ღვინო")
                        ("O3" "დრო - დროის")
                        ("P" "ნიორი - ნივრის - ნივრები")
                        ("R" "ხატვა - ხატვის")
                        ("U" "ხუცესი - ხუცის - ხუცები")
                        ("X" "ტრამვაი - ტრამვაის - ტრამვაები")
                        ("Z" "დავით - დავითის")
                        ("H" "აშშ - აშშ-ის"))))
             ((string-equal type "pos")
              (mapcar (lambda (v) (st-json:jso "value" v))
                      '("a" "ad" "n" "int" "p.a." "p.f." "p.n." "p.p.")))
             ((string-equal type "features")
              (mapcar (lambda (v) (st-json:jso "value" (or v "")))
                      *feature-list*))
             ((string-equal type "styleFeatures")
              (mapcar (lambda (v) (st-json:jso "value" (or v "")))
                      *style-feature-list*))
             ((string-equal type "lang")
              (mapcar (lambda (v) (st-json:jso "value" v))
                      '("ng" "mg" "og")))
             ((string-equal type "template")
              (mapcar (lambda (v) (st-json:jso "value" v))
                      '("" "NOUN-COMP")))
             (t
              nil))))
      (st-json:write-json
       (st-json:jso "type" type
                     "values" values)
       stream))))

;;; ---- Noun Update API ----

(define-url-function api/noun-update-json
    (request (action stem-key stem code pos features style-features lang template comment)
             :type :json
             :path (concatenate 'string "/" *url-base* "/api/noun-update")
             :headers '(("Access-Control-Allow-Origin" . "*")))
  (with-database-connection ()
    (handler-case
        (progn
          (when (equal stem "-") (setf stem nil))
          (destructuring-bind (old-stem old-code old-pos sub-id) (split stem-key #\@)
            (destructuring-bind (old-features old-style-features old-lang old-template old-comment)
                (car (select [features] [style-features] [lang] [template] [comment]
                       :from [morph noun-features]
                       :where [and [= [stem] ?old-stem]
                                   [= [code] ?old-code]
                                   [= [pos] ?old-pos]
                                   [= [sub-id] ?sub-id]]))
              (cond ((equal features "-") (setf features old-features))
                    ((equal features "*") (setf features "")))
              (cond ((equal style-features "-") (setf style-features old-style-features))
                    ((equal style-features "*") (setf style-features "")))
              (cond ((equal template "-") (setf template old-template))
                    ((equal template "*") (setf template "")))
              (when (equal code "-") (setf code nil))
              (when (equal pos "-") (setf pos nil))
              (when (equal comment "-") (setf comment nil))
              (when (equal template "-") (setf template nil))
              (ecase (intern (string-upcase action) :keyword)
                (:add
                 (let* ((sub-ids (select [sub-id]
                                         :from [morph noun-features] :flatp t
                                         :where [and [= [stem] (or stem old-stem)]
                                                     [= [code] (or code old-code)]
                                                     [= [pos] (or pos old-pos)]]))
                        (new-sub-id (loop for i from 1
                                          when (not (find i sub-ids))
                                          do (return i))))
                   (with-transaction ()
                     (insert-records :into [morph noun-features]
                                     :attributes (list [stem] [code] [pos] [sub-id]
                                                       [features] [template] [comment]
                                                       [author] [date])
                                     :values (list (or stem old-stem) (or code old-code) (or pos old-pos) new-sub-id
                                                   (substitute #\+ #\space features) template comment
                                                   "PM" (get-universal-time))))))
                (:change
                 (with-transaction ()
                   (update-records [morph noun-features]
                                    :attributes (list [code] [pos] [features] [style-features]
                                                      [lang] [template] [comment] [date])
                                    :values (list (or code old-code) (or pos old-pos)
                                                  (substitute #\+ #\space features)
                                                  (when style-features (substitute #\+ #\space style-features))
                                                  lang template
                                                  (or comment
                                                      (cond ((null old-comment) "PM")
                                                            ((search "PM" old-comment) old-comment)
                                                            (t (u:concat old-comment ", PM"))))
                                                  (get-universal-time))
                                    :where [and [= [stem] ?old-stem]
                                                [= [code] ?old-code]
                                                [= [pos] ?old-pos]
                                                [= [sub-id] ?sub-id]])))
                (:delete
                 (with-transaction ()
                   (delete-records :from [morph noun-features]
                                    :where [and [= [stem] ?old-stem]
                                                [= [code] ?old-code]
                                                [= [pos] ?old-pos]
                                                [= [sub-id] ?sub-id]]))))))
          (st-json:write-json (st-json:jso "success" :true) stream))
      (error (c)
        (st-json:write-json (st-json:jso "error" (format nil "~a" c)) stream)))))

;;; ---- Serve Vue app static files ----

(publish-directory :prefix (concatenate 'string "/" *url-base* "/app/")
                   :destination (namestring
                                 (merge-pathnames "vue-app/dist/"
                                                  (asdf:system-source-directory :kartuli-paradigm))))

:eof
