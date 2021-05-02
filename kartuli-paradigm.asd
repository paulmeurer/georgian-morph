;;;   -*- Mode: LISP; Package: COMMON-LISP-USER; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package :cl-user)

(asdf:defsystem :kartuli-paradigm
  :depends-on (:encoding
	       :georgian-morph
	       :clsql-postgresql-patches
	       #-(or allegro sbcl) :aserve
	       #-sbcl :aserve-custom
	       :javascript :xml
	       :cl-fst)
  :serial t
  :components ((:file "database")
	       (:file "class")
	       (:file "verb-feature-table-sql")
	       (:file "paradigms")
	       (:file "paradigm-www")
	       (:file "noun-table-www")))


:eof
