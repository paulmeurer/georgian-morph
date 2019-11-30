;;;   -*- Mode: LISP; Package: COMMON-LISP-USER; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package :cl-user)

(asdf:defsystem :georgian-morph
  :name "georgian-morph"
  :depends-on (:directed-graphs
               :unification-fst
	       :encoding
               :string-net :dat)
  :serial t
  :components ((:file "utp-reader")
               (:file "noun-transducer")
               (:file "verb-transducer")
               (:file "conversion")
               (:file "morphology")
	       (:file "feature-templates")))

:eof

