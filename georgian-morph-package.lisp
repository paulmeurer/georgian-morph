;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-


(in-package :cl-user)

(defpackage :georgian-morph
  (:nicknames :gm)
  (:use #+mclzz"CCL" "COMMON-LISP" "UTILS" "STRING-NET" "PARSER" "TRANSDUCER")
  (:export ))

:eof