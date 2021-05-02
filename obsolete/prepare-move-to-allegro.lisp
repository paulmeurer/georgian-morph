;;;-*- Mode: Lisp; Package: TRANSDUCER -*-
;;;

(in-package :fst)

;; MonacoGe -> Amirani
(defparameter *conversion*
    '(#\× #\a #\Ø #\b #\Ù #\g #\Ú #\d #\Û #\e #\Ü #\v #\Ý
      #\z #\Þ #\T #\ß
      #\i #\à #\k #\á #\l #\â #\m #\ã #\n #\ä #\o #\å #\p #\æ
      #\Z #\ç #\r #\è #\s #\é #\t #\ê #\u #\ë #\P #\ì #\K #\í
      #\G #\î #\q #\ï #\S #\ð #\X #\ñ #\C #\ò
      #\j #\ó #\c #\ô #\x #\õ #\H #\ö #\J #\÷ #\h #\± #\1 #\² #\2 #\³ #\3 #\´ #\4 #\µ #\5 #\¶ #\6 #\· #\7))

;; Amirani -> MonacoGe
(defparameter *inverse-conversion*
    '(#\a #\× #\b #\Ø #\g #\Ù #\d #\Ú #\e #\Û #\v #\Ü #\z #\Ý
      #\T #\Þ #\i #\ß
      #\k #\à #\l #\á #\m #\â #\n #\ã #\o #\ä #\p #\å #\Z #\æ
      #\r #\ç #\s #\è #\t #\é #\u #\ê #\P #\ë #\K #\ì #\G #\í
      #\q #\î #\S #\ï #\X #\ð #\C #\ñ #\j #\ò
      #\c #\ó #\x #\ô #\H #\õ #\J #\ö #\h #\÷))

(defun convert (str)
  (let ((c-str (copy-seq str)))
    (loop for i from 0 for c across str
          do (setf (char c-str i) (getf *conversion* c c)))
    c-str))

(defun inv-convert (str)
  (let ((c-str (copy-seq str)))
    (loop for i from 0 for c across str
          do (setf (char c-str i) (getf *inverse-conversion* c c)))
    c-str))

;; verb root list (only main allomorph, no homonyms)
(defun write-keys-to-stream (&optional stream (btree *verb-btree*))
  (when btree
    (wood:p-map-btree 
     btree
     (lambda (key entry-pp)
       (declare (ignore entry-pp))
       (write-line (convert (dict::unmark-word key)) stream)))))

#+test
(with-open-file (stream "projects:georgian-morph;roots.txt" :direction :output :if-exists :supersede)
  (write-keys-to-stream stream))

;; list of roots with allomorphs
(defun write-root-lists-to-stream (&optional (stream t) (btree *root-btree*))
  (when btree
    (wood:p-map-btree 
     btree
     (lambda (key entry-pp)
       (format stream "~a~{ ~a~}~%"
               (convert (dict::unmark-word key))
               (mapcar #'convert (wood:p-load entry-pp)))))))

;; under construction
(defun write-root-entries-to-stream (&optional stream (btree *verb-btree*))
  (when btree
    (wood:p-map-btree 
     btree
     (lambda (key entry-pp)
       (write-line (convert (dict::unmark-word key)) stream)
       (print (wood:p-load entry-pp))))))

#+test
(with-open-file (stream "projects:georgian-morph;root-lists.txt" :direction :output :if-exists :supersede)
  (write-root-lists-to-stream stream))

#+test
(write-root-entries-to-stream)

#|
(aux-root-lookup "èÜá")

(let ((*print-circle* nil))
  (pprint (aux-root-lookup (inv-convert "Sen"))))

|#


:eof