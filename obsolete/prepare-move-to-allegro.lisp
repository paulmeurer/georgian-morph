;;;-*- Mode: Lisp; Package: TRANSDUCER -*-
;;;

(in-package :fst)

;; MonacoGe -> Amirani
(defparameter *conversion*
    '(#\� #\a #\� #\b #\� #\g #\� #\d #\� #\e #\� #\v #\�
      #\z #\� #\T #\�
      #\i #\� #\k #\� #\l #\� #\m #\� #\n #\� #\o #\� #\p #\�
      #\Z #\� #\r #\� #\s #\� #\t #\� #\u #\� #\P #\� #\K #\�
      #\G #\� #\q #\� #\S #\� #\X #\� #\C #\�
      #\j #\� #\c #\� #\x #\� #\H #\� #\J #\� #\h #\� #\1 #\� #\2 #\� #\3 #\� #\4 #\� #\5 #\� #\6 #\� #\7))

;; Amirani -> MonacoGe
(defparameter *inverse-conversion*
    '(#\a #\� #\b #\� #\g #\� #\d #\� #\e #\� #\v #\� #\z #\�
      #\T #\� #\i #\�
      #\k #\� #\l #\� #\m #\� #\n #\� #\o #\� #\p #\� #\Z #\�
      #\r #\� #\s #\� #\t #\� #\u #\� #\P #\� #\K #\� #\G #\�
      #\q #\� #\S #\� #\X #\� #\C #\� #\j #\�
      #\c #\� #\x #\� #\H #\� #\J #\� #\h #\�))

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
(aux-root-lookup "���")

(let ((*print-circle* nil))
  (pprint (aux-root-lookup (inv-convert "Sen"))))

|#


:eof