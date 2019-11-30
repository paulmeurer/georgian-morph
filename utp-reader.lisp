;;;-*- Mode: Lisp; Package: (TRANSDUCER) -*-

(in-package :fst)

(defparameter *saved-readtable* *readtable*)
#+allegro
(setf (excl:named-readtable :augmented-readtable) *saved-readtable*)

(defparameter *fst-readtable* (copy-readtable))
#+allegro
(setf (excl:named-readtable :fst-readtable) *fst-readtable*)
;;(setf (readtable-case *xml-readtable*) :preserve)

;; getting into the fst readtable
(set-dispatch-macro-character 
 #\# #\[
 (lambda (stream c1 c2)
   (declare (ignore c1 c2))
   (let* ((*readtable* *fst-readtable*)
          (list (read-delimited-list #\] stream t)))
     (list 'precompile-u-transducer (quote-operators (caddr list))
           :name (list 'quote (car list)))))
 *readtable*)

(set-macro-character
 #\?
 (lambda (stream char)
   (declare (ignore char))
   (if (member (peek-char nil stream) 
               '(#\( #\Null #\Tab #\Linefeed #\Page #\Return #\Space #.(code-char 202)))
     '?
     (intern (string (read stream t nil t)) :keyword)))
 t *fst-readtable*)

(set-macro-character
 #\[
 (lambda (stream ignore)
   (declare (ignore ignore))
   (let ((fst-list (read-delimited-list #\] stream t)))
     (destructuring-bind (morph &optional fs augment) fst-list
       (cond ((eq morph 'e) ; epsilon transition
              (list 'utp-e (list 'quote fs)))
             ((eq morph 'not)
              (list 'utp-not (list 'quote fs)))
             ((listp morph)
              (cons 'or
                    (mapcar (lambda (morph)
                              (list 'utp
                                    morph (list 'quote
                                                (substitute-morph fs morph))
                                    :augment (list 'quote augment)))
                            morph)))
             (t
              (list 'utp 
                    morph
                    (list 'quote fs)
                    :augment (list 'quote augment)))))))
 nil *fst-readtable*)

(defun substitute-morph (fs morph)
  (cond ((eq :morph fs)
         morph)
        ((consp fs)
         (mapcar (lambda (f) (substitute-morph f morph))
                 fs))
        (t fs)))

(defun quote-operators (reg-list)
  (let ((op (when (listp reg-list) (car reg-list))))
    (cond ((member op '(seq or ? * +))
           (list* 'list
             (list 'quote op)
             (mapcar #'quote-operators (cdr reg-list))))
          ((symbolp reg-list)
           (list 'quote reg-list))
          (t
           reg-list))))

:eof