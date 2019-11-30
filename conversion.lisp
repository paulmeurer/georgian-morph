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
      #\j #\ó #\c #\ô #\x #\õ #\H #\ö #\J #\÷ #\h #\± #\1 #\² #\2 #\³ #\3 #\´ #\4 #\µ #\5 #\¶ #\6 #\· #\7 #\Â #\- #\ª #\|))

(defparameter *conversion-table* (make-hash-table))

(loop for (m a) on *conversion* by #'cddr
      do (setf (gethash m *conversion-table*) a))

;; Amirani -> MonacoGe
(defparameter *inverse-conversion*
    '(#\a #\× #\b #\Ø #\g #\Ù #\d #\Ú #\e #\Û #\v #\Ü #\z #\Ý
      #\T #\Þ #\i #\ß
      #\k #\à #\l #\á #\m #\â #\n #\ã #\o #\ä #\p #\å #\Z #\æ
      #\r #\ç #\s #\è #\t #\é #\u #\ê #\P #\ë #\K #\ì #\G #\í
      #\q #\î #\S #\ï #\X #\ð #\C #\ñ #\j #\ò
      #\c #\ó #\x #\ô #\H #\õ #\J #\ö #\h #\÷))

#+debug
(defparameter *char-list* ())

(defun convert (str)
  (if nil ;;(stringp str)
    (let ((c-str (copy-seq str)))
      (loop for i from 0 for c across str
            do (setf (char c-str i)
                     (let ((c (gethash c *conversion-table* c)))
                       #+debug(pushnew c *char-list*)
                       c)))
      c-str)
    str))

(defun sconvert (str)
  (cond ((stringp str)
	 str)
	((symbolp str)
	 (string-downcase str))
	(t
	 str)))

(defun nconvert (str)
  (declare (optimize (speed 3)))
  (if nil ;; (stringp str)
    (progn (loop for i from 0 for c across str
                 do (setf (char str i)
                          (gethash c *conversion-table* c)
                          #+ignore(getf *conversion* c c)))
           str)
    str))

(defun inv-convert (str)
  str)

;; from paradigm-www.lisp

(defparameter +georgian-unicode+
  (map 'string #'code-char #(4304 4305 4306 4307 4308 4309 4310 4311 4312 4313 4314 4315 4316 4317
			     4318 4319 4320 4321 4322 4323 4324 4325 4326 4327 4328 4329 4330 4331
			     4332 4333 4334 4335 4336
			     4337 4338 4339 4340 4341 4342 ;; 4343 ;; old Georgian
                             ;; Âª±²³´µ¶·
                             124 49 50 51 52 53 54 55 8211 8221 160)))

(defparameter +georgian-amirani+
  "abgdevzTiklmnopZrstuPKGqSXCjcxHJhEywQOF|1234567-\" ")

(defparameter +georgian-monacoGe+
  (map 'string #'code-char #(215 216 217 218 219 220 221 222 223 224 225 226 227 228 229
                             230 231 232 233 234 235 236 237 238 239 240 241 242 243 244
                             245 246 247
			     250 249 251 248 249 250 ;; old Georgian
                             170 177 178 179 180 181 182 183)))

(defparameter +georgian-geo+
  (map 'string #'code-char #(192 193 194 195 196 197 198 200 201 202 203 204 205 207 208
			     209 210 211 212 214 215 216 217 218 219 220 221 222 223 224
                             225 227 228)))

(defparameter +georgian-acad+ ;;     |||||| <- these have to be fixed!
  "abgdevzTiklmnopJrstufqRySCcZwWxjhEywQOF|1234567-\" ")

#+test
(print (convert-encoding "abgdevzTiklmnopZrstuPKGqSXCjcxHJh" :amirani :acad))
#+test
(print (convert-encoding "...iseTive sufTa da carieli, rogoric saklaso dafaa klasSi maswavleblis mosvlamde..." :acad :amirani))

#+test
(with-open-file (stream "~/lisp/projects/treebank/data/sofie-kat/sofie-kat.txt" :direction :output :if-exists :supersede)
  (with-file-lines (line "~/lisp/projects/treebank/data/sofie-kat/Sofi_Geo.doc.txt")
    (write-line (convert-encoding line :acad :amirani) stream)))

#+test
(with-open-file (stream "~/lisp/projects/treebank/data/sofie-kat/sofie1-kat-utf8.txt" :direction :output :if-exists :supersede)
  (with-file-lines (line "~/lisp/projects/treebank/data/sofie-kat/sofie1-kat.txt")
    (write-line (convert-encoding line :amirani :unicode) stream)))

#+test
(with-open-file (stream "~/lisp/projects/georgian-morph/lists/missing-fut-part-utf8.txt" :direction :output :if-exists :supersede)
  (with-file-lines (line "~/lisp/projects/georgian-morph/lists/missing-fut-part.txt")
    (write-line (convert-encoding line :amirani :unicode) stream)))

(defun convert-encoding (string from-encoding to-encoding &key (copy-p t))
  (if (or (eq from-encoding to-encoding)
	  (null from-encoding)
	  (null to-encoding))
      string
      (let* ((from-vector
	      (ecase from-encoding
		(:unicode +georgian-unicode+)
		(:monacoge +georgian-monacoGe+)
		(:amirani +georgian-amirani+)
		(:acad +georgian-acad+)
		(:geo +georgian-geo+)))
	     (to-vector
	      (ecase to-encoding
		((:unicode :utf-8) +georgian-unicode+)
		(:monacoge +georgian-monacoGe+)
		(:amirani +georgian-amirani+)
		(:acad +georgian-acad+)
		(:geo +georgian-geo+))))
        (case to-encoding
          (:utf-8
           (with-output-to-string (stream)
             (loop for c across string for i from 0
	           do (let ((pos (position c from-vector)))
		        (if pos
                          (encoding::write-unicode-to-utf-8 (char-code (char to-vector pos)) stream)
                          (write-char c stream))))))
          (otherwise
           (let ((result (if copy-p (copy-seq string) string)))
	     (loop for c across result for i from 0
	           do 
		   (when (eq (char-code c) 160) (setf c #\space)) ;; nbsp
		   (let ((pos (position c from-vector)))
		     (when pos (setf (char result i) (char to-vector pos)))))
	     result))))))

#+test
(convert-encoding "Ù×â×çöäØ×!" :monacoge :utf-8)

;; converts to unicode (UTF-8) and puts <geo>- and <ger>-elts around language runs
(defun xml-convert-utf-8 (str)
  (let* ((from-vector +georgian-monacoGe+)
         (to-vector +georgian-unicode+)
         (startp t)
         (geo-p nil))
    (with-output-to-string (stream)
      (loop for c across str for i from 0
            do (let* ((c (if (char= c #\Â) #\- c))
                      (pos (position c from-vector)))
                 (when startp
                   (cond (pos
                          (write-string "<geo>" stream)
                          (setf geo-p t))
                         (t
                          (write-string "<ger>" stream)))
                   (setf startp nil))
                 (cond ((and geo-p (not (or pos (find c " ,;.:!?(-)ª/[~]"))))
                        (setf geo-p nil)
                        (write-string "</geo><ger>" stream))
                       ((and (not geo-p) pos)
                        (setf geo-p t)
                        (write-string "</ger><geo>" stream)))
                 (cond (pos
                        (encoding::write-unicode-to-utf-8 (char-code (char to-vector pos)) stream))
                       ((eq c #\<)
                        (write-string "&lt;" stream))
                       ((eq c #\>)
                        (write-string "&gt;" stream))
                       ((eq c #\&)
                        (write-string "&amp;" stream))
                       (t
                        (encoding::write-unicode-to-utf-8 (char-code c) stream)))))
      (write-string (if geo-p "</geo>" "</ger>") stream))))

;;(xml-convert-utf-8 "Ù×â×çöäØ×! hei§t gugu. óßóß")

:eof