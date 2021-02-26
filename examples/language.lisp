(ql:quickload :jackdaw)
(ql:quickload :dexador)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-strings)

(jd:defmodel letter-ppm (jd:dynamic-bayesian-network) 
  (&key 
   order
   update-exclusion?
   (mixtures t)
   (alphabet '(a b c d r)))
  ((Letters (^Letters)
	    (jd:ppms ())
	    (jd:markov order $^letters alphabet))
   (Current-letter (Letters)
		   (jd:uniform ())
		   (list (car $letters))
		   :observer
		   (lambda (m)
		     (if (listp m) (second m) m)))))

(jd:defmodel language (jd:dynamic-bayesian-network)
  (&key order (alphabet '(a b c)))
  ((Language ()
	     (jd:cpt ())
	     (list '(english dutch))
	     :observer #'first)
   (Letters (^Letters Language)
	    (jd:ppms (Language))
	    (jd:markov order $^letters alphabet))
   (Current-letter (Letters)
		   (jd:uniform ())
		   (list (car $letters))
		   :observer
		   (lambda (m)
		     (if (listp m) (second m) m)))))

(defun split-list (lis n)
  (let ((subs))
    (do ((s lis (nthcdr (min n (length s)) s)))
	((null s))
      (push (subseq s 0 (min n (length s))) subs))
    (reverse subs)))

(defun prepare-text (text)
  (let* ((text (cl-ppcre:regex-replace-all
		(cl-ppcre:create-scanner '(:sequence #\- #\newline) :multi-line-mode t)
		text ""))
	 (text (cl-ppcre:regex-replace-all
		(cl-ppcre:create-scanner '(:sequence #\newline) :multi-line-mode t)
		text " "))
	 (text (cl-strings:clean-diacritics text))
	 (text (cl-ppcre:regex-replace-all "[^A-Za-z ]" text ""))
	 (text (cl-ppcre:regex-replace-all "  +" text " "))
	 (text (string-downcase text)))
    (coerce text 'list)))

(defun load-language-data (file) 
  (let* ((text (uiop:read-file-string file)))
    (prepare-text text)))

(defun annotate (data value)
  (mapcar (lambda (item) (list value item))  data))

(defun estimate-letter-ppm-model (filename)
  (let* ((data (load-language-data filename))
	 (alphabet (remove-duplicates data))
	 (data (split-list data 100))
	 (model (make-letter-ppm-model :order 10 :alphabet alphabet :observe '(current-letter))))
    (jd:estimate model data)))

(defun estimate-language-model ()
  (let* ((dutch (load-language-data "materials/de-komedianten.txt"))
	 (english (load-language-data "materials/alice-in-wonderland.txt"))
	 (length (min (length dutch) (length english))) 
	 (dutch (subseq dutch 0 length))
	 (english (subseq english 0 length))
	 (alphabet (remove-duplicates (append dutch english)))
	 (dutch (split-list (annotate dutch 'dutch) 100))
	 (english (split-list (annotate english 'english) 100))
	 (data (append dutch english))
	 (model (make-language-model :alphabet alphabet :observe '(language current-letter))))
    (jd:estimate model data)))

(defun text-info-rate (m s)
  (let ((probabilities:*log-space* t))
    (/ (jd:probability m (prepare-text s))
       (length s))))
