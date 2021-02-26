(ql:quickload :jackdaw)
(ql:quickload :dexador)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-strings)

(jd:defmodel letter-ppm (jd:dynamic-bayesian-network) 
  (&key 
   order
   update-exclusion
   (mixtures t)
   (escape :c)
   (alphabet '(a b c d r)))
  ((Letters (^Letters)
	    (jd:ppms ()
		     :order-bound order
		     :update-exclusion update-exclusion
		     :mixtures mixtures
		     :escape escape)
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
	     '(english dutch)
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
  (let* (;; Replace dash followed by newline with empty string
	 (text (cl-ppcre:regex-replace-all
		(cl-ppcre:create-scanner '(:sequence #\- #\newline) :multi-line-mode t)
		text ""))
	 ;; Replace other newlines with spaces
	 (text (cl-ppcre:regex-replace-all
		(cl-ppcre:create-scanner '(:sequence #\newline) :multi-line-mode t)
		text " "))
	 ;; Attempt to replace diacritics with ASCI characters
	 (text (cl-strings:clean-diacritics text))
	 ;; Remove everything that isn't a letter or whitespace
	 (text (cl-ppcre:regex-replace-all "[^A-Za-z ]" text ""))
	 ;; Remove repetitions of whitespace
	 (text (cl-ppcre:regex-replace-all "  +" text " "))
	 ;; Convert everything to lower case
	 (text (string-downcase text)))
    (coerce text 'list)))

(defun load-language-data (file) 
  (let* ((text (uiop:read-file-string file)))
    (prepare-text text)))

(defun annotate (data value)
  (mapcar (lambda (item) (list value item))  data))

(defmethod estimate-model ((m letter-ppm)
			   &optional (file "materials/alice-in-wonderland.txt"))
  (let* ((data (load-language-data file))
	 (alphabet (remove-duplicates data))
	 (data (split-list data 100)))
    (setf (alphabet m) alphabet)
    (jd:hide m)
    (jd:observe m 'Current-letter)
    (jd:estimate m data)))

(defmethod estimate-model ((m language)
			   &optional (data-dir "materials/"))
  (let* ((dutch (load-language-data
		 (format nil "~ade-komedianten.txt" data-dir)))
	 (english (load-language-data
		   (format nil "~aalice-in-wonderland.txt" data-dir)))
	 (length (print (min (length dutch) (length english))))
	 (dutch (subseq dutch 0 length))
	 (english (subseq english 0 length))
	 (alphabet (remove-duplicates (append dutch english)))
	 (dutch (split-list (annotate dutch 'dutch) 100))
	 (english (split-list (annotate english 'english) 100))
	 (data (append dutch english)))
    (setf (alphabet m) alphabet)
    (jd:hide m)
    (jd:observe m 'Language 'Current-letter)
    (jd:estimate m data)))

(defmethod text-info-rate ((m letter-ppm) s)  
  (let ((probabilities:*log-space* t))
    (/ (jd:probability m (prepare-text s))
       (length s))))

(defmethod classify-language ((m language) s)
  (jd:hide m)
  (jd:observe m 'Current-letter)
  (term:table
   (jd:state-probability-table
    (jd:posterior (jd:generate m (prepare-text s)))
    :variables '(language))
   :column-width 15))

;; Example uses letter-ppm model
;;
;; (defparameter *letter-ppm* (make-letter-ppm-model))
;;
;; (estimate-model *letter-ppm* "path/to/materials/alice-in-wonderland.txt")
;; 
;; (text-info-rate *letter-ppm* "The probability that this particular English sentence has never been written before is substantial")
;; (text-info-rate *letter-ppm* "Een zin in een andere taal")
;; (text-info-rate *letter-ppm* "Bob")
;; (text-info-rate *letter-ppm* "Alice")
;; (text-info-rate *letter-ppm* "Rabbit")
;; (text-info-rate *letter-ppm* "Uasbgoe")
;;
;; Example usage language model
;; 
;; (defparameter *language* (make-language-model))
;;
;; Or with order bound:
;;
;; (defparameter *language* (make-language-model :order 5))
;;
;; (estimate-model *language* "path/to/jackdaw-tutorial/materials/")
;;
;; Models with an order bound are surprisingly unsure about this one:
;; (classify-language *language* "Welke taal is dit?")
;; (classify-language *language* "Which language is this?")

