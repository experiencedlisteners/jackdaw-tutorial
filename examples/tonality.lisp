(ql:quickload "fare-csv")
(ql:quickload "jackdaw")
(ql:quickload "cl-ansi-term")

(defparameter *pitch-names-with-sharps* '(c c# d d# e f f# g g# a a# b ))
(defparameter *pitch-names-with-flats* '(c db d eb e f gb g ab a bb b))

(defun pitch-name (tonic pitch &key (octave-size 12) exclude-octave)
  (let* ((octave (floor (/ pitch octave-size)))
	 (degree (mod pitch octave-size))
	 (name
	   (if (<= tonic 7)
	       (elt *pitch-names-with-flats* degree)
	       (elt *pitch-names-with-sharps* degree))))
    (intern (format nil "~a~a" name (if exclude-octave "" octave)))))

(jd:defmodel musical-key (jd:dynamic-bayesian-network)
  (&key order (octave 12)
	(pitch-alphabet (loop for p below 120 collect p)))
  ((Tonic (^tonic) ; now a variable rather than a constant
	  (jd:uniform ())
	  (jd:persist $^tonic
	   (loop for tonic below octave collect tonic))
	  :observer #'first)
   (Tonic-name (Tonic) ; only for convenience
	       (jd:uniform ())
	       (list
		(pitch-name $tonic $tonic
			    :octave-size octave
			    :exclude-octave t))
	       :observer #'first)
   (Scale ()
	  (jd:cpt ())
	  (list 'major 'minor)
	  :observer #'second)
   (Scale-degree (^Scale-degree Scale)
		 (jd:ppms (Scale) :order-bound order)
		 (jd:markov order $^scale-degree
			    (loop for deg below octave collect deg))
		 :observer #'third)
   (Pitch (Tonic Scale-degree) ; now also depends on Tonic
	  (jd:uniform ())
	  (loop for pitch in pitch-alphabet
		if (eq (mod (- pitch $tonic) 12)
		       (car $scale-degree)) ; use variable $tonic, rather than constant tonic
		  collect pitch)
	  :observer
	  (lambda (m)
	    (if (listp m) (third m) m)))
   (Pitch-name (Tonic Pitch)
	       (jd:uniform ())
	       (list
		(pitch-name $tonic $pitch
			    :octave-size octave))
	       :observer
	       (lambda (m)
		 (if (listp m) (third m) m)))))


(defun annotate-melody (keysig mode melody)
  (let ((scale (case mode
		 (0 'major)
		 (9 'minor)
		 (t (error "Unrecognized model ~a" mode))))
	(tonic (mod (* keysig 7) 12)))
    (loop for pitch in melody collect (list tonic scale pitch))))

(defun preprocess (rows)
  (let* ((dataset)
	 (rows (cdr rows))) ; skip header
    (dolist (row rows)
      (destructuring-bind (name keysig mode melody)
	  (mapcar #'read-from-string row)
	(declare (ignore name))
	(push (annotate-melody keysig mode melody) dataset)))
    (reverse dataset)))

(defun load-data (path)
  (let ((data (fare-csv:with-rfc4180-csv-syntax ()
		(fare-csv:read-csv-file path))))
    (preprocess data)))

(defun parameterize-model (model path)
  (jd:hide model)
  (jd:observe model 'pitch 'scale 'tonic)
  (let ((data (load-data path)))
    (jd:estimate model data)))

;; Example usage 
;;
;; (defparameter *key* (make-musical-key-model))
;; (parameterize-model *key* "path/to/jackdaw-tutorial/materials/mtc-melodies.csv")
;; (jd:hide *key*)
;; (jd:observe *key* 'pitch)
;; (term:table 
;;  (jd:state-probability-table 
;;   (jd:posterior (jd:generate *key* '(7 5 4 0 2 7 0)))
;;   :variables '(tonic-name scale) :sort t)
;;  :column-width 15)
