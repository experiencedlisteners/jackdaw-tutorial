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

(jd:defmodel scale-degree-viewpoint (jd:dynamic-bayesian-network)
  (tonic &key order-bound (octave 12)
	 (pitch-alphabet (loop for p below 24 collect p)))
  ((Scale-degree (^Scale-degree)
		 (jd:ppms ())
		 (jd:markov
		  $^scale-degree
		  order-bound
		  (loop for deg below octave collect deg))
		  :observer #'identity)
   (Pitch (Scale-degree)
	  (jd:uniform ())
	  (loop for pitch in pitch-alphabet
		if (eq (mod (- pitch tonic) octave)
		       (first $scale-degree))
		  collect pitch)
	  :observer #'identity)
   (Pitch-name (Pitch)
	       (jd:deterministic ())
	       (jd:deterministic
		(pitch-name tonic $pitch octave))
	       :observer #'identity)))

(jd:defmodel scale-degree-viewpoint (jd:dynamic-bayesian-network)
  (tonic &key order-bound (octave 12)
	 (pitch-alphabet (loop for p below 24 collect p)))
  ((Scale-degree (^Scale-degree)
		 (jd:ppms ())
		 (jd:markov
		  $^scale-degree
		  order-bound
		  (loop for deg below octave collect deg)))
   (Event (Scale-degree)
	  (jd:uniform ())
	  (let ((events))
	    (dotimes (tonic octave events)
	      (dolist (pitch pitch-alphabet)
		(when (eq (mod (- pitch tonic) octave)
		          (first $scale-degree))
                  (push (list pitch tonic) events)))))
	  :observer #'identity)))

(jd:defmodel pitch-interval-viewpoint (jd:dynamic-bayesian-network)
  (&key (pitches 123))
  ((pitch-intervals (^pitch-intervals ^pitches)
		   (ppms ())
		   (chain (accumulate
			   $^pitch-intervals
			   (loop for pitch below pitches collect (- pitch (car $^pitches))))
			  $^pitches))
   (pitches (^pitches pitch-intervals)
	    (deterministic ())
	    (accumulate
	     $^pitches (list (+ (car $^pitches) (car $pitch-intervals))))
	     :observer #'identity)
   (pitch (Pitches)
	  (deterministic ())
	  (deterministic
	   (car $pitches)))))

(defmodel derived-viewpoint (dynamic-bayesian-network)
  (event-domain viewpoint-function
		&key
		update-exclusion
		order-bound
		(escape :c)
		(mixtures t))
  ((viewpoint-elements (^viewpoint-elements ^events)
		       (ppms ())
		       (accumulate
			$^viewpoint-elements
			(loop for event in event-domain
			      collect
			      (funcall viewpoint-function
				       (cons event (ensure-list $^events))))))
   (events (^events viewpoint-elements)
	   (deterministic ())
	   (accumulate
	    $^events
	    (loop for event in event-domain 
		  if (equal (funcall viewpoint-function
				     (cons event (ensure-list $^events)))
			    (car $viewpoint-elements))
		    collect event)))
   (event (events)
	  (deterministic ())
	  (deterministic
	   (car $events))
	  :observer #'identity)))

(defparameter *viewpoint*
  (make-derived-viewpoint-model
   '(0 1 2 3)
   (lambda (numbers) (evenp (car numbers)))
   :observe '(event)))

(generate-congruent-values *viewpoint* '(0 1 2 3) '(viewpoint-elements))

;; You'll notice that this domain-general solution is somewhat inefficient as we have to loop
;; over event domain twice (inverse viewpoint functions can solve the second issue).
;; Also undefined cannot be handled properly (not that pitch-interval will generate one value
;; as long as it is undefined which is only in the first moment)
;; Furthermore, when defining the viewpoint, we have to account for the case that we're
;; in the first moment. E.g., pitch interval looks like this:

(defvar +undefined+ '⟂)

(defparameter *viewpoint*
  (make-derived-viewpoint-model
   (loop for pitch below 128 collect pitch)
   (lambda (events)
     (if (< (length events) 2) +undefined+
	 (- (first events) (second events))))
   :observe '(event)))

(reverse (first (first (last (first (estimation-dataset *viewpoint* '((0 4 2 0)) 'viewpoint-elements))))))
