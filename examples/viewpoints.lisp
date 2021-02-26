(jd:defmodel pitch-interval-viewpoint (jd:dynamic-bayesian-network)
  (&key order (pitches 123))
  ((pitch-intervals (^pitch-intervals ^pitches)
		    (jd:ppms ())
		    (jd:chain (jd:markov
			       order
			       $^pitch-intervals
			       (loop for pitch below pitches collect (- pitch (car $^pitches))))
			      $^pitches))
   (pitches (^pitches pitch-intervals)
	    (jd:uniform ())
	    (jd:markov
	     nil
	     $^pitches
	     (jd:deterministic (+ (car $^pitches) (car $pitch-intervals))))
	    :observer #'identity)
   (pitch (Pitches)
	  (jd:uniform ())
	  (jd:deterministic
	   (car $pitches)))))

(jd:defmodel derived-viewpoint (jd:dynamic-bayesian-network)
  (event-domain viewpoint-function
		&key
		update-exclusion
		order-bound
		(escape :c)
		(mixtures t))
  ((viewpoint-elements (^viewpoint-elements ^events)
		       (jd:ppms ()
			     :order-bound order-bound
			     :update-exclusion update-exclusion
			     :mixtures mixtures
			     :escape escape)
		       (jd:markov
			order-bound $^viewpoint-elements
			(loop for event in event-domain
			      collect
			      (funcall viewpoint-function
				       (cons event (jd:ensure-list $^events))))))
   (events (^events viewpoint-elements)
	   (jd:uniform ())
	   (jd:markov
	    nil $^events
	    (loop for event in event-domain 
		  if (equal (funcall viewpoint-function
				     (cons event (jd:ensure-list $^events)))
			    (car $viewpoint-elements))
		    collect event)))
   (event (events)
	  (jd:uniform ())
	  (jd:deterministic
	   (car $events))
	  :observer #'identity)))


;; Examples
;;
;; (defparameter *viewpoint*
;;  (make-derived-viewpoint-model
;;   '(0 1 2 3)
;;   (lambda (numbers) (evenp (car numbers)))
;;   :observe '(event)))
;;
;; (jd:generate-congruent-values *viewpoint* '(0 1 2 3) '(viewpoint-elements))
;;
;; You'll notice that this domain-general solution is somewhat inefficient as we have to loop
;; over event domain twice (inverse viewpoint functions can solve the second issue).
;; Also undefined cannot be handled properly (not that pitch-interval will generate one value
;; as long as it is undefined which is only in the first moment)
;; Furthermore, when defining the viewpoint, we have to account for the case that we're
;; in the first moment. E.g., pitch interval looks like this:
;; 
;; (defvar +undefined+ '⟂)
;; 
;; (defparameter *viewpoint*
;;   (make-derived-viewpoint-model
;;    (loop for pitch below 128 collect pitch)
;;    (lambda (events)
;;      (if (< (length events) 2) +undefined+
;; 	 (- (first events) (second events))))
;;    :observe '(event)))
;;
;; To inspect the viewpoint elements after the last moment in the right order
;; (reverse
;;  (first
;;   (first
;;    (last
;;     (first
;;      (jd::estimation-dataset *viewpoint* '((0 4 2 0))
;; 	       		        '(viewpoint-elements)))))))
