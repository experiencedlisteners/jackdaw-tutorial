(ql:quickload "jackdaw")
(ql:quickload "cl-ansi-term")

(jackdaw::defdistribution meter
    (jackdaw::cpt) (correction-factor) (meter)
  (pr:mul correction-factor (car meter)
	  (call-next-method)))

(jackdaw::defestimator
    meter (data distribution) (meter) ()
    ((correction-factor 
      (progn 
	(call-next-method distribution data)
	(apply #'pr:add 
	       (loop
		 for symbol in (jackdaw:domain distribution) 
		 collect (jackdaw:probability
			  distribution
			  (cons symbol nil))))))))

(jackdaw:defmodel rhythm (jackdaw:dynamic-bayesian-network)
  (ioi-domain meter-domain)
  ((M                    ; meter
      (^m)
      (jackdaw:cpt ())   ; conditional probability table
      (jackdaw:persist $^m meter-domain))
   (D                    ; downbeat distance
      (^d ^p m)
      (jackdaw:ppms (m)) ; set of PPM sequence models
      (jackdaw:chain (loop for ioi in ioi-domain
			   collect (cons (+ $^p ioi)
					 (jackdaw:ensure-list $^d)))
		     $^p))
   (P0                   ; initial phase (or pickup interval)
       (^p0 m)
       (jackdaw:uniform ())
       (jackdaw:persist $^p0 (loop for p below (car $m) collect p)))
   (P                    ; phase
      (^p p0 m d)
      (jackdaw:uniform ())
      (jackdaw:recursive $^p (list (mod (car $d) (car $m)))
			 (list $p0)))
   (I                    ; inter-onset interval
      (d ^p ^i)
      (jackdaw:uniform ())
      (if (jackdaw:inactive? $d) (list jackdaw:+inactive+)
	  (list (- (car $d) $^p))))))


(defun annotate (iois meter phase-0)
  "Utility function for annotating a list of IOIs with initial phase and meter."
  (loop for ioi in iois
	collect (cons phase-0 (append meter (list ioi)))))

;; Example usage
;;
;; (defparameter *model*
;;   (make-instance 'rhythm
;; 		 :ioi-domain '(1 2 3 4)
;; 		 :meter-domain '((8 4) (6 4) (6 8))
;; 		 :p0-observer #'first
;; 		 :m-observer (lambda (m) (list (second m) (third m)))
;; 		 :i-observer (lambda (m) (if (listp m) (fourth m) m))))
;;
;; (let ((data (list (annotate (list +inactive+ 4 2 2 4 1 1 1 1 4) '(8 4) 0)
;; 		     (annotate (list +inactive+ 3 1 1 1 2 1 3 3) '(6 8) 0)
;; 		     (annotate (list +inactive+ 2 1 1 2 2 1 1 2 2 2 2 1 1 4) '(6 4) 0))))
;;   (hide *model*) ; hide the entire model
;;   (observe *model* 'i 'm 'p0)  ; configure I and M to be observed in *MODEL*
;;   (estimate *model* data)) ; estimate the model from the data
;; 
;; (hide *model* 'm 'p0)
;;
;; (probability *model* (list +inactive+ 1))
;;
;; (term:table 
;;  (state-probability-table
;;   (marginalize 
;;    (posterior-distribution
;;     *model*
;;     (generate *model* (list +inactive+ 4 2 2 4)))
;;    '(m))
;;   'm) :column-width 12)
