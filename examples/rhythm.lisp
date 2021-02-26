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


(defun rhythm (iois &optional meter phase-0)
  "Utility function for annotating a list of IOIs with initial phase and meter."
  (flet ((annotate (ioi)
	   (cond ((and (null meter) (null phase-0))
		  ioi)
		 ((null phase-0)
		  (error "Providing only a meter and no initial phase is not allowed"))
		 (t
		  (cons phase-0 (append meter (list ioi)))))))
    (mapcar #'annotate (cons jackdaw:+inactive+ iois))))

;; Example usage
;;
;; (jd:defparameter *model*
;;   (make-instance 'rhythm
;;                  :ioi-domain '(1 2 3 4)
;;                  :meter-domain '((8 4) (6 4) (6 8))
;;                  :p0-observer #'first
;;                  :m-observer (lambda (m) (list (second m) (third m)))
;;                  :i-observer (lambda (m) (if (listp m) (fourth m) m))))
;; 
;; (let ((data (list (rhythm '(4 2 2 4 1 1 1 1 4)           '(8 4) 0)
;;                   (rhythm '(3 1 1 1 2 1 3 3)             '(6 8) 0)
;;                   (rhythm '(2 1 1 2 2 1 1 2 2 2 2 1 1 4) '(6 4) 0))))
;;   (jackdaw:hide *model*) ; hide the entire model
;;   (jackdaw:observe *model* 'i 'm 'p0)  ; configure I and M to be observed in *MODEL*
;;   (jackdaw:estimate *model* data)) ; estimate the model from the data
;; 
;; (jackdaw:hide *model* 'm 'p0)
;; 
;; (jackdaw:probability *model* (list +inactive+ 1))
;; 
;; (term:table 
;;  (jackdaw:state-probability-table
;;   (jackdaw:marginalize 
;;    (jackdaw:posterior
;;     (jackdaw:generate *model* (rhythm '(4 2 2 4))))
;;   '(m))) :column-width 12)
