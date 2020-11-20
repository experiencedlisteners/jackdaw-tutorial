#!/usr/bin/sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
				       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(let ((*standard-output* *error-output*) ; re-route standard output to error
      (idyom-root (or (sb-ext:posix-getenv "IDYOM_ROOT")
		      "idyom/"))
      (jackdaw-root (or (sb-ext:posix-getenv "JACKDAW_ROOT")
			"jackdaw/")))
  (defvar *idyom-root* idyom-root)
  (push idyom-root asdf:*central-registry*)
  (push jackdaw-root asdf:*central-registry*)
  (ql:quickload "jackdaw"))

(defvar *cli-commands* nil)

(defmacro defcommand (name args &body body)
  (pushnew name *cli-commands*)
  `(defun ,name (&rest cli-args)
     (unix-options:with-cli-options (cli-args)
	 (,@args)
       ,@body)))

(defmacro defcli (cmd (&optional flags parameters free) model-constructor)
  `(defcommand ,cmd (,@flags unix-options:&parameters dataset freeze read ,@parameters
			     unix-options:&free ,@free)
     (let ((model ,model-constructor))
       (unless (null read)
	 (with-open-file (s read) (jackdaw::deserialize model s)))
       (jackdaw::process-dataset model ;(retrieve-dataset dataset) t)
				 (sort (retrieve-dataset dataset) #'string< :key #'car) t)
       (unless (null freeze)
	 (if (string-equal freeze "stdout")
	     (jackdaw::serialize model t)
	   (with-open-file (s freeze :direction :output :if-exists :supersede
			      :if-does-not-exist :create)
			   (jackdaw::serialize model s)))))))


(defun parse-token (token)
  "Parse a token ocurring in a CSV file into lisp datatype by 
READing it. If token is empty string, return NIL."
  (unless (string-equal token "")
    (read-from-string token)))

(defun tabular->sequences (rows)
  "Find all sequence UIDs, collect all events per sequence uid,
ensure events are consecutive, create rows consisting of event
sequences. Each event is a vector, the first elements of which is
UID, and subsequent elements correspond to (cddr columns)."
  (unless (equal (subseq (car rows) 0 2)
		 (list "uid" "event"))
    (error "First two columns of CSV must be 'uid' and 'event'."))
  (let* ((columns (cddr (car rows)))
	 (rows (cdr rows))
	 (max-indices (make-hash-table :test #'equal)))
    ;; Find sequence lengths
    (dolist (row rows)
      (let ((uid (car row)) (event-index (parse-integer (cadr row))))
	(when (> event-index (gethash uid max-indices 0))
	  (setf (gethash uid max-indices) event-index))))
    ;; Create sequences
    (let ((dataset (make-hash-table :test #'equal)))
      (dolist (row rows)
	(let* ((uid (car row))
	       (event-index (parse-integer (cadr row)))
	       (events (multiple-value-bind (e found?)
			   (gethash uid dataset (make-array (1+ (gethash uid max-indices))))
			 (unless found? (setf (gethash uid dataset) e))
			 e))
	       (event))
	  ;; Create a PLIST representaiton
	  (loop for token in (cddr row) for column in columns do
	       (setf (getf event (intern (string-upcase column) :keyword))
		     (parse-token token)))
	  ;; Store event in sequence vector
	  (setf (svref events event-index) event)))
      ;; Convert sequences to lists
       (loop for uid being the hash-keys of max-indices collect
	    (let ((events (map 'list #'identity (gethash uid dataset))))
	      (when (find 0 events)
		(warn "Some events not found in composition ~a" uid))
	      (cons uid events))))))

(defun parse-or (arg &optional default)
  "Read ARG if it is not NIL, otherwise return DEFAULT."
  (if (null arg) default (read-from-string arg)))

(defun retrieve-dataset (path)
  "Parse CSV data from a file at PATH or from *STANDARD-INPUT* if
PATH is NIL. Convert to sequences after reading."
  (let ((data
	 (fare-csv:with-rfc4180-csv-syntax ()
	   (fare-csv:read-csv-stream (or path *standard-input*)))))
    (tabular->sequences data)))

(defcli temperley ((output train)
		   (outputvars note deviation t-ph0
			       duple-ph0 triple-ph0 triple-ph1
			       t0 u l tacti max-beat-dev))
  (make-instance
   'jackdaw::temperley
   :output output
   :note (parse-or note)
   :deviation (parse-or deviation)
   :t-ph0 (parse-or t-ph0)
   :triple-ph0 (parse-or triple-ph0)
   :triple-ph1 (parse-or triple-ph1)
   :duple-ph0 (parse-or duple-ph0)
   :tacti (parse-or tacti)
   :t0 (parse-or t0)
   :l (parse-or l)
   :u (parse-or u)
   :max-beat-dev (parse-or max-beat-dev)))

(defcli event-based-symbolic-temperley ((train output)
					(outputvars ioi-domain u l t0 t-ph0 duple-ph0
						    triple-ph0 triple-ph1 note))
  (make-instance
   'jackdaw::event-based-symbolic-temperley
   :output output
   :outputvars outputvars
   :ioi-domain (parse-or ioi-domain)
   :note (parse-or note)
   :t-ph0 (parse-or t-ph0)
   :triple-ph0 (parse-or triple-ph0)
   :triple-ph1 (parse-or triple-ph1)
   :duple-ph0 (parse-or duple-ph0)
   :t0 (parse-or t0)
   :l (parse-or l)
   :u (parse-or u)))

(defcli downbeat-distance ((train output update-exclusion disable-mixtures)
			   (outputvars ioi-domain meter-params order-bound
				       escape))
  (make-instance
   'jackdaw::downbeat-distance
   :escape (parse-or escape :c)
   :order-bound (parse-or order-bound)
   :mixtures (not disable-mixtures)
   :update-exclusion update-exclusion
   :outputvars (parse-or outputvars)
   :meter-params (parse-or meter-params)
   :ioi-domain (parse-or ioi-domain)
   :outputvars (parse-or outputvars)
   :output output
   :training? train))

(defcli viewpoint ((train output update-exclusion disable-mixtures)
		   (name basic-domain escape order-bound))
  (let ((name (read-from-string name)))
    (make-instance
     name
     :escape (parse-or escape :c)
     :order-bound (parse-or order-bound)
     :mixtures (not disable-mixtures)
     :update-exclusion update-exclusion
     :output output
     :outputvars (list 'v)
     :basic-domain (parse-or basic-domain)
     :training? train)))

(let* ((argv sb-ext:*posix-argv*)
       (args (cdr argv))
       (command (car args))
       (command-args (cdr args)))
  (unless (null command)
    (let ((command-symbol (intern (string-upcase command))))
      (if (member command-symbol *cli-commands*)
	  (apply command-symbol command-args)
	  (error "Unknown command: ~a." command-symbol)))))

