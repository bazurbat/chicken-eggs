;;;; shell.scm


(module shell (execute run run* shell shell shell-verbose command capture)

(import scheme chicken)
(use extras utils data-structures posix)


(define shell-verbose (make-parameter #f))

(define (command . cmds)
  (apply
   values
   (map (lambda (cmd)
	  (string-intersperse (map ->string (flatten cmd))))
	cmds)))

(define (execute cmds #!key status verbose capture)
  (let ((ss 
	 (map
	  (lambda (cmd)
	    (let ((cmd (command cmd)))
	      (when (or verbose (shell-verbose))
		(printf "  ~A~%~!" cmd))
	      (let ((r (if capture
			   (with-input-from-pipe cmd read-all)
			   (system cmd))))
		(cond ((or capture status) r)
		      ((not (zero? r)) 
		       (error 
			'execute
			"shell command failed with non-zero exit status"
			cmd r))))))
	  cmds)))
    (if (or capture status) (apply values ss) (void))))

(define-syntax run1
  (syntax-rules ()
    ((_ args cmds ...)
     (apply execute `(cmds ...) args))))

(define-syntax run
  (syntax-rules ()
    ((_ cmd ...) (run1 '() cmd ...))))

(define-syntax run*
  (syntax-rules ()
    ((_ cmd ...) (run1 '(status: #t) cmd ...))))

(define-syntax shell
  (syntax-rules ()
    ((_ cmd ...) 
     (lambda args 
       (run1 args cmd ...)))))

(define-syntax capture
  (syntax-rules ()
    ((_ cmd ...) (run1 '(capture: #t) cmd ...))))


)
