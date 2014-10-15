;;;; make.scm - PLT's `make' macro for CHICKEN - felix

#|
> (make ((target (depend ...) command ...) ...) argv)

expands to

  (make/proc
    (list (list target (list depend ...) (lambda () command ...)) ...)
    argv)

> (make/proc spec argv) performs a make according to `spec' and using
`argv' as command-line arguments selecting one or more targets.
`argv' can either be a string or a vector of strings.

`spec' is a MAKE-SPEC:

  MAKE-SPEC = (list-of MAKE-LINE)
  MAKE-LINE = (list TARGET (list-of DEPEND-STRING) COMMAND-THUNK)
  TARGET = (union string (list-of string)) ; either a string or a list of strings
  DEPEND-STRING = string
  COMMAND-THUNK = (-> void)

To make a target, make/proc is first called on each of the target's
dependencies. If a target is not in the spec and it exists, then the
target is considered made. If a target is older than any of its
dependencies, the corresponding COMMAND-THUNK is invoked. The
COMMAND-THUNK is optional; a MAKE-LINE without a COMMAND-THUNK is
useful as a target for making a number of other targets (the
dependencies).

Parameters:

> (make-print-checking [on?]) - If #f, make only prints when it is
making a target. Otherwise, it prints when it is checking the
dependancies of a target. Defaultly #t.

> (make-print-dep-no-line [on?]) - If #f, make only prints "checking..."
lines for dependancies that have a corresponding make line.  Defaultly
#f.

> (make-print-reasons [on?]) If #t, make prints the reason for each
dependency that fires. Defaultly #t.
|#

(require-library srfi-1 posix)

(module make (make make/proc make-print-checking
		   make-print-dep-no-line
		   make-print-reasons
		   make-nonfile-targets)
 
(import scheme chicken extras posix srfi-1)

(define make-print-checking (make-parameter #f))
(define make-print-dep-no-line (make-parameter #f))
(define make-print-reasons (make-parameter #f))
(define make-nonfile-targets (make-parameter '()))

(define (make:find-matching-line str spec)
  (let ((match? (lambda (s) (string=? s str))))
    (let loop ((lines spec))
      (cond
       ((null? lines) #f)
       (else (let* ((line (car lines))
		    (names (if (string? (car line))
			       (list (car line))
			       (car line))))
	       (if (any match? names)
		   line
		   (loop (cdr lines)))))))))

(define (make:form-error s p)
  (error (sprintf "~a: ~s" s p)))
(define (make:line-error s p n)
  (error (sprintf "~a: ~s for line: ~a" s p n)))

(define (make:check-spec spec)
  (and
   (or (list? spec) (make:form-error "specification is not a list" spec))
   (or (pair? spec) (make:form-error "specification is an empty list" spec))
   (every
    (lambda (line)
      (and
       (or (and (list? line) (<= 2 (length line) 3))
           (make:form-error "list is not a list with 2 or 3 parts" line))
       (or (or (string? (car line))
               (and (list? (car line))
                    (every string? (car line))))
           (make:form-error "line does not start with a string or list of strings" line))
       (let ((name (car line)))
         (or (list? (cadr line))
             (make:line-error "second part of line is not a list" (cadr line) name)
             (every (lambda (dep)
                      (or (string? dep)
                          (make:form-error "dependency item is not a string" dep)))
                    (cadr line)))
         (or (null? (cddr line))
             (procedure? (caddr line))
             (make:line-error "command part of line is not a thunk" (caddr line) name)))))
    spec)))

(define (make:check-argv argv)
  (or (string? argv)
      (every 
       string?
       (if (vector? argv) (vector->list argv) argv))
      (error "argument is not a string or string vector" argv)))

(define (make:make/proc/helper spec argv)
  (make:check-spec spec)
  (make:check-argv argv)
  (letrec ((made '())
	   (exn? (condition-predicate 'exn))
	   (exn-message (condition-property-accessor 'exn 'message))
	   (make-file
	    (lambda (s indent)
	      (let ((line (make:find-matching-line s spec))
		    (date (and (file-exists? s)
			       (file-modification-time s))))

		(when (and (make-print-checking)
			   (or line
			       (make-print-dep-no-line)))
		  (printf "make: ~achecking ~a~%" indent s))

		(if line
		    (let ((deps (cadr line)))
		      (for-each (let ((new-indent (string-append " " indent)))
				  (lambda (d) (make-file d new-indent)))
				deps)
		      (let ((reason
			     (or (not date)
				 (find (lambda (dep)
                                         (and (not (member dep (make-nonfile-targets)))
                                              (unless (file-exists? dep)
                                                (error (sprintf "dependency ~a was not made~%" dep)))
                                              (and (> (file-modification-time dep) date)
                                                   dep)))
                                       deps))))
			(when reason
			  (let ((l (cddr line)))
			    (unless (null? l)
			      (set! made (cons s made))
			      (printf
                               "make: ~amaking ~a~a~%"
                               (if (make-print-checking) indent "")
                               s
                               (if (make-print-reasons)
                                   (cond
                                    ((and (not date) (not (member s (make-nonfile-targets))))
                                     (string-append " because " s " does not exist"))
                                    ((string? reason)
                                     (string-append " because " reason " changed"))
                                    (else
                                     (string-append (sprintf " just because (reason: ~a date: ~a)" reason date))))
                                   ""))
			      (handle-exceptions
                               exn
                               (begin
                                 (printf "make: Failed to make ~a: ~a~%"
                                         (car line)
                                         (if (exn? exn)
                                             (exn-message exn)
                                             exn))
                                 (signal exn) )
                               ((car l))))))))
		    (unless date
		      (error (sprintf "don't know how to make ~a" s))))))))
    (cond
     ((string? argv) (make-file argv ""))
     ((or (null? argv) (equal? argv '#())) (make-file (caar spec) ""))
     (else (for-each (lambda (f) (make-file f ""))
		     (if (vector? argv) (vector->list argv) argv))))
    (for-each (lambda (item)
		(printf "make: made ~a~%" item))
	      (reverse made))))

(define make/proc
  (case-lambda
   ((spec) (make:make/proc/helper spec '()))
   ((spec argv) (make:make/proc/helper spec argv))))

(define-syntax make
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((make
            (lambda (spec argv)
              (let ((form-error (lambda (s . p) (apply error s spec p))))
                (and (or (list? spec)
                         (form-error "illegal specification (not a sequence)"))
                     (or (pair? spec)
                         (form-error "empty specification"))
                     (every
                      (lambda (line)
                        (and
                         (or (and (list? line) (>= (length line) 2))
                             (form-error
                              "clause does not have at least 2 parts"
                              line))
                         (let ((name (car line)))
                           (or (list? (cadr line))
                               (form-error
                                "second part of clause is not a sequence"
                                (cadr line))))))
                      spec))
                `(,(rename 'make/proc)
                  (list ,@(map (lambda (line)
                                 `(,(rename 'list) ,(car line)
                                   (,(rename 'list) ,@(cadr line))
                                   ,@(let ((l (cddr line)))
                                       (if (null? l)
                                           '()
                                           `((,(rename 'lambda) ()
                                              ,@l))))))
                               spec))
                  ,(if (vector? argv) `',argv (car argv)))))))
       (cond
        ((null? (cdr expr))
         (error "no arguments to make"))
        ((pair? (cddr expr))
         (make (cadr expr) (cddr expr)))
        (else
         (make (cadr expr) '#())))))))

)
