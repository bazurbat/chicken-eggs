;;
;; getopt-style command-line parser
;;
;; Author: Russ McManus (rewritten by Thien-Thi Nguyen)
;;
;; Ported to Chicken Scheme and extensively modified by Ivan Raikov.
;;
;; Copyright 2009-2013 Ivan Raikov.
;;
;; Portions copyright (C) 1998, 2001, 2006 Free Software Foundation,
;; Inc.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the Lesser GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

;;; Commentary:

;;; This module implements command line option parsing, in the spirit
;;; of the GNU C library function `getopt_long'.  Both long and short
;;; options are supported.
;;;
;;; The theory is that people should be able to constrain the set of
;;; options they want to process using a grammar, rather than some
;;; arbitrary structure.  The grammar makes the option descriptions
;;; easy to read.
;;;
;;; `getopt-long' is a procedure for parsing command-line arguments in
;;; a manner consistent with GNU programs.
;;;
;;; `usage' is a procedure that prints help strings about the
;;; command-line arguments defined in a grammar.

;;; (getopt-long ARGS GRAMMAR) Parse the arguments ARGS according to
;;; the argument list grammar GRAMMAR.
;;;
;;; ARGS should be a list of strings.  Its first element should be the
;;; name of the program; subsequent elements should be the arguments
;;; that were passed to the program on the command line.  The
;;; `program-arguments' procedure returns a list of this form.
;;;
;;; GRAMMAR is a list of the form:
;;; ((OPTION-NAME [DOCSTRING] 
;;;               (PROPERTY VALUE) ...) ...)
;;;
;;; Each OPTION-NAME should be a symbol.  `getopt-long' will accept a
;;; command-line option named `--OPTION-NAME'.
;;;
;;  If DOCSTRING is provided, it must be a either string a string
;;  containing a brief description of the option.
;;;
;;; Each option can have the following (PROPERTY VALUE) pairs:
;;;
;;;   (single-char CHAR) --- Accept `-CHAR' as a single-character
;;;		equivalent to `--OPTION'.  This is how to specify traditional
;;;		Unix-style flags.
;;;
;;;   (required BOOL) --- If BOOL is true, the option is required.
;;;		getopt-long will raise an error if it is not found in ARGS.
;;;
;;;   (multiple BOOL) --- If BOOL is true, this option can be specified 
;;              multiple times. The default is false.
;;;
;;;   (value FLAG [(PROPERTY VALUE) ...])
;;;             --- If FLAG is #t, the option requires a value; if
;;;		it is #f, it does not; 
;;;             if it is of the form (REQUIRED name) then the option requires 
;;;             and the name is used by the usage procedure
;;;             if it is of the form (OPTIONAL name) the option may
;;;             appear with or without a (named) value.
;;;             
;;;             In addition, the following properties can be defined
;;;             for a value:
;;;
;;;            (predicate FUNC) --- 
;;;
;;;                  If the option accepts a value, then getopt will
;;;                  apply FUNC to the value, and throw an exception
;;;                  if it returns #f.  FUNC should be a procedure
;;;                  which accepts a string and returns a boolean
;;;                  value; you may need to use quasiquotes to get it
;;;                  into GRAMMAR.
;;;
;;;            (transformer FUNC) --- 
;;;
;;;                  If the option accepts a value, then getopt will
;;;                  apply FUNC to the string provided on the command
;;;                  line, and put the resulting value in the list of
;;;                  parsed options returned by getopt-long.
;;;
;;; The (PROPERTY VALUE) pairs may occur in any order, but each
;;; property may occur only once.  By default, options do not have
;;; single-character equivalents, are not required, and do not take
;;; values.
;;;
;;; In ARGS, single-character options may be combined, in the usual
;;; Unix fashion: ("-x" "-y") is equivalent to ("-xy").  If an option
;;; accepts values, then it must be the last option in the
;;; combination; the value is the next argument.  So, for example, using
;;; the following grammar:
;;;
;;;      ((apples    (single-char #\a))
;;;       (blimps    (single-char #\b) (value #t))
;;;       (catalexis (single-char #\c) (value #t)))
;;;
;;; the following argument lists would be acceptable:
;;;
;;;    ("-a" "-b" "bang" "-c" "couth")     ("bang" and "couth" are the values
;;;                                         for "blimps" and "catalexis")
;;;    ("-ab" "bang" "-c" "couth")         (same)
;;;    ("-ac" "couth" "-b" "bang")         (same)
;;;    ("-abc" "couth" "bang")             (an error, since `-b' is not the
;;;                                         last option in its combination)
;;;
;;; If an option's value is optional, then `getopt-long' decides
;;; whether it has a value by looking at what follows it in ARGS.  If
;;; the next element is does not appear to be an option itself, then
;;; that element is the option's value.
;;;
;;; The value of a long option can only follow the option name,
;;; separated by an `=' character.
;;;
;;; If the option "--" appears in ARGS, argument parsing stops there;
;;; subsequent arguments are returned as ordinary arguments, even if
;;; they resemble options.  So, in the argument list:
;;;
;;;         ("--apples" "Granny Smith" "--" "--blimp" "Goodyear")
;;;
;;; `getopt-long' will recognize the `apples' option as having the
;;; value "Granny Smith", but it will not recognize the `blimp'
;;; option; it will return the strings "--blimp" and "Goodyear" as
;;; ordinary argument strings.
;;;
;;; The `getopt-long' function returns the parsed argument list as an
;;; assocation list, mapping option names --- the symbols from GRAMMAR
;;; --- onto their values, or #t if the option does not accept a value.
;;; Unused options do not appear in the alist.
;;;
;;; All arguments that are not the value of any option are returned
;;; as a list, associated with the empty list.
;;;
;;; `getopt-long' throws an exception if:
;;; - it finds an unrecognized property in GRAMMAR
;;; - the value of the `single-char' property is not a character, 
;;;   or a single-character string/symbol
;;; - it finds an unrecognized option in ARGS
;;; - a required option is omitted
;;; - an option that requires an argument doesn't get one
;;; - an option that doesn't accept an argument does get one (this can
;;;   only happen using the long option `--opt=value' syntax)
;;; - an option predicate fails
;;;
;;; For an example, see file tests/run.scm.



(module getopt-long

	(getopt-long width separator indent usage make-option-dispatch
                     default-long-option-value-cset 
                     long-option-value-quoting)

	(import scheme chicken)
	
	(require-extension data-structures srfi-1 srfi-13 srfi-14 matchable )


(define (fetch-value kv) 
  (match kv ((k v) v) (else (cdr kv))))


(define (lookup-def k lst . rest)
  (let-optionals rest ((default #f))
      (let ((kv (assoc k lst)))
	(if (not kv) default
	    (fetch-value kv)))))


(define-record-type  unknown-option
  (make-unknown-option name )
  unknown-option?
  (name        unknown-option-name)
  )
  

(define-record-type  value-policy
  (make-value-policy name predicate transformer optional? default )
  value-policy?
  (name        value-policy-name)
  (predicate   value-policy-predicate)
  (transformer value-policy-transformer)
  (optional?   value-policy-optional?)
  (default     value-policy-default))
  

(define-record-type option-spec
  (make-option-spec name value required? single-char docstring multiple? )
  option-spec?
  (name         option-spec-name)
  (value        option-spec-value)
  (required?    option-spec-required?)
  (single-char  option-spec-single-char)
  (docstring    option-spec-docstring)
  (multiple?    option-spec-multiple?))


;; Valid characters for option names and values

(define long-option-name-cset
   (char-set-union char-set:letter
                   (char-set #\-)))


(define short-option-name-cset
  char-set:letter)

(define default-long-option-value-cset
  (char-set-union char-set:letter+digit 
                  char-set:punctuation
                  (char-set #\_ #\^ #\$ #\= #\space)))

(define long-option-value-quoting (make-parameter #f))


;;
;; We don't auto-format the left column (the option keys) based on the
;; length of the longest option, but you can override it manually.
;;
(define width (make-parameter 25))

;; The separator used between options.  Default: ", "

(define separator (make-parameter ", "))
(define indent (make-parameter 1))

(define (spaces n)
  (let loop ((ls '()) (n n))
    (if (<= n 0)
        (list->string ls)
        (loop (cons #\space ls)
              (- n 1)))))

;; Join together option names in spec with commas, and append the
;; argument type and name

(define-record-printer (option-spec x out)
  (let* ((name          (option-spec-name x))
	 (value-policy  (option-spec-value x))
	 (required?     (option-spec-required? x))
	 (single-char   (option-spec-single-char x))
	 (docstring     (option-spec-docstring x))
	 (multiple?     (option-spec-multiple? x))
	 (long-option   (and (not (make-single-char name))
			     (string-append "--" (->string name))))
	 (short-option  (or (and single-char 
				 (list->string (list #\- single-char)))
			    (make-single-char name)))
	 (option-lst    (cond ((and short-option long-option)
			       (list long-option (separator) short-option))
			      (long-option
			       (list long-option))
			      (else (list short-option))))
	 (option-lst
	  (cond
	   (value-policy 
	    (if (value-policy-optional? value-policy)  
		(cons* "]" (->string (value-policy-name value-policy) )
		       "[" "=" option-lst)
		(cons* (->string (value-policy-name value-policy))
		       "=" 
		       option-lst)))
	   (else        option-lst)))

	 (option-string (string-concatenate (reverse option-lst))))

    (display
     (string-append (spaces (indent))
		    (string-pad-right option-string (width))
		    docstring
		    "\n")
     out)))

;; Generate a formatted list of options from OPTION-LIST, and return a
;; string suitable for embedding into help text.  The single string
;; consists of multiple lines, with a newline at the end of each line.
;; Thus, a typical use would be (print (usage opts)).
(define (usage opts) 
  (let ((specs (map parse-option-spec opts)))
    (apply string-append (map ->string specs))))


(define update-option-spec
  (lambda (x . key/values)
    (apply
     (lambda (#!key
	      (name           (option-spec-name x)) 
	      (required?      (option-spec-required? x))
	      (single-char    (option-spec-single-char x))
	      (value          (option-spec-value x))
	      (docstring      (option-spec-docstring x))
	      (multiple?      (option-spec-multiple? x))
	      )
          (make-option-spec 
	   name
	   value
	   required?
	   single-char
	   docstring
	   multiple?
	   ))
     key/values)))


(define (make-predicate pred)
  (lambda (name val)
    (or (not val)
	(pred val)
	(error "option predicate failed" name))))


(define (make-single-char x)
  (let ((lst (string->list (->string x))))
    (and (null? (cdr lst))
	 (car lst))))
    

(define (parse-option-spec desc)
  
  (let* ((name         (car desc))
	 (single-char  (make-single-char name))
	 (spec
	  (make-option-spec 
	   name
	   #f
	   #f
	   single-char
	   ""
	   #f
	   )))

    (fold
     (lambda (desc-elem spec)
       (cond ((string? desc-elem)
	      (update-option-spec spec docstring: desc-elem))

	     (else 
	      (let ((given (lambda () (cdr desc-elem))))

		(case (car desc-elem)

		  ((multiple)
		   (update-option-spec spec multiple?: (car (given))))
		  
		  ((required)
		   (update-option-spec spec required?: (car (given))))
		  
		  ((value)
		   (let ((value-policy
			  (match (given)

				 ((((and flag (or 'required 'optional))
				    (and name (or (? symbol?) (? string?)))) . rst)
				  (let ((predicate 
					 (cond ((lookup-def 'predicate rst) =>
						make-predicate)
					       (else
						(lambda x (identity x)))))

					(transformer 
					 (or (lookup-def 'transformer rst)
					     identity))

					(default (lookup-def 'default rst))
					)

				    (make-value-policy 
				     name
				     predicate
				     transformer
				     (equal? flag 'optional)
				     (and default  (->string default))
				     )))

				 ((#t) (make-value-policy 
				      'ARG
				      (lambda x (identity x))
				      identity
				      #f
				      #f))

				 ((#f) #f)

				 (else (error "invalid value specification "
					      (given)))

				 )))
		     (update-option-spec spec value: value-policy)))
		  
		  ((single-char)
		   (cond
		    ((make-single-char (car (given))) =>
		     (lambda (c)
		       (update-option-spec spec single-char: c)))
		    (else 
		     (error "`single-char' value must be a single character, string, or symbol"))))

		  (else
		   (error "invalid getopt-long option property"
			  (car desc-elem))))))))
     spec (cdr desc))
    ))



(define (split-argument-list argument-list)
  ;; Scan ARGUMENT-LIST for "--" and return (BEFORE-LS . AFTER-LS).
  ;; Discard the "--".  If no "--" is found, AFTER-LS is empty.
  (let loop ((yes '()) (no argument-list))
    (cond ((null? no)               (cons (reverse yes) no))
	  ((string=? "--" (car no)) (cons (reverse yes) (cdr no)))
	  (else (loop (cons (car no) yes) (cdr no))))))


(define (check-long-option str)
  (and (> (string-length str) 1)
       (string=? (substring str 0 2) "--")))

(define (check-short-option str)
  (and (positive? (string-length str))
       (string=? (substring str 0 1) "-")))

(define (long-option-name lst)
  (let loop ((lst lst)  (ax (list)))
    (cond ((null? lst)  (list (list->string (reverse ax)) lst))

	  ((and (char? (car lst))
		(char-set-contains? long-option-name-cset
                                    (car lst))
		(car lst))
	   => (lambda (c) (loop (cdr lst) (cons c ax))))

	  ((char=? (car lst) #\=)
	   (list (list->string (reverse ax)) (cdr lst)))

	  (else (error 'long-option-name 
		       "invalid list" lst)))))


(define (long-option-value lst value-cset)
  (if (null? lst) (list #f lst)
      (let loop ((lst lst)  (ax (list)))
	(cond ((null? lst)  
	       (list (list->string (reverse ax)) lst))
	      
	      ((and (char? (car lst)) (car lst)) =>

	       (lambda (c)

		 (cond ((and (long-option-value-quoting) (char=? c #\"))
			(let quote-loop ((lst (cdr lst)) (ax ax))
			  (if (null? lst) (error 'long-option-value
						 "unclosed option value quotation")
			      (cond ((char=? (car lst) #\")
                                     (loop (cdr lst) ax))
                                    ((char=? (car lst) #\\)
                                     (cond ((char=? (cadr lst) #\\)
                                            (quote-loop (cddr lst) (cons #\\ ax)))
                                           ((char=? (cadr lst) #\")
                                            (quote-loop (cddr lst) (cons #\" ax)))
                                           (else 
                                            (quote-loop (cddr lst) (cons (cadr lst) (cons (car lst) ax))))
                                           ))
                                    (else
                                     (quote-loop (cdr lst) (cons (car lst) ax))))
                              ))
                        )

		       ((char-set-contains? value-cset c)
			(loop (cdr lst) (cons c ax)))

		       (else  (error 'long-option-value 
				     "invalid option character" c)))))
	      
	      (else (error 'long-option-value 
			   "invalid list" lst))))))


(define (long-option? specs a next value-cset)

  (let ((l (string->list a)))
    (match l
	   ((#\- #\-  . rst)
	    (match-let* (((n nrst)  (long-option-name rst))

			 ((v _)     (let ((lv (long-option-value nrst value-cset)))
				      lv))
			 ((next v)
			  (begin
			    (or (and v (list next v))
				(list next #f)))))

			(cond ((alist-ref (string->symbol n) (car specs)) =>
			       (lambda (spec)
				 (cond
				  
				  ((and v (option-spec-value spec)) =>
				   (lambda (value-policy)
				     (or 
				      (and ((or (value-policy-predicate value-policy)
						(lambda x (identity x))) n v)
					   (let ((transformer
						  (or (value-policy-transformer value-policy)
						      identity)))
					     (list next (cons (option-spec-name spec) (transformer v)))))
				      (error 'long-option? 
					     "predicate error on option value" n))))
				  
				  ((and v (not (option-spec-value spec)))
				   (error 'long-option? 
					  "superfluous argument given to option" n))
				  
				  ((and (not v) (option-spec-value spec)
					(value-policy-optional? 
					 (option-spec-value spec)))

				   (let* ((vp (option-spec-value spec))
					  (dflt  (value-policy-default vp))
					  (transformer (or (value-policy-transformer vp)
							   identity))
					  (v (and dflt (transformer dflt))))
				       (list next (cons (option-spec-name spec) (or v #t)))))
				  
				  ((and (not v) (option-spec-value spec))
				   (error 'long-option? "option requires value" n))
				  
				  (else
				   (list next (cons (option-spec-name spec) #t)))
				  
				  )))
			      (else 
			       (list next (make-unknown-option n))))))
	   (else #f))))


(define (short-option-names lst)
  (if (null? lst) (list #f lst)
      (let loop ((lst lst)  (ax (list)))
	(cond ((null? lst)  (list ax lst))

	      ((and (char? (car lst))
		    (char-set-contains? short-option-name-cset (car lst)) 
		    (car lst)) =>
		    (lambda (c) (loop (cdr lst) (cons c ax))))

	      (else (list ax lst))))))

(define (short-options? specs a next)

  (let ((l (string->list a)))
    (match l
	   ((#\-  . rst)
	    (match-let ((((n1 . ns) _)  (short-option-names rst)))
              (match-let
	       ;; special case: check if the last single-letter option
	       ;; has an argument
	       (((next opt1)
		 (cond
		  ((alist-ref n1 (cadr specs) ) =>
		   (lambda (spec)
		     (let ((name (option-spec-name spec)))

		       (cond
			((option-spec-value spec) =>
			 (lambda (value-policy)
			   (let ((v (and (pair? next)
					 (not (check-long-option (car next) ))
					 (not (check-short-option (car next) ))
					 (car next))))
					
			     (if (and (not v) (not (value-policy-optional? value-policy)))
				 (error 'short-options?  "option requires value" name))
			     
			     (if (not v) 
				 (list next (cons name (or (value-policy-default value-policy) #t)))
				 (or (and ((or (value-policy-predicate value-policy) 
					       (lambda x (identity x))) name v)
					  (let ((transformer
						 (or (value-policy-transformer value-policy) 
						     identity)))
					    (list (cdr next) (cons name (transformer v)))))
				  
				  (error 'short-options? 
					 "predicate error on option value" 
					 name))))))
				  
			(else
			 (list next (cons name #t)))))))
		  (else
		   (list next (make-unknown-option (->string n1)))))))
	       (list next 
		     (cons opt1 
			   (map (lambda (n)
				  (cond
				   ((alist-ref n (cadr specs) ) =>
				    (lambda (spec)
				      (cond
				       ((option-spec-value spec)
					(error 'short-options?
					       "option requires value" n))
				       
				       (else 
					(cons (option-spec-name spec) #t)))))
				   
				   (else 
				    (make-unknown-option (->string n)))))
				ns)))))
	      )
	   (else #f))))
	    
  
  

(define (process-options specs argument-ls value-cset)

  ;; Use SPECS to scan ARGUMENT-LS; return (FOUND . ETC).
  ;; FOUND is an unordered list of option specs for found options, while ETC
  ;; is an order-maintained list of elements in ARGUMENT-LS that are neither
  ;; options nor their values.
  ;;
  ;; Argument VALUE-CSET specifies the set of characters allowed in
  ;; option values.

  (let loop ((ls argument-ls)  (found (list)) (etc (list)) (unknown (list)))

    (if (null? ls) 

	(list found (reverse etc) (reverse unknown))

	(let ((arg (car ls)) (rest (cdr ls)))

	  (cond ((long-option? specs arg rest value-cset) =>
		 (lambda (next.val)
		   (let ((optval (cadr next.val)))
		     (if (unknown-option? optval)
			 (loop (car next.val) found etc (cons optval unknown))
			 (loop (car next.val) (cons optval found) etc unknown)))))
		
		((short-options? specs arg rest) =>
		 (lambda (next.vals)
		   (let-values (((unknowns optvals) (partition unknown-option? (cadr next.vals))))
		     (loop (car next.vals) (append optvals found) etc (append unknowns unknown)))))

		(else 
		 (loop (cdr ls) found (cons (car ls) etc) unknown)))))))
				 


(define (getopt-long program-arguments option-desc-list 
		     #!key 
                     (unknown-option-handler (lambda (x) (error 'getopt-long "unknown options" x)))
                     (long-option-value-cset default-long-option-value-cset)
                     )

;;
;; Process options, handling both long and short options, similar to
;; the glibc function 'getopt_long'.  PROGRAM-ARGUMENTS should be a
;; value similar to what (program-arguments) returns.
;;
;; OPTION-DESC-LIST is a list of option descriptions.  Each option
;; description must satisfy the following grammar:
;;
;;     <option-spec>           :: (<name> . <attribute-ls>)
;;     <attribute-ls>          :: (<attribute> . <attribute-ls>)
;;                                | ()
;;     <attribute>             :: <required-attribute>
;;                                | <arg-required-attribute>
;;                                | <single-char-attribute>
;;                                | <value-attribute>
;;     <required-attribute>    :: (required? <boolean>)
;;     <single-char-attribute> :: (single-char <char>)
;;     <value-attribute>       :: (value #t)
;;                                (value #f)
;;                                (value (required <name>))
;;                                (value (optional <name>))
;;                                (<value-attribute> 
;;                                 <predicate-attribute>)
;;     <predicate-attribute>   :: (predicate <1-ary-function>)
;;
;;     The procedure returns an alist of option names and values.
;; Each option name is a symbol.  The option value will be '#t' if no
;; value was specified.  There is a special item in the returned alist
;; with a key @: the list of arguments that are not options or option
;; values.
;;
;;     By default, options are not required, and option values are not
;; required.  By default, single character equivalents are not
;; supported; if you want to allow the user to use single character
;; options, you need to add a `single-char' clause to the option
;; description.

  (let* ((specifications (map parse-option-spec option-desc-list))
	 (spec-long      (map (lambda (spec)
				(cons (option-spec-name spec) spec))
			      specifications))
	 (spec-short     (filter-map
			  (lambda (spec)
			    (and (option-spec-single-char spec)
				 (cons (option-spec-single-char spec) 
				       spec)))
			  specifications))

	 (pair            (split-argument-list program-arguments))
	 (split-ls        (car pair))
	 (non-split-ls    (cdr pair)))

    (match-let (((found etc unknown)
		 (process-options (list spec-long spec-short) split-ls
                                  long-option-value-cset)))


       (let ((rest-ls (append etc non-split-ls)))
    
	 (for-each (lambda (spec)
		     (let ((name (option-spec-name spec)))
		       
		       (and (option-spec-required? spec)
			    (or (assoc name found )
				(error "option must be specified" name)))
		       
		       (and (assoc name found)
			    
			    (and (option-spec-value spec)
				 (not (value-policy-optional?
				       (option-spec-value spec))))
			    
			    (or (cdr (assoc name found))
				(error "option must be specified with argument"
				       name)))))
		   specifications)



       (values 
	(cons (cons '@ rest-ls) found)
	(or (and (not (null? unknown)) 
		 (unknown-option-handler (map unknown-option-name unknown)))
	    '()))
       ))
    ))

(define (make-option-dispatch opts options-desc-list)
  (let* ((specifications (map parse-option-spec options-desc-list))
	 (defaults
	   (filter-map
	    (lambda (spec) 
	      (let* ((name (option-spec-name spec))
		     (value-policy (option-spec-value spec))
		     (default (and value-policy 
				   (value-policy-default value-policy)))
		     )
		(cond ((and value-policy 
			    (value-policy-predicate value-policy)) =>
			    (lambda (pred)
			      (or (pred name default) 
				  (error 'make-option-dispatch
					 "predicate error in default value"
					 default)))))
		(let ((transformer
		       (or (and value-policy 
				(value-policy-transformer value-policy))
			   identity)))
		  (and default (list name (transformer default))))
		))
	    specifications)))

    (lambda (name)
      (case name 
	((@)  (alist-ref '@ opts))
	(else
	 (let* ((spec (find (lambda (x) (eq? (option-spec-name x) name)) specifications))
		(v (filter-map (lambda (x) (and (eq? (car x) name) (cdr x))) (cdr opts))))
	   (if (option-spec-multiple? spec) v (and (pair? v) (car v)))
	   ))
	 ))
    ))
	   
	 

)
;;; getopt-long.scm ends here
