;;;; typed-records.scm


(module typed-records (define-record
			  define-record-type
			defstruct)

(import scheme 
	(rename chicken
		(define-record-type define-record-type1)
		(define-record define-record1))
	(rename defstruct (defstruct defstruct1))
        type-stubs)

(import-for-syntax srfi-1 chicken)


(define-syntax define-record
  (er-macro-transformer
   (lambda (x r c)
     (##sys#check-syntax 'define-record x '(_ symbol . #(_ 0)))
     (let* ((name (strip-syntax (cadr x)))
	    (slots (cddr x))
	    (%define-record (r 'define-record1))
	    (%begin (r 'begin))
	    (%setter (r 'setter))
	    (%colon (r ':))
	    (slots (map (lambda (slot)
			  (if (symbol? slot) `(,slot ,%colon *) slot))
			slots))
	    (names/types
	     (map (lambda (slot)
		    (##sys#check-syntax 'define-record slot '(_ _ _))
		    (assert (c %colon (r (second slot)))
			    "invalid syntax in slot specification" slot)
		    (cond ((symbol? (car slot))
			   (cons (car slot) (third slot)))
			  ((and (pair? (car slot))
				(c %setter (caar slot))
				(symbol? (second (car slot))))
			   (cons (second (car slot)) (third slot)))
			  (else 
			   (syntax-error
			    'define-record
			    "invalid syntax in slot specification" slot))))
		  slots)))
       `(,%begin
	 (,%colon ,(r (symbol-append 'make- name))
		  (,@(map cdr names/types) -> (struct ,name))
		  (,(map cdr names/types)
		   (##sys#make-structure 
		    ',name 
		    ,@(list-tabulate 
		       (length names/types) 
		       (lambda (i) `#(,(add1 i)))))))
	 (,%colon ,(r (symbol-append name '?))
		  (* -> boolean : (struct ,name)))
	 ,@(append-map
	    (lambda (n/t slot i)
	      (let ((sname (strip-syntax (car n/t)))
		    (slot (if (symbol? slot) `(,slot ,%colon *) slot)))
		(cond ((symbol? (car slots)) ; explicit setter procedure?
		       `((,%colon ,(r (symbol-append name '- sname))
				  ((struct ,name) -> ,(cdr n/t))
				  (((struct ,name)) (##sys#slot #(1) ',i)))
			 (,%colon ,(r (symbol-append name '- sname '-set!))
				  ((struct ,name) ,(cdr n/t) -> undefined)
				  (((struct ,name) *) (##sys#setslot #(1) ',i #(2))))))
		      (else
		       `((,%colon ,(r (symbol-append name '- sname))
				  ((struct ,name) -> ,(cdr n/t))
				  (((struct ,name)) (##sys#slot #(1) ',i))))))))
	    names/types slots (iota (length names/types) 1))
	 (,%define-record ,name ,@(unzip1 slots)))))))

(define-syntax define-record-type
  (er-macro-transformer
   (lambda (x r c)
     (##sys#check-syntax
      'define-record-type x
      '(_ symbol (symbol . #(symbol 0)) symbol . #(_ 0)))
     (let* ((name (strip-syntax (second x)))
	    (ctor (third x))
	    (pred (fourth x))
	    (fields (cddddr x))
	    (%define-record-type (r 'define-record-type1))
	    (%begin (r 'begin))
	    (%setter (r 'setter))
	    (%colon (r ':))
	    (accs/mods/types
	     (map (lambda (field)
		    (let* ((len (length field)))
		      (assert 
		       (and (list? field)
			    (>= len 2)
			    (symbol? (first field))
			    (symbol? (second field))
			    (case len
			      ((4) (c %colon (third field)))
			      ((5) (and (c %colon (fourth field))
					(or (symbol? (third field))
					    (and (pair? (third field))
						 (c %setter (r (car (third field))))
						 (symbol? (second (third field)))))))
			      ((2) #t)
			      ((3) (symbol? (third field)))
			      (else #f)))
		       "invalid syntax in field specification" field)
		      (cons*
		       (first field)
		       (second field)
		       (case len
			 ((2) (list #f '*))
			 ((3) (list (third field) '*))
			 ((4) (list #f (fourth field)))
			 ((5) (list (third field) (fifth field)))))))
		  fields)))
       `(,%begin
	 (,%colon ,(car ctor)
		  (,@(map (lambda (tag)
			    (let loop ((fields accs/mods/types))
			      (cond ((null? fields)
				     (syntax-error
				      'define-record-type
				      "constructor tag refers to nonexistent record field"
				      ctor))
				    ((c tag (caar fields)) (fourth (car fields)))
				    (else (loop (cdr fields))))))
			  (cdr ctor))
		   -> (struct ,name))
		  (,(map (lambda (fname)
			   (cond ((assq fname accs/mods/types) => fourth)
				 (else (error 
					'define-record-type
					"contructor tag refers to unknown field"
					ctor))))
			 (cdr ctor))
		   (##sys#make-structure
		    ',name
		    ,@(let lp [(names (map first accs/mods/types))
			       (l '())]
			(if (null? names)
			    (begin
			      (reverse l))
			    (cond ((list-index (cute eq? <> (first names)) (cdr ctor)) =>
				   (lambda (ctor-idx) (lp (cdr names) (cons (vector (add1 ctor-idx)) l))))
				  (else
				   ;; XXX this indicates a problem: the initial value
				   ;;     of the slot is not necessarily of type
				   ;;     undefined - should be make this an error?
				   (lp (cdr names) (cons '(##core#undefined) l)))))))))
	 (,%colon ,pred (* -> boolean : (struct ,name)))
	 ,@(append-map
	    (lambda (a/m/t i)
	      (let ((mod (third a/m/t)))
		`((,%colon ,(second a/m/t)
			   ((struct ,name) -> ,(fourth a/m/t))
			   (((struct ,name)) (##sys#slot #(1) ',i)))
		  ,@(if (symbol? mod)
			`((,%colon ,(third a/m/t) 
				   ((struct ,name) ,(fourth a/m/t) -> undefined)
				   (((struct ,name) *)
				    (##sys#setslot #(1) ',i #(2)))))
			'()))))
	    accs/mods/types (iota (length accs/mods/types) 1))
	 (,%define-record-type 
	  ,name 
	  ,ctor
	  ,pred
	  ,@(map (lambda (a/m/t)
		   (if (third a/m/t)
		       (list (first a/m/t) (second a/m/t) (third a/m/t))
		       (list (first a/m/t) (second a/m/t))))
		 accs/mods/types)))))))

(define-syntax defstruct
  (er-macro-transformer
   (lambda (x r c)
     (##sys#check-syntax 'defstruct x '(_ symbol . #(_ 0)))
     (let* ((name (strip-syntax (cadr x)))
	    (%colon (r ':))
	    (slots (map (lambda (slot)
			  (cond ((symbol? slot) `(,slot ,%colon *))
				((and (list? slot) (= 2 (length slot)))
				 (cons slot `(,%colon *)))
				(else slot)))
			(cddr x))))
       ;; we do this to ensure the same order of fields as in the 
       ;; original "defstruct"
       (let-values (((init-fields no-init-fields)
		     (partition (o pair? car) slots)))
	 (let* ((slots (append no-init-fields init-fields))
		(%defstruct (r 'defstruct1))
		(%begin (r 'begin))
		(names/types
		 (map (lambda (slot)
			(##sys#check-syntax 'defstruct slot '(_ _ _))
			(assert (c %colon (r (second slot)))
				"invalid syntax in slot specification" slot)
			(cond ((symbol? (car slot))
			       (cons (car slot) (third slot)))
			      ((and (pair? (car slot))
				    (symbol? (caar slot)))
			       (cons (caar slot) (third slot)))
			      (else 
			       (syntax-error
				'defstruct
				"invalid syntax in slot specification" slot))))
		      slots)))
	   `(,%begin
	     (,%colon ,(r (symbol-append 'make- name))
		      (#!rest -> (struct ,name)))
	     (,%colon ,(r (symbol-append name '?))
		      (* -> boolean : (struct ,name)))
	     (,%colon ,(r (symbol-append 'update- name))
		      ((struct ,name) #!rest -> (struct ,name)))
	     (,%colon ,(r (symbol-append 'set- name '!))
		      ((struct ,name) #!rest -> undefined))
	     (,%colon ,(r (symbol-append name '->alist))
		      ((struct ,name) -> (list-of (pair symbol *))))
	     (,%colon ,(r (symbol-append 'alist-> name))
		      ((list-of (pair symbol *)) -> (struct ,name)))
	     ,@(append-map
		(lambda (n/t slot i)
		  (let ((sname (strip-syntax (car n/t))))
		    `((,%colon ,(r (symbol-append name '- sname))
			       ((struct ,name) -> ,(cdr n/t))
			       (((struct ,name)) (##sys#slot #(1) ',(add1 i))))
		      (,%colon ,(r (symbol-append name '- sname '-set!))
			       ((struct ,name) ,(cdr n/t) -> undefined)
			       (((struct ,name) *) (##sys#setslot #(1) ',(add1 i) #(2)))))))
		names/types slots (iota (length slots)))
	     (,%defstruct ,name ,@(unzip1 slots)))))))))

)
