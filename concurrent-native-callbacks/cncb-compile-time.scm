;;;; compile-time part of cncb


(use data-structures srfi-1 matchable ports)


(define (cncb-transformer x r c sync)
  (let ((%d (r 'd))
	(%a (r 'a))
	(%begin (r 'begin))
	(%dispatcher-add! (r 'dispatcher-add!))
	(%quote (r 'quote))
	(%define (r 'define))
	(%dispatcher (r 'dispatcher))
	(%lambda (r 'lambda)))
    (define (expand name args dispatcher-id rtype body)
      `(,%begin
	(,%define ,%d (,%dispatcher (,%quote ,dispatcher-id)))
	,(generate-entry-point r name %d args sync rtype)
	(,%dispatcher-add!
	 ,%d
	 (,%quote ,name)
	 (,%lambda
	  (,%d ,%a) 
	  ,(unstash-and-execute r args %a body sync rtype)))))
    (if sync
	(match x
	  ((_ ((name dispatcher-id) args ...) rtype body ...)
	   (expand name args dispatcher-id rtype body))
	  ((_ (name args ...) rtype body ...)
	   (expand name args 'default rtype body)))
	(match x
	  ((_ ((name dispatcher-id) args ...) body ...)
	   (expand name args dispatcher-id #f body))
	  ((_ (name args ...) body ...)
	   (expand name args 'default #f body))))))

(define (generate-entry-point r name dispvar t/a sync rtype)
  (let ((cvar (gensym "cvar"))
	(mutex (gensym "mutex"))
	(infd (gensym "infd"))
	(outfd (gensym "outfd"))
	(data (gensym "data"))
	(ptr (gensym "ptr"))
	(result (symbol->string (gensym "result")))
	(result-arg (gensym "arg"))
	(%begin (r 'begin))
	(%define-foreign-variable (r 'define-foreign-variable))
	(%dispatcher-argument-output-fileno (r 'dispatcher-argument-output-fileno))
	(%dispatcher-result-input-fileno (r 'dispatcher-result-input-fileno))
	(%foreign-declare (r 'foreign-declare))
	(rtype (strip-syntax rtype))
	(t/a (strip-syntax t/a)))
    (define (stash-arguments args)
      (let ((size (gensym "size")))
	(with-output-to-string
	  (lambda ()
	    ;; add ptr to callback-name, mutex-ptr and cvar-ptr
	    ;; we align to 8-byte, which should be sufficient
	    (printf "int ~a = 6 * sizeof(void *);~%char *~a, *~a;~%" size data ptr)
	    (when sync
	      ;; by convention, in a synchronous call the last argument is a
	      ;; pointer to the result value
	      (set! args (append args `(((c-pointer ,rtype) ,result-arg))))
	      (if (eq? rtype 'void)
		  (printf "void *~a;~%" result-arg)
		  (printf "~a; ~a = &~a;~%"
		    (foreign-type-declaration rtype result name)
		    (foreign-type-declaration 
		     `(c-pointer ,rtype)
		     (symbol->string result-arg))
		    result)))
	    (for-each
	     (lambda _
	       (printf "~a += 2 * sizeof(void *);~%" size))
	     args)
	    ;;XXX result of malloc(3) not checked
	    (printf "~a = (char *)malloc(~a);~%~a = ~a;~%" data size ptr data)
	    (printf "*((char **)~a) = \"~a\";~%~a += 2 * sizeof(void *);~%" ptr name ptr)
	    (cond (sync
		   (printf "*((void **)~a) = &~a;~%" ptr mutex)
		   (printf "~a += 2 * sizeof(void *);~%" ptr) 
		   (printf "*((void **)~a) = &~a;~%" ptr cvar)
		   (printf "~a += 2 * sizeof(void *);~%" ptr) )
		  (else
		   ;; two dummy ptrs when no cvar/mutex needed
		   (printf "~a += 4 * sizeof(void *);~%" ptr)))
	    (for-each
	     (match-lambda
	       ((_ arg)
		(printf "memcpy(~a, &~a, sizeof ~a);~%~a += 2 * sizeof(void *);~%" 
		  ptr arg arg ptr)))
	     args)))))
    `(,%begin
      (,%foreign-declare
       "#include <pthread.h>\n"
       ,(conc "static int " infd "," outfd ";\n")
       ,(conc (if sync (foreign-type-declaration rtype "") "void")
	      " " name "("
	      (string-intersperse 
	       (map (match-lambda
		      ((type arg)
		       (foreign-type-declaration type (symbol->string arg))))
		    t/a)
	       ",")
	      "){\n")
       ,(if sync
	    (conc
	     "pthread_cond_t " cvar ";\n"
	     "pthread_mutex_t " mutex ";\n")
	    "")
       ,(stash-arguments t/a)
       ,(if sync
	    (conc "pthread_mutex_init(&" mutex ", NULL);\n"
		  "pthread_cond_init(&" cvar ", NULL);\n"
		  "pthread_mutex_lock(&" mutex ");\n")
	    "")
       ,(conc "(void)write(" outfd ", &" data ", sizeof(void *));\n")
       ,(if sync
	    (conc "pthread_cond_wait(&" cvar ", &" mutex ");\n"
		  "pthread_mutex_unlock(&" mutex ");\n"
		  "pthread_cond_destroy(&" cvar ");\n"
		  "pthread_mutex_destroy(&" mutex ");\n")
	   "")
       ,(if (and sync (not (eq? 'void rtype)))
	    (conc "return " result ";\n")
	    "")
       "}\n")
      (,%define-foreign-variable ,infd int)
      (,%define-foreign-variable ,outfd int)
      (set! ,outfd (,%dispatcher-argument-output-fileno ,dispvar))
      (set! ,infd (,%dispatcher-result-input-fileno ,dispvar)))))


(define (unstash-and-execute r t/a ptr body sync rtype)
  (let ((%let (r 'let))
	(%foreign-lambda* (r 'foreign-lambda*))
	(%begin (r 'begin))
	(%free (r 'free))
	(%begin0 (r 'begin0))
	(%advance! (r 'advance!))
	(ptrvar (gensym "pptr")))
    `(,%let ((,ptrvar ((,%foreign-lambda* c-pointer ((c-pointer ptr)) "return(ptr);")
		       ,ptr))		; copy pointer because of pointer-mutation below
	     (,%advance! 
	      (,%foreign-lambda* 
	       void ((scheme-object ptr))
	       "C_set_block_item(ptr, 0, C_block_item(ptr, 0) + 2 * sizeof(void *));")))
	    (,%advance! ,ptrvar)	; cbname
	    (,%advance! ,ptrvar)	; mutex
	    (,%advance! ,ptrvar)	; cvar
	    (,%let
	     ,(map (match-lambda
		     ((type arg) 
		      `(,arg (,%begin0
			      ((,%foreign-lambda* 
				,type
				(((c-pointer ,type) ptr))
				,(string-append
				  (foreign-type-declaration type "val")
				  " = *ptr;\nreturn(val);"))
			       ,ptrvar)
			      (,%advance! ,ptrvar)))))
		   t/a)
	     ,@(if sync
		   `(((,%foreign-lambda* 
		       void
		       (((c-pointer (c-pointer ,rtype)) ptr)
			(c-pointer buf)
			(,(if (eq? rtype 'void) 'scheme-object rtype) val)
			(int index))
		       "pthread_mutex_t *m = *((pthread_mutex_t **)buf + 2);"
		       "pthread_cond_t *c = *((pthread_cond_t **)buf + 4);"
		       ,(if (eq? rtype 'void) "" "*(*ptr) = val;")
		       "pthread_mutex_lock(m);"
		       "pthread_cond_signal(c);"
		       "pthread_mutex_unlock(m);")
		      ,ptrvar ,ptr (,%begin ,@body) ,(length t/a)))
		   body))
	     (,%free ,ptr))))


;; foreign-type conversion - taken from c-backend.scm

(define (foreign-type-declaration type target #!optional result?)
  (let ((err (lambda () (syntax-error "illegal foreign type" type)))
	(badresult
	 (lambda (thunk)
	   (when result?
	     (warning 
	      "native callback result type may refer to unsafe data"
	      type
	      result?))
	   (thunk)))
	(str (lambda (ts) (string-append ts " " target))) )
    (let-syntax ((check
		  (syntax-rules ()
		    ((_ exp) (badresult (lambda () exp))))))
      (case type
	((scheme-object) (check (str "C_word")))
	((char byte) (str "C_char"))
	((unsigned-char unsigned-byte) (str "unsigned C_char"))
	((unsigned-int unsigned-integer) (str "unsigned int"))
	((unsigned-int32 unsigned-integer32) (str "C_u32"))
	((int integer bool) (str "int"))
	((size_t) (str "size_t"))
	((int32 integer32) (str "C_s32"))
	((integer64) (str "C_s64"))
	((unsigned-integer64) (str "C_u64"))
	((short) (str "short"))
	((long) (str "long"))
	((unsigned-short) (str "unsigned short"))
	((unsigned-long) (str "unsigned long"))
	((float) (str "float"))
	((double number) (str "double"))
	((c-pointer nonnull-c-pointer scheme-pointer nonnull-scheme-pointer)
	 (str "void *"))
	((c-string-list c-string-list*) (check "C_char **"))
	((blob nonnull-blob u8vector nonnull-u8vector)
	 (check (str "unsigned char *")))
	((u16vector nonnull-u16vector)
	 (check (str "unsigned short *")))
	((s8vector nonnull-s8vector) 
	 (check (str "char *")))
	((u32vector nonnull-u32vector)
	 (check (str "unsigned int *")))
	((s16vector nonnull-s16vector)
	 (check (str "short *")))
	((s32vector nonnull-s32vector)
	 (check (str "int *")))
	((f32vector nonnull-f32vector)
	 (check (str "float *")))
	((f64vector nonnull-f64vector)
	 (check (str "double *")))
	((pointer-vector nonnull-pointer-vector) (str "void **"))
	((nonnull-c-string c-string nonnull-c-string* c-string* symbol) 
	 (check (str "char *")))
	((nonnull-unsigned-c-string nonnull-unsigned-c-string* 
				    unsigned-c-string unsigned-c-string*)
	 (check (str "unsigned char *")))
	((void) (str "void"))
	(else
	 (cond				#;((and (symbol? type) 
	  (##sys#hash-table-ref foreign-type-table type))
	 => (lambda (t)
	  (foreign-type-declaration (if (vector? t) (vector-ref t 0) t) target)) )
	 ((string? type) (str type))
	 ((list? type)
	  (let ((len (length type)))
	    (cond 
	     ((and (= 2 len)
		   (memq (car type)
			 '(pointer nonnull-pointer c-pointer 
				   nonnull-c-pointer) ) )
	      (foreign-type-declaration
	       (cadr type)
	       (string-append "*" target)) )
	     ((and (= 2 len)
		   (eq? 'ref (car type)))
	      (foreign-type-declaration
	       (cadr type)
	       (string-append "&" target)) )
	     ((and (> len 2)
		   (eq? 'template (car type)))
	      (str
	       (string-append 
		(foreign-type-declaration (cadr type) "")
		"<"
		(string-intersperse
		 (map (cut foreign-type-declaration <> "") (cddr type))
		 ",")
		"> ") ) )
	     ((and (= len 2) (eq? 'const (car type)))
	      (string-append
	       "const " 
	       (foreign-type-declaration (cadr type) target)))
	     ((and (= len 2) (eq? 'struct (car type)))
	      (string-append
	       "struct "
	       (->string (cadr type)) " " target))
	     ((and (= len 2) (eq? 'union (car type)))
	      (string-append "union " (->string (cadr type)) " " target))
	     ((and (= len 2) (eq? 'enum (car type)))
	      (string-append "enum " (->string (cadr type)) " " target))
	     ((and (= len 3) 
		   (memq (car type) 
			 '(instance nonnull-instance)))
	      (string-append (->string (cadr type)) "*" target))
	     ((and (= len 3) (eq? 'instance-ref (car type)))
	      (check (string-append (->string (cadr type)) "&" target)))
	     ((and (>= len 3) (eq? 'function (car type)))
	      (let ((rtype (cadr type))
		    (argtypes (caddr type))
		    (callconv (optional (cdddr type) "")))
		(string-append
		 (foreign-type-declaration rtype "")
		 callconv
		 " (*" target ")("
		 (string-intersperse
		  (map (lambda (at)
			 (if (eq? '... at) 
			     "..."
			     (foreign-type-declaration at "") ) )
		       argtypes) 
		  ",")
		 ")" ) ) )
	     (else (err)) ) ) )
	 (else (err)) ) ) ) ) ) )
