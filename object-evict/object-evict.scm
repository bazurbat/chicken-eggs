;;; Evict objects into static memory


(module object-evict (object-evicted?
		      object-evict
		      object-evict-to-location
		      object-release
		      object-size
		      object-unevict)

(import scheme chicken foreign)

(use srfi-69
     (only lolevel align-to-word))

(define (object-evicted? x) (##core#inline "C_permanentp" x))

(define (object-evict x . allocator)
  (let ([allocator 
	 (if (pair? allocator) (car allocator) (foreign-lambda c-pointer "malloc" int) ) ] 
	[tab (make-hash-table eq?)] )
    (##sys#check-closure allocator 'object-evict)
    (let evict ([x x])
      (cond [(not (##core#inline "C_blockp" x)) x ]
	    [(hash-table-ref/default tab x #f) ]
	    [else
	     (let* ([n (##sys#size x)]
		    [bytes (if (##core#inline "C_byteblockp" x) (align-to-word n) (##core#inline "C_bytes" n))]
		    [y (##core#inline "C_evict_block" x (allocator (fx+ bytes (##core#inline "C_bytes" 1))))] )
	       (when (symbol? x) (##sys#setislot y 0 (void)))
	       (hash-table-set! tab x y)
	       (unless (##core#inline "C_byteblockp" x)
		 (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x)) 1 0) (fx+ i 1)])
		     [(fx>= i n)]
		   ;; Note the use of `##sys#setislot' to avoid an entry in the mutations-table:
		   (##sys#setislot y i (evict (##sys#slot x i))) ) )
	       y ) ] ) ) ) )

(define (object-evict-to-location x ptr . limit)
  (##sys#check-special ptr 'object-evict-to-location)
  (let* ([limit (and (pair? limit)
		     (let ([limit (car limit)])
		       (##sys#check-exact limit 'object-evict-to-location)
		       limit)) ]
	 [ptr2 (##sys#address->pointer (##sys#pointer->address ptr))]
	 [tab (make-hash-table eq?)]
	 [x2
	  (let evict ([x x])
	    (cond [(not (##core#inline "C_blockp" x)) x ]
		  [(hash-table-ref/default tab x #f) ]
		  [else
		   (let* ([n (##sys#size x)]
			  [bytes 
			   (fx+ (if (##core#inline "C_byteblockp" x) (align-to-word n) (##core#inline "C_bytes" n))
				(##core#inline "C_bytes" 1) ) ] )
		     (when limit
		       (set! limit (fx- limit bytes))
		       (when (fx< limit 0) 
			 (signal
			  (make-composite-condition
			   (make-property-condition
			    'exn 'location 'object-evict-to-location
			    'message "cannot evict object - limit exceeded" 
			    'arguments (list x limit))
			   (make-property-condition 'evict 'limit limit) ) ) ) )
		   (let ([y (##core#inline "C_evict_block" x ptr2)])
		     (when (symbol? x) (##sys#setislot y 0 (void)))
		     (##sys#set-pointer-address! ptr2 (+ (##sys#pointer->address ptr2) bytes))
		     (hash-table-set! tab x y)
		     (unless (##core#inline "C_byteblockp" x)
		       (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x)) 1 0) (fx+ i 1)] )
			   [(fx>= i n)]
			 (##sys#setislot y i (evict (##sys#slot x i))) ) ) ; see above
		     y) ) ] ) ) ] )
    (values x2 ptr2) ) )

(define (object-release x . releaser)
  (let ([free (if (pair? releaser) 
		  (car releaser) 
		  (foreign-lambda void "free" c-pointer) ) ]
	[released '() ] )
    (let release ([x x])
      (cond [(not (##core#inline "C_blockp" x)) x ]
	    [(not (##core#inline "C_permanentp" x)) x ]
	    [(memq x released) x ]
	    [else
	     (let ([n (##sys#size x)])
	       (set! released (cons x released))
	       (unless (##core#inline "C_byteblockp" x)
		 (do ([i (if (##core#inline "C_specialp" x) 1 0) (fx+ i 1)])
		     [(fx>= i n)]
		   (release (##sys#slot x i))) )
	       (free 
		(##sys#address->pointer
		 (##core#inline_allocate ("C_block_address" 4) x))) ) ] ) ) ) )

(define (object-size x)
  (let ([tab (make-hash-table eq?)])
    (let evict ([x x])
      (cond [(not (##core#inline "C_blockp" x)) 0 ]
	    [(hash-table-ref/default tab x #f) 0 ]
	    [else
	     (let* ([n (##sys#size x)]
		    [bytes
		     (fx+ (if (##core#inline "C_byteblockp" x) (align-to-word n) (##core#inline "C_bytes" n))
			  (##core#inline "C_bytes" 1) ) ] )
	       (hash-table-set! tab x #t)
	       (unless (##core#inline "C_byteblockp" x)
		 (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x)) 1 0) (fx+ i 1)])
		     [(fx>= i n)]
		   (set! bytes (fx+ (evict (##sys#slot x i)) bytes)) ) )
	       bytes) ] ) ) ) )

(define (object-unevict x #!optional full)
  (let ([tab (make-hash-table eq?)])
    (let copy ([x x])
    (cond [(not (##core#inline "C_blockp" x)) x ]
	  [(not (##core#inline "C_permanentp" x)) x ]
	  [(hash-table-ref/default tab x #f) ]
	  [(##core#inline "C_byteblockp" x) 
	   (if full
	       (let ([y (##core#inline "C_copy_block" x (##sys#make-string (##sys#size x)))])
		 (hash-table-set! tab x y)
		 y) 
	       x) ]
	  [(symbol? x) 
	   (let ([y (##sys#intern-symbol (##sys#slot x 1))])
	     (hash-table-set! tab x y)
	     y) ]
	  [else
	   (let* ([words (##sys#size x)]
		  [y (##core#inline "C_copy_block" x (##sys#make-vector words))] )
	     (hash-table-set! tab x y)
	     (do ([i (if (##core#inline "C_specialp" x) 1 0) (fx+ i 1)])
		 ((fx>= i words))
	       (##sys#setslot y i (copy (##sys#slot y i))) )
	     y) ] ) ) ) )

)
