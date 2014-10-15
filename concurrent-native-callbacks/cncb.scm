;;;; concurrent native callbacks


(use srfi-69 srfi-1 posix data-structures
     srfi-18 extras lolevel)
(import typed-records miscmacros matchable bind foreign)


(import-for-syntax chicken matchable)

(begin-for-syntax
 (require-extension concurrent-native-callbacks-compile-time))


(bind-file* "twiddle.c")


(define-record dispatcher
  (id : symbol)
  (thread : thread)
  (callbacks : (list-of (pair fixnum ((struct dispatcher) pointer -> * boolean))))
  (argument-input-fileno : fixnum)
  (argument-output-fileno : fixnum)
  (result-input-fileno : fixnum)
  (result-output-fileno : fixnum) )


(define word-size (foreign-value "sizeof(void *)" int))
(define dispatcher-table (make-hash-table eq?))

(define (nonblocking-pipe-input-port fd id)
  (##sys#custom-input-port 'nonblocking-pipe-input-port (->string id) fd #t))

(define (nonblocking-pipe-output-port fd id)
  (##sys#custom-output-port 'nonblocking-pipe-output-port (->string id) fd #t))

(define (dispatch-loop box)
  (let* ((disp (car box))
	 (id (dispatcher-id disp))
	 (in (nonblocking-pipe-input-port (dispatcher-argument-input-fileno disp) id))
	 (out (nonblocking-pipe-output-port (dispatcher-result-output-fileno disp) id)))
    (let loop ()
      (let ((input (extract_argument_ptr (read-string word-size in))))
	(cond ((not (##sys#null-pointer? input))
	       (let ((cbname (extract_callback_name input)))
		 (cond ((alist-ref cbname (dispatcher-callbacks disp)) =>
			(lambda (cb)
			  (cb disp input)))
		       (else
			(warning "callback not found" cbname id)))
		 (loop)))
	      (else			; NULL-ptr aborts dispatcher
	       (close-input-port in)
	       (close-output-port out)
	       (file-close (dispatcher-argument-output-fileno disp))
	       (file-close (dispatcher-result-input-fileno disp))))))))

(define (create-dispatcher id thread)
  (let-values (((in1 out1) (create-pipe))
	       ((in2 out2) (create-pipe)))
    (let ((disp (make-dispatcher id thread '() in1 out1 in2 out2)))
      (hash-table-set! dispatcher-table id disp)
      disp)))

(define (dispatcher id)
  (let ((box (list #f)))
    (define (dthread) (make-thread (cut dispatch-loop box) id))
    (or (hash-table-ref/default dispatcher-table id #f)
	(let ((disp (create-dispatcher id (dthread))))
	  (set-car! box disp)
	  (thread-start! (dispatcher-thread disp))
	  disp))))

(define (dispatch #!optional (id 'default))
  (thread-join! (dispatcher-thread (dispatcher id))))

(define (dispatcher-add! disp name cb)
  (dispatcher-callbacks-set!
   disp
   (alist-update! name cb (dispatcher-callbacks disp))))

(define (dispatcher-terminate! disp)
  (send_termination_message
   (dispatcher-argument-input-fileno disp)))


(define-syntax define-concurrent-native-callback
  (er-macro-transformer
   (lambda (x r c)
     (cncb-transformer x r c #f))))

(define-syntax define-synchronous-concurrent-native-callback
  (er-macro-transformer
   (lambda (x r c)
     (cncb-transformer x r c #t))))
