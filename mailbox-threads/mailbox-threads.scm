(module mailbox-threads
   (current-thread
    make-thread
    thread-specific-set!
    thread-specific
    thread-send
    thread-start!
    thread?
    thread-receive
    thread-mailbox-next
    thread-mailbox-rewind
    thread-mailbox-extract-and-rewind)

(import chicken scheme)

(require-library srfi-18)
(import
  (except srfi-18
   thread?
   make-thread
   thread-specific-set!
   thread-specific
   thread-start!
   current-thread) 
  (prefix (only srfi-18 
   current-thread
   thread?
   make-thread
   thread-specific-set!
   thread-specific
  thread-start!) thread:))
(use mailbox)

(define *tag* 'mboxthread)

(define (thread? t)
  (if (thread:thread? t)
   (let ((specific (thread:thread-specific t)))
      (cond ((list? specific)
                (eq? (car (thread:thread-specific t)) *tag*))
            (else #f)))
  #f))

(define (current-thread)
   (let ((t (thread:current-thread)))
      (if (thread? t) t
         (begin 
            (add-specifics! t)
            t ))))

(define (add-specifics! t)
   (let* ((mbox (make-mailbox 'mbox-thread))
         (mbox-cursor (make-mailbox-cursor mbox))
	 (specs (list *tag* '() mbox mbox-cursor)))
   (thread:thread-specific-set! t specs)
   specs))

(define (make-thread thunk #!optional (name 'anonymous))
   (let ((t (thread:make-thread thunk name)))
      (add-specifics! t)
   t))

(define (thread-specific-set! thread newspec)
   (if (not (thread? thread))
       (add-specifics! thread)
       (let* ((specific (thread:thread-specific thread))
             (mbox (caddr specific))
             (cursor (cadddr specific))
	     (newspecs (list *tag* newspec mbox cursor)))
          (thread:thread-specific-set! thread newspecs)
	  newspecs)))

(define (thread-specific thread)
   (if (not (thread? thread))
       (add-specifics! thread)
       (cadr (thread:thread-specific thread))))

(define (thread-start! thread)
      (if (procedure? thread)
          (thread:thread-start! (make-thread thread))
          (thread:thread-start! thread)))

(define (thread-mbox-cursor thread)
   (if (not (thread? thread))
       (add-specifics! thread)
       (cadddr (thread:thread-specific thread))))

(define (thread-mbox thread)
   (if (not (thread? thread))
       (add-specifics! thread)
       (caddr (thread:thread-specific thread))))

(define (thread-send thread msg)
      (mailbox-send! (thread-mbox thread) msg))

(define (thread-receive . args)
      (apply mailbox-receive! (thread-mbox (current-thread)) args))

(define (thread-mailbox-next . args)
   (let* ((mbox-cursor (thread-mbox-cursor (current-thread))))
      (apply mailbox-cursor-next mbox-cursor args)))

(define (thread-mailbox-rewind)
   (let* ((mbox-cursor (thread-mbox-cursor (current-thread))))
      (mailbox-cursor-rewind mbox-cursor)))

(define (thread-mailbox-extract-and-rewind)
  (let* ((mbox-cursor (thread-mbox-cursor (current-thread))))
      (mailbox-cursor-extract-and-rewind! mbox-cursor))) )
