; Support for queues
;
; Written by Andrew Wilcox (awilcox@astro.psu.edu) on April 1, 1992.
;
; This code is in the public domain.
; 
; (heavily adapated for use with CHICKEN by felix)
;


(module queues (make-queue
		queue?
		queue-length
		queue-empty?
		queue-first
		queue-last
		queue-add!
		queue-remove!
		queue->list
		list->queue
		queue-push-back!
		queue-push-back-list!)
  
  (import scheme chicken)

; Elements in a queue are stored in a list.  The last pair in the list
; is stored in the queue type so that datums can be added in constant
; time.

(define (make-queue) (##sys#make-structure 'queue '() '() 0))
(define (queue? x) (##sys#structure? x 'queue))

(define (queue-length q)		; thread-safe
  (##sys#check-structure q 'queue 'queue-length)
  (##sys#slot q 3))

(define (queue-empty? q)		; thread-safe
  (##sys#check-structure q 'queue 'queue-empty?)
  (eq? '() (##sys#slot q 1)) )

(define queue-first			; thread-safe
  (lambda (q)
    (##sys#check-structure q 'queue 'queue-first)
    (let ((first-pair (##sys#slot q 1)))
      (when (eq? '() first-pair)
	(##sys#error 'queue-first "queue is empty" q))
      (##sys#slot first-pair 0) ) ) )

(define queue-last			; thread-safe
  (lambda (q)
    (##sys#check-structure q 'queue 'queue-last)
    (let ((last-pair (##sys#slot q 2)))
      (when (eq? '() last-pair)
	(##sys#error 'queue-last "queue is empty" q))
      (##sys#slot last-pair 0) ) ) )

(define (queue-add! q datum)		; thread-safe
  (##sys#check-structure q 'queue 'queue-add!)
  (let ((new-pair (cons datum '())))
    (cond ((eq? '() (##sys#slot q 1)) (##sys#setslot q 1 new-pair))
	  (else (##sys#setslot (##sys#slot q 2) 1 new-pair)) )
    (##sys#setslot q 2 new-pair) 
    (##sys#setislot q 3 (fx+ (##sys#slot q 3) 1))
    (##core#undefined) ) )

(define queue-remove!			; thread-safe
  (lambda (q)
    (##sys#check-structure q 'queue 'queue-remove!)
    (let ((first-pair (##sys#slot q 1)))
      (when (eq? '() first-pair)
	(##sys#error 'queue-remove! "queue is empty" q) )
      (let ((first-cdr (##sys#slot first-pair 1)))
	(##sys#setslot q 1 first-cdr)
	(if (eq? '() first-cdr)
	    (##sys#setslot q 2 '()) )
	(##sys#setislot q 3 (fx- (##sys#slot q 3) 1))
	(##sys#slot first-pair 0) ) ) ) )

(define (queue->list q)
  (##sys#check-structure q 'queue 'queue->list)
  (let loop ((lst (##sys#slot q 1)) (lst2 '()))
    (if (null? lst)
	(##sys#fast-reverse lst2)
	(loop (##sys#slot lst 1) (cons (##sys#slot lst 0) lst2)))))

(define (list->queue lst0)		
  (##sys#check-list lst0 'list->queue)
  (##sys#make-structure 
   'queue lst0
   (if (eq? lst0 '())
       '()
       (do ((lst lst0 (##sys#slot lst 1)))
	   ((eq? (##sys#slot lst 1) '()) lst)
	 (if (or (not (##core#inline "C_blockp" lst))
		 (not (##core#inline "C_pairp" lst)) )
	     (##sys#error-not-a-proper-list lst0 'list->queue) ) ) )
   (##sys#length lst0)) )


; (queue-push-back! queue item)
; Pushes an item into the first position of a queue.

(define (queue-push-back! q item)	; thread-safe
  (##sys#check-structure q 'queue 'queue-push-back!)
  (let ((newlist (cons item (##sys#slot q 1))))
    (##sys#setslot q 1 newlist)
    (if (eq? '() (##sys#slot q 2))
	(##sys#setslot q 2 newlist))
    (##sys#setislot q 3 (fx+ (##sys#slot q 3) 1))))

; (queue-push-back-list! queue item-list)
; Pushes the items in item-list back onto the queue,
; so that (car item-list) becomes the next removable item.

(define-inline (last-pair lst0)
  (do ((lst lst0 (##sys#slot lst 1)))
      ((eq? (##sys#slot lst 1) '()) lst)))

(define (queue-push-back-list! q itemlist)
  (##sys#check-structure q 'queue 'queue-push-back-list!)
  (##sys#check-list itemlist 'queue-push-back-list!)
  (let* ((newlist (append itemlist (##sys#slot q 1)))
	 (newtail (if (eq? newlist '())
		       '()
		       (last-pair newlist))))
    (##sys#setslot q 1 newlist)
    (##sys#setslot q 2 newtail)
    (##sys#setislot q 3 (fx+ (##sys#slot q 3) (##core#inline "C_i_length" itemlist)))))

)
