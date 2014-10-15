;;;; inline-queue.scm
;;;; Kon Lovett, Jun '10

;;; Requires (include "chicken-primitive-object-inlines")

;; Support

(define-record-type-variant queue (unsafe unchecked inline)
  (%%make-queue hd tl)
  %queue?
  (hd %queue-first-pair %queue-first-pair-set!)
  (tl %queue-last-pair %queue-last-pair-set!) )

(define-inline (%make-queue) (%%make-queue '() '()))

(define-inline (%queue-empty? q) ($null? (%queue-first-pair q)))
(define-inline (%queue-count q) ($length (%queue-first-pair q)))

;; Operations

(define-inline (%queue-last-pair-empty! q) (%queue-last-pair-set! q '()))

(define-inline (%queue-add! q datum)
  (let ((new-pair ($cons datum '())))
    (if ($null? (%queue-first-pair q)) (%queue-first-pair-set! q new-pair)
        ($set-cdr! (%queue-last-pair q) new-pair) )
    (%queue-last-pair-set! q new-pair) ) )

(define-inline (%queue-remove! q)
  (let* ((first-pair (%queue-first-pair q))
         (next-pair ($cdr first-pair)))
    (%queue-first-pair-set! q next-pair)
    (when ($null? next-pair) (%queue-last-pair-empty! q) )
    ($car first-pair) ) )

(define-inline (%queue-push-back! q item)
  (let ((newlist ($cons item (%queue-first-pair q))))
    (%queue-first-pair-set! q newlist)
    (when ($null? (%queue-last-pair q)) (%queue-last-pair-set! q newlist) ) ) )

(define-inline (%queue-push-back-list! q itemlist)
  (let ((newlist ($append! ($list-copy itemlist) (%queue-first-pair q))))
    (%queue-first-pair-set! q newlist)
    (if ($null? newlist) (%queue-last-pair-empty! q)
        (%queue-last-pair-set! q ($last-pair newlist) ) ) ) )

(define-inline (%queue-extract-pair! q targ-pair)
  ; Scan queue list until we find the item to remove
  (let scanning ((this-pair (%queue-first-pair q)) (prev-pair '()))
    ; Keep scanning until found
    (if (not ($eq? this-pair targ-pair)) (scanning ($cdr this-pair) this-pair)
        ;found so cut out the pair
        (let ((next-pair ($cdr this-pair)))
          ; At the head of the list, or in the body?
          (if ($null? prev-pair) (%queue-first-pair-set! q next-pair)
              ($set-cdr! prev-pair next-pair) )
          ; When the cut pair is the last item update the last pair ref.
          (when ($eq? this-pair (%queue-last-pair q)) (%queue-last-pair-set! q prev-pair)) ) ) ) )
