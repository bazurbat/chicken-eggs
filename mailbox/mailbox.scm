;;;; mailbox.scm
;;;; Kon Lovett, Mar '09
;;;; From Chicken 3 "mailbox" by Felix & Kon

;; Issues
;;
;; - When compile-time feature `unsafe-operations' inlined & primitive routines used.
;;
;; - Has explicit "unspecified" returns in some cases to avoid leaks of internal
;; objects.
;;
;; - 'wait-mailbox' may not return should a timeout exception occur.
;;
;; - Uses ##sys#thread-unblock!
;;
;; - Has knowledge of Unit srfi-18 time object internals.
;;
;; - Uses the Chicken extensions 'thread-suspend' & 'thread-resume'.
;;
;; - The thread waiting on a mailbox cursor may miss items since only
;; the end of the queue is available safely.
;;
;; - Probably should be rewritten to use a mutex & condition-variable rather than
;; disabling interrupts and having own thread waiting queue.

(module mailbox

  (;export
    ;Mailbox Exception API
    mailbox-timeout-condition? mailbox-timeout-exception?
    ;Mailbox API
    make-mailbox
    mailbox?
    mailbox-name
    mailbox-empty?
    mailbox-count
    mailbox-waiting?
    mailbox-waiters
    mailbox-send!
    mailbox-wait!
    mailbox-receive!
    mailbox-push-back!
    mailbox-push-back-list!
    ;Mailbox Cursor API
    make-mailbox-cursor
    mailbox-cursor?
    mailbox-cursor-mailbox
    mailbox-cursor-next
    mailbox-cursor-rewind
    mailbox-cursor-rewound?
    mailbox-cursor-unwound?
    mailbox-cursor-extract-and-rewind!)

  (import
    scheme
    chicken
    (only ports with-output-to-port)
    (only srfi-1 append! delete! list-copy last-pair)
    (only srfi-18
      current-thread
      thread-signal! thread-sleep!
      thread-suspend! thread-resume!
      time?)
    (only type-errors define-error-type error-list)
    (only condition-utils make-exn-condition+ make-condition-predicate)
    record-variants)

  (require-library
    ports srfi-1 srfi-18
    type-errors condition-utils
    record-variants)

  (declare
    (disable-interrupts) ;A MUST!
    (bound-to-procedure
      ##sys#signal-hook
      ##sys#thread-unblock!) )

;;; Primitives

  (include "chicken-primitive-object-inlines")
  (include "chicken-thread-object-inlines")
  (include "inline-type-checks")
  (include "inline-queue")

(define-inline (->boolean obj) (and obj #t))

(cond-expand
  (unsafe-operations
    (define-syntax $eq? (syntax-rules () ((_ ?arg0 ...) (%eq? ?arg0 ...))))
    (define-syntax $null? (syntax-rules () ((_ ?arg0 ...) (%null? ?arg0 ...))))
    (define-syntax $list? (syntax-rules () ((_ ?arg0 ...) (%list? ?arg0 ...))))
    (define-syntax $length (syntax-rules () ((_ ?arg0 ...) (%length ?arg0 ...))))
    (define-syntax $append! (syntax-rules () ((_ ?arg0 ...) (%append! ?arg0 ...))))
    (define-syntax $delq! (syntax-rules () ((_ ?arg0 ...) (%delq! ?arg0 ...))))
    (define-syntax $cons (syntax-rules () ((_ ?arg0 ...) (%cons ?arg0 ...))))
    (define-syntax $car (syntax-rules () ((_ ?arg0 ...) (%car ?arg0 ...))))
    (define-syntax $cdr (syntax-rules () ((_ ?arg0 ...) (%cdr ?arg0 ...))))
    (define-syntax $set-car! (syntax-rules () ((_ ?arg0 ...) (%set-car! ?arg0 ...))))
    (define-syntax $set-cdr! (syntax-rules () ((_ ?arg0 ...) (%set-cdr! ?arg0 ...))))
    (define-syntax $list-copy (syntax-rules () ((_ ?arg0 ...) (%list-copy ?arg0 ...))))
    (define-syntax $last-pair (syntax-rules () ((_ ?arg0 ...) (%last-pair ?arg0 ...))))
    (define-syntax $current-thread (syntax-rules () ((_ ?arg0 ...) (%current-thread ?arg0 ...))))
    (define-syntax $thread-blocked? (syntax-rules () ((_ ?arg0 ...) (%thread-blocked? ?arg0 ...))))
    (define-syntax $thread-blocked-for-timeout? (syntax-rules () ((_ ?arg0 ...) (%thread-blocked-for-timeout? ?arg0 ...))))
    )
  (else
    (define-syntax $eq? (syntax-rules () ((_ ?arg0 ...) (eq? ?arg0 ...))))
    (define-syntax $null? (syntax-rules () ((_ ?arg0 ...) (null? ?arg0 ...))))
    (define-syntax $list? (syntax-rules () ((_ ?arg0 ...) (list? ?arg0 ...))))
    (define-syntax $length (syntax-rules () ((_ ?arg0 ...) (length ?arg0 ...))))
    (define-syntax $append! (syntax-rules () ((_ ?arg0 ...) (append! ?arg0 ...))))
    (define-syntax $delq! (syntax-rules () ((_ ?arg0 ...) (delete! ?arg0 ...))))
    (define-syntax $cons (syntax-rules () ((_ ?arg0 ...) (cons ?arg0 ...))))
    (define-syntax $car (syntax-rules () ((_ ?arg0 ...) (car ?arg0 ...))))
    (define-syntax $cdr (syntax-rules () ((_ ?arg0 ...) (cdr ?arg0 ...))))
    (define-syntax $set-car! (syntax-rules () ((_ ?arg0 ...) (set-car! ?arg0 ...))))
    (define-syntax $set-cdr! (syntax-rules () ((_ ?arg0 ...) (set-cdr! ?arg0 ...))))
    (define-syntax $list-copy (syntax-rules () ((_ ?arg0 ...) (list-copy ?arg0 ...))))
    (define-syntax $last-pair (syntax-rules () ((_ ?arg0 ...) (last-pair ?arg0 ...))))
    (define-syntax $current-thread (syntax-rules () ((_ ?arg0 ...) (current-thread ?arg0 ...))))
    (define ($thread-blocked? th) (eq? 'blocked (##sys#slot th 3)))
    (define ($thread-blocked-for-timeout? th) (and (##sys#slot th 4) (not (##sys#slot th 11))))
    ) )

;;; Mailbox Support

(define-record-type-variant mailbox (unsafe unchecked inline)
  (%%make-mailbox nm qu wt)
  %mailbox?
  (nm %mailbox-name)
  (qu %mailbox-queue)
  (wt %mailbox-waiters %mailbox-waiters-set!) )

(define-inline (%make-mailbox nm) (%%make-mailbox nm (%make-queue) '()))

(define-error-type mailbox)
(define-inline-check-type mailbox)

;; Message queue

(define-inline (%mailbox-queue-first-pair mb) (%queue-first-pair (%mailbox-queue mb)))
(define-inline (%mailbox-queue-last-pair mb) (%queue-last-pair (%mailbox-queue mb)))
(define-inline (%mailbox-queue-empty? mb) (%queue-empty? (%mailbox-queue mb)))
(define-inline (%mailbox-queue-count mb) (%queue-count (%mailbox-queue mb)))
(define-inline (%mailbox-queue-add! mb x) (%queue-add! (%mailbox-queue mb) x))
(define-inline (%mailbox-queue-remove! mb) (%queue-remove! (%mailbox-queue mb)))
(define-inline (%mailbox-queue-push-back! mb x) (%queue-push-back! (%mailbox-queue mb) x))
(define-inline (%mailbox-queue-push-back-list! mb ls) (%queue-push-back-list! (%mailbox-queue mb) ls))

;; Waiting threads

(define-inline (%mailbox-waiters-empty? mb) ($null? (%mailbox-waiters mb)))
(define-inline (%mailbox-waiters-count mb) ($length (%mailbox-waiters mb)))

(define-inline (%mailbox-waiters-add! mb th)
  (%mailbox-waiters-set! mb ($append! (%mailbox-waiters mb) ($cons th '()))) )

(define-inline (%mailbox-waiters-delete! mb th)
  (%mailbox-waiters-set! mb ($delq! th (%mailbox-waiters mb))) )

(define-inline (%mailbox-waiters-pop! mb)
  (let ((ts (%mailbox-waiters mb)))
    (%mailbox-waiters-set! mb ($cdr ts))
    ($car ts) ) )

;;; Mailbox Cursor Support

(define-record-type-variant mailbox-cursor (unsafe unchecked inline)
  (%%make-mailbox-cursor np pp mb)
  %mailbox-cursor?
  (np %mailbox-cursor-next-pair %mailbox-cursor-next-pair-set!)
  (pp %mailbox-cursor-prev-pair %mailbox-cursor-prev-pair-set!)
  (mb %mailbox-cursor-mailbox) )

(define-inline (%make-mailbox-cursor mb) (%%make-mailbox-cursor '() #f mb))

(define-error-type mailbox-cursor)
(define-inline-check-type mailbox-cursor)

(define-inline (%mailbox-cursor-winding? mbc) (->boolean (%mailbox-cursor-prev-pair mbc)))
(define-inline (%mailbox-cursor-next-pair-empty! mbc) (%mailbox-cursor-next-pair-set! mbc '()))
(define-inline (%mailbox-cursor-prev-pair-clear! mbc) (%mailbox-cursor-prev-pair-set! mbc #f))

(define-inline (%mailbox-cursor-rewind! mbc)
  (%mailbox-cursor-next-pair-empty! mbc)
  (%mailbox-cursor-prev-pair-clear! mbc) )

(define-inline (%mailbox-cursor-extract! mbc)
  ;Unless 'mailbox-cursor-next' has been called don't remove
  (and-let* ((prev-pair (%mailbox-cursor-prev-pair mbc)))
    (%queue-extract-pair! (%mailbox-queue (%mailbox-cursor-mailbox mbc)) prev-pair) ) )

;; Time Support

(define-inline (%timeout? obj) (or (number? obj) (time? obj)))
(define-error-type timeout)
(define-inline-check-type timeout)

;;;

;Unique objects used as tags
(define UNBLOCKED-TAG (%make-unique-object 'unblocked))
(define SEQ-FAIL-TAG (%make-unique-object 'seq-fail))
(define NO-TOVAL-TAG (%make-unique-object 'timeout-value))
#; ;XXX
(define MESSAGE-WAITING-TAG (%make-unique-object 'message-waiting))

;;; Mailbox Exceptions

(define (make-mailbox-timeout-condition loc timout timout-value)
  (let ((args (if ($eq? timout-value NO-TOVAL-TAG) (list timout)
                  (list timout timout-value))))
    (make-exn-condition+ loc "mailbox wait timeout occured" args 'mailbox 'timeout) ) )

;;; Mailbox Threading

;; Select next waiting thread for the mailbox

(define (ready-mailbox-thread! mb)
  ;Ready oldest waiting thread
  (unless (%mailbox-waiters-empty? mb)
    (let ((thread (%mailbox-waiters-pop! mb)))
      ;Ready the thread based on wait mode
      (if (not ($thread-blocked? thread)) (thread-resume! thread)
          ;else wake early if sleeping
          (when ($thread-blocked-for-timeout? thread)
            ;Ready the thread
            (##sys#thread-unblock! thread)
            ;Tell 'wait-mailbox-thread!' we unblocked early
            (thread-signal! thread UNBLOCKED-TAG) ) ) )
    (void) ) )

;; Sleep current thread until timeout, known condition,
;; or some other condition

(define-inline (thread-sleep/maybe-unblock! tim unblocked-tag)
  ;Sleep current thread for desired seconds, unless unblocked "early".
  (call/cc
    (lambda (return)
      (with-exception-handler
        (lambda (exp)
          (if ($eq? unblocked-tag exp) (return #f)
              ;Propagate any "real" exception.
              (signal exp)))
        (lambda () (thread-sleep! tim) #t)))) )

;; Wait current thread on the mailbox until timeout, available message
;; or some other condition

(define (wait-mailbox-thread! loc mb timout timout-value)
  ;Push current thread on mailbox waiting queue
  (%mailbox-waiters-add! mb ($current-thread))
  ;Waiting action
  (cond
    (timout   ;Timeout wanted so sleep until something happens
      (cond
        ((thread-sleep/maybe-unblock! timout UNBLOCKED-TAG)
          ;Timedout, so no message
          ;Remove from wait queue
          (%mailbox-waiters-delete! mb ($current-thread))
          ;Indicate no available message
          (if (not ($eq? timout-value NO-TOVAL-TAG)) timout-value
            (begin
              (thread-signal!
                ($current-thread)
                (make-mailbox-timeout-condition loc timout timout-value))
              SEQ-FAIL-TAG ) ) )
        (else
          ;Unblocked early
          UNBLOCKED-TAG ) ) )
    (else   ;No timeout so suspend until something delivered
      (thread-suspend! ($current-thread))
      ;We're resumed
      UNBLOCKED-TAG ) ) )

;; Wait current thread on the mailbox unless a message available

;Note that the arguments, except the ?expr0 ..., must be base values.
(define-syntax on-mailbox-available
  (syntax-rules ()
    ((_ ?loc ?mb ?timout ?timout-value ?expr0 ...)
      (let waiting ()
        (cond
          ((%mailbox-queue-empty? ?mb)
            (let ((res (wait-mailbox-thread! ?loc ?mb ?timout ?timout-value)))
              ;When a thread ready then check mailbox again, could be empty.
              (if ($eq? UNBLOCKED-TAG res) (waiting)
                  ;else some sort of problem
                  res ) ) )
          (else
            ?expr0 ... ) ) ) ) ) )

#; ;XXX
(define (wait-mailbox-if-empty! loc mb timout timout-value)
  (on-mailbox-available loc mb timout timout-value
    MESSAGE-WAITING-TAG ) )

;;; Mailbox

;; Mailbox Exceptions

(define mailbox-timeout-condition? (make-condition-predicate exn mailbox timeout))
(define mailbox-timeout-exception? mailbox-timeout-condition?)

;; Mailbox Constructor

(define (make-mailbox #!optional (nm (gensym 'mailbox)))
  (%make-mailbox nm) )

(define (mailbox? obj) (%mailbox? obj))

;; Mailbox Properties

(define (mailbox-name mb)
  (%check-mailbox 'mailbox-name mb)
  (%mailbox-name mb) )

(define (mailbox-empty? mb)
  (%check-mailbox 'mailbox-empty? mb)
  (%mailbox-queue-empty? mb) )

(define (mailbox-count mb)
  (%check-mailbox 'mailbox-count mb)
  (%mailbox-queue-count mb) )

(define (mailbox-waiting? mb)
  (%check-mailbox 'mailbox-waiting? mb)
  (not ($null? (%mailbox-waiters mb))) )

(define (mailbox-waiters mb)
  (%check-mailbox 'mailbox-waiters mb)
  ($list-copy (%mailbox-waiters mb)) )

;; Mailbox Operations

(define (mailbox-send! mb x)
  (%check-mailbox 'mailbox-send! mb)
  (%mailbox-queue-add! mb x)
  (ready-mailbox-thread! mb) )

(define (mailbox-wait! mb #!optional timout)
  (%check-mailbox 'mailbox-wait! mb)
  (when timout (%check-timeout 'mailbox-wait! timout))
  (on-mailbox-available 'mailbox-wait! mb timout NO-TOVAL-TAG
    (void) ) )

(define (mailbox-receive! mb #!optional timout (timout-value NO-TOVAL-TAG))
  (%check-mailbox 'mailbox-receive! mb)
  (when timout (%check-timeout 'mailbox-receive! timout))
  (on-mailbox-available 'mailbox-receive! mb timout timout-value
    (%mailbox-queue-remove! mb) ) )

(define (mailbox-push-back! mb x)
  (%check-mailbox 'mailbox-send! mb)
  (%mailbox-queue-push-back! mb x)
  (ready-mailbox-thread! mb) )

(define (mailbox-push-back-list! mb ls)
  (%check-mailbox 'mailbox-send! mb)
  (%check-list ls 'mailbox-send!)
  (%mailbox-queue-push-back-list! mb ls)
  (ready-mailbox-thread! mb) )

;;; Mailbox Cursor

;; Mailbox Cursor Constructor

(define (make-mailbox-cursor mb)
  (%check-mailbox 'make-mailbox-cursor mb)
  (%make-mailbox-cursor mb) )

;; Mailbox Cursor Properties

(define (mailbox-cursor? obj)
  (%mailbox-cursor? obj) )

(define (mailbox-cursor-mailbox mbc)
  (%check-mailbox-cursor 'mailbox-cursor-mailbox mbc)
  (%mailbox-cursor-mailbox mbc) )

(define (mailbox-cursor-rewound? mbc)
  (%check-mailbox-cursor 'mailbox-cursor-rewound? mbc)
  (not (%mailbox-cursor-winding? mbc)) )

(define (mailbox-cursor-unwound? mbc)
  (%check-mailbox-cursor 'mailbox-cursor-unwound? mbc)
  ($null? (%mailbox-cursor-next-pair mbc)) )

;; Mailbox Cursor Operations

(define (mailbox-cursor-rewind mbc)
  (%check-mailbox-cursor 'mailbox-cursor-rewind mbc)
  (%mailbox-cursor-rewind! mbc) )

#; ;XXX
(define (mailbox-cursor-next mbc #!optional timout (timout-value NO-TOVAL-TAG))
  (%check-mailbox-cursor 'mailbox-cursor-next mbc)
  (when timout (%check-timeout 'mailbox-cursor-next timout))
  ;Waiting mailbox peek.
  (let ((mb (%mailbox-cursor-mailbox mbc)))
    (receive (mailbox-waiter cursor-pair-getter)
               (if (%mailbox-cursor-winding? mbc)
                   ;then unconditionally wait until something added
                   (values wait-mailbox-thread!
                           (lambda () (%mailbox-queue-last-pair mb)))
                   ;else grab the start of a, probably, non-empty queue
                   (values wait-mailbox-if-empty!
                           (lambda () (%mailbox-queue-first-pair mb))))
      (let scanning ()
        (let ((next-pair (%mailbox-cursor-next-pair mbc)))
          ;Anything next?
          (if (not (%null? next-pair))
              ;then peek into the queue for the next item
              (let ((item (%car next-pair)))
                (%mailbox-cursor-prev-pair-set! mbc next-pair)
                (%mailbox-cursor-next-pair-set! mbc (%cdr next-pair))
                item )
              ;else wait for something in the mailbox
              (let ((res (mailbox-waiter 'mailbox-cursor-next mb timout timout-value)))
                (cond
                  ((or ($eq? MESSAGE-WAITING-TAG res) ;so continue scanning
                       ($eq? UNBLOCKED-TAG res))
                    (%mailbox-cursor-next-pair-set! mbc (cursor-pair-getter))
                    (scanning) )
                  (else                               ;otherwise timedout
                    res ) ) ) ) ) ) ) ) )

(define (mailbox-cursor-next mbc #!optional timout (timout-value NO-TOVAL-TAG))
  (%check-mailbox-cursor 'mailbox-cursor-next mbc)
  (when timout (%check-timeout 'mailbox-cursor-next timout))
  (let ((mb (%mailbox-cursor-mailbox mbc)))
    ;Seed rewound cursor
    (unless (%mailbox-cursor-winding? mbc)
      (%mailbox-cursor-next-pair-set! mbc (%mailbox-queue-first-pair mb)) )
    ;Pull next item from queue at cursor
    (let scanning ()
      (let ((curr-pair (%mailbox-cursor-next-pair mbc)))
        ;Anything next?
        (if (not ($null? curr-pair))
            ;then peek into the queue for the next item
            (let ((item ($car curr-pair)))
              (%mailbox-cursor-prev-pair-set! mbc curr-pair)
              (%mailbox-cursor-next-pair-set! mbc ($cdr curr-pair))
              item )
            ;else wait for something in the mailbox
            (let ((res (wait-mailbox-thread! 'mailbox-cursor-next mb timout timout-value)))
              (cond
                (($eq? UNBLOCKED-TAG res) ;so continue scanning
                  (%mailbox-cursor-next-pair-set! mbc (%mailbox-queue-last-pair mb))
                  (scanning) )
                (else                     ;some problem (timeout maybe)
                  res ) ) ) ) ) ) ) )

(define (mailbox-cursor-extract-and-rewind! mbc)
  (%check-mailbox-cursor 'mailbox-cursor-extract-and-rewind! mbc)
  (%mailbox-cursor-extract! mbc)
  (%mailbox-cursor-rewind! mbc) )

;;; Read/Print Syntax

(define-record-printer (mailbox mb out)
  (with-output-to-port out
    (lambda ()
      (display "#<mailbox")
      (display #\space) (display (%mailbox-name mb))
      (display " queued = ") (display (%mailbox-queue-count mb))
      (display " waiters = ") (display (%mailbox-waiters-count mb))
      (display ">") ) ) )

(define-record-printer (mailbox-cursor mbc out)
  (with-output-to-port out
    (lambda ()
      (display "#<mailbox-cursor")
      (display " mailbox = ") (display (%mailbox-name (%mailbox-cursor-mailbox mbc)))
      (display " status = ") (display (if (%mailbox-cursor-winding? mbc) "winding" "rewound"))
      (display ">") ) ) )

) ;module mailbox
