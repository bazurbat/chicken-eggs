;;;; message-digest-primitive.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, May '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

;; Issues

(module message-digest-primitive

  (;export
    ; Algorithm API
    make-message-digest-primitive
    message-digest-primitive? check-message-digest-primitive error-message-digest-primitive
    message-digest-primitive-name
    message-digest-primitive-context-info
    message-digest-primitive-digest-length
    message-digest-primitive-init
    message-digest-primitive-update
    message-digest-primitive-final)

  (import
    scheme
    chicken
    (only type-checks
      define-check+error-type
      check-positive-fixnum
      check-procedure)
    (only type-errors
      error-argument-type))

  (require-library
    type-checks
    type-errors)

;;; Support

;;

(define (positive-fixnum? obj)
  (and (fixnum? obj) (positive? obj)) )

;;; Message Digest Algorithm API

;;

(define (check-message-digest-arguments loc ctx-info digest-len init update final)
  (unless (or (procedure? ctx-info) (positive-fixnum? ctx-info))
    (error-argument-type loc ctx-info "positive-fixnum or procedure" 'context-info) )
  (check-positive-fixnum loc digest-len 'digest-length)
  (check-procedure loc init 'digest-initializer)
  (check-procedure loc update 'digest-updater)
  (check-procedure loc final 'digest-finalizer) )

;;

(define-record-type message-digest-primitive
  (*make-message-digest-primitive ctxi digest-len init update final name)
  message-digest-primitive?
  (ctxi message-digest-primitive-context-info)
  (digest-len message-digest-primitive-digest-length)
  (init message-digest-primitive-init)
  (update message-digest-primitive-update)
  (final message-digest-primitive-final)
  (name message-digest-primitive-name) )

(define-check+error-type message-digest-primitive)

(define (make-message-digest-primitive ctx-info digest-len init update final
                                       #!optional (name (gensym "mdp")))
  (check-message-digest-arguments 'make-message-digest-primitive
    ctx-info digest-len init update final)
  (*make-message-digest-primitive
    ctx-info
    digest-len
    init update final
    name) )

) ;module message-digest-primitive
