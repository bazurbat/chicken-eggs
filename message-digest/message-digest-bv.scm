;;;; message-digest-bv.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, May '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

;; Issues

(module message-digest-bv

  (;export
    ;
    message-digest-update-blob
    message-digest-update-string
    message-digest-update-substring  ;DEPRECATED
    message-digest-blob
    message-digest-string)

  (import
    scheme
    chicken
    message-digest-primitive
    message-digest-type
    message-digest-support
    (only srfi-13 substring/shared)
    (only type-checks
      check-blob check-string))

  (require-library
  	srfi-13
    message-digest-primitive
    message-digest-type
    message-digest-support
    type-checks)

;;; Message Digest API

;; Update

;;

(define (message-digest-update-blob md blb)
  (check-message-digest 'message-digest-update-blob md)
  (check-blob 'message-digest-update-blob blb)
  (*message-digest-update-blob md blb) )

;;

(define (message-digest-update-string md str)
  (check-message-digest 'message-digest-update-string md)
  (check-string 'message-digest-update-string str)
  (*message-digest-update-string md str) )

;;

;DEPRECATED
(define (message-digest-update-substring md str start end)
  (check-message-digest 'message-digest-update-substring md)
  (check-string 'message-digest-update-substring str)
  (*message-digest-update-string md (substring/shared str start end)) )

;; Single Source API

(define (message-digest-string mdp str #!optional (result-type 'hex))
  (let ((md (initialize-message-digest mdp)))
    (message-digest-update-string md str)
    (finalize-message-digest md result-type) ) )

(define (message-digest-blob mdp blb #!optional (result-type 'hex))
  (let ((md (initialize-message-digest mdp)))
    (message-digest-update-blob md blb)
    (finalize-message-digest md result-type) ) )

) ;module message-digest-bv
