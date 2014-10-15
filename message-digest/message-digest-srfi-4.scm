;;;; message-digest-srfi-4.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, May '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

;; Issues
;;
;; - Passes u8vector to update phase as a blob.

(module message-digest-srfi-4

  (;export
    message-digest-update-bytevector			;DEPRECATED
    message-digest-update-u8vector
    message-digest-update-subu8vector			;DEPRECATED
    message-digest-update-packed-vector		;DEPRECATED
    message-digest-u8vector)

  (import
    scheme
    chicken
    data-structures
    srfi-4
    (only lolevel number-of-bytes)
    message-digest-primitive
    message-digest-type
    message-digest-support
    message-digest-bv
    (only type-checks check-u8vector)
    (only type-errors error-argument-type))

  (require-library
    data-structures
    srfi-4
    lolevel
    message-digest-primitive
    message-digest-type
    message-digest-support
    message-digest-bv
    type-errors)

;;; Support

;;

(define (get-bytevector-object loc obj)
	(cond
		((string? obj)										(string->blob obj) )
		((blob? obj)											obj )
		((packed-vector->blob/shared obj) )
		(else
    	(error-argument-type loc obj "string, blob, or SRFI 4 vector" obj) ) ) )

;;; Update API

;;

(define (message-digest-update-u8vector md u8vec)
  (message-digest-update-blob md (u8vector->blob/shared u8vec)) )

;;

;DEPRECATED
(define (message-digest-update-subu8vector md u8vec start end)
  (message-digest-update-blob md (u8vector->blob/shared (subu8vector u8vec start end))) )

;;

;DEPRECATED
(define (message-digest-update-packed-vector md pkdvec)
  (let ((blb (packed-vector->blob/shared pkdvec)))
    (if blb (message-digest-update-blob md blb)
        (error-argument-type 'message-digest-update-packed-vector pkdvec "SRFI 4 vector") ) ) )

;;

;DEPRECATED
(define (message-digest-update-bytevector md bv #!optional (len (number-of-bytes bv)))
  (check-message-digest 'message-digest-update-bytevector md)
  (let ((mdp (message-digest-algorithm md))
        (ctx (message-digest-context md)) )
    ((message-digest-primitive-update mdp)
    	ctx
    	(get-bytevector-object 'message-digest-update-bytevector bv)
    	len) ) )

;;; Single Source API

(define (message-digest-u8vector mdp u8vec #!optional (result-type 'hex))
  (let ((md (initialize-message-digest mdp)))
    (message-digest-update-u8vector md u8vec)
    (finalize-message-digest md result-type) ) )

) ;module message-digest-srfi-4
