;;;; message-digest-parameters.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, May '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

;; Issues
;;
;; - Uses 'context-info' to determine whether active context is "own" allocation or
;; callers. Again, a kludge.
;;
;; - Passes u8vector to update phase as a blob.

(module message-digest-parameters

  (;export
    ; Parameters
    message-digest-chunk-size
    message-digest-chunk-read-maker
    message-digest-chunk-converter)

  (import
    scheme
    chicken
    (only srfi-4
    	u8vector->blob/shared subu8vector
    	read-u8vector! make-u8vector)
    variable-item
    (only type-checks
			check-procedure check-positive-fixnum )
    (only type-errors
      error-argument-type))

  (require-library
  	srfi-4
    variable-item
    type-errors
    type-checks)

;;; Update Phase Helpers

;;

(define (default-chunk-read-maker in #!optional (size (message-digest-chunk-size)))
  (let ((u8buf (make-u8vector size)))
    (lambda ()
      (let ((len (read-u8vector! size u8buf in)))
        (and (positive? len)
             (let ((u8buf (if (fx= len size) u8buf (subu8vector u8buf 0 len))))
               (u8vector->blob/shared u8buf) ) ) ) ) ) )

;;

(define-constant DEFAULT-CHUNK-SIZE 1024)

;;; Message Digest "Parameters"

;;

(define-checked-variable message-digest-chunk-size
  DEFAULT-CHUNK-SIZE
  positive-fixnum)

;;

(define-checked-variable message-digest-chunk-read-maker
  default-chunk-read-maker
  procedure)

;;

(define-variable message-digest-chunk-converter #f
  (lambda (obj)
    (if (or (not obj) (procedure? obj)) obj
      (error-argument-type 'message-digest-chunk-converter obj "#f or procedure"))))

) ;module message-digest-parameters
