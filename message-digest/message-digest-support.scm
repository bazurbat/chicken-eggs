;;;; message-digest-support.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, May '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

;; Issues
;;
;; - Uses 'context-info' to determine whether active context is "own" allocation or
;; callers. Again, a kludge.
;;
;; - Passes u8vector to update phase as a blob.

(module message-digest-support

  (;export
  	; Support
  	packed-vector->blob/shared
  	*message-digest-update-blob
  	*message-digest-update-string)

  (import
    scheme
    chicken
    (only lolevel number-of-bytes)
    (only srfi-4
			s8vector?
			u8vector?
			s16vector?
			u16vector?
			s32vector?
			u32vector?
			#;u64vector?
			#;u64vector?
			f32vector?
			f64vector?
			u8vector->blob/shared
			s8vector->blob/shared
			s16vector->blob/shared
			u16vector->blob/shared
			s32vector->blob/shared
			u32vector->blob/shared
			#;s64vector->blob/shared
			#;u64vector->blob/shared
			f32vector->blob/shared
			f64vector->blob/shared)
    message-digest-primitive
		message-digest-type)


  (require-library
    srfi-4
    message-digest-primitive
    message-digest-type)

;;; Support

;;

;Used by update-item & srfi-4 modules
(define (packed-vector->blob/shared obj)
  (cond
    ((u8vector? obj)        (u8vector->blob/shared obj))
    ((s8vector? obj)        (s8vector->blob/shared obj))
    ((u16vector? obj)       (u16vector->blob/shared obj))
    ((s16vector? obj)       (s16vector->blob/shared obj))
    ((u32vector? obj)       (u32vector->blob/shared obj))
    ((s32vector? obj)       (s32vector->blob/shared obj))
    #;((u64vector? obj)       (u64vector->blob/shared obj))
    #;((s64vector? obj)       (s64vector->blob/shared obj))
    ((f32vector? obj)       (f32vector->blob/shared obj))
    ((f64vector? obj)       (f64vector->blob/shared obj))
    (else                   #f ) ) )

;;

(define (*message-digest-update-blob md blb #!optional (siz (blob-size blb)))
  (let ((mdp (message-digest-algorithm md))
        (ctx (message-digest-context md)) )
    ((message-digest-primitive-update mdp) ctx blb siz) ) )

(define (*message-digest-update-string md str)
	(*message-digest-update-blob md (string->blob str)) )

) ;module message-digest-support
