;;;; message-digest-int.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, May '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

;; Issues

(module message-digest-int

  (;export
    message-digest-update-char-u8
    message-digest-update-char
    message-digest-update-char-be
    message-digest-update-char-le
    message-digest-update-u8
    message-digest-update-u16
    message-digest-update-u16-be
    message-digest-update-u16-le
    message-digest-update-u32
    message-digest-update-u32-be
    message-digest-update-u32-le
    message-digest-update-u64
    message-digest-update-u64-be
    message-digest-update-u64-le)

  (import
    scheme
    chicken
    message-digest-type
    message-digest-support
    blob-set-int
    (only type-checks
      check-integer check-char)
    (only type-errors
      error-argument-type))

  (require-library
    message-digest-type
    message-digest-support
    blob-set-int
    type-checks
    type-errors)

;;; Support
;;

(define (get-byte-order loc obj)
  (case obj
  	((big-endian be big msb)				'big-endian )
  	((little-endian le little lsb)	'little-endian )
    (else
    	(error-argument-type loc obj "symbol in {big-endian little-endian}" obj) ) ) )

;;

(define (*message-digest-update-uint loc md n size setter)
  (check-message-digest loc md)
  (check-integer loc n)
  (let ((blb (setup-message-digest-buffer! md size)))
  	(setter blb n 0)
  	(*message-digest-update-blob md blb size) ) )

;;; Char & Integer Update

;; Char

(define (message-digest-update-char-u8 md ch)
  (check-char 'message-digest-update-char-u8 ch)
	(*message-digest-update-uint 'message-digest-update-char-u8 md (char->integer ch) 1 *blob-set-u8!) )

(define (message-digest-update-char-be md ch)
  (check-char 'message-digest-update-char ch)
	(*message-digest-update-uint 'message-digest-update-char-be md (char->integer ch) 4 *blob-set-u32-be!) )

(define (message-digest-update-char-le md ch)
  (check-char 'message-digest-update-char ch)
	(*message-digest-update-uint 'message-digest-update-char-le md (char->integer ch) 4 *blob-set-u32-le!) )

;; Unsigned Integer 8, 16, 32, & 64 bits

(define (message-digest-update-u8 md n)
	(*message-digest-update-uint 'message-digest-update-u8 md n 1 *blob-set-u8!) )

(define (message-digest-update-u16-be md n)
	(*message-digest-update-uint 'message-digest-update-u16-be md n 2 *blob-set-u16-be!) )

(define (message-digest-update-u16-le md n)
	(*message-digest-update-uint 'message-digest-update-u16-le md n 2 *blob-set-u16-le!) )

(define (message-digest-update-u32-be md n)
	(*message-digest-update-uint 'message-digest-update-u32-be md n 4 *blob-set-u32-be!) )

(define (message-digest-update-u32-le md n)
	(*message-digest-update-uint 'message-digest-update-u32-le md n 4 *blob-set-u32-le!) )

(define (message-digest-update-u64-be md n)
	(*message-digest-update-uint 'message-digest-update-u64-be md n 8 *blob-set-u64-be!) )

(define (message-digest-update-u64-le md n)
	(*message-digest-update-uint 'message-digest-update-u64-le md n 8 *blob-set-u64-le!) )

;; Machine Byte Order w/ Char & Unsigned Integer

(define (message-digest-update-char md ch #!optional (order (machine-byte-order)))
	(case (get-byte-order 'message-digest-update-char order)
		((little-endian)	(message-digest-update-char-le md ch) )
		((big-endian)			(message-digest-update-char-be md ch) ) ) )

(define (message-digest-update-u16 md n #!optional (order (machine-byte-order)))
	(case (get-byte-order 'message-digest-update-u16 order)
		((little-endian)	(message-digest-update-u16-le md n) )
		((big-endian)			(message-digest-update-u16-be md n) ) ) )

(define (message-digest-update-u32 md n #!optional (order (machine-byte-order)))
	(case (get-byte-order 'message-digest-update-u32 order)
		((little-endian)	(message-digest-update-u32-le md n) )
		((big-endian)			(message-digest-update-u32-be md n) ) ) )

(define (message-digest-update-u64 md n #!optional (order (machine-byte-order)))
	(case (get-byte-order 'message-digest-update-u64 order)
		((little-endian)	(message-digest-update-u64-le md n) )
		((big-endian)			(message-digest-update-u64-be md n) ) ) )

) ;module message-digest-int
