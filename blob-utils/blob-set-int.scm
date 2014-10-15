;;;; blob-set-int.scm
;;;; Kon Lovett, Apr '12

;; Issues
;;
;; - Chicken uses "signed integer" but treated here as "unsigned integer"

(module blob-set-int

  (;export
    blob-set-u8!
    blob-set-u16-le! blob-set-u32-le! blob-set-u64-le!
    blob-set-u16-be! blob-set-u32-be! blob-set-u64-be!
    ;
    *blob-set-u8!
    *blob-set-u16-le! *blob-set-u32-le! *blob-set-u64-le!
    *blob-set-u16-be! *blob-set-u32-be! *blob-set-u64-be!)

  (import
    scheme
    chicken
    foreign
    (only type-checks check-natural-fixnum check-blob check-fixnum check-integer))

  (require-library
    type-checks)

  (declare
    (type
      (*blob-set-u8! ((or blob string) number fixnum -> undefined))
      (*blob-set-u16-le! ((or blob string) number fixnum -> undefined))
      (*blob-set-u32-le! ((or blob string) number fixnum -> undefined))
      (*blob-set-u64-le! ((or blob string) number fixnum -> undefined))
      (*blob-set-u16-be! ((or blob string) number fixnum -> undefined))
      (*blob-set-u32-be! ((or blob string) number fixnum -> undefined))
      (*blob-set-u64-be! ((or blob string) number fixnum -> undefined))

      (blob-set-u8! (blob fixnum #!optional fixnum -> undefined))
      (blob-set-u16-le! (blob fixnum #!optional fixnum -> undefined))
      (blob-set-u32-le! (blob number #!optional fixnum -> undefined))
      (blob-set-u64-le! (blob number #!optional fixnum -> undefined))
      (blob-set-u16-be! (blob fixnum #!optional fixnum -> undefined))
      (blob-set-u32-be! (blob number #!optional fixnum -> undefined))
      (blob-set-u64-be! (blob number #!optional fixnum -> undefined)) ) )

;;; Only Blob Bytevector, No Argument Checking

(define *blob-set-u8!
  (foreign-lambda* void ((nonnull-scheme-pointer bv) (unsigned-integer32 u32) (int off))
#<<EOS
    ((uint8_t *)bv)[off] = (uint8_t)(u32 & 0xff);
EOS
	))

(define *blob-set-u16-le!
  (foreign-lambda* void ((nonnull-scheme-pointer bv) (unsigned-integer32 u32) (int off))
#<<EOS
    ((uint8_t *)bv)[off]   = (uint8_t)(u32 & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u32 >> 8) & 0xff);
EOS
	))

(define *blob-set-u16-be!
  (foreign-lambda* void ((nonnull-scheme-pointer bv) (unsigned-integer32 u32) (int off))
#<<EOS
    ((uint8_t *)bv)[off]   = (uint8_t)((u32 >> 8) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)(u32 & 0xff);
EOS
	))

(define *blob-set-u32-le!
  (foreign-lambda* void ((nonnull-scheme-pointer bv) (unsigned-integer32 u32) (int off))
#<<EOS
    ((uint8_t *)bv)[off]   = (uint8_t)(u32 & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u32 >> 8) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u32 >> 16) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u32 >> 24) & 0xff);
EOS
	))

(define *blob-set-u32-be!
  (foreign-lambda* void ((nonnull-scheme-pointer bv) (unsigned-integer32 u32) (int off))
#<<EOS
    ((uint8_t *)bv)[off]   = (uint8_t)((u32 >> 24) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u32 >> 16) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u32 >> 8) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)(u32 & 0xff);
EOS
	))

(define *blob-set-u64-le!
  (foreign-lambda* void ((nonnull-scheme-pointer bv) (unsigned-integer64 u64) (int off))
#<<EOS
    ((uint8_t *)bv)[off]   = (uint8_t)(u64 & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 8) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 16) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 24) & 0xff);
		((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 32) & 0xff);
		((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 40) & 0xff);
		((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 48) & 0xff);
		((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 56) & 0xff);
EOS
	))

(define *blob-set-u64-be!
  (foreign-lambda* void ((nonnull-scheme-pointer bv) (unsigned-integer64 u64) (int off))
#<<EOS
    ((uint8_t *)bv)[off]   = (uint8_t)((u64 >> 56) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 48) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 40) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 32) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 24) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 16) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)((u64 >> 8) & 0xff);
    ((uint8_t *)bv)[++off] = (uint8_t)(u64 & 0xff);
EOS
	))

;;; Only Blob Bytevector

;; 8

(define (blob-set-u8! blb uint #!optional (off 0))
	(check-blob 'blob-set-u8! blb)
	(check-natural-fixnum 'blob-set-u8! off 'offset)
	(check-fixnum 'blob-set-u8! uint)
  (*blob-set-u8! blb uint off) )

;; Little Endian 16, 32, & 64

(define (blob-set-u16-le! blb uint #!optional (off 0))
	(check-blob 'blob-set-u16-le! blb)
	(check-natural-fixnum 'blob-set-u16-le! off 'offset)
	(check-fixnum 'blob-set-u16-le! uint)
  (*blob-set-u16-le! blb uint off) )

(define (blob-set-u32-le! blb uint #!optional (off 0))
	(check-blob 'blob-set-u32-le! blb)
	(check-natural-fixnum 'blob-set-u32-le! off 'offset)
	(check-integer 'blob-set-u32-le! uint)
  (*blob-set-u32-le! blb uint off) )

(define (blob-set-u64-le! blb uint #!optional (off 0))
	(check-blob 'blob-set-u64-le! blb)
	(check-natural-fixnum 'blob-set-u64-le! off 'offset)
	(check-integer 'blob-set-u64-le! uint)
  (*blob-set-u64-le! blb uint off) )

;; Big Endian 16, 32, & 64

(define (blob-set-u16-be! blb uint #!optional (off 0))
	(check-blob 'blob-set-u16-be! blb)
	(check-natural-fixnum 'blob-set-u16-be! off 'offset)
	(check-fixnum 'blob-set-u16-be! uint)
  (*blob-set-u16-be! blb uint off) )

(define (blob-set-u32-be! blb uint #!optional (off 0))
	(check-blob 'blob-set-u32-be! blb)
	(check-natural-fixnum 'blob-set-u32-be! off 'offset)
	(check-integer 'blob-set-u32-be! uint)
  (*blob-set-u32-be! blb uint off) )

(define (blob-set-u64-be! blb uint #!optional (off 0))
	(check-blob 'blob-set-u64-be! blb)
	(check-natural-fixnum 'blob-set-u64-be! off 'offset)
	(check-integer 'blob-set-u64-be! uint)
  (*blob-set-u64-be! blb uint off) )

#| ;Useful API?
;;; Blob, String, & U8Vector Bytevector

;;

(define (get-bv-alias loc obj)
  (cond
    ((blob? obj)			obj )
    ((string? obj)		obj )
    ((u8vector? obj)	(u8vector->blob/shared obj) )
    (else
    	(error-argument-type loc obj "blob, u8vector, or string" obj) ) ) )

#; ;Too Many options
(define (get-byte-order loc obj)
  (case obj
  	((big-endian be big msb)				'big-endian )
  	((little-endian le little lsb)	'little-endian )
    (else
    	(error-argument-type loc obj "symbol in {big-endian be big msb little-endian le little lsb}" obj) ) ) )

;; 8

(define (set-u8! bv uint idx)
	(blob-set-u8! (get-bv-alias 'set-u8! bv) uint idx) )

;; Little Endian 16, 32, & 64

(define (set-u16-le! bv uint #!optional (idx 0))
  (blob-set-u16-le! (get-bv-alias 'set-u16-le! bv) uint idx) )

(define (set-u32-le! bv uint #!optional (idx 0))
  (blob-set-u32-le! (get-bv-alias 'set-u32-le! bv) uint idx) )

(define (set-u64-le! bv uint #!optional (idx 0))
  (blob-set-u64-le! (get-bv-alias 'set-u64-le! bv) uint idx) )

;; Big Endian 16, 32, & 64

(define (set-u16-be! bv uint #!optional (idx 0))
  (blob-set-u16-be! (get-bv-alias 'set-u16-be! bv) uint idx) )

(define (set-u32-be! bv uint #!optional (idx 0))
  (blob-set-u32-be! (get-bv-alias 'set-u32-be! bv) uint idx) )

(define (set-u64-be! bv uint #!optional (idx 0))
  (blob-set-u64-be! (get-bv-alias 'set-u64-be! bv) uint idx) )

;; Both Endian 16, 32, & 64

(define (set-u16! bv uint #!optional (idx 0) (order (machine-byte-order)))
	(let ((bv (get-bv-alias 'set-u16! bv)))
		(case (get-byte-order 'set-u16! order)
			((little-endian)	(blob-set-u16-le! bv uint idx) )
			((big-endian)			(blob-set-u16-be! bv uint idx) ) ) ) )

(define (set-u32! bv uint #!optional (idx 0) (order (machine-byte-order)))
	(let ((bv (get-bv-alias 'set-u32! bv)))
		(case (get-byte-order 'set-u32! order)
			((little-endian)	(blob-set-u32-le! bv uint idx) )
			((big-endian)			(blob-set-u32-be! bv uint idx) ) ) ) )

(define (set-u64! bv uint #!optional (idx 0) (order (machine-byte-order)))
	(let ((bv (get-bv-alias 'set-u64! bv)))
		(case (get-byte-order 'set-u64! order)
			((little-endian)	(blob-set-u64-le! bv uint idx) )
			((big-endian)			(blob-set-u64-be! bv uint idx) ) ) ) )
|#

) ;module blob-set-int
