;;;; pack-integer.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, May '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

;; Issues

(module pack-integer

  (;export
		pack-u8 pack-u16 pack-u32 pack-u64 pack-integer)

  (import
    scheme
    chicken
    foreign
    (only type-checks check-blob check-integer)
    (only type-errors error-argument-type))

  (require-library
    type-checks type-errors)

;;; Integer Packing Utilities

;;

#>
/* start is not a general offset. bytes length <= size */
static void
pack_uint64( uint8_t *bytes, uint64_t n, int size, int direction, int start )
{
  int end;

  if (size == 1) {                        /* 1 byte */

    bytes[start] = n;

  } else if (direction == -1) { /* Big endian */

    end = start;

    bytes[start += size - 1] = n & 0xff;  /* 2 bytes */
    bytes[--start] = (n >> 8) & 0xff;

    if (start != end) {                   /* 4 bytes */

      bytes[--start] = (n >> 16) & 0xff;
      bytes[--start] = (n >> 24) & 0xff;

      if (start != end) {                 /* 8 bytes */

        bytes[--start] = (n >> 32) & 0xff;
        bytes[--start] = (n >> 40) & 0xff;
        bytes[--start] = (n >> 48) & 0xff;
        bytes[--start] = (n >> 56) & 0xff;
      }
    }

  } else {                      /* Little endian */

    end = start + size - 1;

    bytes[start] = n & 0xff;              /* 2 bytes */
    bytes[++start] = (n >> 8) & 0xff;

    if (start != end) {                   /* 4 bytes */

      bytes[++start] = (n >> 16) & 0xff;
      bytes[++start] = (n >> 24) & 0xff;

      if (start != end) {                 /* 8 bytes */

        bytes[++start] = (n >> 32) & 0xff;
        bytes[++start] = (n >> 40) & 0xff;
        bytes[++start] = (n >> 48) & 0xff;
        bytes[++start] = (n >> 56) & 0xff;
      }
    }
  }
}
<#

;;

; All the below primitive pack routines must return the supplied buffer object.

;; Pack an 8 bit integer

(define-inline (pack-u8/u8vector! u8vec n i)
  (u8vector-set! u8vec i n)
  u8vec )

(define-inline (pack-u8/bytevector! bv n i)
  (##core#inline "C_setbyte" bv i n) ;(bytevector-set! bv i n)
  bv )

(define-inline (pack-u8/blob! blb n i)
  (pack-u8/bytevector! blb n i) )

(define-inline (pack-u8/string! str n i)
  (pack-u8/bytevector! str n i) )

; Pack a 16, 32, or 64 bit integer with endian order

(define-inline (pack-u64/u8vector! u8vec n size direction start)
  ((foreign-lambda void "pack_uint64" nonnull-u8vector unsigned-integer64 int int int)
     u8vec n size direction start)
  u8vec )

(define-inline (pack-u64/bytevector! bv n size direction start)
  ((foreign-lambda void "pack_uint64" nonnull-scheme-pointer unsigned-integer64 int int int)
    bv n size direction start)
  bv )

(define-inline (pack-u64/blob! blb n size direction start)
  (pack-u64/bytevector! blb n size direction start) )

(define-inline (pack-u64/string! str n size direction start)
  (pack-u64/bytevector! str n size direction start) )

;;

(define (byte-order? obj)
  (and (memq obj '(big-endian be big little-endian le little))
       #t) )

(define-check+error-type byte-order byte-order? "symbol in {big-endian be big little-endian le little}")

#; ;UNUSED
(define (direction->byte-order n)
  (if (negative? n) 'big-endian
    'little-endian ) )

(define-inline (byte-order->direction order)
  (case order
    ((big-endian be big)        -1 )
    ((little-endian le little)  1 ) ) )

(define-error-type byte-buffer "u8vector, blob, string or symbol in {u8vector blob string}" )

(define-inline (check-byte-size loc obj)
  (unless (memq obj '(1 2 4 8))
    (error-argument-type loc obj "integer in {1 2 4 8}" 'size) )
  obj )

(define-constant MAX-BV-LEN 16777215) ; 2^24-1 is the maximum length of a bytevector

(define-inline (check-byte-buffer-size loc dessiz actsiz)
  (unless (fx<= dessiz actsiz)
    ;FIXME this message is too strong
    (error-half-closed-interval loc actsiz dessiz MAX-BV-LEN "byte-buffer size+start") )
  actsiz )

(define (ensure-byte-buffer loc size bufsel start)
  (let ((need-size (fx+ start size)))
    ; Cases ordered by a guess of probability
    (cond
      ((symbol? bufsel)
        (case bufsel
          ((string)     (values 'string (make-string need-size)) )
          ((blob)       (values 'blob (make-blob need-size)) )
          ((u8vector)   (values 'u8vector (make-u8vector need-size)) )
          (else
            (error-byte-buffer loc bufsel) ) ) )
      ((string? bufsel)
        (check-byte-buffer-size loc need-size (number-of-bytes bufsel))
        (values 'string bufsel) )
      ((blob? bufsel)
        (check-byte-buffer-size loc need-size (number-of-bytes bufsel))
        (values 'blob bufsel) )
      ((u8vector? bufsel)
        (check-byte-buffer-size loc need-size (u8vector-length bufsel))
        (values 'u8vector bufsel) )
      (else
        (error-byte-buffer loc bufsel) ) ) ) )

;;

(define (*pack-u8 loc n bufsel start)
  (check-integer loc n)
  (let-values (((typ obj) (ensure-byte-buffer loc 1 bufsel start)))
    (case typ
      ((string)   (pack-u8/string! obj n start) )
      ((blob)     (pack-u8/blob! obj n start) )
      ((u8vector) (pack-u8/u8vector! obj n start) ) )
    obj ) )

(define (pack-u8 n #!key (bufsel 'string) (start 0))
  (*pack-u8 'pack-u8 n bufsel start) )

;;

(define (*pack-integer loc n bufsel size order start)
  (check-integer loc n)
  (check-byte-order loc order)
  (let-values (((typ obj) (ensure-byte-buffer loc size bufsel start)))
    (let ((direction (byte-order->direction order)))
      (case typ
        ((string)   (pack-u64/string! obj n size direction start) )
        ((blob)     (pack-u64/blob! obj n size direction start) )
        ((u8vector) (pack-u64/u8vector! obj n size direction start) ) ) )
    obj ) )

;;

(define (pack-u16 n #!key (bufsel 'string) (start 0) (order (machine-byte-order)))
  (*pack-integer 'pack-u16 n bufsel 2 order start) )

;;

(define (pack-u32 n #!key (bufsel 'string) (start 0) (order (machine-byte-order)))
  (*pack-integer 'pack-u32 n bufsel 4 order start) )

;;

(define (pack-u64 n #!key (bufsel 'string) (start 0) (order (machine-byte-order)))
  (*pack-integer 'pack-u64 n bufsel 8 order start) )

;;

(define (pack-integer n #!key (bufsel 'string) (start 0) (order (machine-byte-order)) (size 4))
  (check-byte-size 'pack-integer size)
  (if (fx= 1 size) (*blob-set-u8! 'pack-integer n bufsel start)
    (*pack-integer 'pack-integer n bufsel size order start) ) )

) ;module pack-integer
