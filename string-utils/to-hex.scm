;;;; to-hex.scm  -*- Hen -*-
;;;; Kon Lovett, Sep '10

(module to-hex

  (;export
    mem_to_hex
    s8vec_to_hex
    u8vec_to_hex
    blob_to_hex
    str_to_hex)

  (import
    scheme
    chicken
    foreign)

;;

#>
static void
bv_to_hex( uint8_t *out, uint8_t *in, int off, int len )
{
  static char digits[] = "0123456789abcdef";
  in += off;
  while (len--) {
    *out++ = digits[ *in >> 4 ];
    *out++ = digits[ *in++ & 0x0f ];
  }
}
<#

(define str_to_hex
  (foreign-lambda void "bv_to_hex" nonnull-scheme-pointer nonnull-scheme-pointer int int))

(define blob_to_hex
  (foreign-lambda void "bv_to_hex" nonnull-scheme-pointer nonnull-blob int int))

(define u8vec_to_hex
  (foreign-lambda void "bv_to_hex" nonnull-scheme-pointer nonnull-u8vector int int))

(define s8vec_to_hex
  (foreign-lambda void "bv_to_hex" nonnull-scheme-pointer nonnull-s8vector int int))

(define mem_to_hex
  (foreign-lambda void "bv_to_hex" nonnull-scheme-pointer nonnull-c-pointer int int))

) ;module to-hex
