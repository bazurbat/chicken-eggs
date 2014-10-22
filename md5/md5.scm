;;
;; md5 - Scheme wrapper for Colin Plumb's Public Domain md5 implementation
;;
;; All code in this egg is in the Public Domain
(module md5
  (md5-primitive md5-init md5-update md5-final)

(import chicken scheme foreign)
(use message-digest-primitive)

#>#include "md5-base.c"<#
;#>#include "md5-ssl.c"<# ;an alternative coding

(define digest-length (foreign-value "MD5_DIGEST_SIZE" unsigned-int))
(define context-size (foreign-value "sizeof(struct MD5Context)" unsigned-int))

(define init (foreign-lambda void MD5Init c-pointer))
(define update (foreign-lambda void MD5Update c-pointer scheme-pointer unsigned-int))
(define final (foreign-lambda void MD5Final c-pointer scheme-pointer))

(define (md5-init)
  (let ((ctx (make-blob context-size)))
    ((foreign-lambda void "MD5Init" scheme-pointer) ctx)
    ctx))

(define md5-update
  (foreign-lambda void "MD5Update" scheme-pointer scheme-pointer unsigned-int))

(define (md5-final ctx)
  (let ((digest (make-blob digest-length)))
    ((foreign-lambda void "MD5Final" scheme-pointer scheme-pointer) ctx digest)
    digest))

(define md5-primitive
  (let ((the-md5-primitive #f))
    (lambda ()
      (unless the-md5-primitive
        (set! the-md5-primitive
              (make-message-digest-primitive context-size digest-length
                                             init update final 'md5-primitive)) )
      the-md5-primitive ) ) )
)
