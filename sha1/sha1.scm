;;
;; sha1 - Scheme wrapper for Steve Reid's Public Domain sha1 implementation
;;
;; All code in this egg is in the Public Domain

(module sha1
  (sha1-primitive)

	(import scheme chicken foreign)
	(use message-digest-primitive)

(foreign-declare "#include \"sha1-base.c\"")

(define-constant *digest-length* 20)

(define *context-size* (foreign-value "sizeof(SHA1_CTX)" unsigned-int))

(define init (foreign-lambda void SHA1Init c-pointer))
(define update (foreign-lambda void SHA1Update c-pointer scheme-pointer unsigned-int))
(define final (foreign-lambda void SHA1Final c-pointer scheme-pointer))

(define sha1-primitive
  (let ((the-sha1-primitive #f))
    (lambda ()
      (unless the-sha1-primitive
        (set! the-sha1-primitive
              (make-message-digest-primitive *context-size* *digest-length*
                                             init update final 'sha1-primitive)) )
      the-sha1-primitive ) ) )
)
