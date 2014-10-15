(module blowfish
  (blowfish-init make-blowfish-encryptor make-blowfish-decryptor)
  (import scheme chicken foreign)

#>
#include "blowfish/blowfish.c"
#define BLOWFISH_CTX_SIZE sizeof(BLOWFISH_CTX)
typedef uint32_t block_type;
<#

(define BLOWFISH-CTX-SIZE (foreign-value "BLOWFISH_CTX_SIZE" int))

(define c-blowfish-init
  (foreign-lambda* void ((scheme-pointer ctx) (scheme-pointer key) (int len))
  "Blowfish_Init(ctx,key,len);"))

(define c-blowfish-encrypt
        (foreign-lambda* void ((scheme-pointer ctx) (scheme-pointer input) (scheme-pointer result) (int offset))
  "block_type* in;
  block_type* out;
  in = (block_type*) ((char*)input + offset);
  out = (block_type*) ((char*)result + offset);
  out[0] = in[0];
  out[1] = in[1];
  Blowfish_Encrypt(ctx,&out[0],&out[1]);"))

(define c-blowfish-decrypt
        (foreign-lambda* void ((scheme-pointer ctx) (scheme-pointer input) (scheme-pointer result) (int offset))
  "block_type* in;
  block_type* out;
  in = (block_type*) ((char*)input + offset);
  out = (block_type*) ((char*)result + offset);
  out[0] = in[0];
  out[1] = in[1];
  Blowfish_Decrypt(ctx,&out[0],&out[1]);"))


(define (blowfish-init key len)
    (or (and (<= 4 len) (<= len 56))
        (error "blowfish invalid key length" len))
  (let ((ctx (make-blob BLOWFISH-CTX-SIZE)))
    (c-blowfish-init ctx key len)
    ctx))

(define (make-blowfish-encrypt-decrypt key func)
  (let ((ctx (blowfish-init key (blob-size key))))
                (lambda (input #!optional (size (blob-size input)))
                  (let loop ((i 0)
                             (e size)
                             (result (make-blob size)))
                    (when (< i e)
                      (or
                        (<= 8 (- e i))
                        (error "blowfish invalid encryptor/decryptor block size" size))
                      (func ctx input result i)
                      (loop (+ i 8) e result))
                    result))))

(define (make-blowfish-encryptor key)
  (make-blowfish-encrypt-decrypt key c-blowfish-encrypt))

(define (make-blowfish-decryptor key)
  (make-blowfish-encrypt-decrypt key c-blowfish-decrypt))

);module blowfish
