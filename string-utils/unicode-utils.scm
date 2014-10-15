;;;; unicode-utils.scm  -*- Hen -*-
;;;; Kon Lovett, Aug '10

;; Issues
;;
;; - Implies Unicode support that is not present.

(module unicode-utils

  (;export
    ascii-codepoint?
    unicode-char->string
    unicode-string
    unicode-make-string
    unicode-surrogate?
    unicode-surrogates->codepoint)

  (import
    scheme
    chicken
    (only data-structures reverse-string-append)
    (only type-checks
      check-natural-fixnum check-char))

  (require-library
  	srfi-13
  	type-checks)

  (declare
    (bound-to-procedure
      ##sys#string-append
      ##sys#char->utf8-string
      ##sys#unicode-surrogate?
      ##sys#surrogates->codepoint))

;; SImple UTF 8

(define (ascii-codepoint? ch)
  (let ((x (char->integer (check-char 'ascii-codepoint? ch))))
    (and (fx<= 0 x) (fx<= x #x7F)) ) )

(define (unicode-char->string ch)
  (##sys#char->utf8-string (check-char 'unicode-char->string ch)) )

;inefficient
(define (unicode-string . chs)
  (cond
    ((null? chs)        "")
    ((null? (cdr chs))  (unicode-char->string (car chs)))
    (else
      (let loop ((chs chs) (ls '()))
        (if (null? chs)
            (reverse-string-append ls)
          	(loop (cdr chs) (cons (unicode-char->string (car chs)) ls) ) ) ) ) ) )

;inefficient
(define (unicode-make-string len #!optional (fill #\space))
  (check-natural-fixnum 'unicode-make-string len)
  (check-char 'unicode-make-string fill)
  (cond
    ((fx= 0 len)              "")
    ((ascii-codepoint? fill)  (##sys#make-string len fill))
    (else
      (let ((fill (##sys#char->utf8-string fill)))
        (let loop ((len len) (ls '()))
          (if (fx= 0 len)
              (reverse-string-append ls)
              (loop (fx- len 1) (cons fill ls)) ) ) ) ) ) )

(define (unicode-surrogate? n)
  (##sys#unicode-surrogate? (check-natural-fixnum 'unicode-surrogate? n)) )

(define (unicode-surrogates->codepoint hi lo)
  (##sys#surrogates->codepoint
    (check-natural-fixnum 'unicode-surrogates->codepoint hi "high")
    (check-natural-fixnum 'unicode-surrogates->codepoint lo "low")) )


) ;module unicode-utils
