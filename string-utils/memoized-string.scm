;;;; memoized-string.scm  -*- Hen -*-
;;;; Kon Lovett, Aug '10

(module memoized-string

  (;export
    make-string*)

  (import
    scheme
    chicken
    (only lookup-table
      make-dict dict-ref dict-set! dict-update-dict!)
    (only miscmacros
      if*)
    (only unicode-utils
      ascii-codepoint? unicode-make-string)
    (only type-checks
      check-natural-fixnum check-char))

  (require-library
    miscmacros lookup-table type-checks
    unicode-utils)

  (declare
    (bound-to-procedure
      ##sys#make-string))

;; Memeoized `make-string'

(define make-string*
  (let ((+strings+ (make-dict eqv?)))
    (lambda (len #!optional (ch #\space))
      (check-natural-fixnum 'make-string* len)
      (check-char 'make-string* ch)
      (let loop ((len len) (ch ch))
        (if* (dict-ref +strings+ ch)
          (or (dict-ref it len)
              (let ((str (if (ascii-codepoint? ch) (##sys#make-string len ch)
                           (unicode-make-string len ch) ) ) )
                (dict-set! it len str)
                ; dict `it' already member of +strings+
                str ) )
          (begin
            (dict-update-dict! +strings+ ch)
            (loop len ch) ) ) ) ) ) )

) ;module memoized-string
