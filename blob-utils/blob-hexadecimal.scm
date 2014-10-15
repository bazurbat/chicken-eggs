;;;; blob-hexadecimal.scm  -*- Hen -*-
;;;; Kon Lovett, Apr '11

(module blob-hexadecimal

  (;export
    blob->hex)

  (import
    scheme
    chicken
    (only to-hex blob_to_hex)
    (only type-checks check-natural-fixnum check-blob))

  (require-library to-hex type-checks)

  (declare
    (bound-to-procedure
      ##sys#signal-hook))

  (declare
    (type
      (blob->hex (blob #!optional fixnum (or fixnum boolean) -> string)) ) )

;;

(define (blob->hex blb #!optional (start 0) (end #f))
  (check-blob 'blob->hex blb)
  (check-natural-fixnum 'blob->hex start 'start)
  (when end (check-natural-fixnum 'blob->hex end 'end))
  (let ((end (or end (blob-size blb))))
    (unless (<= start end)
      (##sys#signal-hook #:bounds-error 'blob->hex
                         "illegal subvector specification" start end))
    (let ((len (fx- end start)))
      (if (fx= 0 len) ""
        (let ((res (make-string (fx* len 2))))
          (blob_to_hex res blb start len)
          res ) ) ) ) )

) ;module blob-hexadecimal
