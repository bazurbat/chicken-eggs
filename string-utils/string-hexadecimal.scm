;;;; string-hexadecimal.scm  -*- Hen -*-
;;;; Kon Lovett, Aug '10

(module string-hexadecimal

  (;export
    string->hex)

  (import
    scheme
    chicken
    (only lolevel number-of-bytes)
    (only to-hex str_to_hex)
    (only type-checks check-natural-fixnum check-string))

  (require-library lolevel to-hex type-checks)

  (declare
    (bound-to-procedure
      ##sys#signal-hook))

;;

#;
(define (*bytevector->hex tohex bv start end)
  (let ((len (fx- end start)))
		(if (fx= 0 len)
			""
			(let ((res (make-string (fx* len 2))))
				(tohex res bv start len)
				res ) ) ) )

;;

(define (string->hex str #!optional (start 0) (end #f))
  (check-string 'string->hex str)
  (check-natural-fixnum 'string->hex start 'start)
  (when end (check-natural-fixnum 'string->hex end 'end))
  (let ((end (or end (number-of-bytes str))))
    (unless (<= start end)
      (##sys#signal-hook #:bounds-error 'string->hex
                         "illegal subvector specification" start end))
    (let ((len (fx- end start)))
      (if (fx= 0 len)
        ""
        (let ((res (make-string (fx* len 2))))
          (str_to_hex res str start len)
          res ) ) ) ) )

) ;module string-hexadecimal
