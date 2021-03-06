;;;; message-digest-update-item.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, May '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

;; Issues

(module message-digest-update-item

  (;export
    message-digest-update-object
    message-digest-update-procedure
    message-digest-update-port
    message-digest-update-file)

  (import
    scheme
    chicken
    (only lolevel number-of-bytes)
    (only miscmacros while*)
    message-digest-primitive
    message-digest-type
    message-digest-parameters
    message-digest-support
    type-checks
    type-errors)

  (require-library
  	lolevel
    miscmacros
    message-digest-primitive
    message-digest-type
    message-digest-parameters
    message-digest-support
  	type-checks)

;;; Support

;;

(define (chunk-convert obj)
  (and-let* ((cnv (message-digest-chunk-converter))) (cnv obj)) )

(define (get-chunk-reader in)
	((message-digest-chunk-read-maker) in) )

(define (get-update md)
  (message-digest-primitive-update (message-digest-algorithm md)) )

;;

(define (object->bytevector-like obj)
  (or (packed-vector->blob/shared obj)
      (chunk-convert obj)) )

(define (do-byte-source-update loc ctx src updt)
  (cond
    ; simple bytes
    ((blob? src)
    	(updt ctx src (number-of-bytes src)) )
    ((string? src)
    	(do-byte-source-update loc ctx (string->blob src) updt) )
    ; more complicated bytes
    ((object->bytevector-like src) =>
    	(cut do-byte-source-update loc ctx <> updt) )
    ; too complicated bytes
    (else
      (signal-type-error loc "indigestible object" src) ) ) )

(define (do-procedure-update loc md proc)
  (let ((updt (get-update md))
        (ctx (message-digest-context md)) )
    (while* (proc) (do-byte-source-update loc ctx it updt) ) ) )

(define (do-port-update loc md in)
  (do-procedure-update loc md (get-chunk-reader in)) )

(define (do-bytes-update loc md src)
  (let ((updt (get-update md))
        (ctx (message-digest-context md)) )
    (do-byte-source-update loc ctx src updt) ) )

(define (do-object-update loc md src)
  (cond
    ((input-port? src)    (do-port-update loc md src) )
    ((procedure? src)     (do-procedure-update loc md src) )
    (else                 (do-bytes-update loc md src) ) ) )

;;; Update Operation

;;

(define (message-digest-update-object md obj)
  (check-message-digest 'message-digest-update-object md)
  (do-object-update 'message-digest-update-object md obj) )

;;

(define (message-digest-update-procedure md proc)
  (check-message-digest 'message-digest-update-procedure md)
  (check-procedure 'message-digest-update-procedure proc)
  (do-procedure-update 'message-digest-update-procedure md proc) )

;;

(define (message-digest-update-port md in)
  (check-message-digest 'message-digest-update-port md)
  (check-input-port 'message-digest-update-port in)
  (do-port-update 'message-digest-update-port md in) )

;;

(define (message-digest-update-file md flnm)
  (check-message-digest 'message-digest-update-file md)
  (check-string 'message-digest-update-file flnm)
  (let ((in (open-input-file flnm)))
    (handle-exceptions exn
        (begin (close-input-port in) (abort exn))
      (do-port-update 'message-digest-update-file md in) )
    (close-input-port in) ) )

#;
(define (message-digest-update-file md flnm)
  (check-message-digest 'message-digest-update-file md)
  (check-string 'message-digest-update-file flnm)
  (let ((in #f))
  	(dynamic-wind
  		(lambda () (set! in (open-input-file flnm)) )
  		(lambda () (do-port-update 'message-digest-update-file md in) )
    	(lambda () (close-input-port in) ) ) ) )

) ;module message-digest-update-item
