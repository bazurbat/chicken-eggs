;;;; message-digest-port.scm
;;;; Kon Lovett, May '10

;; Issues
;;
;; - Use of sys namespace routines.

(module message-digest-port

	(;export
	  digest-output-port? check-digest-output-port error-digest-output-port
	  digest-output-port-name
    open-output-digest
    get-output-digest
    call-with-output-digest
    with-output-to-digest)

  (import
    scheme
    chicken
    (only data-structures ->string)
    (only ports make-output-port with-input-from-port)
    (only srfi-13 string-suffix-length-ci)
    (only type-checks define-check+error-type)
    (only type-errors make-error-type-message signal-type-error)
    message-digest-primitive
    message-digest-type
    message-digest-bv)

  (require-library
  	data-structures
  	ports
  	srfi-13
  	type-errors
    message-digest-primitive
    message-digest-type
    message-digest-bv)

;;; Message Digest Output Port API

;
(define (%port-type p) (##sys#slot p 7))
(define (%port-type-set! p t) (##sys#setslot p 7 t))
(define (%port-name p) (##sys#slot p 3))
(define (%port-name-set! p s) (##sys#setslot p 3 s))

;

(define (check-open-digest-output-port loc obj)
  (##sys#check-port* obj loc) ;must be open
  (##sys#check-port-mode obj #f loc)
  (unless (eq? 'digest (%port-type obj))
    (signal-type-error loc (make-error-type-message 'digest-output-port) obj) ) )

; Synthesize a port-name from a primitive-name

(define (make-digest-port-name mdp)
  (let ((nam (->string (or (message-digest-primitive-name mdp) 'digest))))
    (let ((remlen (string-suffix-length-ci nam "-primitive")))
      (string-append
        "("
        (if (positive? remlen)
            (substring nam 0 (fx- (string-length nam) remlen))
            nam )
        ")") ) ) )

;; Returns a digest-output-port for the MDP

(define (open-output-digest mdp)
  (let* ((md (initialize-message-digest mdp))
         (writer (lambda (obj)
         						; It will only ever be a string for now.
         						(if (string? obj) (message-digest-update-string md obj)
         							(message-digest-update-blob md obj))))
         (port (make-output-port writer void)) ) ;use default close behavior
    (##sys#set-port-data! port md)
    (%port-type-set! port 'digest)
    (%port-name-set! port (make-digest-port-name mdp))
    port ) )

(define (digest-output-port? obj)
  (and (output-port? obj)
       (eq? 'digest (%port-type obj)) ) )

(define-check+error-type digest-output-port)

(define (digest-output-port-name p)
  (check-digest-output-port 'digest-output-port-name p)
  (%port-name p) )

;; Finalizes the digest-output-port and returns the result in the form requested

(define (*close-output-digest loc digest-port result-type)
  (check-open-digest-output-port loc digest-port)
  (let ((res (finalize-message-digest (##sys#port-data digest-port) result-type)))
    (close-output-port digest-port)
    res ) )

(define (get-output-digest digest-port #!optional (result-type 'hex))
  (*close-output-digest 'get-output-digest digest-port result-type) )

;;;

;; Calls the procedure PROC with a single argument that is a digest-output-port for the MDP.
;; Returns the accumulated output string | blob | u8vector | hexstring

(define (call-with-output-digest mdp proc #!optional (result-type 'hex))
  (let ((port (open-output-digest mdp)))
    (proc port)
    (*close-output-digest 'call-with-output-digest port result-type) ) )

;; Calls the procedure THUNK with the current-input-port temporarily bound to a
;; digest-output-port for the MDP.
;; Returns the accumulated output string | blob | u8vector | hexstring

(define (with-output-to-digest mdp thunk #!optional (result-type 'hex))
  (call-with-output-digest mdp (cut with-input-from-port <> thunk) result-type) )

) ;module message-digest
