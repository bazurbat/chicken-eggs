;;;; message-digest-old.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, May '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

;; Issues

(module message-digest-old

  (;export
    string->hex
    byte-string->hexadecimal
    make-binary-message-digest
    make-message-digest
    message-digest-primitive-apply)

	(import
    scheme
    chicken
    message-digest-basic
    message-digest-single
    (only string-hexadecimal string->hex))

  (require-library
    message-digest-basic
    message-digest-single
    string-hexadecimal)

;;; Byte-string Utilities

(define byte-string->hexadecimal string->hex)

;;; Old API

;;

(define (message-digest-primitive-apply mdp src . args) ;DEPRECATED
  (message-digest-object mdp src 'string) )

;;

(define (make-binary-message-digest src ctx-info digest-len init update final
                                    #!optional (name 'make-binary-message-digest)) ;DEPRECATED
  (message-digest-object
    (make-message-digest-primitive ctx-info digest-len init update final name)
    src
    'string) )

;;

(define (make-message-digest src ctx-info digest-len init update final
                             #!optional (name 'make-message-digest)) ;DEPRECATED
  (message-digest-object
    (make-message-digest-primitive ctx-info digest-len init update final name)
    src
    'hex) )

) ;module message-digest
