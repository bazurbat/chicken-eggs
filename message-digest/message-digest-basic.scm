;;;; message-digest-basic.scm
;;;; Kon Lovett, May '10

;; Issues

(module message-digest-basic ()

  (import scheme chicken)

	(reexport
    message-digest-primitive
    message-digest-type
    message-digest-parameters
    message-digest-bv
    message-digest-int)

  (require-library
    message-digest-primitive
    message-digest-type
    message-digest-parameters
    message-digest-bv
    message-digest-int)

) ;module message-digest-basic