;;;; message-digest.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, May '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

(module message-digest ()

  (import scheme chicken)

  (reexport
    message-digest-primitive
    message-digest-type
    message-digest-parameters
    message-digest-bv
    message-digest-int
    message-digest-srfi-4
    message-digest-update-item
    message-digest-item)

  (require-library
    message-digest-primitive
    message-digest-type
    message-digest-parameters
    message-digest-bv
    message-digest-int
    message-digest-srfi-4
    message-digest-update-item
    message-digest-item)

) ;module message-digest
