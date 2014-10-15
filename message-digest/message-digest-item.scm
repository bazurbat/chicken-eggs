;;;; message-digest-item.scm
;;;; Kon Lovett, Jan '06 (message-digest.scm)
;;;; Kon Lovett, may '10 (message-digest.scm)
;;;; Kon Lovett, Apr '12

;; Issues

(module message-digest-item

  (;export
    message-digest-object
    message-digest-file
    message-digest-port)

  (import
    scheme
    chicken
    message-digest-type
    message-digest-update-item)

  (require-library
    message-digest-type
    message-digest-update-item)

;;; Single Source API

(define (message-digest-object mdp obj #!optional (result-type 'hex))
  (let ((md (initialize-message-digest mdp)))
    (message-digest-update-object md obj)
    (finalize-message-digest md result-type) ) )

(define (message-digest-file mdp flnm #!optional (result-type 'hex))
  (let ((md (initialize-message-digest mdp)))
    (message-digest-update-file md flnm)
    (finalize-message-digest md result-type) ) )

(define (message-digest-port mdp port #!optional (result-type 'hex))
  (let ((md (initialize-message-digest mdp)))
    (message-digest-update-port md port)
    (finalize-message-digest md result-type) ) )

) ;module message-digest-item
