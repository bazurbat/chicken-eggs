;;;; lookup-table.scm
;;;; Kon Lovett, Sep '09

;;;

(module lookup-table

  (;export
    dict-safe-mode
    make-dict
    alist->dict
    dict->alist
    dict?
    check-dict
    error-dict
    dict-equivalence-function
    dict-count
    dict-keys
    dict-values
    dict-ref
    dict-set!
    dict-exists?
    dict-indempotent-ref!
    dict-update!
    dict-update-list!
    dict-update-dict!
    dict-delete!
    dict-for-each
    dict-search
    dict-merge!
    dict-print )

  (import scheme chicken)

  (include "lookup-table-body")

) ;module lookup-table
