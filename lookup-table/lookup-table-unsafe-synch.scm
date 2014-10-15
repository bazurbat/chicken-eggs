;;;; lookup-table-unsafe-synch.scm
;;;; Kon Lovett, Sep '09

;;;

(module lookup-table-unsafe-synch

  (;export
    make-dict/synch
    alist->dict/synch
    dict?/synch
    dict->alist/%synch
    dict-equivalence-function/%synch
    dict-count/%synch
    dict-keys/%synch
    dict-values/%synch
    dict-ref/%synch
    dict-indempotent-ref!/%synch
    dict-set!/%synch
    dict-exists?/%synch
    dict-update!/%synch
    dict-update-list!/%synch
    dict-update-dict!/%synch
    dict-delete!/%synch
    dict-for-each/%synch
    dict-search/%synch
    dict-merge!/%synch
    dict-print/%synch)

  (import scheme chicken lookup-table-unsafe synch)

  (require-library lookup-table-unsafe synch)

;;; UnSafe Synchronized UnSafe Dictionary

(define-constructor/synch make-dict dict/synch:)
(define-constructor/synch alist->dict dict/synch:)

(define-predicate/synch dict?)

(define-operation/%synch dict->alist)
(define-operation/%synch dict-equivalence-function)
(define-operation/%synch dict-count)
(define-operation/%synch dict-keys)
(define-operation/%synch dict-values)
(define-operation/%synch dict-ref)
(define-operation/%synch dict-indempotent-ref!)
(define-operation/%synch dict-set!)
(define-operation/%synch dict-exists?)
(define-operation/%synch dict-update!)
(define-operation/%synch dict-update-list!)
(define-operation/%synch dict-update-dict!)
(define-operation/%synch dict-delete!)
(define-operation/%synch dict-for-each)
(define-operation/%synch dict-search)
(define (dict-merge!/%synch mtxtbl1 mtxtbl2)
  (%let/synch ((tbl1 mtxtbl1) (tbl2 mtxtbl2))
    (dict-merge! tbl1 tbl2) ) )
(define-operation/%synch dict-print)

) ;module lookup-table-unsafe-synch
