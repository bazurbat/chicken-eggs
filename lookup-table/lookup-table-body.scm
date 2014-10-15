;;;; lookup-table-body.scm
;;;; Kon Lovett, Sep '09

(import
  #;scheme  ;imported by including module
  #;chicken ;imported by including module
  (only srfi-1
    reverse! fold list-copy find alist-delete!)
  (only srfi-69
    hash-table->alist hash-table-ref/default hash-table-set!
    hash-table-delete! hash-table-for-each hash-table-merge!
    hash-table-walk hash-table-size hash-table-keys hash-table-values
    hash-table-exists? alist->hash-table make-hash-table)
  (only ports with-output-to-port)
  (only data-structures
    identity alist-ref alist-update!)
  (only extras pretty-print)
  (only miscmacros let/cc)
  type-checks
  type-errors
  record-variants)

(require-library
  srfi-1 srfi-69
  ports data-structures extras
  miscmacros
  type-checks type-errors
  record-variants)

;;;

(define-syntax safety
  (syntax-rules ()
    ((_ body ...)
      (cond-expand (unsafe) (else body ... )) ) ) )

;;;

(cond-expand
  (unsafe
    (include "chicken-primitive-object-inlines")
    (define-record-type-variant dict (unsafe unchecked inline)
      (make-dictbase data)
      dict::dict?
      (data dict-data-ref dict-data-set!)
      (test dict-test-ref dict-test-set!)
      (to-alist dict->alist-ref dict->alist-set!)
      (ref dict-ref-ref dict-ref-set!)
      (set dict-set-ref dict-set-set!)
      (delete dict-delete-ref dict-delete-set!)
      (for-each dict-for-each-ref dict-for-each-set!)
      (merge dict-merge-ref dict-merge-set!)
      (search dict-search-ref dict-search-set!)
      (count dict-count-ref dict-count-set!)
      (keys dict-keys-ref dict-keys-set!)
      (values dict-values-ref dict-values-set!)
      (exists dict-exists-ref dict-exists-set!) )
    (define-inline (dict::undefined-value? a) (%undefined-value? a))
    (define-inline (dict::undefined-value) (%undefined-value))
    (define-inline (dict::list-map/1 a b) (%list-map/1 a b))
    (define-inline (dict::list-for-each/1 a b) (%list-for-each/1 a b))
    (define-inline (dict::list-length a) (%list-length a))
    (define-inline (dict::list-find a b) (%list-find a b))
    (define-inline (dict::eq? a b) (%eq? a b))
    (define-inline (dict::alist-delete! a b c) (%alist-delete! a b c))
    (define-inline (dict::alist-update! a b c d) (%alist-update! a b c d))
    (define-inline (dict::alist-ref a b c d) (%alist-ref a b c d))
    (define-inline (dict::list-copy a) (%list-copy a))
    (define-inline (dict::set-cdr! a b) (%set-cdr! a b))
    (define-inline (dict::cons a b) (%cons a b))
    (define-inline (dict::cdr a) (%cdr a))
    (define-inline (dict::car a) (%car a)) )
  (else
    (define-record-type-variant dict (unchecked inline)
      (make-dictbase data)
      dict::dict?
      (data dict-data-ref dict-data-set!)
      (test dict-test-ref dict-test-set!)
      (to-alist dict->alist-ref dict->alist-set!)
      (ref dict-ref-ref dict-ref-set!)
      (set dict-set-ref dict-set-set!)
      (delete dict-delete-ref dict-delete-set!)
      (for-each dict-for-each-ref dict-for-each-set!)
      (merge dict-merge-ref dict-merge-set!)
      (search dict-search-ref dict-search-set!)
      (count dict-count-ref dict-count-set!)
      (keys dict-keys-ref dict-keys-set!)
      (values dict-values-ref dict-values-set!)
      (exists dict-exists-ref dict-exists-set!) )
    (define-inline (dict::undefined-value? obj) (eq? (void) obj))
    (define-inline (dict::undefined-value) (void))
    (define dict::list-map/1 map)
    (define dict::list-for-each/1 for-each)
    (define dict::list-length length)
    (define dict::list-find find)
    (define dict::eq? eq?)
    (define dict::alist-delete! alist-delete!)
    (define dict::alist-update! alist-update!)
    (define dict::alist-ref alist-ref)
    (define dict::list-copy list-copy)
    (define dict::set-cdr! set-cdr!)
    (define dict::cons cons)
    (define dict::cdr cdr)
    (define dict::car car) ) )

;;; Argument Checks

(define-check+error-type dict dict::dict?)

;;;

(define (set-dict-procs! dict tst to ref set del for mrg sch cnt keys vals exsts)
	(dict-test-set! dict tst)
	(dict->alist-set! dict to)
	(dict-ref-set! dict ref)
	(dict-set-set! dict set)
	(dict-delete-set! dict del)
	(dict-for-each-set! dict for)
	(dict-merge-set! dict mrg)
	(dict-search-set! dict sch)
	(dict-count-set! dict cnt)
	(dict-keys-set! dict keys)
	(dict-values-set! dict vals)
	(dict-exists-set! dict exsts)
	dict )

; Representation independent primitive calls

(define (dictbase-test dict) ((dict-test-ref dict) (dict-data-ref dict)))
(define (dictbase->alist dict) ((dict->alist-ref dict) (dict-data-ref dict)))
(define (dictbase-ref dict key def) ((dict-ref-ref dict) (dict-data-ref dict) key def))
(define (dictbase-set! dict key val) ((dict-set-ref dict) (dict-data-ref dict) key val))
(define (dictbase-delete! dict key) ((dict-delete-ref dict) (dict-data-ref dict) key))
(define (dictbase-for-each dict proc) ((dict-for-each-ref dict) (dict-data-ref dict) proc))
(define (dictbase-merge! dict1 dict2) ((dict-merge-ref dict1) (dict-data-ref dict1) (dict-data-ref dict2)))
(define (dictbase-search dict proc def) ((dict-search-ref dict) (dict-data-ref dict) proc def))
(define (dictbase-count dict) ((dict-count-ref dict) (dict-data-ref dict)))
(define (dictbase-keys dict) ((dict-keys-ref dict) (dict-data-ref dict)))
(define (dictbase-values dict) ((dict-values-ref dict) (dict-data-ref dict)))
(define (dictbase-exists? dict key) ((dict-exists-ref dict) (dict-data-ref dict) key))

;; Association List

(define (alist-search al proc #!optional def)
  (let ((p (dict::list-find (lambda (p) (proc (dict::car p) (dict::cdr p))) al)))
    (if p (dict::cdr p)
        def ) ) )

(define (make-alist-data test al) (dict::cons test al))
(define (alist-dict-test data) (dict::car data))
(define (alist-dict-alist data) (dict::cdr data))
(define (alist-dict-alist-set! data al) (dict::set-cdr! data al))

(define (set-alist-dict-procs! dict)
	(set-dict-procs! dict
		alist-dict-test-ref
		alist-dict->alist
		alist-dict-ref
		alist-dict-set!
		alist-dict-delete!
		alist-dict-for-each
		alist-dict-merge!
		alist-dict-search
		alist-dict-count
		alist-dict-keys
		alist-dict-values
		alist-dict-exists?) )

;; Hash Table

(define (make-htable-data test ht) (dict::cons test ht))
(define (htable-dict-test data) (dict::car data))
(define (htable-dict-htable data) (dict::cdr data))
(define (htable-dict-htable-set! data ht) (dict::set-cdr! data ht))

(define (set-htable-dict-procs! dict)
	(set-dict-procs! dict
		htable-dict-test-ref
		htable-dict->alist
		htable-dict-ref
		htable-dict-set!
		htable-dict-delete!
		htable-dict-for-each
		htable-dict-merge!
		htable-dict-search
		htable-dict-count
		htable-dict-keys
		htable-dict-values
		htable-dict-exists?) )

;;;

;;; Alist Dictionary

(define (alist-dict-test-ref data) (alist-dict-test data))

(define (alist-dict->alist data)
  (cond-expand (unsafe (alist-dict-alist data)) (else (dict::list-copy (alist-dict-alist data)))) )

(define (alist-dict-ref data key def)
	(dict::alist-ref key (alist-dict-alist data) (alist-dict-test data) def) )

(define (alist-dict-set! data key obj)
	(alist-dict-alist-set!
	  data
    (dict::alist-update! key obj (alist-dict-alist data) (alist-dict-test data))) )

(define (alist-dict-delete! data key)
	(alist-dict-alist-set!
	  data
    (dict::alist-delete! key (alist-dict-alist data) (alist-dict-test data))) )

(define (alist-dict-for-each data proc)
	(dict::list-for-each/1
	  (lambda (p) (proc (dict::car p) (dict::cdr p)))
    (alist-dict-alist data)) )

(define (alist-dict-merge! data1 data2)
	(let ((test (alist-dict-test data1))
	      (al (alist-dict-alist data1)))
		(dict::list-for-each/1
		  (lambda (p) (set! al (dict::alist-update! (dict::car p) (dict::cdr p) al test)))
      (alist-dict-alist data2))
		(alist-dict-alist-set! data1 al) ) )

(define (alist-dict-search data proc def)
  (alist-search (alist-dict-alist data) proc def))

(define (alist-dict-count data)
  (dict::list-length (alist-dict-alist data)))

(define (alist-dict-keys data)
  (dict::list-map/1 (lambda (x) (dict::car x)) (alist-dict-alist data)))

(define (alist-dict-values data)
  (dict::list-map/1 (lambda (x) (dict::cdr x)) (alist-dict-alist data)))

(define (alist-dict-exists? data key)
  (not (dict::undefined-value? (alist-dict-ref data key (dict::undefined-value)))) )

(define (make-alist-dict test al)
  (set-alist-dict-procs! (make-dictbase (make-alist-data test al))) )

(define (alist-dict? dict)
  (dict::eq? alist-dict-test-ref (dictbase-test dict)))

(define (become-alist-dict! dict)
	(dict-data-set! dict (make-alist-data (dictbase-test dict) (dictbase->alist dict)))
	(set-alist-dict-procs! dict) )

;;; Hash-table Dictionary

(define (htable-dict-test-ref data) (htable-dict-test data))

(define (htable-dict->alist data) (hash-table->alist (htable-dict-htable data)))

(define (htable-dict-ref data key def)
	(hash-table-ref/default (htable-dict-htable data) key def) )

(define (htable-dict-set! data key obj)
	(hash-table-set! (htable-dict-htable data) key obj) )

(define (htable-dict-delete! data key)
	(hash-table-delete! (htable-dict-htable data) key) )

(define (htable-dict-for-each data proc)
	(hash-table-for-each (htable-dict-htable data) proc) )

(define (htable-dict-merge! data1 data2)
	(htable-dict-htable-set!
 	  data1
	  (hash-table-merge! (htable-dict-htable data1) (htable-dict-htable data2))) )

(define (htable-dict-search data proc def)
	(let ((ht (htable-dict-htable data))
				(ret #f))
		(let ((res (let/cc return
                 (hash-table-walk ht
                   (lambda (key val) (when (proc key val) (set! ret #t) (return val)))))))
			(if ret res def) ) ) )

(define (htable-dict-count data) (hash-table-size (htable-dict-htable data)))

(define (htable-dict-keys data) (hash-table-keys (htable-dict-htable data)))

(define (htable-dict-values data) (hash-table-values (htable-dict-htable data)))

(define (htable-dict-exists? data key) (hash-table-exists? (htable-dict-htable data) key))

(define (make-htable-dict test ht)
  (set-htable-dict-procs! (make-dictbase (make-htable-data test ht))) )

(define (htable-dict? dict) (dict::eq? htable-dict-test-ref (dictbase-test dict)))

(define (become-htable-dict! dict)
	(let ((test (dictbase-test dict)))
		(dict-data-set! dict (make-htable-data test (alist->hash-table (dictbase->alist dict) test))))
	(set-htable-dict-procs! dict) )

;; Dictionary Type

(define (dict-same-kind? dict1 dict2) (dict::eq? (dict-test-ref dict1) (dict-test-ref dict2)))
(safety
  (define (dict-same-test? dict1 dict2) (dict::eq? (dictbase-test dict1) (dictbase-test dict2))) )

;; Optimal form

(define-inline (magic-count? count) (<= count MAGIC-LIMIT))

(define (dict-bestfit dict)
	(if (magic-count? (dictbase-count dict))
	  (unless (alist-dict? dict) (become-alist-dict! dict))
    (unless (htable-dict? dict) (become-htable-dict! dict)) )
  ;enforce unspecified return
  (void) )

;; Print worker

(define (*dict-print dict)
  (define (print-node-table dict spcr)
    (dictbase-for-each dict
      (lambda (key val)
        (dict::list-for-each/1 display spcr)
        (cond
          ((dict::dict? val)
            (write key) (display " :") (newline)
            (print-node-table val (dict::cons "  " spcr)) )
          (else
            (write key) (display " : ") (pretty-print val)) ) ) ) )
    (print-node-table dict '()) )

;; Update workers

(define (*dict-update! loc dict key valu-func updt-func)

  (define (do-dict-update! curr)
    (let* ((val (if (not (dict::undefined-value? curr)) curr
                  (let ((val (valu-func))) (safety (check-defined-value loc val)) val)))
           (updval (updt-func val)) )
      (dictbase-set! dict key updval)
      (dict-bestfit dict)
      updval ) )

  (safety
    (check-dict loc dict)
    (check-procedure loc valu-func)
    (check-procedure loc updt-func) )
  (do-dict-update! (dictbase-ref dict key (dict::undefined-value))) )

;;; Globals

; Cannot set but can still get
(define (dict-safe-mode . args) (cond-expand (unsafe #f) (else #t)))

(define (make-dict #!optional (test eq?) (size 0))
  (safety
    (check-cardinal-fixnum 'make-dict size "size")
    (check-procedure 'make-dict test) )
	(if (magic-count? size) (make-alist-dict test '())
    (make-htable-dict test (make-hash-table test)) ) )

(define (alist->dict al #!optional (test eq?) (size 0))
  (safety
    (check-alist 'alist->dict al "alist")
    (check-cardinal-fixnum 'alist->dict size "size")
    (check-procedure 'alist->dict test) )
	(if (magic-count? (fxmax (dict::list-length al) size)) (make-alist-dict test al)
    (make-htable-dict test (alist->hash-table al test)) ) )

(define (dict? obj) (dict::dict? obj))

(define (dict->alist dict)
  (safety (check-dict 'dict->alist dict))
	(dictbase->alist dict) )

(define (dict-equivalence-function dict)
  (safety (check-dict 'dict-equivalence-function dict))
	(dictbase-test dict) )

(define (dict-count dict)
  (safety (check-dict 'dict-count dict))
	(dictbase-count dict) )

(define (dict-keys dict)
  (safety (check-dict 'dict-keys dict))
	(dictbase-keys dict) )

(define (dict-values dict)
  (safety (check-dict 'dict-values dict))
	(dictbase-values dict) )

(define (dict-ref dict key #!optional def)
  (safety (check-dict 'dict-ref dict))
	(dictbase-ref dict key def) )

(define (dict-indempotent-ref! dict key func #!optional def)
  (safety
    (check-dict 'dict-indempotent-ref! dict)
    (check-procedure 'dict-indempotent-ref! func) )
  (let ((val (dictbase-ref dict key def)))
    (if (not (eq? def val)) val
      (let ((val (func def)))
        (if (eq? def val) def
          (begin
            (dictbase-set! dict key val)
            (dict-bestfit dict)
            val ) ) ) ) ) )

(define (dict-set! dict key obj)
  (safety
    (check-defined-value 'dict-set! obj)
    (check-dict 'dict-set! dict) )
	(dictbase-set! dict key obj)
	(dict-bestfit dict) )

(define (dict-exists? dict key)
  (safety (check-dict 'dict-exists? dict))
  (dictbase-exists? dict key) )

(define (dict-update! dict key valu-func #!optional (updt-func identity))
	(*dict-update! 'dict-update! dict key valu-func updt-func) )

(define (dict-update-list! dict key . vals)
  (*dict-update! 'dict-update-list!
    dict key (lambda () '()) (cut fold cons <> (reverse! vals))) )

(define (dict-update-dict! dict key #!optional (test eq?) (size 0))
  (*dict-update! 'dict-update-dict!
    dict key (lambda () (make-dict test size)) identity) )

(define (dict-delete! dict key)
  (safety (check-dict 'dict-delete! dict))
	(dictbase-delete! dict key)
	(dict-bestfit dict) )

(define (dict-for-each dict proc)
  (safety
    (check-dict 'dict-for-each dict)
    (check-procedure 'dict-for-each proc) )
	(dictbase-for-each dict proc) )

(define (dict-search dict proc #!optional def)
  (safety
    (check-dict 'dict-search dict)
    (check-procedure 'dict-search proc) )
	(dictbase-search dict proc def) )

(define (dict-merge! dict . dicts)
  (safety (check-dict 'dict-merge! dict))
	(dict::list-for-each/1
		(lambda (dictx)
      (safety
        (check-dict 'dict-merge! dictx)
        (unless (dict-same-test? dict dictx)
          (error "cannot merge lookup-tables; incompatible test") ) )
			(if (dict-same-kind? dict dictx) (dictbase-merge! dict dictx)
        (dictbase-for-each dictx (cut dict-set! dict <> <>)) ) )
		dicts)
	(dict-bestfit dict) )

(define (dict-print dict #!optional port)
  (if (not port) (*dict-print dict)
    (with-output-to-port port (lambda () (*dict-print dict)) ) ) )
