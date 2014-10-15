;;;; type-checks.scm
;;;; Kon Lovett, Apr '09

; Chicken Generic Arithmetic!

(module type-checks

  (;export
    define-check-type
    define-check+error-type
    check-defined-value
    check-bound-value
    check-number
    check-fixnum
    check-flonum
    check-integer
    check-real
    check-complex
    check-rational
    check-exact
    check-inexact
    check-positive-fixnum
    check-natural-fixnum
    check-positive-integer
    check-natural-integer
    check-positive-number
    check-natural-number
    check-procedure check-closure
    check-input-port
    check-output-port
    check-list
    check-pair
    check-blob
    check-vector
    check-structure
    check-record
    check-record-type
    check-symbol
    check-keyword
    check-string
    check-char
    check-boolean
    check-alist
    check-minimum-argument-count check-argument-count
    check-closed-interval check-open-interval
    check-half-closed-interval check-half-open-interval
    ;
    check-cardinal-fixnum
    check-cardinal-integer
    check-cardinal-number)

  (import chicken scheme type-errors)

  (require-library type-errors)

  (declare (bound-to-procedure ##sys#structure?))

;;

; maybe a problem with expansion environment namespace pollution
(define-for-syntax (symbolize . elts)
  (string->symbol (apply conc (map strip-syntax elts))) )

;;

(cond-expand

  (unsafe

    (define-syntax define-check-type
      (er-macro-transformer
        (lambda (frm rnm cmp)
          (let ((_define (rnm 'define)))
            (let* ((typ (cadr frm))
                   (nam (string->symbol (string-append "check-" (symbol->string typ)))) )
              `(,_define (,nam loc obj . _) obj) ) ) ) ) )

    ;; Backwards
    (define (check-cardinal-fixnum . _) (begin))
    (define (check-cardinal-integer . _) (begin))
    (define (check-cardinal-number . _) (begin))

    (define (check-positive-fixnum . _) (begin))
    (define (check-natural-fixnum . _) (begin))
    (define (check-positive-integer . _) (begin))
    (define (check-natural-integer . _) (begin))
    (define (check-positive-number . _) (begin))
    (define (check-natural-number . _) (begin))
    (define (check-structure . _) (begin))
    (define (check-record . _) (begin))
    (define (check-record-type . _) (begin))
    (define (check-minimum-argument-count . _) (begin))
    (define (check-argument-count . _) (begin))
    (define (check-closed-interval . _) (begin))
    (define (check-open-interval . _) (begin))
    (define (check-half-closed-interval . _) (begin))
    (define (check-half-open-interval . _) (begin)) )

  (else

    ;; These are weak predicates. Only check for structure.

    (export alist? plist?)

    (define (alist? obj)
      (or (null? obj)
          (and (list? obj)
               (let loop ((ls obj))
                 (or (null? ls)
                     (and (pair? (car ls))
                          (loop (cdr ls) ) ) ) ) ) ) )

    (define (plist? obj)
      ;since anything can be a key no stronger check possible
      (and (list? obj) (even? (length obj))) )

    ;;

    ; <symbol>          : <pred> is '<symbol>?'
    ; <symbol> <symbol> : <pred> is <symbol>
    ; ->
    ; (define (check-<symbol> loc obj . args)
    ;   (unless (<pred> obj)
    ;     (error-<symbol> loc obj (optional args)))
    ;   obj )

    (define-syntax define-check-type
      (er-macro-transformer
        (lambda (frm rnm cmp)
          (let ((_define (rnm 'define))
                (_unless (rnm 'unless))
                (_optional (rnm 'optional)) )
            (let* ((typ (cadr frm))
                   (typstr (symbol->string typ))
                   (pred (if (not (null? (cddr frm))) (caddr frm)
                            (string->symbol (string-append typstr "?"))))
                   (nam (string->symbol (string-append "check-" typstr)))
                   (errnam (string->symbol (string-append "error-" typstr))) )
              `(,_define (,nam loc obj . args)
                 (,_unless (,pred obj)
                   (,errnam loc obj (,_optional args)))
                 obj ) ) ) ) ) )

    ;; Is the object non-void?

    (define (defined-value? obj) (not (eq? (void) obj)))

    ;; Is the object bound to value?

    ; is obj the value from the de-ref of an unbound variable.
    ; could only occur in a rather unsafe calling environnment.

    (define (bound-value? obj) (##core#inline "C_unboundvaluep" obj))

    ;;

    (define (check-positive-fixnum loc obj . args)
      (unless (and (fixnum? obj) (fx< 0 obj))
        (error-positive-fixnum loc obj (optional args)))
      obj )

    (define (check-natural-fixnum loc obj . args)
      (unless (and (fixnum? obj) (fx<= 0 obj))
        (error-natural-fixnum loc obj (optional args)))
      obj )

    ;;

    (define (check-positive-integer loc obj . args)
      (unless (and (integer? obj) (positive? obj))
        (error-positive-integer loc obj (optional args)))
      obj )

    (define (check-natural-integer loc obj . args)
      (unless (and (integer? obj) (<= 0 obj))
        (error-natural-integer loc obj (optional args)))
      obj )

    ;;

    (define (check-positive-number loc obj . args)
      (unless (and (number? obj) (positive? obj))
        (error-positive-number loc obj (optional args)))
      obj )

    (define (check-natural-number loc obj . args)
      (unless (and (number? obj) (<= 0 obj))
        (error-natural-number loc obj (optional args)))
      obj )

    ;;

    (define (check-structure loc obj tag . args)
      (unless (##sys#structure? obj tag)
        (error-structure loc obj tag (optional args)))
      obj )

    (define (check-record loc obj tag . args)
      (unless (##sys#structure? obj tag)
        (error-record loc obj tag (optional args)))
      obj )

    (define (check-record-type loc obj tag . args)
      (unless (##sys#structure? obj tag)
        (error-record-type loc obj tag (optional args)))
      obj ) ) )

;;

(define-check-type defined-value)
(define-check-type bound-value)

(define-check-type fixnum)
(define-check-type flonum)
(define-check-type integer)
(define-check-type real)
(define-check-type complex)
(define-check-type rational)
(define-check-type exact)
(define-check-type inexact)
(define-check-type number)
(define-check-type symbol)
(define-check-type keyword)
(define-check-type string)
(define-check-type char)
(define-check-type boolean)
(define-check-type procedure)
(define check-closure check-procedure)
(define-check-type input-port)
(define-check-type output-port)
(define-check-type list)
(define-check-type pair)
(define-check-type blob)
(define-check-type vector)
(define-check-type plist)
(define-check-type alist)

; closed interval
(define (check-closed-interval loc num min max . args)
  (unless (and (<= min num) (<= num max))
    (error-closed-interval loc num min max (optional args)))
  num )

; open interval
(define (check-open-interval loc num min max . args)
  (unless (and (< min num) (< num max))
    (error-open-interval loc num min max (optional args)))
  num )

; closed+open interval
(define (check-half-open-interval loc num min max . args)
  (unless (and (< min num) (<= num max))
    (error-half-open-interval loc num min max (optional args)))
  num )

; open+closed interval
(define (check-half-closed-interval loc num min max . args)
  (unless (and (<= min num) (< num max))
    (error-half-closed-interval loc num min max (optional args))) 
  num)

(define (check-minimum-argument-count loc argc minargc)
  (unless (fx<= minargc argc)
    (error-minimum-argument-count loc argc minargc))
  argc )

(define (check-argument-count loc argc maxargc)
  (unless (fx<= argc maxargc)
    (error-argument-count loc argc maxargc))
  argc )

;;

; <type-symbol> [<type-predicate> [<message-string>]]

(define-syntax define-check+error-type
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (let ((_define-check-type (rnm 'define-check-type))
            (_define-error-type (rnm 'define-error-type)) )
        (let* ((typ (cadr frm))
               (pred (and (not (null? (cddr frm))) (caddr frm)))
               (mesg (and pred (not (null? (cdddr frm))) (cadddr frm))) )
          `(begin
             (,_define-error-type ,typ ,@(if mesg `(,mesg) '()))
             (,_define-check-type ,typ ,@(if pred `(,pred) '())) ) ) ) ) ) )

;; Backwards

(define check-cardinal-fixnum check-natural-fixnum)
(define check-cardinal-integer check-natural-integer)
(define check-cardinal-number check-natural-number)

) ;module type-checks
