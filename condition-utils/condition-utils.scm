;;;; condition-utils.scm
;;;; Kon Lovett, Aug '10
;;;; Kon Lovett, Apr '09

;; Issues
;;
;; - The memoized condition-predicate & condition-property-accessor facility
;; uses an association-list. Past approximately 10 items this will become
;; slower than a hash-table.

(module condition-utils

  (;export
    make-exn-condition
    make-exn-condition+
    make-condition+
    condition-predicate*
    condition-property-accessor*
    (make-condition-predicate condition-predicate*)
    (make-condition-property-accessor condition-property-accessor*))

  (import scheme chicken #;(only type-checks check-symbol))

  (require-library #;type-checks)

;;

(define (make-exn-condition #!optional (loc #f) (msg #f) (args #f))
  (make-property-condition 'exn
    'location loc
    'message (or msg "")
    'arguments (or args '())) )

;; <condition>  ->  <condition>
;; <symbol>     ->  (make-property-condition <symbol>)
;; <pair>       ->  (apply make-property-condition <pair>)
;;
;; (<symbol> [<symbol> <object>]...)

(define (expand-property-conditions cnds)
   (map (lambda (cnd)
          (cond
            ((condition? cnd)  cnd )
            ((symbol? cnd)     (make-property-condition cnd) )
            ((pair? cnd)       (apply make-property-condition cnd) ) ) )
        cnds) )

;;

(define (make-exn-condition+ loc msg args . cnds)
  (apply make-composite-condition
         (make-exn-condition loc msg args)
         (expand-property-conditions cnds)) )

;;

(define (make-condition+ . cnds)
  (apply make-composite-condition (expand-property-conditions cnds)) )

;;

(define condition-predicate*
  (let ((+preds+ '()))
    (lambda (kind)
      #;(check-symbol 'condition-predicate* kind)
      (let ((p (assq kind +preds+)))
        (if p (cdr p)
          (let ((pred (condition-predicate kind)))
            (set! +preds+ (cons (cons kind pred) +preds+))
            pred ) ) ) ) ) )

;;

(define condition-property-accessor*
  (let ((+getters+ '()))
    (lambda (kind prop #!optional dflt)
      #;(check-symbol 'condition-property-accessor* kind)
      #;(check-symbol 'condition-property-accessor* prop)
      (let ((key (cons kind prop)))
        (let ((p (assoc key +getters+)))
          (if p (cdr p)
            (let ((getter (condition-property-accessor kind prop dflt)))
              (set! +getters+ (cons (cons key getter) +getters+))
              getter ) ) ) ) ) ) )

;;

(define-syntax make-condition-predicate
  (syntax-rules ()
    ((_ ?kind0 ...)
      (lambda (obj)
        (and ((condition-predicate* '?kind0) obj) ...) ) ) ) )

;;

(define-syntax make-condition-property-accessor
  (syntax-rules ()

    ((_ ?kind ?prop)
      (make-condition-property-accessor ?kind ?prop #f) )

    ((_ ?kind ?prop ?dflt)
      (condition-property-accessor* '?kind '?prop ?dflt) ) ) )

) ;module condition-utils
