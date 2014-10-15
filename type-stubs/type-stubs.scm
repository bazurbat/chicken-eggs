(module type-stubs

(: assume
   compiler-typecase
   define-specialization
   define-type
   the)

(import scheme)

(define-syntax :
  (syntax-rules ()
    ((_ identifier type rest ...) (begin))))

(define-syntax assume
  (syntax-rules ()
    ((_ ((variable type) ...) body ...)
     (begin body ...))))

(define-syntax compiler-typecase
  (syntax-rules (else)
    ((_ exp (type body ... ) ... (else else-body ...))
     (begin else-body ...))))


(define-syntax define-specialization
  (syntax-rules ()
    ((_ (name argument ...) results body) (begin))
    ((_ (name argument ...) body) (begin))))


(define-syntax define-type
  (syntax-rules ()
    ((_ name type) (begin))))

(define-syntax the
  (syntax-rules ()
    ((_ type expression)
     expression)))

)
