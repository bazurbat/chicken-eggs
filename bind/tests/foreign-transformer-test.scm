;;; Test for foreign-transform
;;; May also be useful as an example of how to use it.
(use bind test srfi-4)

(import-for-syntax bind-translator)
(import-for-syntax matchable)

(begin-for-syntax
 (print "===== compile-time")
 ;; convert any foreign-lambda with a point2d struct return-type,
 ;; and make it return a 2-element f32vector instead.
 (define (point2d-ret-transformer* x rename)
   (match x
     ;; return-type is a point2d, need to convert
     ((foreign-lambda* '(struct "point2d") args body)
      (pp x)
      (print "=>")
      (let* ((argnames (map cadr args))
             ;; return-type -> void, add f32vector destination
             ;; argument, and cast to point2d.
             (lambda-with-destination
              (bind-foreign-lambda*
               `(,foreign-lambda*
                 void                           ;; new return type
                 ,(cons '(f32vector dest) args) ;; add destination arg
                 (stmt
                  (= "struct point2d _r" ,body) ;; allocate, cast & assign
                  (= (deref "((struct point2d*)dest)") _r)))
               rename))
             ;; allocate a f32vector and use it as desination
             (destination-wrapper
              `(lambda ,argnames
                 (,(rename 'let) ((destination (make-f32vector 2)))
                  (,lambda-with-destination destination ,@argnames)
                  destination)))
             )
        (pp destination-wrapper)
        destination-wrapper
        ))
     ;; ignore other return-types
     (else (bind-foreign-lambda* x rename)))
   )

 ;; convert any arguments of type (struct "point2d") to f32vectors,
 ;; and cast & dereference from C.
 (define (point2d-arg-transformer* x rename)
   (match x
     ;; return-type is a point2d, need to convert
     ((foreign-lambda* rtype args body)
      (pp x)
      (print "=>")
      (define (type varname)
        (any (lambda (spec)
               (and (eq? (cadr spec) varname)
                    (car spec))) args))
      (define (struct? type)
        (and (pair? type)
             (eq? (car type) 'struct)))
      (define (point2d->f32vector as)
        (if (struct? (car as))
            (list 'f32vector (cadr as))
            as))
      ;; recursively look for variables which reference arguments of
      ;; type struct and cast from f32vector to struct point2d*.
      (define (dereference body)
        (if (list? body)
            (map dereference body)
            (if (and (symbol? body) (struct? (type body)))
                `(deref ,(conc "((struct point2d*)" body ")"))
                body)))
      (point2d-ret-transformer*
       `(,foreign-lambda* ,rtype
                     ,(map point2d->f32vector args)
                     ,(dereference body))
       rename)))))



(bind-options foreign-transformer: point2d-arg-transformer*)

(bind*
 "
struct point2d { float x, y; };
struct line    { struct point2d a, b; };

float lensq(struct point2d p) {
  return (p.x * p.x) + (p.y * p.y);
}

struct point2d init_point() {
   struct point2d g = { -1, -2};
   return g;
}

")

(print "==== runtime")

(test "plain struct return" (f32vector -1 -2) (init_point))
(test "plain struct arguments" 13.0 (lensq (f32vector 2 -3)))
(test "nested structs" (f32vector 3 4) (line-b (make-line (f32vector 1 2) (f32vector 3 4))))

(test-exit)
