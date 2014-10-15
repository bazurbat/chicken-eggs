;;;; tests.scm


(use coops test miscmacros)

;;

(test-begin "miscellaneous")

(define-class <i1> () ((x 99) y))

(define-method (initialize-instance (x <i1>))
  (call-next-method)
  (inc! (slot-value x 'x)))

(define i1 (make <i1> 'x 88))
(define i2 (make <i1>))

(print-object i1)
(newline)
(print-object i1 (current-error-port))
(newline)

(test #f (slot-initialized? i1 'y))
(test #f (slot-initialized? i2 'y))
(test #t (slot-initialized? i1 'x))
(test #t (slot-initialized? i2 'x))
(test 89 (slot-value i1 'x))
(test 100 (slot-value i2 'x))

;; undefined generic

(test #f (##sys#symbol-has-toplevel-binding? 'm1))

(define-method (m1 (x <i1>)) x)

(test i1 (m1 i1))
(test #t (##sys#symbol-has-toplevel-binding? 'm1))

(define-method (m1 around: (x <i1>))
  (list (call-next-method)))

(test (list i1) (m1 i1))

;; metaclass

(define-class <mc> (<standard-class>))
(define-class <mc1> () () metaclass: <mc>)

(test <mc> (class-of <mc1>))

(define-method ((setter m1) (x <i1>) y)
  `(set ,y))

(test '(set 42) (set! (m1 i1) 42))

(module mod1 (<cx> mx)
(import scheme chicken coops test)
(define-class <cx>)
(define-method (mx (x <cx>)) 'cx)
(define-method (mx (x <procedure>)) 'proc)
(test 'cx (mx (make <cx>)))
(test 'proc (mx +))
)

(import mod1)
(test 'proc (mx +))

(define-method (mx (x <i1>)) 'i1)
(test 'proc (mx +))
(test 'i1 (mx i1))

;; accessor 

(define-class <c1> () ((s1 accessor: s1 initform: 100)))

(define c1 (make <c1>))

(test 100 (s1 c1))
(set! (s1 c1) 1)
(test 1 (s1 c1))

;; diamond inheritance

(define-class A)
(define-class B (A))
(define-class C (A))
(define-class D (B C))

(define clist '())

(define-method (m (x A)) (push! 'A clist))
(define-method (m (x B)) (push! 'B clist) (call-next-method))
(define-method (m (x C)) (push! 'C clist) (call-next-method))
(define-method (m (x D)) (push! 'D clist) (call-next-method))

(test '(A C B D) (begin (m (make D)) clist)) ; is this ok?

(test-end)

;; 

(test-begin "ScmObj examples")

(define human-c
  (make-class ()
    (name: favorite-drink:)))

(define Telemakhos
  (make human-c
    'name: "Telemakhos"
    'favorite-drink:
      (string-append
        "warm" " " "milk")))

(test "Telemakhos" (slot-value Telemakhos 'name:))
(test "warm milk" (slot-value Telemakhos 'favorite-drink:))

(set! (slot-value Telemakhos 'favorite-drink:) "tequila")

(test "tequila" (slot-value Telemakhos 'favorite-drink:))

(set! (slot-value Telemakhos 'name:) "Telemachus")

(test human-c (class-of Telemakhos))

(define schemer-c
  (make-class (human-c)
    (favorite-dialect:)))

(define Odysseus
  (make schemer-c
    'name: "Odysseus"))

(test "Odysseus" (slot-value Odysseus 'name:))

(test #f (slot-initialized? Odysseus 'favorite-drink:))

(test #f (handle-exceptions ex #f (slot-value Odysseus 'favorite-island:)))

(test #t (subclass? schemer-c human-c))
(test #f (subclass? human-c schemer-c))

(define lisper-c
  (make-class (human-c)
    (favorite-loop-construct:)))

(define eclectic-lisper-c
  (make-class (schemer-c lisper-c)
    (favorite-other-language:)))

(define food-c
  (make-class ()
    (name: wholesomeness:)))

(define beverage-c
  (make-class (food-c) ()))

(define snack-c
  (make-class (food-c) ()))

(define Diomedes
  (make schemer-c
    'name: "Diomedes"))

(define Nestor
  (make lisper-c
    'name: "Nestor"))

(define Menelaos
  (make lisper-c
    'name: "Menelaos"))

(define Penelope
  (make eclectic-lisper-c
    'name: "Penelope"))

(define beer
  (make beverage-c
    'name: "beer"
    'wholesomeness: .2))

(define coke
  (make beverage-c
    'name: "coke"
    'wholesomeness: .4))

(define milk
  (make beverage-c
    'name: "milk"
    'wholesomeness: 1))

(define candy
  (make snack-c
    'name: "candy"
    'wholesomeness: .1))

(define french-fries
  (make snack-c
    'name: "french fries"
    'wholesomeness: .4))

(define carrots
  (make snack-c
    'name: "carrots"
    'wholesomeness: 1))

(define ingests
  (make-generic-procedure person food))

(define-method (ingests (p schemer-c) (f beverage-c))
  (if (>= (slot-value f 'wholesomeness:)
          .5)
    (format #t "~a sips some ~a.~%"
      (slot-value p 'name:)
      (slot-value f 'name:))))

(define-method (ingests (p schemer-c) (f snack-c))
  (format #t "~a wolfs down some ~a.~%"
    (slot-value p 'name:)
    (slot-value f 'name:)))

(define-method (ingests (p lisper-c) (f beverage-c))
  (format #t "~a guzzles some ~a.~%"
    (slot-value p 'name:)
    (slot-value f 'name:)))

(define-method (ingests (p lisper-c) (f snack-c))
  (if (>= (slot-value f 'wholesomeness:)
          .5)
    (format #t "~a pecks at some ~a.~%"
      (slot-value p 'name:)
      (slot-value f 'name:))))

(define-method (ingests (p human-c) (f food-c))
  (format #t "~a consumes ~a.~%"
    (slot-value p 'name:)
    (slot-value f 'name:)))

(test #<<EOF
Telemachus consumes beer.
Telemachus consumes coke.
Telemachus consumes milk.
Telemachus consumes candy.
Telemachus consumes french fries.
Telemachus consumes carrots.
Odysseus sips some milk.
Odysseus wolfs down some candy.
Odysseus wolfs down some french fries.
Odysseus wolfs down some carrots.
Diomedes sips some milk.
Diomedes wolfs down some candy.
Diomedes wolfs down some french fries.
Diomedes wolfs down some carrots.
Nestor guzzles some beer.
Nestor guzzles some coke.
Nestor guzzles some milk.
Nestor pecks at some carrots.
Menelaos guzzles some beer.
Menelaos guzzles some coke.
Menelaos guzzles some milk.
Menelaos pecks at some carrots.
Penelope sips some milk.
Penelope wolfs down some candy.
Penelope wolfs down some french fries.
Penelope wolfs down some carrots.

EOF
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (person)
	 (for-each
	  (lambda (food)
	    (ingests person food))
	  (list beer coke milk candy
		french-fries carrots)))
       (list Telemakhos Odysseus Diomedes Nestor Menelaos Penelope)))))

;;       Telemakhos consumes everything in sight;
;;       the Schemers Odysseus and Diomedes wolf down candy, french fries and carrots but sip only milk; and
;;       the Lispers Nestor and Menelaos guzzle beer, coke and milk but eat only carrots.

(define-method (ingests before: (p schemer-c) (f food-c))
  (format #t "~a puts on a napkin.~%"
    (slot-value p 'name:)))

(define-method (ingests after: (p lisper-c) (f food-c))
  (format #t "~a puts away the plate.~%"
    (slot-value p 'name:)))

(test #<<EOF
Telemachus consumes carrots.
Odysseus puts on a napkin.
Odysseus wolfs down some carrots.
Diomedes puts on a napkin.
Diomedes wolfs down some carrots.
Nestor pecks at some carrots.
Nestor puts away the plate.
Menelaos pecks at some carrots.
Menelaos puts away the plate.
Penelope puts on a napkin.
Penelope wolfs down some carrots.
Penelope puts away the plate.

EOF
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (person)
	 (ingests person carrots))
       (list Telemakhos Odysseus Diomedes Nestor Menelaos Penelope)))))

;; Telemakhos doesn't bother with any niceties;
;; the Schemers Odysseus and Diomedes put on a napkin before eating but don't care to put away the plate; and
;; the Lispers Nestor and Menelaos forget the napkin but remember to put away the plate.

(define likes
  (make-generic-procedure person food))

(define-method (likes (p human-c) (f food-c))
  #t)

(define-method (likes (p schemer-c) (f snack-c))
  #t)

(define-method (likes (p schemer-c) (f beverage-c))
  (>= (slot-value f 'wholesomeness:)
      .5))

(define aged-mixin
  (make-class () ()))
(define aged-beverage-c
  (make-class (aged-mixin beverage-c)))
(define aged-snack-c
  (make-class (aged-mixin snack-c)))

(define lutefisk
  (make aged-snack-c
    'name: "lutefisk"
    'wholesomeness: .3))

(define champagne
  (make aged-beverage-c
    'name: "champagne"
    'wholesomeness: .6))

(define toddy
  (make aged-beverage-c
    'name: "toddy"
    'wholesomeness: .2))

(define-method (likes around: (p schemer-c) (f aged-snack-c))
  #f)

(define-method (likes around: (p schemer-c) (f aged-beverage-c))
  (not (call-next-method)))

(test 
 '(#f #f #t)
 (list
  (likes Odysseus lutefisk)
  (likes Odysseus champagne)
  (likes Odysseus toddy)))

(test-end)

;;

;; (test-begin "inheritance rules")

;; (define a (make-class a () ()))
;; (define b (make-class b () ()))
;; (define c (make-class c () ()))
;; (define s (make-class s (a b) ()))
;; (define r (make-class r (a c) ()))
;; (define q (make-class q (s r) ()))

;; ;; CPL:
;; ;;
;; ;; Flavors: (q s a b r c standard-object t)
;; ;; Loops:   (q s b r a c standard-object t)
;; ;; CLOS:    (q s r a c b standard-object t)

;; (define (full-cpl x)
;;   (let ((lst (slot-value x 'class-precedence-list)))
;;     (delete-duplicates (append-map (lambda (x) (cons x (full-cpl x))) lst) eq?)))

;; ;; apparently seems to go depth-first
;; (test '(q s a b r c) (map class-name (full-cpl q)))

;; (test-end)

;;

(test-begin "primitive classes")

(use coops-primitive-objects)

(define-generic (m2 x y))

(define-method (m2 (x #t) (y #t)) 'plain)
(define-method (m2 (x <number>) (y <symbol>)) 'non-plain)

(test 'plain (m2 "1" 2))
(test 'non-plain (m2 2.5 'a))

(test-end)

;;

(test-begin "metaclasses")

;; excample provided by Peter Lane:

(define-class <ma> (<standard-class>)
  ((a accessor: meta-a initform: 0)))

(define-class <a> () ((b initform: 2)) metaclass: <ma>)

(test 2 (slot-value (make <a>) 'b))
(test 0 (meta-a <a>))

(test-end)

;; handling of multiple values (#867 - contributed by "megane")

(test-begin "multiple values")

(define-class <mv> ())

(define (mv-foo a)
  (values 1 2))

(define-method (mv-bar (a <mv>))
  (values 1 2))

(define-method (mv-baz (a <mv>))
  (mv-bar a))

(test '(1 2) (receive (mv-foo (make <mv>))))
(test '(1 2) (receive (mv-bar (make <mv>))))
(test '(1 2) (receive (mv-baz (make <mv>))))

(test-end)

(test-exit)
