#+tc
(use tinyclos)
#+c
(use coops)

#+tc
(define-generic foo)
#+c
(define-generic (foo x))

(define-class <test> () ((x #f)))
(define-class <subtest> (<test>) ())

(define num 0)

(define-method (foo (x <test>))
  (set! num (add1 num)))

(define-method (foo (x <subtest>))
  (set! num (add1 num)))

(define +count+ 
  (cond-expand (csi 10000) (else 1000000)))

(define-syntax times
  (syntax-rules ()
    ((_ n check body ...)
     (time
      (set! num 0)
      (do ((i n (fx- i 1)))
	  ((zero? i))
	body ...)
      check))))

(define (function x) (set! num (add1 num)))

(define t1 (make <test>))
(define t2 (make <subtest>))

(print "\nbenchmarking " +count+ " normal procedure calls ... ")
(times
 +count+
 (assert (= +count+ num))
 (function t1))

(print "\nbenchmarking " +count+ " generic procedure calls ... ")
(times
 +count+
 (assert (= +count+ num))
 (foo t1))

(print "\nbenchmarking " +count+ " generic procedure calls (alternating) ... ")
(times
 (fx/ +count+ 2)
 (assert (= +count+ num))
 (foo t1)
 (foo t2))

(print "\nbenchmarking " +count+ " slot accesses ...")
(times
 +count+
 (void)
 (slot-ref t1 'x))
