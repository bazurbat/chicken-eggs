(use binary-search)

;; fill and search vector

(define count 10000000)

(print "fill vector")

(define seq (make-vector count))

(do ((i 0 (add1 i)))
    ((>= i count))
  (vector-set! seq i i))

(define ((compare x) y) (- x y))

(print "search vector")

(time
 (do ((i 0 (add1 i)))
     ((>= i 10000))
   (let ((j (random count)))
     (assert (= j (binary-search seq (compare j)))))))

(print "failing search of vector")
(assert (not (binary-search seq (compare -3))))

;; same for list

(print "search list")

(let ((lst (vector->list seq)))
  (time
   (do ((i 0 (add1 i)))
       ((>= i 10))
     (let ((j (random count)))
       (assert (= j (binary-search lst (compare j)))))))
  (print "failing search of list")
  (assert (not (binary-search lst (compare -3)))))

;; abstract sequence

(define (square x) (* x x))

(print "search abstract sequence")
(time
 (do ((i 0 (add1 i)))
     ((>= i 10000))
   (let* ((j (random count))
	  (k (square j)))
     (assert (= j (binary-search
		   count
		   (lambda (x) (- k (square x)))))))))
