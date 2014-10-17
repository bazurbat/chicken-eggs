(use object-evict)


; object-evict
; object-evicted?
; object-size
; object-release

(define tstvec (vector #f))
(let ((sz (object-size tstvec)))
  (assert (and (integer? sz) (positive? sz))) )
(define ev-tstvec (object-evict tstvec))
(assert (not (eq? tstvec ev-tstvec)))
(assert (object-evicted? ev-tstvec))
(set! ev-tstvec
  (let ((old ev-tstvec))
    (object-release old)
    #f))

; object-evict-to-location

; object-unevict
