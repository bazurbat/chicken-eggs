;;; Binary search


(module binary-search (binary-search)

  (import scheme chicken)

  (define (binary-search seq proc)
    (let-values (((seq len) 
		  (cond ((pair? seq) 
			 (let ((seq (list->vector seq)))
			   (values seq (vector-length seq))))
			((vector? seq)
			 (values seq (vector-length seq)))
			((fixnum? seq) (values #f seq))
			(else (error 'binary-search "invalid sequence" seq)))))
      (and (fx> len 0)
	   (let loop ((ps 0)
		      (pe len) )
	     (let ((p (fx+ ps (##core#inline "C_fixnum_shift_right" (fx- pe ps) 1))))
	       (let* ((x (if seq (##sys#slot seq p) p))
		      (r (proc x)) )
		 (cond ((fx= r 0) p)
		       ((fx< r 0) (and (not (fx= pe p)) (loop ps p)))
		       (else (and (not (fx= ps p)) (loop p pe))) ) ) ) ) ) ) )

)
