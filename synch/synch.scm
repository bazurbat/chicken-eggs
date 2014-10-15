;;;; synch.scm
;;;; Kon Lovett, Mar '06

;; Issues
;;
;; - syntax checking is minimal so expansion errors are cryptic

(module synch

  (;export
    ;;
    synch
    synch-with
    call/synch
    call-with/synch
    apply/synch
    apply-with/synch
    let/synch
    set!/synch
    synch/lock
    synch/unlock
    object/synch
    record/synch
    record-synch/lock
    record-synch/unlock
    ;;
    %synch
    %synch-with
    %call/synch
    %call-with/synch
    %apply/synch
    %apply-with/synch
    %let/synch
    %set!/synch
    %synch/lock
    %synch/unlock
    %object/synch
    %record/synch
    %record-synch/lock
    %record-synch/unlock
    ;;
    make-object/synch
    object?/synch
    ;;
    define-constructor/synch
    define-predicate/synch
    (define-operation/synch check-mutex+object)
    define-operation/%synch)

  (import
    scheme
    (only chicken
      define-for-syntax optional
      void unless warning gensym dynamic-wind)
    (only data-structures any?)
    (only srfi-18
      thread?
      make-mutex mutex?
      mutex-specific mutex-specific-set!
      mutex-lock! mutex-unlock!
      mutex-state)
    (only type-checks define-check+error-type) )

  (import-for-syntax (only data-structures conc))

  (require-library data-structures srfi-18 type-checks)

;;;

(define-for-syntax (recmuxnam nam) (string->symbol (conc nam #\- 'mutex)))

;;; Protected

(define-syntax synch
	(syntax-rules ()
		((_ (?mtx (?lock-arg0 ...) (?unlock-arg0 ...)) ?body ...)
    	(let ((mtx ?mtx))
        (dynamic-wind
          (lambda () (mutex-lock! mtx ?lock-arg0 ...))
          (lambda () ?body ...)
          (lambda () (mutex-unlock! mtx ?unlock-arg0 ...)) ) ) )
		((_ (?mtx (?lock-arg0 ...)) ?body ...)
    	(synch (?mtx (?lock-arg0 ...) ()) ?body ...) )
		((_ ?mtx ?body ...)
    	(synch (?mtx () ()) ?body ...) ) ) )

(define-syntax synch-with
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax 'synch-with frm '(_ _ variable . #(_ 0)))
      (let ((_dynamic-wind (rnm 'dynamic-wind))
            (_let (rnm 'let))
            (_lambda (rnm 'lambda))
            (_mutex-unlock! (rnm 'mutex-unlock!))
            (_mutex-specific (rnm 'mutex-specific))
            (_mutex-lock! (rnm 'mutex-lock!))
            (mtxvar (rnm (gensym))))
        (let ((?mtx (cadr frm)) (?var (caddr frm)) (?body (cdddr frm)) )
          (call-with-values
            (lambda ()
              (if (not (pair? ?mtx)) (values ?mtx '() '())
                  (let ((mtx (car ?mtx))
                        (lock-args (if (<= 2 (length ?mtx)) (cadr ?mtx) '()))
                        (unlock-args (if (= 3 (length ?mtx)) (caddr ?mtx) '())) )
                    (values mtx lock-args unlock-args) ) ) )
            (lambda (?mtx ?lock-args ?unlock-args)
              `(,_let ((,mtxvar ,?mtx))
                 (,_let ((,?var (,_mutex-specific ,mtxvar)))
                   (,_dynamic-wind
                     (,_lambda () (,_mutex-lock! ,mtxvar ,@?lock-args))
                     (,_lambda () ,@?body)
                     (,_lambda () (,_mutex-unlock! ,mtxvar ,@?unlock-args)) ) ) ) ) ) ) ) ) ) )

(define-syntax call/synch
	(syntax-rules ()
		((_ (?mtx (?lock-arg0 ...) (?unlock-arg0 ...)) ?proc ?arg0 ...)
		  (let ((mtx ?mtx))
			  (dynamic-wind
				  (lambda () (mutex-lock! mtx ?lock-arg0 ...))
				  (lambda () (?proc ?arg0 ...))
				  (lambda () (mutex-unlock! mtx ?unlock-arg0 ...)) ) ) )
		((_ (?mtx (?lock-arg0 ...)) ?proc ?arg0 ...)
		  (call/synch (?mtx (?lock-arg0 ...) ()) ?proc ?arg0 ...) )
		((_ ?mtx ?proc ?arg0 ...)
		  (call/synch (?mtx () ()) ?proc ?arg0 ...) ) ) )

(define-syntax call-with/synch
	(syntax-rules ()
		((_ (?mtx (?lock-arg0 ...) (?unlock-arg0 ...)) ?proc ?arg0 ...)
		  (let ((mtx ?mtx))
			  (dynamic-wind
				  (lambda () (mutex-lock! mtx ?lock-arg0 ...))
				  (lambda () (?proc (mutex-specific mtx) ?arg0 ...))
				  (lambda () (mutex-unlock! mtx ?unlock-arg0 ...)) ) ) )
		((_ (?mtx (?lock-arg0 ...)) ?proc ?arg0 ...)
		  (call-with/synch (?mtx (?lock-arg0 ...) ()) ?proc ?arg0 ...) )
		((_ ?mtx ?proc ?arg0 ...)
		  (call-with/synch (?mtx () ()) ?proc ?arg0 ...) ) ) )

(define-syntax apply/synch
	(syntax-rules ()
	  ((_ (?mtx (?lock-arg0 ...) (?unlock-arg0 ...)) ?proc ?arg0 ...)
		  (let ((mtx ?mtx))
			  (dynamic-wind
				  (lambda () (mutex-lock! mtx ?lock-arg0 ...))
				  (lambda () (apply ?proc ?arg0 ...))
				  (lambda () (mutex-unlock! mtx ?unlock-arg0 ...)) ) ) )
	  ((_ (?mtx (?lock-arg0 ...)) ?proc ?arg0 ...)
		  (apply/synch (?mtx (?lock-arg0 ...) ()) ?proc ?arg0 ...) )
		((_ ?mtx ?proc ?arg0 ...)
		  (apply/synch (?mtx () ()) ?proc ?arg0 ...) ) ) )

(define-syntax apply-with/synch
	(syntax-rules ()
		((_ (?mtx (?lock-arg0 ...) (?unlock-arg0 ...)) ?proc ?arg0 ...)
		  (let ((mtx ?mtx))
			  (dynamic-wind
				  (lambda () (mutex-lock! mtx ?lock-arg0 ...))
				  (lambda () (apply ?proc (mutex-specific mtx) ?arg0 ...))
				  (lambda () (mutex-unlock! mtx ?unlock-arg0 ...)) ) ) )
		((_ (?mtx (?lock-arg0 ...)) ?proc ?arg0 ...)
		  (apply-with/synch (?mtx (?lock-arg0 ...) ()) ?proc ?arg0 ...) )
		((_ ?mtx ?proc ?arg0 ...)
		  (apply-with/synch (?mtx () ()) ?proc ?arg0 ...) ) ) )

(define-syntax let/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax 'let/synch frm '(_ list . _))
      (let ((_synch-with (rnm 'synch-with)))
        (let* ((?body (cddr frm))
               (res
                (let loop ((bnds (cadr frm)))
                  (if (null? bnds) ?body
                      (let ((?bnd (car bnds)))
                        (##sys#check-syntax 'let/synch ?bnd '(variable . _))
                        `((,_synch-with ,(cadr ?bnd) ,(car ?bnd) ,@(loop (cdr bnds)))) ) ) ) ) )
          (car res) ) ) ) ) )

(define-syntax set!/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax 'set!/synch frm '(_ pair . _))
      (let ((_synch-with (rnm 'synch-with))
            (_mutex-specific (rnm 'mutex-specific))
            (_mutex-specific-set! (rnm 'mutex-specific-set!))
            (_begin (rnm 'begin)))
        (let ((?bnd (cadr frm))
              (?body (cddr frm)))
          (let ((?var (car ?bnd))
                (?mtx (cadr ?bnd)))
            `(,_synch-with ,?mtx ,?var
               (,_mutex-specific-set! ,?mtx (,_begin ,@?body))
               (,_mutex-specific ,?mtx) ) ) ) ) ) ) )

(define-syntax synch/lock
	(syntax-rules ()
		((_ (?mtx (?lock-arg0 ...)) ?body ...)
		  (let ((mtx ?mtx) (ok? #f))
				(mutex-lock! mtx)
				(dynamic-wind
				  (lambda () (mutex-lock! mtx ?lock-arg0 ...))
					(lambda () (let ((res (begin ?body ...))) (set! ok? #t) res))
					(lambda () (unless ok? (mutex-unlock! mtx)))) ) )
		((_ ?mtx ?body ...)
		  (synch/lock (?mtx ()) ?body ...) ) ) )

(define-syntax synch/unlock
	(syntax-rules ()
		((_ (?mtx (?unlock-arg0 ...)) ?body ...)
		  (let ((mtx ?mtx))
			  (dynamic-wind
				  (lambda ()
					  (unless (thread? (mutex-state mtx))
						  (warning 'synch/unlock "mutex is not locked - locking")
						  (mutex-lock! mtx)))
				  (lambda () ?body ...)
				  (lambda () (mutex-unlock! mtx ?unlock-arg0 ...)) ) ) )
		((_ ?mtx ?body ...)
		  (synch/unlock (?mtx ()) ?body ...) ) ) )

(define-syntax object/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax 'object/synch frm '(_ _ . _))
      (let ((_synch-with (rnm 'synch-with))
            (_>< (rnm '><))
            (var (rnm (gensym)))
            (mtx (cadr frm)))
        (let body-loop ((unparsed (cddr frm)) (parsed '()))
          (if (not (null? unparsed))
              (let ((expr (car unparsed))
                    (next (cdr unparsed)))
                (let expr-loop ((rest expr) (parsedexpr '()))
                  (cond ((null? rest)
                          (body-loop next (cons (reverse parsedexpr) parsed)))
                        ((pair? rest)
                          (let ((arg (car rest))
                                (next (cdr rest)))
                            (if (cmp _>< arg)
                                (expr-loop next (cons var parsedexpr))
                                (expr-loop next (cons arg parsedexpr)) ) ))
                        ((cmp _>< rest)
                          (body-loop next (cons var parsed)))
                        (else
                          (body-loop next (cons rest parsed))) ) ) )
              `(,_synch-with ,mtx ,var ,@(reverse parsed)) ) ) ) ) ) )

(define-syntax record/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax 'record/synch frm '(_ symbol _ . _))
      (let ((_synch (rnm 'synch)))
        (let ((?sym (cadr frm))
              (?rec (caddr frm))
              (?body (cdddr frm)))
          `(,_synch (,(recmuxnam ?sym) ,?rec) ,@?body) ) ) ) ) )

(define-syntax record-synch/lock
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax 'record-synch/lock frm '(_ symbol _ . _))
      (let ((_synch/lock (rnm 'synch/lock)))
        (let ((?sym (cadr frm))
              (?rec (caddr frm))
              (?body (cdddr frm)))
          `(,_synch/lock (,(recmuxnam ?sym) ,?rec) ,@?body) ) ) ) ) )

(define-syntax record-synch/unlock
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax 'record-synch/unlock frm '(_ symbol _ . _))
      (let ((_synch/unlock (rnm 'synch/unlock)))
        (let ((?sym (cadr frm))
              (?rec (caddr frm))
              (?body (cdddr frm)))
          `(,_synch/unlock (,(recmuxnam ?sym) ,?rec) ,@?body) ) ) ) ) )


;;; Unprotected

(define-syntax %*synch
	(syntax-rules ()
		((_ (?mtx (?lock-arg0 ...) (?unlock-arg0 ...)) ?body ...)
		  (let ((mtx ?mtx))
        (mutex-lock! mtx ?lock-arg0 ...)
				(call-with-values
					(lambda () ?body ...)
					(lambda ret
						(mutex-unlock! mtx ?unlock-arg0 ...)
						(apply values ret))) ) )
		((_ ?mtx ?body ...)
		  (%*synch (?mtx () ()) ?body ...) ) ) )

(define-syntax %*synch-with
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax '%*synch-with frm '(_ _ variable . _))
      (let ((_call-with-values (rnm 'call-with-values))
            (_mutex-specific (rnm 'mutex-specific))
            (_mutex-lock! (rnm 'mutex-lock!))
            (_mutex-unlock! (rnm 'mutex-unlock!))
            (_let (rnm 'let))
            (_apply (rnm 'apply))
            (_values (rnm 'values))
            (_lambda (rnm 'lambda))
            (_ret (rnm 'ret))
            (mtxvar (rnm (gensym))))
        (let ((?mtx (cadr frm))
              (?var (caddr frm))
              (?body (cdddr frm)))
          (call-with-values
            (lambda ()
              (if (not (pair? ?mtx)) (values ?mtx '() '())
                (let ((mtx (car ?mtx))
                      (lock-args (if (<= 2 (length ?mtx)) (cadr ?mtx) '()))
                      (unlock-args (if (= 3 (length ?mtx)) (caddr ?mtx) '())) )
                  (values mtx lock-args unlock-args) ) ) )
            (lambda (?mtx ?lock-args ?unlock-args)
              `(,_let ((,mtxvar ,?mtx))
                 (,_let ((,?var (,_mutex-specific ,mtxvar)))
                   (,_mutex-lock! ,mtxvar ,@?lock-args)
                   (,_call-with-values
                     (,_lambda () ,@?body)
                     (,_lambda ,_ret
                       (,_mutex-unlock! ,mtxvar ,@?unlock-args)
                       (,_apply ,_values ,_ret)) ) ) ) ) ) ) ) ) ) )

(define-syntax %synch
	(syntax-rules ()
		((_ ?mtx ?body ...) (%*synch ?mtx ?body ...) ) ) )

(define-syntax %synch-with
	(syntax-rules ()
		((_ ?mtx ?var ?body ...) (%*synch-with ?mtx ?var ?body ...) ) ) )

(define-syntax %call/synch
	(syntax-rules ()
		((_ ?mtx ?proc ?arg0 ...) (%*synch ?mtx (?proc ?arg0 ...)) ) ) )

(define-syntax %call-with/synch
	(syntax-rules ()
		((_ ?mtx ?proc ?arg0 ...) (%*synch-with ?mtx var (?proc var ?arg0 ...)) ) ) )

(define-syntax %apply/synch
	(syntax-rules ()
		((_ ?mtx ?proc ?arg0 ...) (%*synch ?mtx (apply ?proc ?arg0 ...)) ) ) )

(define-syntax %apply-with/synch
	(syntax-rules ()
		((_ ?mtx ?proc ?arg0 ...) (%*synch-with ?mtx var (apply ?proc var ?arg0 ...)) ) ) )

(define-syntax %let/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax '%let/synch frm '(_ list . _))
      (let ((_%synch-with (rnm '%synch-with)))
        (let ((?body (cddr frm)))
          (car
            (let loop ((?bnds (cadr frm)))
              (if (null? ?bnds) ?body
                (let ((bnd (car ?bnds)))
                  (##sys#check-syntax '%let/synch bnd '(variable _))
                  `((,_%synch-with ,(cadr bnd) ,(car bnd) ,@(loop (cdr ?bnds)))) ) ) ) ) ) ) ) ) )

(define-syntax %set!/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax '%set!/synch frm '(_ pair . _))
      (let ((_%synch-with (rnm '%synch-with))
            (_mutex-specific (rnm 'mutex-specific))
            (_mutex-specific-set! (rnm 'mutex-specific-set!))
            (_let (rnm 'let))
            (_begin (rnm 'begin))
            (mtxvar (rnm (gensym))))
        (let ((?bnd (cadr frm))
              (?body (cddr frm)))
          (let ((?var (car ?bnd))
                (?mtx (cadr ?bnd)))
            `(,_let ((,mtxvar ,?mtx))
               (,_%synch-with ,mtxvar ,?var
                 (,_mutex-specific-set! ,mtxvar (,_begin ,@?body))
                 (,_mutex-specific ,mtxvar) ) ) ) ) ) ) ) )

(define-syntax %synch/lock
	(syntax-rules ()
		((_ (?mtx (?lock-arg0 ...)) ?body ...)
		  (let ((mtx ?mtx) (ok? #f))
				(mutex-lock! mtx ?lock-arg0 ...)
				(call-with-values
					(lambda () (let ((res (begin ?body ...))) (set! ok? #t) res))
					(lambda ret
						(unless ok? (mutex-unlock! mtx))
						(apply values ret))) ) )
		((_ ?mtx ?body ...)
		  (%synch/lock (?mtx ()) ?body ...) ) ) )

(define-syntax %synch/unlock
	(syntax-rules ()
		((_ (?mtx (?unlock-arg0 ...)) ?body ...)
      (let ((mtx ?mtx))
        (unless (thread? (mutex-state mtx))
          (warning '%synch/unlock "mutex is not locked - locking")
          (mutex-lock! mtx))
        (call-with-values
          (lambda () ?body ...)
          (lambda ret
            (mutex-unlock! mtx ?unlock-arg0 ...)
            (apply values ret)) ) ) )
		((_ ?mtx ?body ...)
      (%synch/unlock (?mtx ()) ?body ...) ) ) )

(define-syntax %object/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax '%object/synch frm '(_ _ . _))
      (let ((_%synch-with (rnm '%synch-with))
            (_>< (rnm '><))
            (var (rnm (gensym)))
            (mtx (cadr frm)))
        (let body-loop ((unparsed (cddr frm)) (parsed '()))
          (if (not (null? unparsed))
              (let ((expr (car unparsed))
                    (next (cdr unparsed)))
                (let expr-loop ((rest expr) (parsedexpr '()))
                  (cond ((null? rest)
                          (body-loop next (cons (reverse parsedexpr) parsed)))
                        ((pair? rest)
                          (let ((arg (car rest))
                                (next (cdr rest)))
                            (if (cmp _>< arg)
                                (expr-loop next (cons var parsedexpr))
                                (expr-loop next (cons arg parsedexpr)) ) ))
                        ((cmp _>< rest)
                          (body-loop next (cons var parsed)))
                        (else
                          (body-loop next (cons rest parsed))) ) ) )
              `(,_%synch-with ,mtx ,var ,@(reverse parsed)) ) ) ) ) ) )

(define-syntax %record/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax '%record/synch frm '(_ symbol _ . _))
      (let ((_%synch (rnm '%synch)))
        (let ((?sym (cadr frm))
              (?rec (caddr frm))
              (?body (cdddr frm)))
          `(,_%synch (,(recmuxnam ?sym) ,?rec) ,@?body) ) ) ) ) )

(define-syntax %record-synch/lock
  (er-macro-transformer
  (lambda (frm rnm cmp)
    (##sys#check-syntax '%record-synch/lock frm '(_ symbol _ . _))
    (let ((_%synch/lock (rnm '%synch/lock)))
      (let ((?sym (cadr frm))
            (?rec (caddr frm))
            (?body (cdddr frm)))
        `(,_%synch/lock (,(recmuxnam ?sym) ,?rec) ,@?body) ) ) ) ) )

(define-syntax %record-synch/unlock
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax '%record-synch/unlock frm '(_ symbol _ . _))
      (let ((_%synch/unlock (rnm '%synch/unlock)))
        (let ((?sym (cadr frm))
              (?rec (caddr frm))
              (?body (cdddr frm)))
          `(,_%synch/unlock (,(recmuxnam ?sym) ,?rec) ,@?body) ) ) ) ) )


;;; Synch Object

(define (mutex+object? obj)
	(and (mutex? obj)
		   (not (eq? (void) (mutex-specific obj)))) )

(define-check+error-type mutex+object)

;;

(define (make-object/synch obj #!optional (name '(object/synch-)))
  (let ((mutex (make-mutex (if (pair? name) (gensym (car name)) name))))
    (mutex-specific-set! mutex obj)
    mutex) )

(define (object?/synch obj #!optional (pred any?))
  (and (mutex+object? obj)
       (pred (mutex-specific obj))) )

;;

(define-for-syntax (synchsym sym)
	(string->symbol (string-append (symbol->string sym) "/synch")) )

;;

(define-syntax define-constructor/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax 'define-constructor/synch frm '(_ symbol . _))
      (let ((_define (rnm 'define))
            (_apply (rnm 'apply))
            (_args (rnm (gensym 'args)))
            (_make-object/synch (rnm 'make-object/synch)) )
        (let* ((prcnam (cadr frm))
               (id (if (not (null? (cddr frm))) `('(,(caddr frm))) '()))
               (newnam (synchsym prcnam)) )
          `(,_define (,newnam . ,_args)
             (,_make-object/synch (,_apply ,prcnam ,_args) ,@id)) ) ) ) ) )

;;

(define-syntax define-predicate/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax 'define-predicate/synch frm '(_ symbol))
      (let ((_define (rnm 'define))
            (_obj (rnm (gensym 'obj)))
            (_object?/synch (rnm 'object?/synch)) )
        (let* ((prcnam (cadr frm))
               (newnam (synchsym prcnam)) )
          `(,_define (,newnam ,_obj) (,_object?/synch ,_obj ,prcnam)) ) ) ) ) )

;;

;operand must be the 1st argument

(define-syntax define-operation/synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (##sys#check-syntax 'define-operation/synch frm '(_ symbol))
      (let ((_define (rnm 'define))
            (_apply (rnm 'apply))
            (_let (rnm 'let))
            (_car (rnm 'car))
            (_cdr (rnm 'cdr))
            (_if (rnm 'if))
            (_pair? (rnm 'pair?))
            (_synch-with (rnm 'synch-with))
            (_check-mutex+object (rnm 'check-mutex+object))
            (_mutex-specific (rnm 'mutex-specific))
            (_mtx+obj (rnm (gensym 'mtx+obj)))
            (_args (rnm (gensym 'args)))
            (_obj (rnm (gensym 'obj)))
            (_mtx (rnm (gensym 'mtx))) )
        (let* ((prcnam  (cadr frm))
               (newnam (synchsym prcnam)) )
          `(,_define (,newnam ,_mtx+obj . ,_args)
             (,_let ((,_mtx (,_if (,_pair? ,_mtx+obj) (,_car ,_mtx+obj) ,_mtx+obj)))
               (,_check-mutex+object ',newnam ,_mtx 'object/synch)
               (,_synch-with ,_mtx+obj ,_obj (,_apply ,prcnam ,_obj ,_args))) ) ) ) ) ) )

;;

;operand must be the 1st argument

(define-syntax define-operation/%synch
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (define (%synchsym sym) (string->symbol (string-append (symbol->string sym) "/%synch")))
      (##sys#check-syntax 'define-operation/%synch frm '(_ symbol))
      (let ((_define (rnm 'define))
            (_apply (rnm 'apply))
            (_let (rnm 'let))
            (_car (rnm 'car))
            (_cdr (rnm 'cdr))
            (_if (rnm 'if))
            (_pair? (rnm 'pair?))
            (_%synch-with (rnm '%synch-with))
            (_check-mutex+object (rnm 'check-mutex+object))
            (_mtx+obj (rnm (gensym 'mtx+obj)))
            (_args (rnm (gensym 'args)))
            (_obj (rnm (gensym 'obj)))
            (_mtx (rnm (gensym 'mtx))) )
        (let* ((prcnam (cadr frm))
               (newnam (%synchsym prcnam)) )
          `(,_define (,newnam ,_mtx+obj . ,_args)
             (,_let ((,_mtx (,_if (,_pair? ,_mtx+obj) (,_car ,_mtx+obj) ,_mtx+obj)))
               (,_check-mutex+object ',newnam ,_mtx 'object/synch)
							 (,_%synch-with ,_mtx+obj ,_obj (,_apply ,prcnam ,_obj ,_args)) ) ) ) ) ) ) )

) ;module synch
