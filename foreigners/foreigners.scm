;;; foreigners module

;;; renaming module

;; Renaming helper macro for er-macros below.  Must be a module
;; so we can import via (import-for-syntax renaming).

(module renaming (with-renamed)
  (import scheme)
  ;; (with-renamed r (begin car cdr) body ...)
  ;; -> (let ((%begin (r 'begin)) (%car (r 'car)) (%cdr (r 'cdr)))
  ;;      body ...)
  (define-syntax with-renamed
    (lambda (f r c)
      (##sys#check-syntax 'with-renamed f '(_ _ (_ . _) . _))
      (let ((renamer (cadr f))
            (identifiers (caddr f))
            (body (cdddr f))
            (munger (lambda (x) (string->symbol
                            (string-append "%" (symbol->string x))))))
        `(,(r 'let)
          ,(map (lambda (x)
                  `(,(munger x) (,renamer ',x)))
                identifiers)
          ,@body)))))

;;; define-foreign-record-type

(module foreigners
  (define-foreign-record-type define-foreign-enum-type)

  (import scheme chicken foreign)

  (define-syntax define-foreign-record-type
    (lambda (f r c)
      (##sys#check-syntax 'define-foreign-record-type f '(_ _ . _))

      (let ((name (cadr f))
            (slots (cddr f)))
        (let ([fname (if (pair? name) (->string (cadr name))
                         (sprintf "struct ~A" name))]
              [tname (if (pair? name) (car name) name)]
              [ctor #f]
              [dtor #f])
          (define (stype type)
            (cond [(not (pair? type)) type]
                  [(memq (car type) '(struct union)) `(c-pointer ,type)]
                  [else type] ) )
          (define (strtype type)
            (or (eq? type tname)
                (and (pair? type)
                     (memq (car type) '(struct union))  ) ) ) ; handle instances?

          ;; Process special declarations, which must occur first.
          (do ((slts slots (cdr slts)))
              ((or (null? slts) (not (pair? (car slts)))
                   (not (keyword? (caar slts))) (pair? (caar slts)))
               (set! slots slts) )
            (let ((decl (caar slts)))
              (cond ((c decl (r #:constructor)) (set! ctor (cadar slts)))
                    ((c decl (r #:destructor)) (set! dtor (cadar slts)))
                    (else (syntax-error 'define-foreign-record-type
                                        "invalid foreign record-type declaration" (car slts))) )) )
        
          (chicken.compiler.support#register-foreign-type! tname `(c-pointer ,fname))

          (let ((%void 'void)  ; foreign-lambda* recognizes renamed type identifiers now,
                (%int 'int))   ; but we keep this temporarily for BC
            (with-renamed r
                (declare foreign-declare begin define foreign-lambda*
                 if let lambda declare syntax-error and fx>= fx<)
              `(,%begin
                 ,@(if (pair? name)
                       '()
                       `((,%declare
                          (,%foreign-declare
                           ,(string-intersperse
                             (append
                              (cons
                               (string-append "struct " (->string name) " { ")
                               (map (lambda (slot)
                                      (##sys#check-syntax 'define-foreign-record-type
                                                          slot '(_ _ _ . _))
                                      (if (pair? (cadr slot)) ; (type (name size) ...)
                                          (sprintf "~A[~A];"
                                                   (##compiler#foreign-type-declaration
                                                    (car slot)
                                                    (->string (caadr slot)) )
                                                   (cadadr slot) )
                                          (sprintf "~A;" ; (type name ...)
                                                   (##compiler#foreign-type-declaration
                                                    (car slot)
                                                    (->string (cadr slot)) ) ) )
                                      ;; [else (syntax-error 'define-foreign-record
                                      ;;                     "bad slot spec" slot)]
                                      )
                                    slots) )
                              (list "};") )
                             "\n") ) ) ) )
                 ,@(if (not ctor)
                       '()
                       `((,%define ,ctor
                           (,%foreign-lambda* ,tname ()
                             ,(sprintf "return((~a *)C_malloc(sizeof(~a)));" fname fname)))))
                 ,@(if (not dtor)
                       '()
                       (let ((ptr (gensym)))
                         `((,%define (,dtor ,ptr)
                             (and ,ptr (##core#inline "C_qfree" ,ptr))))))
                 ,@(map (lambda (slot)
                          (##sys#check-syntax 'define-foreign-record-type slot '(_ _ _ . _))
                          (let* ((type (car slot))
                                 (namesz (cadr slot))
                                 (type2 (stype type))
                                 (getr (caddr slot))
                                 (setr (cdddr slot)))
                            (if (pair? namesz)
                                (let ((sname (car namesz))
                                      (size (cadr namesz))
                                      (var (gensym))
                                      (cvar (gensym))
                                      (svar (gensym))
                                      (xvar (gensym)))
                                  `(,%begin
                                     (,%define ,getr
                                       (,%let ([,cvar
                                                (,%foreign-lambda* ,type2 ([,tname ,var] [,%int ,svar])
                                                  ,(sprintf "return(~A~A->~A[~A]);"
                                                            (if (not (strtype type)) "" "&")
                                                            var sname svar) ) ] )
                                         (,%lambda (,var ,svar)
                                           (,%if (##core#check (,%and (,%fx>= ,svar 0)
                                                                      (,%fx< ,svar ,size)))
                                                 (,cvar ,var ,svar)
                                                 ;; this should signal a range exn...
                                                 (,%syntax-error 'define-foreign-record
                                                                 "array access out of range"
                                                                 ',tname ',svar ,size)))))
                                     ,@(if (null? setr)
                                           '()
                                           (if (eq? type type2)
                                               `((,%define ,(car setr)
                                                   (,%let ([,cvar
                                                            (,%foreign-lambda* ,%void
                                                                ([,tname ,var] [,%int ,svar] [,type ,xvar])
                                                              ,(sprintf "~A->~A[~A] = ~A;"
                                                                        var sname svar xvar))])
                                                     (,%lambda (,var ,svar ,xvar)
                                                       (,%if (##core#check (,%and (,%fx>= ,svar 0)
                                                                                  (,%fx< ,svar ,size)))
                                                             (,cvar ,var ,svar ,xvar)
                                                             (,%syntax-error
                                                              'define-foreign-record
                                                              "array access out of range"
                                                              ',tname ',svar ,size))))))
                                               '() ))))
                              
                                (let ([sname (cadr slot)]
                                      [var (gensym)]
                                      [xvar (gensym)])
                                  `(,%begin
                                     (,%define ,getr
                                       (,%foreign-lambda* ,type2 ([,tname ,var])
                                         ,(sprintf "return(~A~A->~A);"
                                                   (if (not (strtype type)) "" "&")
                                                   var sname) ) )
                                     ,@(if (null? setr)
                                           '()
                                           (if (eq? type type2)
                                               `((,%define ,(car setr)
                                                   (,%foreign-lambda* ,%void ([,tname ,var] [,type ,xvar])
                                                     ,(sprintf "~A->~A = ~A;" var sname xvar))))
                                               '() ))))
                                ;; [else (syntax-error 'define-foreign-record
                                ;;                     "bad slot spec" slot)]
                                )))
                        slots))))))))

;;; define-foreign-enum-type

(require-library matchable)
(import-for-syntax matchable)
(import-for-syntax renaming)

;; Ignored case where typename is a symbol, for now.
;; Permit string or symbol as REALTYPE in ENUMSPEC.
(define-syntax define-foreign-enum-type
  (lambda (f r c)
    (match
     f
     ((_ (type-name native-type default-value)
         (to-native from-native)
         enumspecs ...)
      (let ((enums (map (lambda (spec)
                          (match spec
                                 (((s v) n d) spec)
                                 (((s v) n)   `((,s ,v) ,n ',s))
                                 (((s) n d)   `((,s ,(gensym)) ,n ,d))
                                 (((s) n)     `((,s ,(gensym)) ,n ',s))
                                 ((s n d)     `((,s ,s) ,n ,d))
                                 ((s n)       `((,s ,s) ,n ',s))
                                 ((s ...)     (error 'define-foreign-enum-type
                                                     "error in enum spec" spec))
                                 (s          `((,s ,s) ,s ',s))
                                 (else
                                  (syntax-error 'default-foreign-enum-type
                                         "error in enum spec" spec))))
                        enumspecs)))
        (with-renamed
         r (begin define cond else if let symbol? list null?
                  car cdr case bitwise-ior error =
                  define-foreign-type define-foreign-variable)

         `(,%begin
           ,@(map (lambda (e)
                    (match-let ([ ((s var) name d) e ])
                      `(,%define-foreign-variable ,var ,native-type
                         ,(if (symbol? name) (symbol->string name) name))))
                  enums)

           (,%define (,from-native val)
             (,%cond
              ,@(map (lambda (e)
                       (match-let ([ ((s var) n value) e ])
                         `((,%= val ,var) ,value)))
                     enums)
              (,%else ,default-value)))

           (,%define (,to-native syms)
             (,%let loop ((syms (,%if (,%symbol? syms) (,%list syms) syms))
                          (sum 0))
               (,%if (,%null? syms)
                     sum
                     (loop (,%cdr syms)
                           (,%bitwise-ior
                            sum
                            (,%let ((val (,%car syms)))
                              (,%case
                               val
                               ,@(map (lambda (e)
                                        (match-let ([((symbol var) n d) e])
                                          `((,symbol ,(string->keyword
                                                       (symbol->string symbol)))
                                            ,var)))
                                      enums)
                               (,%else (,%error "not a member of enum" val
                                                ',type-name)))))))))

           (,%define-foreign-type ,type-name
             ,native-type ,to-native ,from-native)

           ))))

     ; handle missing default-value
     ((_ (type-name native-type) . rest)
      `(define-foreign-enum-type (,type-name ,native-type '()) ,@rest))
     ))))


;;; Testing

#|
,x
(define-foreign-record-type (servent "struct servent")
  (constructor: make-servent)
  (destructor: free-servent)
  (c-string s_name servent-name servent-name-set!)
  (c-pointer s_aliases servent-s_aliases)
  (port-number s_port servent-port servent-port-set!)
  (c-string s_proto servent-proto servent-proto-set!))

,x
(define-foreign-record-type point
  (int (xyz 3) point-coords point-coords-set!))

; rename: not used
; const specifier not used, avoid specifying setter

|#

#|
;; for interactive testing
(define (##compiler#foreign-type-declaration t n)
  (conc t " " n))
(define ##compiler#foreign-type-table (make-hash-table))
(define (##sys#hash-table-set! . args) (display "hash-table-set! ") (write args) (newline))
|#
