(module
 parley-auto-completion
 (auto-completion-handler
  beep?
  completion-choices
  completion-list
  print-completions
  word-class)

 (import chicken scheme)
 (use data-structures extras irregex (srfi 1 13))

 (define completion-choices (make-parameter (lambda (input position last-word) '())))
 (define word-class (make-parameter '($ (+ (~ whitespace)))))

 (define print-completions
   (make-parameter (lambda (cs)
                     (printf "~%~a matches~%~a~%" (length cs) (string-intersperse cs " ")))))

 (define beep? (make-parameter #t))

 (define (completion-list lst)
   (unless (and (list? lst) (or (null? lst) (every string? lst)))
           (error "simple-list expects a list of strings as arguments, was given " lst))
   (completion-choices (lambda _ lst)))

 (define (find-last-word input pos)
   (let ((m (irregex-match `(: (*? any) ,(word-class) eos) input 0 pos)))
     (if (and (irregex-match-data? m)
              (irregex-match-valid-index? m 1))
         (begin
;           (fprintf (current-error-port) "~%last word ~a~%" (irregex-match-substring m 1))
           (values (irregex-match-substring m 1)
                   (irregex-match-start-index m 1)))
         (values "" pos))))

 (define (find-matches input completions)
   (and-let* ((sorted (sort completions string-ci<?))
              (matches
               (filter-map (lambda (c)
                             (and (irregex-search `(: bos ,input) c) c))
                           completions)))
;             (fprintf (current-error-port) "~%found ~a matches~%" (length matches))
             matches))

 (define (smallest-prefix lst)
   (let* ((l (string-length (car lst)))
          (lst (sort lst (lambda (a b) (> (string-length a) (string-length b)))))
          (number-of-prefixes
           (lambda (p lst)
             (length (filter-map (lambda (l) (string-prefix? p l)) lst)))))
;     (fprintf (current-error-port) "~% ~a ~a ~%" l lst)
     (cond ((null? lst) "")
           ((= (length lst) 1) (car lst))
           (else
            (let loop ((nops 0)
                       (plen 0)
                       (prefix ""))
 ;             (fprintf (current-error-port) "~% ~a ~a ~a (max ~a)" nops plen prefix (string-length (car lst)))
              (let* ((new-p (string-take (car lst) plen))
                     (new-nops (number-of-prefixes new-p lst)))
  ;              (fprintf (current-error-port) " new: ~a ~a~%" new-nops new-p)
                (if (and (>= new-nops nops) (<= (add1 plen) (string-length (car lst))))
                    (loop new-nops (add1 plen) new-p)
                    prefix)))))))

 (define (auto-complete input completions)
   (let* ((ms (or (find-matches input completions) '()))
          (sorted (sort ms (lambda (a b) (< (string-length a) (string-length b))))))
     (if (or (string-null? input) (null? sorted))
         (values #f completions)
         (let ((p (smallest-prefix sorted)))
           (values p
                   (delete p sorted))))))

 (define (auto-completion-handler prompt in out line pos exit offset)
   (let*-values (((word word-index) (find-last-word line pos))
                 ((first rest) (auto-complete word ((completion-choices) (substring line 0 word-index) pos word))))
;    (fprintf (current-error-port) "~%f: ~a r:~a w:~s~%" first rest word)
     (when (and first (not (null? rest)))
           ((print-completions) (if (string-null? word) (cons first rest) rest))
           (when (beep?) (display #\alarm)))
     (cond ((and (not (string-null? word)) first)
            (list prompt in out (string-replace line first word-index pos) (+ pos (- (string-length first) (string-length word))) exit offset))
           (else
            (list prompt in out line pos exit offset))))))
