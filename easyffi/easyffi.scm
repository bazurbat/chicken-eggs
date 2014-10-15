;;;; easyffi.scm


(require 'easyffi-base)


(module easyffi (foreign-declare 
		 foreign-parse 
		 foreign-parse/declare
		 foreign-include-path)

  (import scheme chicken)
  (import (except foreign foreign-declare))
  (import-for-syntax srfi-1 srfi-13 easyffi-base) 

  (define-syntax foreign-declare
    (lambda (x r c)
      (let ((strs (append (cdr x) '("\n"))))
	(check-c-syntax (string-concatenate strs) 'foreign-declare)
	(if (every string? strs)
	    `(,(r 'declare)
	      (foreign-declare ,@strs))
	    (syntax-error
	     'foreign-declare "syntax error in declaration" strs) ) ) ))

  (define-syntax foreign-parse 
    (lambda (x r c)
      (let ((strs (append (cdr x) '("\n"))))
	(if (every string? strs)
	    `(,(r 'begin)
	      ,@(parse-easy-ffi (string-concatenate strs)))
	    (syntax-error 'foreign-parse "syntax error in declaration" strs) ) )))
  
  (define-syntax foreign-parse/declare 
    (lambda (x r c)
      (let ((strs (append (cdr x) '("\n"))))
	`(,(r 'begin)
	  (,(r 'declare) (foreign-declare ,@strs))
	  (,(r 'foreign-parse) ,@strs)) ) ))

  (define-syntax foreign-include-path
    (lambda (x r c)
      (set! ffi-include-path-list (append (cdr x) ffi-include-path-list))
      '(,(r 'void) ) ) ) 

)
