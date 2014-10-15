;;;; type-errors.scm
;;;; Kon Lovett, Apr '09

;; Issues
;;
;; - The type error message is built so as to look like those of the Chicken
;; "core". This constraint necessarily means some knowledge of the use of the
;; indefinite article. So any I18N effort will either have some logic needed or
;; a change to the Chicken "core" form.
;;
;; Maybe "... not an integer" -> "... integer required" &
;; "... not a list" -> "... list required".

(module type-errors

  (;export
    make-bad-argument-message
    make-type-name-message
    make-error-type-message
    signal-type-error
    error-argument-type
    warning-argument-type
    (define-error-type error-argument-type)
    error-bound-value
    error-defined-value
    error-number
    error-fixnum
    error-flonum
    error-integer
    error-real
    error-complex
    error-rational
    error-exact
    error-inexact
    error-positive-number
    error-natural-number
    error-positive-fixnum
    error-natural-fixnum
    error-positive-integer
    error-natural-integer
    error-procedure error-closure
    error-input-port
    error-output-port
    error-list
    error-pair
    error-blob
    error-vector
    error-structure
    error-record
    error-record-type
    error-symbol
    error-keyword
    error-string
    error-char
    error-boolean
    error-plist
    error-alist
    error-minimum-argument-count
    error-argument-count
    error-interval
    error-closed-interval error-open-interval
    error-half-open-interval error-half-closed-interval
    ;
    error-cardinal-fixnum
    error-cardinal-integer
    error-cardinal-number)

  (import scheme chicken foreign (only data-structures ->string conc))

  (require-library data-structures)

  (declare
    (pure
      vowel? get-indefinite-article
      make-error-type-message make-type-name-message
      make-bad-argument-message)
    (bound-to-procedure ##sys#signal-hook ##sys#error-hook) )

;;;

; maybe a problem with expansion environment namespace pollution
(define-for-syntax (symbolize . elts)
  (string->symbol (apply conc (map strip-syntax elts))) )

(define (vowel? ch) (and (memq ch '(#\a #\e #\i #\o #\u)) #t))

(define (get-indefinite-article wrdstr)
  (if (vowel? (string-ref wrdstr 0)) 'an
    'a) )

;;

(define (make-bad-argument-message #!optional argnam)
  (if (not argnam) "bad argument"
    (string-append "bad `" (->string argnam) "' argument") ) )

(define (make-type-name-message typnam)
  (let ((typnam (->string typnam)))
    (conc (get-indefinite-article typnam) #\space typnam) ) )

(define (make-error-type-message typnam #!optional argnam)
  (string-append
    (make-bad-argument-message argnam)
    " type - not "
    (make-type-name-message typnam)) )

;;

(define (signal-type-error loc msg . objs)
  (apply ##sys#signal-hook #:type-error loc msg objs) )

;;

(define (error-argument-type loc obj typnam #!optional argnam)
  (signal-type-error loc (make-error-type-message typnam argnam) obj) )

;;

(define (warning-argument-type loc obj typnam #!optional argnam)
  (warning
    (string-append
      (if loc (conc #\( (symbol->string loc) #\) #\space) "")
      (conc (make-error-type-message typnam argnam) #\: #\space)
      (->string obj))) )

;;

; <symbol>          : <typnam> is "<symbol>"
; <symbol> <string> : <typnam> is <string>
; ->
; (define (error-<symbol> loc obj #!optional argnam)
;   (error-argument-type loc obj <typnam> argnam) )

(define-syntax define-error-type
  (er-macro-transformer
    (lambda (frm rnm cmp)
      (let ((_define (rnm 'define))
            (_#!optional (rnm '#!optional))
            (_error-argument-type (rnm 'error-argument-type)) )
        (let* ((typ (cadr frm))
               (typstr (symbol->string typ))
               (typnam (if (null? (cddr frm)) typstr (caddr frm)))
               (nam (string->symbol (string-append "error-" typstr))) )
          `(,_define (,nam loc obj ,_#!optional argnam)
             (,_error-argument-type loc obj ,typnam argnam) ) ) ) ) ) )

;;

(define (error-bound-value loc obj tag #!optional argnam)
	(error-argument-type loc "#<unbound>" "bound-value" argnam) )

(define (error-defined-value loc obj tag #!optional argnam)
	(error-argument-type loc "#<unspecified>" "defined-value" argnam) )

(define-error-type number)
(define-error-type fixnum)
(define-error-type flonum)
(define-error-type integer)
(define-error-type real)
(define-error-type complex)
(define-error-type rational)
(define-error-type exact)
(define-error-type inexact)
(define-error-type positive-number)
(define-error-type natural-number)
(define-error-type positive-fixnum)
(define-error-type natural-fixnum)
(define-error-type positive-integer)
(define-error-type natural-integer)
(define-error-type procedure)
(define error-closure error-procedure)
(define-error-type input-port)
(define-error-type output-port)
(define-error-type list)
(define-error-type pair)
(define-error-type blob)
(define-error-type vector)
(define-error-type symbol)
(define-error-type keyword)
(define-error-type string)
(define-error-type char)
(define-error-type boolean)
(define-error-type plist "property-list")
(define-error-type alist "association-list")

(define (*error-structure loc obj kndnam tag argnam)
	(error-argument-type loc obj (conc kndnam #\space tag) argnam) )

(define (error-structure loc obj tag #!optional argnam)
	(*error-structure loc obj "structure" tag argnam) )

(define (error-record loc obj tag #!optional argnam)
	(*error-structure loc obj "record" tag argnam) )

(define (error-record-type loc obj tag #!optional argnam)
	(*error-structure loc obj "record-type" tag argnam) )

(define (error-interval loc num lft min max rgt #!optional argnam)
  (##sys#signal-hook #:bounds-error loc
    (conc (make-bad-argument-message argnam) " must be in " lft min #\space max rgt)
    num) )

(define (error-closed-interval loc num min max #!optional argnam)
  (error-interval loc num '|[| min max '|]| argnam))

(define (error-open-interval loc num min max #!optional argnam)
  (error-interval loc num '|]| min max '|[| argnam))

(define (error-half-open-interval loc num min max #!optional argnam)
  (error-interval loc num '|]| min max '|]| argnam))

(define (error-half-closed-interval loc num min max #!optional argnam)
  (error-interval loc num '|[| min max '|[| argnam))

(define (error-minimum-argument-count loc argc minargc)
  (##sys#error-hook (foreign-value "C_BAD_MINIMUM_ARGUMENT_COUNT_ERROR" int) loc
		    minargc argc #f) )

(define (error-argument-count loc argc maxargc)
  (##sys#error-hook (foreign-value "C_BAD_ARGUMENT_COUNT_ERROR" int) loc
		    maxargc argc #f) )

;; Backwards

(define error-cardinal-fixnum error-natural-fixnum)
(define error-cardinal-integer error-natural-integer)
(define error-cardinal-number error-natural-number)

) ;module type-errors
