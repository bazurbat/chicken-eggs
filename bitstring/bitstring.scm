;; bitstirng module implements the subset of Erlang bit syntax.

(module bitstring
  (bitmatch
   bitpacket
   bitconstruct
   bitstring-pattern-continue
   make-bitstring
   bitstring?
   bitstring-length
   ->bitstring
   vector->bitstring
   u8vector->bitstring
   string->bitstring
   blob->bitstring
   bitstring-read
   bitstring-share
   bitstring-create
   bitstring-reserve
   bitstring=?
   bitstring-append
   bitstring-append! 
   bitstring-not
   bitstring-bit-set?
   bitstring-reverse
   bitstring->list
   bitstring->blob
   bitstring->string
   bitstring->vector
   bitstring->u8vector
   bitstring->integer
   bitstring->integer-big
   bitstring->integer-little
   bitstring->integer-host
   integer->bitstring
   integer->bitstring-big
   integer->bitstring-little
   integer->bitstring-host
   bitstring-start
   bitstring-end
   bitstring-buffer
   bitstring-getter
   bitstring->half
   bitstring->single
   single->bitstring
   bitstring->double
   double->bitstring
   list->bitstring)

  (import scheme chicken extras foreign)
  (require-extension srfi-1 srfi-4)

(define-syntax symbol??
  (er-macro-transformer
    (lambda (e r c)
      (let* ((args (cdr e))
      	     (name (car args))
      	     (yes (cadr args))
      	     (no (caddr args)))
      	(if (symbol? name) yes no)))))

; (expand-value x char str int)
(define-syntax expand-value
  (er-macro-transformer
    (lambda (e r c)
      (let* ((args (cdr e))
             (name (car args))
             (char-branch (cadr args))
             (string-branch (caddr args))
             (integer-branch (cadddr args)))
        (cond
          ((char? name) char-branch)
          ((string? name) string-branch)
          ((integer? name) integer-branch)
          (else (error "invalid value" `name)))))))

(define-syntax bitpacket
  (syntax-rules ()
    ((_ name fields ...)
      (define-syntax name
      	(er-macro-transformer
      	  ;; (name (mode stream handler) args ...)
      	  (lambda (e r c)
      	    (let* ((context (cadr e))
                   (mode (first context))
                   (stream (second context))
                   (handler (third context))
      	    	     (args (cddr e)))
      	      ;; inline packet fields
              ;(print "inline:" mode stream handler " fields:" `(fields ...) " args:" args)
      	      `(bitstring-pattern-continue ,mode ,stream ,handler (fields ...) ,args)))
                  
      	  )))))

(define-syntax bitstring-pattern-continue
  (syntax-rules ()
    ((_ mode stream handler (fields ...) (rest ...))
      (bitstring-pattern mode stream handler fields ... rest ...))))

(define-syntax capture-handler
  (syntax-rules ()
    ((_ (handler ...))
      (lambda () handler ...))))

(define-syntax bitconstruct
  (syntax-rules ()
    ((_ patterns ...)
      (let ((bstr (bitstring-reserve 64)))
        (bitstring-pattern "write" bstr "no-handler" patterns ...)))))

(define-syntax bitmatch
  (syntax-rules ()
    ((_ value patterns ...)
      ;; invoke user code with captured variables
      ((let ((bstr (->bitstring value)))
        (or (bitmatch-pattern-list bstr patterns ...)))))))

(define-syntax bitmatch-pattern-list
  (syntax-rules (else ->)
    ((_ bstr (else handler ...))
      (capture-handler (handler ...)))
    ((_ bstr (pattern ... -> handler) rest ...)
      (bitmatch-pattern-list bstr ((pattern ...) handler) rest ...))
    ((_ bstr (pattern ... -> handler ...) rest ...)
      (bitmatch-pattern-list bstr ((pattern ...) handler ...) rest ...))
    ((_ bstr ((pattern ...) handler ...))
      (or
        (bitmatch-pattern bstr (handler ...) pattern ...)
        (error 'bitstring-match-failure)))
    ((_ bstr ((pattern ...) handler ...) rest ...)
      (or
        (bitmatch-pattern bstr (handler ...) pattern ...)
        (bitmatch-pattern-list bstr rest ...)))))

(define-syntax bitmatch-pattern
  (syntax-rules ()
    ((_ bstr handler pattern ...)
      ; share bitstring instance
      (let ((stream (->bitstring bstr)))
        (bitstring-pattern "read" stream handler pattern ...)))))

(define-syntax bitstring-pattern
  (syntax-rules (big little host bitstring check float double bitpacket signed unsigned boolean)
    ; all patterns take expansion
    ((_ "read" stream handler)
      (and
        ; ensure that no more bits left
        (zero? (bitstring-length stream))
        (capture-handler handler)))
    ((_ "write" stream handler)
      stream)
    ; zero-length bitstring
    ((_ "read" stream handler ())
      (and
        (zero? (bitstring-length stream))
        (capture-handler handler)))
    ((_ "write" stream handler ())
      stream)
    ; user guard expression
    ((_ mode stream handler (check guard) rest ...)
      (and
        guard
        (bitstring-pattern mode stream handler rest ...)))
    ; evaluate constructing function
    ((_ "write" stream handler ((VALUE ...) bitstring) rest ...)
      (and-let* ((tmp (VALUE ...))
                 (bits (bitstring-length tmp)))
        (bitstring-pattern "write" stream handler (tmp bits bitstring) rest ...)))
    ; bitpacket
    ((_ mode stream handler (NAME bitpacket) rest ...)
      (bitstring-packet-expand mode stream handler NAME rest ...))
    ; bitpacket at tail
    ((_ mode stream handler (NAME bitpacket))
      (bitstring-packet-expand mode stream handler NAME))
    ; allow in bitconstruct dont type length
    ((_ "write" stream handler (NAME bitstring) rest ...)
      (bitstring-pattern-expand "write" stream NAME
        (bitstring-pattern "write" stream handler rest ...)))
    ; greedy bitstring
    ((_ mode stream handler (NAME bitstring))
      (bitstring-pattern-expand mode stream NAME
        (bitstring-pattern mode stream handler)))
    ; boolean
    ((_ mode stream handler (NAME boolean))
     (bitstring-pattern-expand mode stream NAME 8 (boolean big unsigned)
       (bitstring-pattern mode stream handler)))
    ; boolean bits
    ((_ mode stream handler (NAME BITS boolean))
     (bitstring-pattern-expand mode stream NAME BITS (boolean big unsigned)
       (bitstring-pattern mode stream handler)))
    ; boolean bits endian
    ((_ mode stream handler (NAME BITS boolean ENDIAN))
     (bitstring-pattern-expand mode stream NAME BITS (boolean ENDIAN unsigned)
       (bitstring-pattern mode stream handler)))
    ; double 64
    ((_ mode stream handler (NAME double) rest ...)
      (bitstring-pattern-expand mode stream NAME 64 (float big)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME double ENDIAN) rest ...)
      (bitstring-pattern-expand mode stream NAME 64 (float ENDIAN)
        (bitstring-pattern mode stream handler rest ...)))
    ; float 32
    ((_ mode stream handler (NAME float) rest ...)
      (bitstring-pattern-expand mode stream NAME 32 (float big)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME float ENDIAN) rest ...)
      (bitstring-pattern-expand mode stream NAME 32 (float ENDIAN)
        (bitstring-pattern mode stream handler rest ...)))
    ; float bits
    ((_ mode stream handler (NAME BITS float ENDIAN) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (float ENDIAN)
        (bitstring-pattern mode stream handler rest ...)))
    ; bigendian
    ((_ mode stream handler (NAME BITS big) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (big unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ; littleendian
    ((_ mode stream handler (NAME BITS little) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (little unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ; same endianness as host
    ((_ mode stream handler (NAME BITS host) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (host unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ; bitstring
    ((_ mode stream handler (NAME BITS bitstring) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS bitstring
        (bitstring-pattern mode stream handler rest ...)))
    ; integer attibutes
    ((_ mode stream handler (NAME BITS signed) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (big signed)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME BITS unsigned) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (big unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME BITS signed ENDIAN) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (ENDIAN signed)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME BITS unsigned ENDIAN) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (ENDIAN unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME BITS ENDIAN SIGNED) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (ENDIAN SIGNED)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME signed) rest ...)
      (bitstring-pattern-expand mode stream NAME 8 (big signed)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME unsigned) rest ...)
      (bitstring-pattern-expand mode stream NAME 8 (big unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ; rewrite by default to (NAME BITS (big unsigned))
    ((_ mode stream handler (NAME BITS) rest ...)
      (bitstring-pattern mode stream handler (NAME BITS big) rest ...))
    ; rewrite immidiate value
    ((_ mode stream handler (NAME) rest ...)
      (symbol?? NAME
        ; yes
        (bitstring-pattern mode stream handler (NAME 8 big) rest ...)
        ; no
        (bitstring-pattern-value mode stream handler (NAME) rest ...)))
    ; dismiss other pattern forms
    ((_ mode stream handler . rest)
      (error "bitstring-malformed-pattern" `mode `stream `handler `rest))))

(define-syntax bitstring-pattern-value
  (syntax-rules ()
    ((_ mode stream handler (VALUE) rest ...)
      (expand-value VALUE
        ; char
        (bitstring-pattern mode stream handler ((char->integer VALUE) 8 big) rest ...)
        ; string
        (bitstring-pattern mode stream handler
          (VALUE (* 8 (string-length VALUE)) bitstring) rest ...)
        ; integer
        (bitstring-pattern mode stream handler (VALUE 8 big) rest ...)))))

(define-syntax bitstring-packet-expand
  (syntax-rules ()
    ((_ mode stream handler name)
      (name (mode stream handler)))
    ((_ mode stream handler name rest ...)
      (name (mode stream handler) rest ...))))

(define-syntax bitstring-pattern-expand
  (syntax-rules ()
    ((_ "write" stream name continuation)
      (and-let* ((tmp (->bitstring name)))
        ;(print "write-expand:" `stream " name:" `name)
      	(bitstring-append! stream tmp)
      	continuation))
    ((_ "write" stream name bits type continuation)
      (and-let* ((tmp (bitstring-write-expand name bits type)))
        ;(print "write-expand:" `stream " name:" `name)
      	(bitstring-append! stream tmp)
      	continuation))
    ((_ "read" stream name continuation) ; read all rest bytes
      (symbol?? name
      	(and-let* ((bits (bitstring-length stream))
      	           (name (bitstring-read stream bits)))
          ;(print "read-expand: " `(name bits type) " rest: " `continuation)         
      	  continuation)
        (syntax-error "not a symbol name" `name)))
    ((_ "read" stream name bits type continuation)
      (symbol?? name
        (and-let* ((tmp (bitstring-read stream bits)))
         (let ((name (bitstring-read-expand tmp bits type)))
           ;(print "expand-symbol: " `(name bits type) " rest: " `continuation)
           continuation))
        (and-let* ((tmp (bitstring-read stream bits)))
          ;(print "expand-value: " `(name bits type) " rest: " `continuation)
      	  (and
            (optimize-compare tmp name bits type)
      	    continuation))))))

(define-syntax optimize-compare
  (syntax-rules ()
    ((_ tmp value bits (ENDIAN SIGNED))
     (= value (bitstring-read-integer tmp bits ENDIAN SIGNED)))
    ((_ tmp value bits type)
     (bitstring=? tmp (bitstring-write-expand value bits type)))))

(define-syntax float-reorder-bytes
  (syntax-rules (little big host)
    ((_ host tmp)
     (cond-expand
       (little-endian (float-reorder-bytes little tmp))
       (else (float-reorder-bytes big tmp))))
    ((_ little tmp)
     (cond-expand
       (little-endian tmp)
       (else (bitstring-reverse tmp 8))))
    ((_ big tmp)
     (cond-expand
       (little-endian (bitstring-reverse tmp 8))
       (else tmp)))))

(define-syntax bitstring-read-expand
  (syntax-rules (bitstring float boolean)
    ((_ tmp 32 (float ENDIAN))
     (bitstring->single (float-reorder-bytes ENDIAN tmp)))
    ((_ tmp 64 (float ENDIAN))
     (bitstring->double (float-reorder-bytes ENDIAN tmp)))
    ((_ tmp bits (boolean ENDIAN SIGNED))
     (not (zero? (bitstring-read-integer tmp bits ENDIAN SIGNED))))
    ((_ tmp bits (ENDIAN SIGNED))
     (bitstring-read-integer tmp bits ENDIAN SIGNED))
    ((_ tmp bits bitstring)
     tmp))) ; return bitstring as is

(define-syntax bitstring-read-integer
  (syntax-rules (big little host signed unsigned)
    ((_ tmp bits big signed)
     (if (bitstring-bit-set? tmp 0)
       (- (+ 1 (bitstring->integer-big (bitstring-not tmp))))
       (bitstring->integer-big tmp)))
    ((_ tmp bits little signed)
     (if (bitstring-bit-set? tmp (if (< bits 8) (sub1 bits) (- bits 8)))
       (- (+ 1 (bitstring->integer-little (bitstring-not tmp))))
       (bitstring->integer-little tmp)))
    ((_ tmp bits host signed)
      (cond-expand
        (little-endian (bitstring-read-integer tmp bits little signed))
        (else (bitstring-read-integer tmp bits big signed))))
    ((_ tmp bits big unsigned)
      (bitstring->integer-big tmp))
    ((_ tmp bits little unsigned)
      (bitstring->integer-little tmp))
    ((_ tmp bits host unsigned)
      (bitstring->integer-host tmp))
    ((_ tmp bits ENDIAN SIGNED)
      (syntax-error "invalid integer attibute" `ENDIAN `SIGNED))))

(define-syntax bitstring-write-expand
  (syntax-rules (bitstring float boolean)
    ((_ tmp 32 (float ENDIAN))
     (float-reorder-bytes ENDIAN (single->bitstring tmp)))
    ((_ tmp 64 (float ENDIAN))
     (float-reorder-bytes ENDIAN (double->bitstring tmp)))
    ((_ tmp bits (boolean ENDIAN SIGNED))
     (bitstring-write-integer (if tmp 1 0) bits ENDIAN SIGNED))
    ((_ tmp bits (ENDIAN SIGNED))
      (bitstring-write-integer tmp bits ENDIAN SIGNED))
    ((_ tmp bits bitstring)
      (if (bitstring? tmp)
      	tmp
        (->bitstring tmp)))))

(define-syntax bitstring-write-integer
  (syntax-rules (big little host signed unsigned)
    ((_ tmp bits big signed)
      (integer->bitstring-big tmp bits))
    ((_ tmp bits little signed)
      (integer->bitstring-little tmp bits))
    ((_ tmp bits host signed)
      (integer->bitstring-host tmp bits))
    ((_ tmp bits big unsigned)
      (integer->bitstring-big tmp bits))
    ((_ tmp bits little unsigned)
      (integer->bitstring-little tmp bits))
    ((_ tmp bits host unsigned)
      (integer->bitstring-host tmp bits))
    ((_ tmp bits ENDIAN SIGNED)
      (syntax-error "invalid integer attibute" `ENDIAN `SIGNED))))


;;;;;;;;;;;;;;;;;;;;;;
;; bitstring

(define-record bitstring
  start   ; buffer offset in bits
  end     ; buffer offset in bits
  buffer  ; any container with random access
  getter  ; (lambda (buffer index) -> byte)
  setter) ; (lambda (buffer index byte) -> void)

(define-record-printer (bitstring x out)
  (fprintf out "<bitstring ~A ~A ~A>"
    (bitstring-start x) (bitstring-end x) (bitstring-buffer x)))

(define (bitstring-length bs)
  (- (bitstring-end bs) (bitstring-start bs)))
  
; compute space required for {{n}} bits
(define (space-required n alignment)
  (+ (quotient n 8) (if (zero? (remainder n 8)) 0 1)))

(define (bitstring-create)
  (bitstring-reserve 128))

(define (bitstring-reserve size-in-bits)
  (let ((size (space-required size-in-bits 8)))
    (make-bitstring 0 0 (make-u8vector size 0) u8vector-ref u8vector-set!)))

(define (string->bitstring s)
  (make-bitstring 0 (* 8 (string-length s)) s
    (lambda (str index) (char->integer (string-ref str index)))
    (lambda (str index byte) (string-set! str index (integer->char byte)))))

(define (vector->bitstring v)
  (make-bitstring 0 (* 8 (vector-length v)) v vector-ref vector-set!))

(define (u8vector->bitstring v)
  (make-bitstring 0 (* 8 (u8vector-length v)) v u8vector-ref u8vector-set!))

(define (blob->bitstring b)
  (u8vector->bitstring (blob->u8vector/shared b)))

(define (->bitstring x)
  (cond
    ((bitstring? x)
      (bitstring-share x (bitstring-start x) (bitstring-end x)))
    ((u8vector? x)
      (u8vector->bitstring x))
    ((string? x)
      (string->bitstring x))
    ((vector? x)
      (vector->bitstring x))
    ((blob? x)
      (u8vector->bitstring (blob->u8vector/shared x)))
    (else
      (error "bitstring-invalid-value" x))))

(define (bitstring-size-in-bytes bs)
  (let ((n (bitstring-length bs)))
    (+ (quotient n 8) (if (zero? (remainder n 8)) 0 1))))

(define (bitstring->blob bs #!optional (zero-extending 'left))
  (u8vector->blob/shared (bitstring->u8vector bs zero-extending)))

(define (bitstring->u8vector bs #!optional (zero-extending 'left))
  (let loop ((data bs)
             (index 0)
             (tmp (make-u8vector (space-required (bitstring-length bs) 8))))
    (bitmatch data
      (()
       tmp)
      (((value 8 bitstring) (rest bitstring))
       (u8vector-set! tmp index (bitstring->integer-big value))
       (loop rest (add1 index) tmp))
      (((value bitstring))
       (let ((len (bitstring-length value))
             (byte (bitstring->integer-big value)))
         (u8vector-set! tmp index (if (eq? zero-extending 'left)
                                    byte
                                    (fxshl byte (- 8 len))))
         tmp)))))

(define (bitstring->string bs)
  (list->string (map integer->char (bitstring->list bs 8))))

(define (bitstring->vector bs)
  (list->vector (bitstring->list bs 8)))

(define (bitstring->list bs #!optional (bits 1) (endian 'big))
  (bitstring->listn bs bits endian))

(define (bitstring->listn bs bits endian)
  (let loop ((data bs)
             (acc (list)))
    (bitmatch data
      (()
        (reverse acc))
      (((value bits bitstring) (rest bitstring))
        (loop rest (cons (bitstring->integer value endian) acc)))
      (((rest-value bitstring))
        (loop "" (cons (bitstring->integer rest-value endian) acc))))))

(define (list->bitstring lst #!optional (bits 1) (endian 'big))
  (let loop ((rest lst)
             (acc (bitstring-reserve (* (length lst) bits))))
    (if (null-list? rest)
      acc
      (loop (cdr rest) (bitstring-append! acc (integer->bitstring (car rest) bits endian))))))

(define (bitstring-reverse bs #!optional (bits 1) (endian 'big))
  (list->bitstring (reverse (bitstring->list bs bits endian)) bits endian))

(define (bitstring=? a b)
  (and
    (= (bitstring-length a) (bitstring-length b))
    (if (and (bytestring? a) (bytestring? b))
      (bytestring=? a b)
      (equal? (bitstring->list a 8) (bitstring->list b 8)))))

(define (bytestring? bs)
  (and (zero? (remainder (bitstring-start bs) 8))
       (zero? (remainder (bitstring-length bs) 8))))

(define (bytestring=? a b)
  (let ((alen (quotient (bitstring-length a) 8))
        (blen (quotient (bitstring-length b) 8))
        (e (quotient (bitstring-end a) 8)))
    (and (= alen blen)
      (let loop ((i (quotient (bitstring-start a) 8))
                 (j (quotient (bitstring-start b) 8)))
        (if (< i e)
          (if (= (bitstring-load-byte a i)
                 (bitstring-load-byte b j))
            (loop (add1 i) (add1 j))
               #f)
          #t)))))

(define (bitstring-load-byte bs index)
  ((bitstring-getter bs) (bitstring-buffer bs) index))

(define (bitstring-store-byte bs index value)
  ((bitstring-setter bs) (bitstring-buffer bs) index value))

; extract {{count}} bits starting from {{offset}}, {{value}} should'be 8 bit integer.
(define-inline (extract-bits value offset count)
  (fxshr (fxand (fxshl value offset) #xFF)
         (- 8 count)))

(define (bitstring-fold proc init bs)
  (let loop ((start (bitstring-start bs))
             (end (bitstring-end bs))
             (index (quotient (bitstring-start bs) 8))
             (drift (remainder (bitstring-start bs) 8))
             (count (- 8 (remainder (bitstring-start bs) 8)))
             (acc init))
    (let ((n (min (- end start) count)))
      (if (<= n 0)
        acc
        (loop (+ start n) end
              (add1 index) ; move index
              0 ; reset drift
              8 ; setup 8 bit chunk
              (proc (extract-bits (bitstring-load-byte bs index) drift n) n acc))))))

(define (bitstring-not bs)
  (let ((len (bitstring-length bs))
        (tmp (bitstring->u8vector bs 'right)))
    ((foreign-primitive void ((u8vector data) (int size))
      "int i; for(i=0;i<size;++i) data[i] = ~data[i];") tmp (u8vector-length tmp))
    (make-bitstring 0 len tmp u8vector-ref u8vector-set!)))

(define (bitstring-bit-set? bs n)
  (let ((start (bitstring-start bs))
        (end (bitstring-end bs)))
    (let* ((index (if (negative? n)
                    (+ end n)
                    (+ start n)))
           (byte-index (quotient index 8))
           (bit-index (- 7 (remainder index 8))))
      (if (and (<= start index) (< index end))
        (bit-set? (bitstring-load-byte bs byte-index) bit-index)
        (error "out of range" start end n)))))

(define (bitstring->integer-big bs)
  (bitstring-fold
    (lambda (value count result)
      (bitwise-ior (arithmetic-shift result count) value))
    0
    bs))

(define (bitstring->integer-little bs)
  (car (bitstring-fold
         (lambda (value count acc)
           (let ((result (car acc))
                 (shift (cdr acc)))
             (cons (bitwise-ior result (arithmetic-shift value shift))
                   (+ shift count))))
         (cons 0 0)
         bs)))

(define (integer->bitstring-little value count)
  (let loop ((start 0)
             (n (min count 8))
             (bs (bitstring-reserve count)))
    (bitstring-end-set! bs count)
    (if (<= count start)
      bs
      (let ((x (bitwise-and (arithmetic-shift value (- start)) 255)))
        (bitstring-store-byte bs (quotient start 8) (fxshl x (- 8 n)))
        (loop (+ start n) (min (- count start n) 8) bs)))))

(define (integer->bitstring-big value count)
  (let loop ((start count)
             (n (min count 8))
             (bs (bitstring-reserve count)))
    (bitstring-end-set! bs count)
    (if (<= start 0)
      bs
      (let ((x (bitwise-and (arithmetic-shift value (- n start)) 255)))
        (bitstring-store-byte bs (quotient (- count start) 8) (fxshl x (- 8 n)))
        (loop (- start n) (min start 8) bs)))))

(define (bitstring->integer bitstring endian)
  (case endian
    ((big)
      (bitstring->integer-big bitstring))
    ((little)
      (bitstring->integer-little bitstring))
    ((host)
      (bitstring->integer-host bitstring))
    (else
      (error "invalid endian value" `endian))))

(define bitstring->integer-host
  (cond-expand
    (little-endian bitstring->integer-little)
    (else bitstring->integer-big)))

(define integer->bitstring-host
  (cond-expand
    (little-endian integer->bitstring-little)
    (else integer->bitstring-big)))

(define (integer->bitstring value count endian)
  (case endian
    ('little
      (integer->bitstring-little value count))
    ('host
      (integer->bitstring-host value count))
    (else
      (integer->bitstring-big value count))))

(define (bitstring->half bs)
  (let ((s (bitstring-read bs 1))
        (e (bitstring-read bs 5))
        (m (bitstring-read bs 10)))
    (make-half-float
      (bitstring->integer-big s)
      (bitstring->integer-big e)
      (bitstring->integer-big m))))

(define (make-half-float signbit exponent mantissa)
  ;(newline)
  ;(print "s: " signbit " e: " exponent " m: " mantissa)
  (cond
    ((and (zero? exponent) (zero? mantissa))
      (if (zero? signbit) +0. -0.))
    ((= exponent 31)
      (if (zero? mantissa)
      	(if (zero? signbit) +inf.0 -inf.0)
      	(if (zero? signbit) +nan.0 -nan.0)))
    (else
      (let ((e (- exponent 15))
      	    (m (bitwise-ior #x400 mantissa)))
      	(let loop ((i 10) (s 1.) (f 0.))
      	  (let* ((x (arithmetic-shift 1 i))
      	         (b (bitwise-and m x)))
      	    (if (or (zero? i))
      	      (* f (expt 2 e) (if (zero? signbit) 1. -1.))
      	      (loop (- i 1) (/ s 2) (if (zero? b) f (+ f s))))))))))

(define float->uint32
  (foreign-lambda* void ((u8vector i) (float f))
    "*(uint32_t*)i = *(uint32_t*)&f;"))

(define double->uint64
  (foreign-lambda* void ((u8vector i) (double d))
    "*(uint64_t*)i = *(uint64_t*)&d;"))

(define uint32->float
  (foreign-lambda* float ((blob i))
    "C_return(*(float*)i);"))

(define uint64->double
  (foreign-lambda* double ((blob i))
    "C_return(*(double*)i);"))
    
(define (single->bitstring value)
    (let ((buf (make-u8vector 4)))
        (float->uint32 buf value)
        (->bitstring buf)))

(define (double->bitstring value)
    (let ((buf (make-u8vector 8)))
        (double->uint64 buf value)
        (->bitstring buf)))

(define (bitstring->single bs)
    (uint32->float (bitstring->blob bs)))

(define (bitstring->double bs)
    (uint64->double (bitstring->blob bs)))

(define (bitstring-share bs from to)
  (make-bitstring from to (bitstring-buffer bs) (bitstring-getter bs) (bitstring-setter bs)))
   
(define (bitstring-read bs n)
  (let ((from (bitstring-start bs))
        (to (+ (bitstring-start bs) n)))
    (and (<= to (bitstring-end bs))
      (let ((bs/shared (bitstring-share bs from to)))
        (bitstring-start-set! bs to)
        bs/shared))))

(define (bitstring-buffer-size bs)
  (let ((buffer (bitstring-buffer bs)))
    (* 8 ; return size in bits
      (cond
      	((u8vector? buffer)
      	  (u8vector-length buffer))
      	((string? buffer)
      	  (string-length buffer))
      	(else
      	  (abort "not implemented for this buffer type"))))))

(define (bitstring-buffer-resize bs size-in-bits)
  (let* ((new-size (space-required size-in-bits 32))
         (tmp (make-u8vector new-size 0))
         (used (bitstring-buffer-size bs)))
    (let copy ((i 0)
    	       (e (quotient used 8)))
      (when (< i e)
        (u8vector-set! tmp i (bitstring-load-byte bs i))
        (copy (+ i 1) e)))
    ; replace buffer with accessors
    (bitstring-buffer-set! bs tmp)
    (bitstring-getter-set! bs u8vector-ref)
    (bitstring-setter-set! bs u8vector-set!)))

(define (bitstring-required-length args)
  (fold
    (lambda (bs len)
      (+ len (bitstring-length bs)))
    0
    args))

(define (bitstring-append . args)
  (fold
    (lambda (bs acc)
      (bitstring-append! acc bs))
    (bitstring-reserve (bitstring-required-length args))
    args))

(define (bitstring-append! dst . args)
  (fold                      
    (lambda (bs acc)
      (bitstring-append2! acc bs))
    dst
    args))

(define (bitstring-append2! dest src)
  ; need ensure that dest buffer long enough
  (let ((required (bitstring-length src))
        (position (bitstring-end dest))
        (reserved (bitstring-buffer-size dest)))
    (when (< (- reserved position) required)
      (bitstring-buffer-resize dest
        (+ reserved (inexact->exact (* 0.50 reserved)) required)))
    (bitstring-fold
      (lambda (value nbits acc)
        (bitstring-append-safe! acc (fxshl value (- 8 nbits)) nbits))
      dest
      src)))

(define (bitstring-append-safe! bs value nbits)
  (let* ((position (bitstring-end bs))
         (index (quotient position 8))
         (drift (remainder position 8)))
    (if (zero? drift) 
      ; store aligned
      (begin
        (bitstring-store-byte bs index value)
        (bitstring-end-set! bs (+ position nbits)))
      ; store unaligned
      (let ((byte-src (bitstring-load-byte bs index))
            (byte-dst (fxshr value drift))
      	    (restbits (- 8 drift)))
        (bitstring-store-byte bs index (fxior byte-src byte-dst))
      	; store rest bits if didnt fit in current byte
      	(if (< restbits nbits)
          (bitstring-store-byte bs (+ index 1) (fxshl value restbits)))
        (bitstring-end-set! bs (+ position nbits))))
    bs));return bitstring

);module
