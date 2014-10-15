;;;; test-support.scm -- runtime for test extension
;;
;; Copyright (c) 2007-2014 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library regex data-structures extras ports)
(import scheme chicken regex data-structures extras ports)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-group representation

(define (make-test-group name)
  (list name
        (cons 'start-time (current-seconds))
        (cons 'start-milliseconds (current-milliseconds))))

(define test-group-name car)

(define (test-group-ref group field . o)
  (apply assq-ref (cdr group) field o))

(define (test-group-set! group field value)
  (cond ((assq field (cdr group))
         => (lambda (x) (set-cdr! x value)))
        (else (set-cdr! group (cons (cons field value) (cdr group))))))

(define (test-group-inc! group field)
  (cond ((assq field (cdr group))
         => (lambda (x) (set-cdr! x (+ 1 (cdr x)))))
        (else (set-cdr! group (cons (cons field 1) (cdr group))))))

(define (test-group-push! group field value)
  (cond ((assq field (cdr group))
         => (lambda (x) (set-cdr! x (cons value (cdr x)))))
        (else (set-cdr! group (cons (cons field (list value)) (cdr group))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define (every pred ls)
  (let lp ((ls ls))
    (or (null? ls) (and (pred (car ls)) (lp (cdr ls))))))

(define (assq-ref ls key . o)
  (cond ((assq key ls) => cdr)
        ((pair? o) (car o))
        (else #f)))

(define (approx-equal? a b epsilon)
  (cond
   ((> (abs a) (abs b))
    (approx-equal? b a epsilon))
   ((zero? b)
    (< (abs a) epsilon))
   (else
    (< (abs (/ (- a b) b)) epsilon))))

;; partial pretty printing to abbreviate `quote' forms and the like
(define (write-to-string x)
  (with-output-to-string
    (lambda ()
      (let wr ((x x))
        (if (pair? x)
            (cond
              ((and (symbol? (car x)) (pair? (cdr x)) (null? (cddr x))
                    (assq (car x)
                          '((quote . "'") (quasiquote . "`")
                            (unquote . ",") (unquote-splicing . ",@"))))
               => (lambda (s) (display (cdr s)) (wr (cadr x))))
              (else
               (display "(")
               (wr (car x))
               (let lp ((ls (cdr x)))
                 (cond ((pair? ls)
                        (display " ")
                        (wr (car ls))
                        (lp (cdr ls)))
                       ((not (null? ls))
                        (display " . ")
                        (write ls))))
               (display ")")))
            (write x))))))

(define (truncate-source x width . o)
  (let* ((str (write-to-string x))
         (len (string-length str)))
    (cond
      ((<= len width)
       str)
      ((and (pair? x) (eq? 'let (car x)))
       (if (and (pair? o) (car o))
           (truncate-source (car (reverse x)) width #t)
           (string-append "..."
                          (truncate-source (car (reverse x)) (- width 3) #t))))
      ((and (pair? x) (eq? 'call-with-current-continuation (car x)))
       (truncate-source (cons 'call/cc (cdr x)) width (and (pair? o) (car o))))
      (else
       (string-append
        (substring str 0 (min (max 0 (- width 3)) (string-length str)))
        "...")))))

(define (test-get-name! info)
  (or
   (assq-ref info 'name)
   (assq-ref info 'gen-name)
   (let ((name
          (cond
            ((assq-ref info 'source)
             => (lambda (src)
                  (truncate-source src (- (current-column-width) 12))))
            ((current-test-group)
             => (lambda (g)
                  (string-append
                   "test-"
                   (number->string (test-group-ref g 'count 0)))))
            (else ""))))
     (if (pair? info)
         (set-cdr! info (cons (cons 'gen-name name) (cdr info))))
     name)))

(define (test-print-name info . indent)
  (let ((width (- (current-column-width)
                  (or (and (pair? indent) (car indent)) 0)))
        (name (test-get-name! info)))
    (display name)
    (display " ")
    (let ((diff (- width 9 (string-length name))))
      (cond
       ((positive? diff)
        (display (make-string diff #\.)))))
    (display " ")
    (flush-output)))

(define (test-group-indent-width group)
  (let ((level (max 0 (+ 1 (- (test-group-ref group 'level 0)
                              (test-first-indentation))))))
    (* 4 (min level (test-max-indentation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi tools

(define (red x) (string-append "\x1B[31m" (->string x) "\x1B[0m"))
(define (green x) (string-append "\x1B[32m" (->string x) "\x1B[0m"))
(define (yellow x) (string-append "\x1B[33m" (->string x) "\x1B[0m"))
;; (define (blue x) (string-append "\x1B[34m" (->string x) "\x1B[0m"))
;; (define (magenta x) (string-append "\x1B[35m" (->string x) "\x1B[0m"))
;; (define (cyan x) (string-append "\x1B[36m" (->string x) "\x1B[0m"))
(define (bold x) (string-append "\x1B[1m" (->string x) "\x1B[0m"))
(define (underline x) (string-append "\x1B[4m" (->string x) "\x1B[0m"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-run expect expr info)
  (if (and (cond ((current-test-group)
                  => (lambda (g) (not (test-group-ref g 'skip-group?))))
                 (else #t))
           (every (lambda (f) (f info)) (current-test-filters)))
    ((current-test-applier) expect expr info)
    ((current-test-skipper) expect expr info)))

(define (test-default-applier expect expr info)
  (let* ((group (current-test-group))
         (verbose? (or (not group) (test-group-ref group 'verbosity)))
         (indent (and group (test-group-indent-width group))))
    (cond
     (verbose?
      (cond
       ((and group
             (equal? 0 (test-group-ref group 'count 0))
             (zero? (test-group-ref group 'subgroups-count 0))
             (test-group-ref group 'verbosity))
        (newline)
        (print-header-line
         (string-append "testing " (or (test-group-name group) ""))
         (or indent 0))))
      (if (and indent (positive? indent))
          (display (make-string indent #\space)))
      (test-print-name info indent)))
    (let ((expect-val
           (condition-case
               (expect)
             (e ()
                (warning "bad expect value")
                (print-error-message e)
                #f))))
      (condition-case
          (let ((res (expr)))
            (let ((status
                   (if (and (not (assq-ref info 'expect-error))
                            (if (assq-ref info 'assertion)
                                res
                                ((current-test-comparator) expect-val res)))
                       'PASS
                       'FAIL))
                  (info `((result . ,res) (expected . ,expect-val) ,@info)))
              ((current-test-handler) status expect expr info)))
        (e ()
           ((current-test-handler)
            (if (assq-ref info 'expect-error) 'PASS 'ERROR)
            expect
            expr
            (append `((exception . ,e) (trace . ,get-call-chain)) info)))))))

(define (test-default-skipper expect expr info)
  ((current-test-handler) 'SKIP expect expr info))

(define (test-default-handler status expect expr info)
  ;; update group info
  (let* ((group (current-test-group))
         (verbose? (or (not group) (test-group-ref group 'verbosity)))
         (indent
          (cond (group
                 => (lambda (group)
                      (make-string (+ 4 (or (test-group-indent-width group) 0))
                                   #\space)))
                (else (make-string 4 #\space)))))
    (cond ((current-test-group)
           => (lambda (group)
                (if (not (eq? 'SKIP status))
                    (test-group-inc! group 'count))
                (test-group-inc! group status))))
    (cond ((not (eq? 'SKIP status))
           (test-total-count (+ 1 (test-total-count)))))
    (cond
     ((or (eq? status 'FAIL) (eq? status 'ERROR))
      (test-failure-count (+ 1 (test-failure-count)))))
    (cond
     ((not verbose?)
      (write-char (case status
                    ((PASS) #\.) ((FAIL) #\x) ((ERROR) #\!) (else #\space)))
      (if (zero? (modulo (test-group-ref group 'count)
                         (current-column-width)))
          (newline)))
     ((not (eq? status 'SKIP))
      ;; display status
      (display "[")
      (if (not (eq? status 'ERROR)) (display " ")) ; pad
      (display ((if (test-ansi?)
                    (case status
                      ((ERROR) (compose underline red))
                      ((FAIL) red)
                      ((SKIP) yellow)
                      (else green))
                    identity)
                status))
      (display "]")
      (newline)
      ;; display status explanation
      (cond ((not (eq? status 'PASS))
             (display indent)))
      (cond
       ((eq? status 'ERROR)
        (cond ((assq 'exception info)
               => (lambda (e)
                    (print-error-message (cdr e) (current-output-port)))))
        ;;(print-call-chain (current-output-port) 10)
        )
       ((and (eq? status 'FAIL) (assq-ref info 'assertion))
        (display "assertion failed\n"))
       ((and (eq? status 'FAIL) (assq-ref info 'expect-error))
        (display "expected an error but got ")
        (write (assq-ref info 'result)) (newline))
       ((eq? status 'FAIL)
        (display "expected ") (write (assq-ref info 'expected))
        (display " but got ") (write (assq-ref info 'result)) (newline)))
      ;; display line, source and values info
      (cond
       ((or (not (current-test-group))
            (test-group-ref (current-test-group) 'verbosity))
        (case status
          ((FAIL ERROR)
           (cond
            ((assq-ref info 'line-number)
             => (lambda (line)
                  (display indent)
                  (display "in line ")
                  (write line)
                  (cond ((assq-ref info 'file-name)
                         => (lambda (file) (display " of file ") (write file))))
                  (newline))))
           (cond
            ((assq-ref info 'source)
             => (lambda (s)
                  (if (or (assq-ref info 'name)
                          (> (string-length (write-to-string s))
                             (current-column-width)))
                      (for-each
                       (lambda (line) (display indent) (display line) (newline))
                       (string-split
                        (with-output-to-string (lambda () (pp s)))
                        "\n"))))))
           (cond
            ((assq-ref info 'values)
             => (lambda (v)
                  (for-each
                   (lambda (v)
                     (display indent) (display (car v))
                     (display ": ") (write (cdr v)) (newline))
                   v)))))
          ))))))
  status)

(define (test-default-group-reporter group)
  (define (plural word n)
    (if (= n 1) word (string-append word "s")))
  (define (percent n d)
    (string-append " (" (number->string (/ (round (* 1000 (/ n d))) 10)) "%)"))
  (let* ((end-time (current-seconds))
         (end-milliseconds (current-milliseconds))
         (start-time (test-group-ref group 'start-time))
         (start-milliseconds
          (or (test-group-ref group 'start-milliseconds) 0))
         (duration
          (if (and start-time (> (- end-time start-time) 60))
              (/ (- (+ (* end-time 1000) end-milliseconds)
                    (+ (* start-time 1000) start-milliseconds))
                 1000)
              (/ (- end-milliseconds start-milliseconds) 1000)))
         (count (or (test-group-ref group 'count) 0))
         (pass (or (test-group-ref group 'PASS) 0))
         (fail (or (test-group-ref group 'FAIL) 0))
         (err (or (test-group-ref group 'ERROR) 0))
         (skip (or (test-group-ref group 'SKIP) 0))
         (subgroups-count (or (test-group-ref group 'subgroups-count) 0))
         (subgroups-pass (or (test-group-ref group 'subgroups-pass) 0))
         (indent (make-string (or (test-group-indent-width group) 0) #\space)))
    (if (not (test-group-ref group 'verbosity))
        (newline))
    (cond
     ((or (positive? count) (positive? subgroups-count))
      (if (not (= count (+ pass fail err)))
          (warning "inconsistent count:" count pass fail err))
      (display indent)
      (cond
       ((positive? count)
        (write count) (display (plural " test" count))))
      (if (and (positive? count) (positive? subgroups-count))
          (display " and "))
      (cond
       ((positive? subgroups-count)
        (write subgroups-count)
        (display (plural " subgroup" subgroups-count))))
      (display " completed in ") (write duration) (display " seconds")
      (cond
       ((not (zero? skip))
        (display " (") (write skip) (display (plural " test" skip))
        (display " skipped)")))
      (display ".") (newline)
      (cond ((positive? fail)
             (display indent)
             (display
              ((if (test-ansi?) red identity)
               (string-append
                (number->string fail) (plural " failure" fail)
                (percent fail count) ".")))
             (newline)))
      (cond ((positive? err)
             (display indent)
             (display
              ((if (test-ansi?) (compose underline red) identity)
               (string-append
                (number->string err) (plural " error" err)
                (percent err count) ".")))
             (newline)))
      (cond
       ((positive? count)
        (display indent)
        (display
         ((if (and (test-ansi?) (= pass count)) green identity)
          (string-append
           (number->string pass) " out of " (number->string count)
           (percent pass count) (plural " test" pass) " passed.")))
        (newline)))
      (cond
       ((positive? subgroups-count)
        (display indent)
        (display
         ((if (and (test-ansi?) (= subgroups-pass subgroups-count))
              green identity)
          (string-append
           (number->string subgroups-pass) " out of "
           (number->string subgroups-count)
           (percent subgroups-pass subgroups-count)
           (plural " subgroup" subgroups-pass) " passed.")))
        (newline)))
      ))
    (print-header-line
     (string-append "done testing " (or (test-group-name group) ""))
     (or (test-group-indent-width group) 0))
    (newline)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-equal? expect res)
  (or (equal? expect res)
      (and (number? expect)
           (inexact? expect)
           (inexact? res)
           (approx-equal? expect res (current-test-epsilon)))))

(define (print-header-line str . indent)
  (let* ((header (string-append
                  (make-string (if (pair? indent) (car indent) 0) #\space)
                  "-- " str " "))
         (len (string-length header)))
      (display (if (test-ansi?) (bold header) header))
      (display (make-string (max 0 (- (current-column-width) len)) #\-))
      (newline)))

(define (test-begin . o)
  (let* ((name (if (pair? o) (car o) ""))
         (group (make-test-group name))
         (parent (current-test-group)))
    (cond
     ((and parent
           (equal? 0 (test-group-ref parent 'count 0))
           (zero? (test-group-ref parent 'subgroups-count 0))
           (test-group-ref parent 'verbosity))
      (newline)
      (print-header-line
       (string-append "testing " (test-group-name parent))
       (or (test-group-indent-width parent) 0))))
    (test-group-set! group 'parent parent)
    (test-group-set! group 'verbosity
                     (if parent
                         (test-group-ref parent 'verbosity)
                         (current-test-verbosity)))
    (test-group-set! group 'level
                     (if parent
                         (+ 1 (test-group-ref parent 'level 0))
                         0))
    (test-group-set!
     group
     'skip-group?
     (or (and parent (test-group-ref parent 'skip-group?))
         (not (every (lambda (f) (f group)) (current-test-group-filters)))))
    (current-test-group group)))

(define (test-end . o)
  (cond
    ((current-test-group)
     => (lambda (group)
          (if (and (pair? o) (not (equal? (car o) (test-group-name group))))
            (warning "mismatched test-end:" (car o) (test-group-name group)))
          (let ((parent (test-group-ref group 'parent)))
            (cond
             ((not (test-group-ref group 'skip-group?))
              ;; only report if there's something to say
              ((current-test-group-reporter) group)
              (cond
               (parent
                (test-group-inc! parent 'subgroups-count)
                (cond
                 ((and (zero? (test-group-ref group 'FAIL 0))
                       (zero? (test-group-ref group 'ERROR 0))
                       (= (test-group-ref group 'subgroups-pass 0)
                          (test-group-ref group 'subgroups-count 0)))
                  (test-group-inc! parent 'subgroups-pass)))))))
            (current-test-group parent)
            group)))))

(define (test-exit . o)
  (exit (if (positive? (test-failure-count)) (if (pair? o) (car o) 1) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

(define current-test-group (make-parameter #f))
(define current-test-verbosity
  (make-parameter
   (cond ((get-environment-variable "TEST_QUIET") => (lambda (s) (equal? s "0"))) (else #t))))
(define current-test-epsilon (make-parameter 1e-5))
(define current-test-comparator (make-parameter test-equal?))
(define current-test-applier (make-parameter test-default-applier))
(define current-test-handler (make-parameter test-default-handler))
(define current-test-skipper (make-parameter test-default-skipper))
(define current-test-group-reporter
  (make-parameter test-default-group-reporter))
(define test-failure-count (make-parameter 0))
(define test-total-count (make-parameter 0))

(define test-first-indentation
  (make-parameter
   (or (cond ((get-environment-variable "TEST_FIRST_INDENTATION") => string->number) (else #f))
       1)))

(define test-max-indentation
  (make-parameter
   (or (cond ((get-environment-variable "TEST_MAX_INDENTATION") => string->number) (else #f))
       5)))

(define (string->info-matcher str)
  (let ((rx (regexp str)))
    (lambda (info)
      (cond ((test-get-name! info)
             => (lambda (n) (string-search rx n)))
            (else #f)))))

(define (string->group-matcher str)
  (let ((rx (regexp str)))
    (lambda (group)
      (string-search rx (car group)))))

(define (getenv-filter-list proc name . o)
  (cond
    ((get-environment-variable name)
     => (lambda (s)
          (condition-case
           (let ((f (proc s)))
             (list (if (and (pair? o) (car o)) (complement f) f)))
           (e ()
             (warning
              (string-append "invalid filter regexp '" s
                             "' from environment variable: " name))
             (print-error-message e)
             '()))))
    (else '())))

(define current-test-filters
  (make-parameter
   (append (getenv-filter-list string->info-matcher "TEST_FILTER")
           (getenv-filter-list string->info-matcher "TEST_REMOVE" #t))))

(define current-test-group-filters
  (make-parameter
   (append (getenv-filter-list string->group-matcher "TEST_GROUP_FILTER")
           (getenv-filter-list string->group-matcher "TEST_GROUP_REMOVE" #t))))

(define current-column-width
  (make-parameter
   (or (cond ((get-environment-variable "TEST_COLUMN_WIDTH") => string->number) (else #f))
       78)))

(define test-ansi?
  (make-parameter
   (cond
    ((get-environment-variable "TEST_USE_ANSI")
     => (lambda (s) (not (equal? s "0"))))
    (else
     (and (##sys#tty-port? (current-output-port))
          (member (get-environment-variable "TERM")
                  '("xterm" "xterm-color" "xterm-256color" "rxvt" "kterm"
                    "linux" "screen" "screen-256color" "vt100")))))))
