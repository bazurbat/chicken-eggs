;;
;; SCSH process form notation
;;
;; See http://www.scsh.net/docu/html/man-Z-H-3.html#node_chap_2
;;
;; Some minor changes due to Chicken- and R7RS-incompatible identifiers:
;; | was changed to pipe, |+ was changed to pipe+
;;
;; || wasn't changed, but it's really the zero-length symbol
;;
;; WARNING: Don't mix with threading unless you're using
;;          Chicken 4.8.1 rev 47b5be71 or later.
;;
;;; Copyright (c) 2012-2013, Peter Bex
;; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGE.

(module scsh-process
  (;; procedures
   exec-path exec-path* fork %fork fork/pipe %fork/pipe fork/pipe+ %fork/pipe+
   run/collecting* run/string* run/strings* run/port* run/file* run/sexp* run/sexps*

   ;; macros
   run/collecting run/string run/strings run/port run/file run/sexp run/sexps
   || && (& run-final-thunk maybe->string) (run maybe->string) (exec-epf maybe->string)

   process? wait)

(import chicken scheme data-structures)

(use extras utils files ports posix srfi-1 srfi-69)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process bookkeeping ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; This stuff is all required so we can more cleanly and simply run
;; processes without having to wait for all of them in user code.  We
;; need to keep a hash table around so that the user can still wait
;; for his own processes without the signal/chld handler interfering
;; with those.  It's a bit of hack the way we overwrite the regular
;; process-wait procedure from POSIX, but this allows us to
;; transparently mark off processes which were waited on by the user.

(define-record scsh-process pid exit-status ok?)

(define process? scsh-process?)

(define *scsh-pending-processes* (make-hash-table))

(define (add-scsh-pending-process! pid)
  (let ((process (make-scsh-process pid #f #f)))
    (hash-table-set! *scsh-pending-processes* pid process)
    process))

(define (remove-scsh-pending-process! pid)
  (hash-table-delete! *scsh-pending-processes* pid))

(define wait #f)

(let ((posix-process-wait process-wait))
  (set! process-wait
        (lambda (#!optional pid nohang)
          (receive (status ok? pid) (wait pid nohang) (values pid ok? status))))

  (set! wait
        (lambda (#!optional pid-or-process nohang)
          (unless (or (not pid-or-process)
                      (scsh-process? pid-or-process)
                      (number? pid-or-process))
            (error 'process-wait
                   "Not a scsh-type process object or pid"
                   pid-or-process))
          (let ((p (if (and pid-or-process (number? pid-or-process))
                       (hash-table-ref/default *scsh-pending-processes*
                                               pid-or-process #f)
                       pid-or-process)))
            (if (and p (scsh-process-exit-status p))
                (values (scsh-process-exit-status p)
                        (scsh-process-ok? p)
                        (scsh-process-pid p))
                (handle-exceptions exn
                  (if (and p (scsh-process-exit-status p)) ; Signal might've occurred
                      (values (scsh-process-exit-status p)
                              (scsh-process-ok? p)
                              (scsh-process-pid p))
                      (abort exn))
                  (receive (pid ok? status)
                    (posix-process-wait (and p (scsh-process-pid p)) nohang)
                    (cond
                     ((zero? pid) (values #f #f #f))
                     (else (when p
                             (scsh-process-exit-status-set! p status)
                             (scsh-process-ok?-set! p ok?))
                           (remove-scsh-pending-process! pid)
                           (values status ok? pid)))))))))

  (set-signal-handler!
   signal/chld
   ;; This workaround fixes the "signal-hander" type in the types.db of 4.8.0
   (let-syntax ((workaround
                 (cond-expand
                  (chicken-4.8
                   (syntax-rules ()
                     ((_ val) (the (or boolean (procedure (fixnum) . *)) val))))
                  (else (syntax-rules () ((_ val) val))))))
     (let ((old-handler (workaround (signal-handler signal/chld))))
       (lambda (signal)
         (for-each (lambda (pid)
                     (handle-exceptions exn
                       ;; User might have waited manually
                       (begin (remove-scsh-pending-process! pid) (void))
                       (receive (pid ok? status)
                         (posix-process-wait pid #t)
                         (unless (zero? pid)
                           (let ((p (hash-table-ref *scsh-pending-processes* pid)))
                             (scsh-process-exit-status-set! p status)
                             (scsh-process-ok?-set! p ok?)
                             ;; The GC can clean it up
                             (remove-scsh-pending-process! pid))))))
                   (hash-table-keys *scsh-pending-processes*))
         (when old-handler (old-handler signal)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution and forking helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Perhaps expose environment, and mess around with the path so that
;; execve can be used in a sensible way?  Scsh has its own PATH, so we could
;; use something similar to that, but it's more work.
(define (exec-path prog . args)
  (exec-path* prog args))

;; Internal variant, collecting args
(define (exec-path* prog args)
  (process-execute (maybe->string prog) (map maybe->string args)))

(define (fork/pipe #!optional thunk continue-threads?)
  (fork/pipe+ '((1 2 0)) thunk continue-threads?))

;; Run a thunk and exit 0 after the thunk returns.
;; If an exception occurs, handle it and exit 1.
(define (run-final-thunk thunk)
  (handle-exceptions exn
    ;; TODO: Figure out how SCSH does this.  It shows the error
    ;; on stderr in the REPL, but then still quits it.
    ;; If we just invoke current-handler, it'll get a second REPL
    (begin (print-error-message exn) (exit 1))
    (thunk)
    (exit 0)))

(define (fork #!optional thunk continue-threads?)
  (let ((pid (cond-expand
              (has-thread-killer
               (process-fork thunk (not continue-threads?)))
              (else ;; Ignore both args if thunk is #f, so #f won't be applied
               (if thunk (process-fork thunk) (process-fork))))))
    (and (not (zero? pid)) (add-scsh-pending-process! pid))))

(define %fork fork)

(define (fork/pipe+ conns #!optional thunk continue-threads?)
  ;; Blergh, this is silly overhead we don't really need
  (let* ((from-fds (map (lambda (x) (drop-right x 1)) conns))
         (to-fds (map last conns))
         (pipe-pairs (map (lambda _ (receive (create-pipe))) to-fds))
         (proc (fork #f continue-threads?)))
    (if (not proc)                      ; Child
        (begin
          (for-each (lambda (p from-fds-for-this-p)
                      ;; Close receiving ends of pipes in child.
                      (file-close (car p))
                      ;; Set up linkage from output fds to created pipes.
                      (for-each (lambda (from-fd)
                                  (duplicate-fileno (cadr p) from-fd))
                                from-fds-for-this-p)
                      ;; Not needed anymore after duplication is complete.
                      (file-close (cadr p)))
                    pipe-pairs from-fds)
          (if thunk (run-final-thunk thunk) #f))
        (begin                          ; Parent
          (for-each (lambda (p to-fd)
                      ;; Close sending end in parent.
                      (file-close (cadr p))
                      ;; Set up linkage from created pipes to the input fds.
                      (duplicate-fileno (car p) to-fd)
                      ;; No longer needed after duplication.
                      (file-close (car p)))
                    pipe-pairs to-fds)
          proc))))

;; TODO: Differentiate between fork and %fork
(define %fork/pipe fork/pipe)
(define %fork/pipe+ fork/pipe+)

(define (maybe->string s)
  (cond ((string? s) s)
        ((or (symbol? s) (number? s)) (->string s))
        (else (error "Expected a string, symbol or number"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Baroque procedural interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Documented under http://www.scsh.net/docu/html/man-Z-H-3.html#node_sec_2.4.2
(define (run/collecting* fds thunk)
  (let* ((temp-files (map (lambda (fd)
                            (let* ((file-name (create-temporary-file))
                                   (port (open-input-file file-name)))
                              (delete-file file-name)
                              port))
                          fds))
         (conns (map (lambda (from-fd temp-file)
                       (list from-fd (port->fileno temp-file)))
                     fds temp-files)))
    (apply values (wait (fork/pipe+ conns thunk)) temp-files)))

(define (run/port* thunk)
  (receive (in out)
    (create-pipe)
    (fork
     (lambda ()
       (run-final-thunk
        (lambda ()
          (file-close in)
          (duplicate-fileno out 1)
          (duplicate-fileno out 2)
          (with-output-to-port (open-output-file* out)
            (lambda ()
              (with-error-output-to-port (open-output-file* out) thunk)))))))
    (file-close out)
    (open-input-file* in)))

(define (run/file* thunk)
  (let* ((temp-file (create-temporary-file)))
    (wait                               ; This is peculiar
     (fork/pipe (lambda ()
                  (let ((fd (file-open temp-file open/wronly)))
                    (duplicate-fileno fd 1)
                    (duplicate-fileno fd 2)
                    (with-output-to-port (open-output-file* 1)
                      (lambda ()
                        (with-error-output-to-port (open-output-file* 2) thunk)))))))
    temp-file))

(define (run/string* thunk)
  (read-string #f (run/port* thunk)))
(define (run/strings* thunk)
  (read-lines (run/port* thunk)))
(define (run/sexp* thunk)
  (let* ((p (run/port* thunk))
         (res (read p)))
    (close-input-port p)
    res))
(define (run/sexps* thunk)
  (read-file (run/port* thunk)))

;;;;;;;;;;;;
;; Syntax ;;
;;;;;;;;;;;;

(define-syntax run/collecting
  (syntax-rules ()
    ((_ ?fds ?epf ...) (run/collecting* `?fds (lambda () (exec-epf ?epf ...))))))
(define-syntax run/file
  (syntax-rules ()
    ((_ ?epf ...) (run/file* (lambda () (exec-epf ?epf ...))))))
(define-syntax run/port
  (syntax-rules ()
    ((_ ?epf ...) (run/port* (lambda () (exec-epf ?epf ...))))))
(define-syntax run/string
  (syntax-rules ()
    ((_ ?epf ...) (run/string* (lambda () (exec-epf ?epf ...))))))
(define-syntax run/strings
  (syntax-rules ()
    ((_ ?epf ...) (run/strings* (lambda () (exec-epf ?epf ...))))))
(define-syntax run/sexp
  (syntax-rules ()
    ((_ ?epf ...) (run/sexp* (lambda () (exec-epf ?epf ...))))))
(define-syntax run/sexps
  (syntax-rules ()
    ((_ ?epf ...) (run/sexps* (lambda () (exec-epf ?epf ...))))))

(define-syntax &&
  (syntax-rules ()
    ((_ ?epf ...) (and (zero? (run ?epf)) ...))))
(define-syntax ||
  (syntax-rules ()
    ((_ ?epf ...) (or (zero? (run ?epf)) ...))))

(define-syntax &
  (syntax-rules ()
    ((_ ?epf ...)
     (fork (lambda () (run-final-thunk (lambda () (exec-epf ?epf ...))))))))

(define-syntax run
  (syntax-rules ()
    ((_ ?epf ...)
     (wait (& ?epf ...)))))

;; Perhaps this should really be a procedure?
(define-syntax setup-redirection
  (syntax-rules (< > << >> = - stdports)
    ((_ (< ?file-name)) (setup-redirection (< 0 ?file-name)))
    ((_ (<< ?object)) (setup-redirection (<< 0 ?object)))
    ((_ (> ?file-name)) (setup-redirection (> 1 ?file-name)))
    ((_ (>> ?file-name)) (setup-redirection (>> 1 ?file-name)))
    ((_ (> ?fd ?file-name))
     (duplicate-fileno (file-open (maybe->string `?file-name)
                                  (fx+ open/wronly open/creat))
                       `?fd))
    ((_ (>> ?fd ?file-name))
     (duplicate-fileno (file-open (maybe->string `?file-name)
                                  (fx+ open/wronly (fx+ open/append open/creat)))
                       `?fd))
    ((_ (< ?fd ?file-name))
     (duplicate-fileno (file-open (maybe->string `?file-name) open/rdonly)
                       `?fd))
    ((_ (<< ?fd ?object))
     (fork/pipe+ `((1 ?fd)) (lambda () (display `?object (open-output-file* 1)))))
    ((_ (= ?fd-from ?fd/port-to))
     (let* ((fd/port-to ?fd/port-to)    ; Evaluate once
            (fd-to (if (port? fd/port-to)
                       (port->fileno fd/port-to)
                       fd/port-to)))
       (duplicate-fileno ?fd-from fd-to)))
    ((_ (- ?fd/port))
     (let ((o `?fd/port))
       (cond
        ((fixnum? ?fd/port) (file-close o))
        ((output-port? ?fd/port) (close-output-port o))
        ((input-port? ?fd/port) (close-input-port o))
        (else (error "Can only close i/o-ports and file descriptor numbers" o)))))
    ((_ stdports)
     (begin (setup-redirection (= 0 (current-input-port)))
            (setup-redirection (= 1 (current-output-port)))
            (setup-redirection (= 2 (current-error-port)))))
    ((_ ?arg0 ...)
     (syntax-error "Invalid redirection pattern: " `?arg0 ...))))

;; The most "core" syntax form
(define-syntax exec-epf
  ;; The nested let-syntaxes exist to let us pre-empt the fallthrough
  ;; whenever we see one of the recognised special rules so we don't end up
  ;; with the generic one if we happen to make a small mistake
  (syntax-rules (pipe pipe+ begin epf)
    ((_ (pipe ?pf0 ...) ?redir0 ...)
     (exec-epf (pipe+ ((1 2 0)) ?pf0 ...) ?redir0 ...))
    ((_ (pipe+ ?args ...) ?redir0 ...)
     (let-syntax
         ((pipe+
           (syntax-rules ___ ()
             ((_ ((?from0 ?from1 ___ ?to) ___) ?pf0 ___ ?last-pf)
              (let ((conns `((?from0 ?from1 ___ ?to) ___)))
                 (setup-redirection ?redir0) ...
                 (begin (fork/pipe+ conns (lambda () (exec-epf (epf ?pf0))))
                        ___
                        (exec-epf (epf ?last-pf))))))))
       (pipe+ ?args ...)))
    ((_ (begin ?expr0 ...))
     (begin (setup-redirection stdports) ?expr0 ...))
    ((_ (epf ?args ...))              ; This disambiguates redirection
     (exec-epf ?args ...))
    ((_ (?prog ?arg0 ...) ?redir0 ...)
     (begin
       (setup-redirection ?redir0) ...
       (exec-path* `?prog `(?arg0 ...))))))

)
