;; Copyright 2011 Christian Kellermann <ckeen@pestilenz.org>. All
;; rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;    1. Redistributions of source code must retain the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer.
;;    2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.
;; THIS SOFTWARE IS PROVIDED BY CHRISTIAN KELLERMANN ``AS IS'' AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL CHRISTIAN KELLERMANN OR
;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed or
;; implied, of Christian Kellermann.

;; This is parley a small readline alike implementation in scheme. It
;; has been based on the algorithm of linenoise another excellent
;; library written by Salvatore Sanfilippo.  It aims at simplicity so
;; you may miss certain features. Nevertheless it provides hooks for
;; users of this library to extend its capabilities.
;;
;; Basic usage:
;; (parley "Prompt> ") => string or #!eof
;;
;; To use it in csi add this to your .csirc:
;;
;; (use parley)
;; (let ((old (current-input-port)))
;;   (current-input-port (make-parley-port old)))
;;
;; TODOs: * Map string position to screen position as non printable
;;          chars take up more than one ascii char on screen
;;        * Support unicode somehow
;;        * Separate state from module instance


(module parley

(add-key-binding!
 history-from-file
 history-max-lines
 history-to-file
 make-parley-port
 parley
 parley-debug
 terminal-supported?)

(import chicken scheme)
(use data-structures extras ports posix srfi-1 srfi-13 srfi-18 stty)

(define parley-debug (make-parameter #f))

(define-syntax dbg
  (syntax-rules ()
    ((_ fmt ...) (if (parley-debug) (fprintf (current-error-port) fmt ...)))))


(define history-max-lines (make-parameter 100))
(define history (make-parameter #f))
(define exit-handler-installed? #f)
(define user-key-bindings '())
(define user-esc-sequences '())

(define port-list '())

(define +unsupported-terminals+ '("dumb", "cons25"))

(define (parley? p)
  (and (port? p) (equal? (port-name p)
                         "(parley)")))

(define (real-port? p)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (e) (k #f))
      (lambda ()
        (and (port? p) (port->fileno p)))))))

(define (first-usable-port port plist)
  (cond
   ((and (not (port-closed? port))
         (real-port? port))
    port)
   ((null? plist) (error "No real port to read from available"))
   (else (first-usable-port (car plist) (cdr plist)))))

(define (read-one-char port #!optional (plist port-list))
  (let ((real-port (first-usable-port port plist)))
    (unless (char-ready? port)
      (thread-wait-for-i/o! (port->fileno real-port) #:input))
    (read-char port)))

(define (read-one-line port)
  (let loop ((l '())
             (c (read-one-char port)))
    (cond ((equal? c #\newline)
           (list->string (reverse (cons c l))))
          ((eof-object? c) #!eof)
          (else
           (loop (cons c l) (read-one-char port))))))

(define (terminal-supported? port term)
  (and (and (port? port) (or (parley? port) (terminal-port? port)))
       (not (member term +unsupported-terminals+))))

(define (restore-terminal-settings port attrs)
  (set-terminal-attributes! port TCSADRAIN attrs))

(define (install-exit-handler! port attributes)
  (unless exit-handler-installed?
    (on-exit (lambda ()
               (restore-terminal-settings
                port attributes)))
    (set! exit-handler-installed #t)))

(define (enable-raw-mode port)
  (let ((old-attrs (get-terminal-attributes port)))
    (install-exit-handler! port old-attrs)
    (stty port '(not echo icanon isig brkint icrnl inpck istrip ixon))
    (stty port '(cs8 opost))
    (set-buffering-mode! port #:none)
    old-attrs))

(define (get-terminal-columns port)
  (receive (rows cols) (terminal-size port)
    (if (= 0 cols) 80 cols)))

(define +esc-sequences+
  `(( cur-left-edge  . ,(lambda () "\x1b[0G"))
    ( erase-to-right . ,(lambda () "\x1b[0K"))
    ( move-forward   . ,(lambda (n) (sprintf "\x1b[~aC" n)))
    ( move-backward  . ,(lambda (n) (sprintf "\x1b[~aD" n)))
    ( move-to-col    . ,(lambda (col) (sprintf "\x1b[~aG" col)))
    ( move-to        . ,(lambda (row col) (sprintf "\x1b[~a;~aH" row col)))
    ( save-position  . ,(lambda () "\x1b[s"))
    ( restore-position . ,(lambda () "\x1b[u"))
    ( erase-screen   . ,(lambda (n) (sprintf "\x1b[~aJ" n)))))

(define (esc-seq name)
  (cond ((alist-ref name +esc-sequences+) =>
         identity)
        (else (error "Unknown ESC sequence " name))))

(define (history-init! #!optional (items '()))
  (when (not (history)) (history (make-history-cursor items))))

(define (history-add! line)
  (unless (or (equal? line "") (eof-object? line))
    ((history) 'add line))
  line)

(define (history-to-file filename #!optional port)
  (when (and port (not (parley? port)))
    (error "Not a parley port " port))
  (parameterize ((history
                  (if port (##sys#slot port 11) (history))))
                (with-output-to-file filename
                  (lambda ()
                    (for-each print ((history) '->list))))))

(define (history-from-file filename)
  (history-init! (with-input-from-file filename read-lines)))

(define (make-history-cursor h)
  (unless (list? h) (error "Wrong type for history " h))
  (let ((h h)
        (pos -1)
        (syntax-check
         (lambda (args)
           (if (not (= 2 (length args)))
               (error "make-history-cursor: expected 1 argument got "
                      (sub1 (length args)) args)))))
    (lambda (#!rest args)
      (if (null? args) (list-ref pos h)
          (case (car args)
            ((reset)
             (set! pos -1))
            ((->list) h)
            ((replace)
             (syntax-check args)
             (if (not (null? h))
                 (set! (car h) (cadr args))))
            ((add)
             (syntax-check args)
             (set! h (cons (cadr args) h))
             (set! pos -1)
             (if (> (length h)
                    (history-max-lines))
                 (set-cdr! (list-tail h (history-max-lines))
                           '())))
            ((next)
             (cond ((> pos 0)
                    (set! pos (sub1 pos))
                    (list-ref h pos))
                   ((<= pos 0) "")
                   (else (error "position in history cursor out of range " pos))))
            ((prev)
             (cond ((null? h) "")
                   (else
                      (when (< (add1 pos) (length h))
                        (set! pos (add1 pos)))
                      (list-ref h pos))))
            (else (list-ref h pos)))))))

(define (string-insert s i t)  (string-replace s t i i))

(define +caret-notation+
  '((0 . "^@")
    (1 . "^A")
    (2 . "^B")
    (3 . "^C")
    (4 . "^D")
    (5 . "^E")
    (6 . "^F")
    (7 . "^G")
    (8 . "^H")
    (9 . "^I")
    (10 . "^J")
    (11 . "^K")
    (12 . "^L")
    (13 . "^M")
    (14 . "^N")
    (15 . "^O")
    (16 . "^P")
    (17 . "^Q")
    (18 . "^R")
    (19 . "^S")
    (20 . "^T")
    (21 . "^U")
    (22 . "^V")
    (23 . "^W")
    (24 . "^X")
    (25 . "^Y")
    (26 . "^Z")
    (27 . "^[")
    (28 . "^\\")
    (29 . "^]")
    (30 . "^^[j]")
    (31 . "^_")
    (127 . "^?")))

(define (convert-if-control-char c)
  (cond ((alist-ref (char->integer c) +caret-notation+) => identity)
        (else (string c))))

(define (get-complete-esc-sequence port #!optional (res #f))
  (let ((c (read-one-char port)))
    (cond ((eof-object? c) #f)
          ((and (not res)
                (equal? c #\x5b))
           (get-complete-esc-sequence port c))
          (res (list res c))
          (else #f))))

(define (slurp-all-input port)
  (let loop ((r '()))
    (if (char-ready? port)
        (loop (cons (read-one-char port) r))
        (reverse r))))

(define (get-column port)
  (display "\x1b[6n")
  (flush-output (current-output-port))
  (let loop ((r '())
             (c (read-one-char port)))
    (if (or (equal? c #\R)
            (eof-object? c))
        (string->number (cadr (string-split (list->string (reverse r)) ";R")))
        (loop (cons c r) (read-one-char port)))))

(define (add-key-binding! key handler #!key (esc-sequence #f))
  (if esc-sequence
      (set! user-esc-sequences
            (alist-update! key handler user-esc-sequences equal?))
      (set! user-key-bindings
            (alist-update! key handler user-key-bindings equal?))))

;; each handler gets the current prompt, in, out, line,
;; position of the cursor, exit continuation and prompt offset
;; and has to return a list of these arguments for the next
;; loop iteration of prompt-loop
(define +key-handlers+
  `((nop .
         ,(lambda (prompt in out line pos exit offset)
            (list prompt in out line pos exit offset)))
    (discard-and-restart .
                         ,(lambda (prompt in out line pos exit offset)
                            (list prompt in out "" 0 exit offset)))
    (delete-curr-char .
                      ,(lambda (prompt in out line pos exit offset)
                         (if (> pos 0)
                             (let ((nline (string-append
                                           (string-take line (sub1 pos))
                                           (string-drop line pos))))
                               (list prompt in out nline (sub1 pos) exit offset))
                             (list prompt in out line pos exit offset))))
    (swap-char .
               ,(lambda (prompt in out line pos exit offset)
                  (let ((len (string-length line)))
                    (if (and (> pos 0)
                             (< pos len))
                        (let* ((before (sub1 pos))
                               (token-1 (string (string-ref line before)))
                               (token (string (string-ref line pos)))
                               (tmp (string-replace line token before pos))
                               (nline (string-replace tmp token-1 pos (add1 pos)))
                               (npos (if (not (= pos (sub1 len)))
                                         (add1 pos)
                                         pos)))
                          (refresh-line prompt out nline npos offset)
                          (list prompt in out nline npos exit offset))
                        (list prompt in out line pos exit offset)))))
    (left-arrow .
                ,(lambda (prompt in out line pos exit offset)
                   (list prompt in out line (if (> pos 0) (sub1 pos) pos) exit offset)))
    (right-arrow .
                 ,(lambda (prompt in out line pos exit offset)
                    (list prompt
                          in out
                          line
                          (if (not (= pos
                                      (string-length line)))
                              (add1 pos)
                              pos)
                          exit offset)))
    (prev-history .
                  ,(lambda (prompt in out line pos exit offset)
                     (let ((nline ((history) 'prev)))
                       (list prompt in out nline (string-length nline) exit offset))))
    (next-history .
                  ,(lambda (prompt in out line pos exit offset)
                     (let ((nline ((history) 'next)))
                       (list prompt in out nline (string-length nline) exit offset))))
    (delete-until-eol .
                      ,(lambda (prompt in out line pos exit offset)
                         (list prompt in out (string-take line pos) pos exit offset)))
    (jump-to-start-of-line .
                           ,(lambda (prompt in out line pos exit offset)
                              (list prompt in out line 0 exit offset)))
    (jump-to-eol .
                 ,(lambda (prompt in out line pos exit offset)
                    (list prompt in out line (string-length line) exit offset)))
    (escape-sequence .
                     ,(lambda (prompt in out line pos exit offset)
                        (cond ((escape-sequence-handler-ref (get-complete-esc-sequence in)) =>
                               (lambda (handler)
                                 (handler prompt in out line pos exit offset)))
                              (else (list prompt in out line pos exit offset)))))
    (erase-screen . ,(lambda (prompt in out line pos exit offset)
                       (display ((esc-seq 'erase-screen) 2) out)
                       (display ((esc-seq 'move-to) 1 1) out)
                       (list prompt in out line pos exit offset)))))

(define (handle event)
  (cond ((alist-ref event +key-handlers+) =>
         identity)
        (else (error "Unhandled event " event))))

(define +escape-sequence-handlers+
  `((#\x43 . ,(handle 'right-arrow))
    (#\x44 . ,(handle 'left-arrow))
    (#\x41 . ,(handle 'prev-history))
    (#\x42 . ,(handle 'next-history))))

(define (escape-sequence-handler-ref seq)
  (and seq (or (alist-ref (second seq) user-esc-sequences)
               (alist-ref (second seq) +escape-sequence-handlers+))))

(define (refresh-line prompt port line pos offset)
  (let* ((cols (- (get-terminal-columns port)
                  offset 3))
         (plen (+ offset (string-length prompt)))
         (chunk-size (- cols offset 1))
         (chunkno (inexact->exact (floor (/ pos chunk-size))))
         (start (* chunk-size chunkno))
         (end (min (string-length line)
                   (+ start chunk-size)))
         (npos (modulo (- pos start) chunk-size))
         (delimited-line (substring line start end)))
    ;; (newline)
    ;; (print "offset " offset " cols " cols " plen " plen " chunkno " chunkno " chunksize " chunk-size " start " start " end " end " npos " npos)
    (parameterize ((current-output-port port))
      ;; (display ((esc-seq 'cur-left-edge)))
      (display ((esc-seq 'move-to-col) offset))
      (if (= chunkno 0) (display prompt)
          (display "< "))
      (display (string-fold
                (lambda (c r)
                  (string-append r (convert-if-control-char c)))
                ""
                delimited-line))
      (when (> (string-length line) end)
       (display ">"))
      (display ((esc-seq 'erase-to-right)))
      (display ((esc-seq 'cur-left-edge)))
      (display ((esc-seq 'move-to-col)
                (if (> chunkno 0) ;(= 0 (+ npos plen))
                    (+ 3 npos)
                    (+ npos plen))))
      (flush-output))))

(define (prompt-loop prompt in out line pos return offset)
  (refresh-line prompt out line pos offset)
  (apply prompt-loop
         ((let ((c (read-one-char in)))
            (cond ((alist-ref c user-key-bindings) => identity)
                  (else
                   (case c
                     ((#\xd)
                      (newline out)
                      (return line))
                     ((#!eof #\x04)
                      (if (string-null? line)
                          (begin
                            (display "^D" out)
                            (return #!eof))
                          (begin
                            (newline out)
                            (return line))))
                     ((#\x3)
                      (display "^C" out)
                      (newline out)
                      (return ""))
                     ((#\x15)
                      (handle 'discard-and-restart))
                     ((#\x8 #\x7f)
                      (handle 'delete-curr-char))
                     ((#\x14)
                      (handle 'swap-char))
                     ((#\x2)
                      (handle 'left-arrow))
                     ((#\x6)
                      (handle 'right-arrow))
                     ((#\x10)
                      (handle 'prev-history))
                     ((#\xe)
                      (handle 'next-history))
                     ((#\x1b)
                      (handle 'escape-sequence))
                     ((#\xb)
                      (handle 'delete-until-eol))
                     ((#\xc)
                      (handle 'erase-screen))
                     ((#\x1)
                      (handle 'jump-to-start-of-line))
                     ((#\x5)
                      (handle 'jump-to-eol))
                     (else
                      (lambda (prompt in out line pos return offset)
                        (list
                         prompt
                         in out
                         (string-insert line pos (string c))
                         (add1 pos)
                         return
                         offset)))))))
          prompt in out line pos return offset)))

(define (read-raw prompt in out offset)
  (let ((l (call-with-current-continuation
            (lambda (return)
              (prompt-loop prompt in out "" 0 return offset)))))
    (history-add! l)))

(define (useful-term? port)
  (terminal-supported?
   (first-usable-port port port-list)
   (get-environment-variable "TERM")))

(define (parley prompt #!key (in ##sys#standard-input) (out (current-output-port)))
  (set-buffering-mode! out #:none)
  (let* ((parley-port (parley? in))
         (real-in-port (first-usable-port in port-list))
         (old-attrs (and (useful-term? in)
                         (enable-raw-mode real-in-port))))
    (let ((lines (if (useful-term? in)
                     (begin
                       (history-init!)
                       (unless (member in port-list)
                         (set! port-list (cons in port-list)))
                       (let* ((prev-input (and (char-ready? in) (slurp-all-input in)))
                              (offset (if parley-port 1 (get-column real-in-port))))
                         (if prev-input
                             (call-with-input-string
                                 (list->string prev-input)
                               (lambda (in)
                                 (let loop ((r '()))
                                   (if (eof-object? (peek-char in))
                                       (reverse r)
                                       (loop (cons (read-raw prompt in out offset) r))))))
                             (read-raw prompt real-in-port out offset))))
                     (begin
                       (dbg "; Warning: dumb terminal")
                       (set-buffering-mode! real-in-port #:none)
                       (when (or (terminal-port? in)
                                 parley-port)
                         (display prompt out))
                       (flush-output out)
                       (let ((l (read-one-line real-in-port)))
                         l)))))
      (if old-attrs (restore-terminal-settings real-in-port old-attrs))
      lines)))

(define (input-missing? line)
  ;; Walk the line like Johnny Cash
  (define (wtl lst esc parens brackets str quote-syntax)
    (cond ((null? lst) (or (< 0 parens)
                           (< 0 brackets)
                           str
                           esc
                           quote-syntax))
          (esc (wtl (cdr lst) #f parens brackets str #f))
          (str (wtl (cdr lst) (equal? (car lst) #\\) parens brackets (not (equal? (car lst) #\")) #f))
          (else (case (car lst)
                  ((#\;) (let (( r (drop-while (lambda (c) (not (equal? c #\newline))) lst)))
                           (wtl (if (pair? r) (cdr r) r)  #f parens brackets str #f)))
                  ((#\\) (wtl (cdr lst) #t parens brackets str #f))
                  ((#\") (wtl (cdr lst) #f parens brackets (not str) #f))
                  ((#\() (wtl (cdr lst) #f (add1 parens) brackets str #f))
                  ((#\)) (wtl (cdr lst) #f (if (> parens 0) (sub1 parens) parens) brackets str #f))
                  ((#\[) (wtl (cdr lst) #f parens (add1 brackets) str #f))
                  ((#\]) (wtl (cdr lst) #f parens (if (> brackets 0) (sub1 brackets) brackets) str #f))
                  ((#\') (wtl (cdr lst) #f parens brackets str #t))
                  (else (wtl (cdr lst) esc parens brackets str #f))))))
  (wtl (string->list line) #f 0 0 #f #f))


;; parley uses port slot 11 to store the history because we need the
;; port argument to get to the history, this mimics make-input-port
;; from the ports unit directly
(define (make-parley-port in #!key prompt prompt2 history-file)
  (define (flush-history p)
    (when history-file
      (history-to-file history-file p)))
  (let ((l "")
        (handle #f)
        (p1 prompt)
        (p2 (or prompt2 "> "))
        (pos 0))
    (unless (member in port-list)
      (set! port-list (cons in port-list)))
    (letrec ((append-while-incomplete
              (lambda (start)
                (let* ((lines (parley (if (string-null? start)
                                          (or p1 ((repl-prompt)))
                                          p2)
                                      in: in))
                       (line (if (list? lines) (string-intersperse lines (string #\newline)) lines))
                       (res (and (string? line) (string-append start line))))
                  (cond ((and (eof-object? line) (string-null? start)) line)
                        ((eof-object? line) start)
                        ((input-missing? res)
                         (append-while-incomplete res))
                        (else res)))))
             (parley-char-ready?
              (lambda ()
                (and (string? l)
                     (< pos (string-length l)))))
             (get-next-char!
              (lambda ()
                (cond ((not l)
                       #!eof)
                      ((parley-char-ready?)
                       (let ((ch (string-ref l pos)))
                         (set! pos (+ 1 pos))
                         ch))
                      (else
                       (set! pos 0)
                       (set! l
                             (append-while-incomplete ""))
                       (if (and (useful-term? in) (string? l))
                           (set! l (string-append l "\n")))
                       (if (not (eof-object? l))
                           (get-next-char!)
                           l))))))

;; XXX Set the signal handler to clear the internal state, why print a call chain here?
      (set! handle (lambda (s)
                     (print-call-chain)
                     (set! pos 0)
                     (set! l "")
                     (##sys#user-interrupt-hook)))
      (set-signal-handler! signal/int handle)

      (let* ((class
              (vector
               (lambda (p)              ; read-char
                 (parameterize ((history (##sys#slot p 11)))
                               (let ([last (##sys#slot p 10)])
                                 (cond [last
                                        (##sys#setislot p 10 #f)
                                        last]
                                       [else (get-next-char!)] ) )) )
               (lambda (p)              ; peek-char
                 (parameterize ((history (##sys#slot p 11)))
                               (let ([last (##sys#slot p 10)])
                                 (if last
                                     last
                                     (let ([last (get-next-char!)])
                                       (##sys#setslot p 10 last)
                                       last)  ) )))
               #f                       ; write-char
               #f                       ; write-string
               (lambda (p)              ; close XXX Really disable the
; signal handler here? What if the user installed one before or after
; us? This is broken
                 (set-signal-handler! signal/int #f)
                 (unless (##sys#slot p 8) (flush-history p))
                 (##sys#setislot p 8 #t))
               #f                       ; flush-output
               (lambda (p)              ; char-ready?
                 (parley-char-ready?) )
               #f                       ; read-string!
               #f                       ; read-line
               #f))                     ; read-buffered
             (data (vector #f))
             (port (##sys#make-port #t class "(parley)" 'custom)) )
        (##sys#set-port-data! port data)
        (parameterize
         ((history #f))
         (if (and history-file (file-read-access? history-file))
             (history-from-file history-file)
             (history-init!))
         (##sys#setslot port 11 (history)))
        (set-finalizer! port flush-history)
        (on-exit (lambda ()
                   (flush-history port)))
        port))))
  )
