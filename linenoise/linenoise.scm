(module linenoise
  (linenoise
   history-add
   set-history-length!
   save-history-to-file
   load-history-from-file
   make-linenoise-port)

  (import chicken scheme)
  (import foreign)
  (use ports posix)

  (foreign-declare  "#include \"linenoise-src.c\"")

  (define history-add (foreign-lambda int linenoiseHistoryAdd c-string))
  (define set-history-length! (foreign-lambda int linenoiseHistorySetMaxLen int))
  (define load-history-from-file (foreign-lambda int linenoiseHistoryLoad c-string))
  (define save-history-to-file (foreign-lambda int linenoiseHistorySave c-string))
  (define linenoise (foreign-lambda  c-string linenoise c-string))

  (define (make-linenoise-port #!optional prompt )
    (let ((l "")
	  (handle #f)
	  (p1 prompt)
	  (pos 0))
      (letrec ((char-ready?
		(lambda ()
		  (< pos (string-length l))))
	       (get-next-char!
		(lambda ()
		  (cond ((not l)
			 #!eof)
			((char-ready?)
			 (let ((ch (string-ref l pos)))
			   (set! pos (+ 1 pos))
			   ch))
			(else
			 (set! pos 0)
			 (set! l
			       (let* ((prompt (or p1 ((repl-prompt))))
				      (r (linenoise prompt)))
				 (when r (history-add r))
				 r))
			 (if (string? l)
			     (set! l (string-append l "\n")))
			 (get-next-char!))))))
	(set! handle (lambda (s)
		       (print-call-chain)
		       (set! pos 0)
		       (set! l "")
		       (##sys#user-interrupt-hook)))
	(set-signal-handler! signal/int handle)
	(let ((p (make-input-port
		  get-next-char!
		  char-ready?
		  (lambda ()
		    (set-signal-handler! signal/int #f)
		    'closed-linenoise-port))))
	  (set-port-name! p "(linenoise)")
	  p)))))