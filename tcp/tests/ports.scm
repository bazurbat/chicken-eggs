(cond-expand
  ((not (or mingw32 msvc))

   (define proc (process-fork (lambda () (tcp-accept (tcp-listen 8080)))))

   (on-exit (lambda () (handle-exceptions exn #f (process-signal proc))))

   (print "\n\nProcedures check on TCP ports being closed\n")

   (receive (in out)
       (let lp ()
	 (condition-case (tcp-connect "localhost" 8080)
	   ((exn i/o net) (lp))))
     (close-output-port out)
     (close-input-port in)
     (check (tcp-addresses in))
     (check (tcp-port-numbers in))
     (check (tcp-abandon-port in)))	; Not sure about abandon-port

   
   ;; This tests for two bugs which occurred on NetBSD and possibly
   ;; other platforms, possibly due to multiprocessing:
   ;; read-line with EINTR would loop endlessly and process-wait would
   ;; signal a condition when interrupted rather than retrying.
   (set-signal-handler! signal/chld void) ; Should be a noop but triggers EINTR
   (receive (in out)
     (create-pipe)
     (receive (pid ok? status)
       (process-wait
        (process-fork
         (lambda ()
           (file-close in)              ; close receiving end
           (with-output-to-port (open-output-file* out)
             (lambda ()
               (display "hello, world\n")
               ;; exit prevents buffers from being discarded by implicit _exit
               (exit 0))))))
       (file-close out)                 ; close sending end
       (assert (equal? '(#t 0 ("hello, world"))
                       (list ok? status (read-lines (open-input-file* in)))))))
   )
  (else))
