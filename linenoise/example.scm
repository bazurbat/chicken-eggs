(use linenoise)

(set-history-length! 300)

(load-history-from-file ".linenoise-history")

(let loop ((l (linenoise "> ")))
   (cond ((equal? l "bye")
       (save-history-to-file ".linenoise-history")
       "Bye!")
       (else
          (display l)
          (newline)
          (history-add l)
          (loop (linenoise "> ")))))


