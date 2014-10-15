(use parley)

(let loop ((l (parley "> ")))
  (if (or (eof-object? l)
          (equal? l "quit"))
      (print "bye!")
      (begin
        (printf "you typed: ~s~%" l)
        (loop (parley "> ")))))
