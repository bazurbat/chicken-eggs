(use srfi-14)

(define (delete-last-word line pos)
  (let* ((del-pos (or (string-index-right
                      line
                      (char-set-union char-set:whitespace
                                      char-set:punctuation)
                      pos)
                     0))
         (left-part (if (> del-pos 0) (string-take line del-pos)
                    ""))
         (npos (- pos (- pos
                         del-pos))))
    (values (string-append left-part (string-drop line pos))
            npos)))

(define (cw prompt in out line pos exit offset)
  (receive (l p) (delete-last-word line pos)
           (list prompt in out l p exit offset)))

(add-key-binding! #\x17 cw)