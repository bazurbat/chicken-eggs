#;(include "../scsh-process.scm")
(use scsh-process)

(use test posix srfi-13 srfi-18 (only setup-api chicken-version version>=?))

(test-begin "scsh-process")

(test-group "Procedural interface"
  (test "Fork/pipe \"hello world\" example from SCSH reference manual"
        '(0 #t "Hello, world.")
        (receive (exit-status exited-ok? pid)
          (wait (fork/pipe
                 (lambda ()
                   (with-output-to-port (open-output-file* 1)
                     (lambda () (display "Hello, world.\n"))))))
          (list exit-status exited-ok? (read-line (open-input-file* 0)))))
  (test "run/string* returns a string output in a subprocess"
        "This is a test"
        (run/string* (lambda () (display "This is a test"))))

  (test-error "run*/string* raises error if subprocess has nonzero exit status"
              (run*/string* (lambda () (display "ohai") (exit 1)))))

(test-group "Macro (EPF) interface"
  (delete-file* "outfile")              ; Leftovers from last run

  (test-group "Various run/... forms"
    (test "Simple run/string"
          "hi, there\n"
          (run/string (echo "hi, there")))

    (test "Unquote-splicing run/string"
          "hi, there\n"
          (run/string (echo ,@(list "hi," "there"))))

    (test "Simple run/sexp"
          '("hi, there")
          (run/sexp (echo "(\"hi, there\") trailing stuff is ignored")))

    (test "Simple run/sexps"
          '(("hi, there") (a b c))
          (run/sexps (echo "(\"hi, there\") (a b c)")))

    (test "Simple run/port"
          '(a b c)
          (read (run/port (echo "(a b c)"))))

    (let ((tmpfile (run/file (echo "blah"))))
      (test "Simple run/file"
            "blah\n"
            (with-input-from-file tmpfile read-all))

      (test "Appending to a file"
            '("blah" "foo")
            (begin (run (echo foo) (>> ,tmpfile))
                   (read-lines tmpfile)))
      
      (let ((message "testing, 1 2 3"))
        (test "Redirecting from object"
              `("blah" "foo" ,(string-delete #\t message))
              (run/strings (pipe (epf (tr -d t) (<< ,message))
                                 (cat ,tmpfile -)))))
      (delete-file* tmpfile)))

  (test-group "Subprocesses"
    (test "run/string with begin form"
          "hi, there\n"
          (run/string (pipe (begin (print "hi, there")) (cat))))
    (when (version>=? (chicken-version) "4.8.1")
      (let ((child? #f))
        (thread-start! (lambda ()
                         (thread-sleep! 0.5)
                         (when child? (print "haihai"))))
        (test "Simple 'begin' form with threading"
              "hi, there\n"
              (run/string (pipe (begin (set! child? #t)
                                       (thread-sleep! 1)
                                       (print "hi, there"))
                                (cat))))))
    
    (let ((outfile "outfile"))
      (test "Subprocess writing to a file"
            "hi, there\n"
            (begin (run (echo "hi, there") (> ,outfile))
                   (read-all "outfile"))))
  
    (delete-file* "outfile")
    (let ((echo-command 'echo))
      (test "Subprocess piped to another process, writing to a file"
            "1235\n"
            (begin (run (pipe (,echo-command "1234" + 1)
                              ("sh" -c "read foo; echo $(($foo))"))
                        (> outfile))
                   (read-all "outfile"))))
    (delete-file* "outfile")

    (test "Nested output redirection with pipe+"
          "foo\n"
          (run/string (pipe+ ((1 0))
                             (pipe+ ((2 0)) (sh -c "echo foo >&2") (cat))
                             (cat))))

    (test "Collecting FDs"
          (list 0 "foo\n" "bar\n")
          (receive (status port1 port2)
            (run/collecting (2 1) (sh -c "echo foo >&2; echo bar"))
            (list status (read-all port1) (read-all port2)))))

  (test-group "Conditional process sequencing forms"
    (test "&& runs for all true values"
          (list #t "bar\n")
          (list (&& (epf (echo "foo") (> outfile))
                    (true)
                    (epf (echo "bar") (> outfile)))
                (read-all "outfile")))
    (delete-file* "outfile")

    (test "&& stops at first false value and returns false"
          (list #f "foo\n")
          (list (&& (epf (echo "foo") (> outfile))
                    (false)
                    (epf (echo "bar") (> outfile)))
                (read-all "outfile")))
    (delete-file* "outfile")

    (test "|| stops at first true value and returns true"
          (list #t "foo\n")
          (list (|| (epf (echo "foo") (> outfile))
                 (true)
                 (epf (echo "bar") (> outfile)))
                (read-all "outfile")))
    (delete-file* "outfile")

    (test "|| continues after first false value and returns true"
          (list #t "bar\n")
          (list (|| (false)
                 (epf (echo "bar") (> outfile)))
                (read-all "outfile")))
    (delete-file* "outfile")

    (test "|| continues beyond all false values and returns false"
          #f
          (|| (false) (epf (sh -c "echo hi && false") (- 1))))))

(test-group "finalization"
  ;; TODO: Find a way to test that the input port didn't get replaced by
  ;;       one from a subshell.  This happened before, but not sure how
  ;;       to detect this except running it manually from the REPL.
  (test-error "No more zombies lying around after we're done" (wait)))

(test-end)

(test-exit)
