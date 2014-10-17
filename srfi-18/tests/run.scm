(use utils)


(load "simple-thread-test.scm")
(load "mutex-test.scm")

(compile-file "srfi-18-signal-test.scm")
(compile-file "signal-tests.scm")
