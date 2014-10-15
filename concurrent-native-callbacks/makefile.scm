(use make shell matchable files)

(shell-verbose #t)

(define (build args)
  (make (("concurrent-native-callbacks.so" 
	  ("cncb.scm"
	   "cncb-module.scm"
	   "twiddle.c"
	   "concurrent-native-callbacks-compile-time.import.so")
	  (compile -sJ -O3 cncb-module.scm
		    ;-C -g -k			;XXX
		    -o concurrent-native-callbacks.so))
	 ("concurrent-native-callbacks.import.so"
	  ("concurrent-native-callbacks.so")
	  (compile -s -O3 -d0 concurrent-native-callbacks.import.scm))
	 ("concurrent-native-callbacks-compile-time.so"
	  ("cncb-compile-time.scm" 
	   "cncb-compile-time-module.scm")
	  (compile -sJ -O3 cncb-compile-time-module.scm
		    -o concurrent-native-callbacks-compile-time.so))
	 ("concurrent-native-callbacks-compile-time.import.so"
	  ("concurrent-native-callbacks-compile-time.so")
	  (compile -s -O3 -d0
		    concurrent-native-callbacks-compile-time.import.scm)))
    args))

(for-each
 (match-lambda
   ("clean"
    (run (rm -f *.so *.import.* a.out *.o)))
   ("all"
    (build
     '("concurrent-native-callbacks.so"
       "concurrent-native-callbacks.import.so"
       "concurrent-native-callbacks-compile-time.so"
       "concurrent-native-callbacks-compile-time.import.so")))
   (arg (build (list arg))))
 (let ((args (command-line-arguments)))
   (if (null? args)
       (list "all")
       args)))
