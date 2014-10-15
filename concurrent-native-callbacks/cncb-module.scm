(module concurrent-native-callbacks (dispatcher
				     dispatcher?
				     dispatcher-id
				     dispatcher-thread
				     dispatcher-argument-input-fileno
				     dispatcher-argument-output-fileno
				     dispatcher-result-input-fileno
				     dispatcher-result-output-fileno
				     dispatch
				     dispatcher-add!
				     dispatcher-terminate!
				     define-concurrent-native-callback
				     define-synchronous-concurrent-native-callback)
				     
  (import scheme chicken)
  (include "cncb.scm") )
