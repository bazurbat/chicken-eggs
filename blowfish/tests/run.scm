(use blowfish)
(import chicken)

(use test)

(define enc (make-blowfish-encryptor (string->blob "TESTKEY")))
(define dec (make-blowfish-decryptor (string->blob "TESTKEY")))

(test '#${d23f33dfb41ba730} (enc '#${0100000002000000}))
(test '#${0100000002000000} (dec (enc '#${0100000002000000})))

(test '#${d23f33dfb41ba730} (enc '#${0100000002000000 0100000002000000} 8))

(test-error (enc '#${0100000002000000} 9))
(test-error (enc '#${01 02 03}))
(test-error (dec '#${01 02 03 04 05 06 07}))

(test-error (make-blowfish-encryptor (string->blob (make-string 3))))
(test-error (make-blowfish-encryptor (string->blob (make-string 57))))
