(load "crc32")

(use test crc32)

(test 2743272264. (crc32 "ABC"))
(test 2743272264. (crc32 "ABC" 3))
(test 2743272264. (crc32 "C" 1 (crc32 "B" 1 (crc32 "A" 1))))
(test 2743272264. (crc32-mid "\x00ABC\x00" 1 4))
(test 2743272264. (crc32-mid "\x00ABC" 1))
(test 2743272264. (crc32-mid "XC" 1 2 (crc32-mid "\x00AB" 1)))
(test 2743272264. (crc32-of-file "tests/testdata1.bin"))
(test 1090970928. (crc32-of-file "tests/testdata2.bin"))
