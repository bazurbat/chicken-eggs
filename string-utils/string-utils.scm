;;;; string-utils.scm  -*- Hen -*-
;;;; Kon Lovett, Aug '10

(module string-utils ()

  (import
    scheme chicken)

  (reexport
    memoized-string
    unicode-utils
    string-hexadecimal)

  (require-library
    memoized-string
    unicode-utils
    string-hexadecimal)

) ;module string-utils
