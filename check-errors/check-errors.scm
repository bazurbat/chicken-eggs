;;;; check-errors.scm  -*- Hen -*-
;;;; Kon Lovett, Dec '12

(module check-errors ()

  (import scheme chicken foreign)
  (reexport type-checks type-errors srfi-4-checks srfi-4-errors)
  (require-library type-checks type-errors srfi-4-checks srfi-4-errors)

) ;module check-errors
