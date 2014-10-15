;;;; run.scm - run coops tests


(use setup-api)

(print "(interpreted)")
(run (csi -s tests.scm))

(print "(compiled)")
(run (csc tests.scm -o a.out))
(run (./a.out))

(print "(benchmarks - coops)")
(run (csc bench.scm -D c -O3 -o a.out))
(run (./a.out))

(when (extension-information 'tinyclos)
  (print "\n(benchmarks - tinyclos)")
  (run (csc bench.scm -D tc -O3 -o a.out))
  (run (./a.out)))
