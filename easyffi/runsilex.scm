;;;; runsilex.scm
;
; Runs silex and generates easyffi.l.scm

(require-extension silex)

(lex "easyffi.l" "easyffi.l.scm" 'counters 'none)
