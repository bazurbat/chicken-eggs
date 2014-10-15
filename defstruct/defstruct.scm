;;
;; defstruct - a more convenient form of define-record
;;
; Copyright (c) 2005, Dorai Sitaram
; Copyright (c) 2005, Felix Winkelmann (Chicken port)
; Copyright (c) 2008-2010, Peter Bex (Hygienic Chicken port + extensions)
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. Neither the name of the author nor the names of its
;    contributors may be used to endorse or promote products derived
;    from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGE.

(module defstruct
  (defstruct)

(import chicken scheme)
(begin-for-syntax (require-extension srfi-1))

(define-syntax defstruct
  (er-macro-transformer
   (lambda (exp rename compare)
     (let* ((type-name (cadr exp))
            (fields (cddr exp))
            (field-names (map (lambda (f)
                                (if (pair? f)
                                    (car f)
                                    f)) fields))
            (make (string->symbol (conc "make-" type-name)))
            (copy (string->symbol (conc "update-" type-name)))
            (set  (string->symbol (conc "set-" type-name "!")))
            (predicate (string->symbol (conc type-name "?")))
            (to-alist (string->symbol (conc type-name "->alist")))
            (from-alist (string->symbol (conc "alist->" type-name)))
            (%begin (rename 'begin))
            (%define-record (rename 'define-record))
            (%define (rename 'define))
            (%lambda (rename 'lambda))
            (%list (rename 'list))
            (%cons (rename 'cons))
            (%if (rename 'if))
            (%not (rename 'not))
            (%eq? (rename 'eq?))
            (%let (rename 'let))
            (%uninitialized (rename 'uninitialized))
            (%case (rename 'case))
            (%loop (rename 'loop))
            (%obj (rename 'obj))
            (%lst (rename 'lst))
            (%car (rename 'car))
            (%cdr (rename 'cdr)))
       (receive (init-fields no-init-fields)
         (partition pair? fields)
         `(,%begin
           (,%define-record ,type-name ,@no-init-fields ,@(map car init-fields))
           (,%define ,make
             (,%let ((old-make ,make))
               (,%lambda (#!key ,@fields)
                 (old-make ,@no-init-fields ,@(map car init-fields)))))
           (,%define ,set
             (,%let ((,%uninitialized (,%list 'uninitialized)))
               (,%lambda (,%obj #!key ,@(map (lambda (f)
                                                 (list f %uninitialized))
                                               field-names))
                 ,@(map
                    (lambda (f)
                      `(,%if (,%not (,%eq? ,f ,%uninitialized))
                             (,(string->symbol (conc type-name "-" f "-set!"))
                              ,%obj ,f)))
                    field-names)
                 ,%obj)))
           (,%define ,copy
             (,%let ((,%uninitialized (,%list 'uninitialized)))
               (,%lambda (old #!key ,@(map (lambda (f)
                                               (list f %uninitialized))
                                             field-names))
                  (let ((new (,make ,@(fold (lambda (f rest)
                                              (cons (string->keyword
                                                     (symbol->string f))
                                                    (cons %uninitialized rest)))
                                            '() field-names))))
                    ,@(map
                       (lambda (f)
                         `(,%if (,%eq? ,f ,%uninitialized)
                                (,(string->symbol (conc type-name "-" f "-set!"))
                                 new
                                 (,(string->symbol (conc type-name "-" f)) old))
                                (,(string->symbol (conc type-name "-" f "-set!"))
                                 new ,f)))
                       field-names)
                    new))))
           (,%define ,to-alist
             (,%lambda (,%obj)
               (,%list . ,(map
                           (lambda (f)
                             `(,%cons
                               ',f
                               (,(string->symbol (conc type-name "-" f)) ,%obj)))
                           field-names))))
           (,%define ,from-alist
             (,%lambda (alist)
               (,%let ,%loop ((,%lst alist)
                              (,%obj (,make)))
                 (,%if (,%eq? ,%lst '())
                       ,%obj
                       (,%case (,%car (,%car ,%lst))
                         ,@(map (lambda (f)
                                    `((,f) (,(string->symbol
                                              (conc type-name "-" f "-set!"))
                                            ,%obj (,%cdr (,%car ,%lst)))
                                      (,%loop (,%cdr ,%lst) ,%obj)))
                                  field-names)
                         ;; Unknown fields are ignored, like in the constructor
                         (else (,%loop (,%cdr ,%lst) ,%obj)))))))))))))
)
