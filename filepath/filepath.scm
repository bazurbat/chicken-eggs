;; 
;; A library for manipulating file paths in a cross platform way on
;; both Windows and Unix.
;; 
;; Based on the Haskell FilePath library by Neil Mitchell.
;; http://www-users.cs.york.ac.uk/~ndm/filepath
;;
;; Copyright 2008-2011 Ivan Raikov.
;;
;; 
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;  notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above
;;  copyright notice, this list of conditions and the following
;;  disclaimer in the documentation and/or other materials provided
;;  with the distribution.
;; 
;;  - Neither name of the copyright holders nor the names of its
;;  contributors may be used to endorse or promote products derived
;;  from this software without specific prior written permission.
;; 
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE
;;  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR THE
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;  POSSIBILITY OF SUCH DAMAGE.
;;

(module filepath

 ( 
  filepath:posix filepath:is-windows? filepath:is-posix?

  ;; Separator predicates
  filepath:path-separator filepath:path-separator-set
  filepath:is-path-separator? 
  filepath:search-path-separator filepath:is-search-path-separator? 
  filepath:ext-separator filepath:is-ext-separator?

  ;; Path methods (environment $PATH)
  filepath:split-search-path filepath:get-search-path

  ;; Extension procedures
  filepath:split-extension filepath:take-extension filepath:replace-extension
  filepath:drop-extension filepath:add-extension filepath:has-extension?
  filepath:split-all-extensions filepath:drop-all-extensions filepath:take-all-extensions

  ;; Drive procedures
  filepath:split-drive filepath:join-drive
  filepath:take-drive filepath:has-drive? filepath:drop-drive
  filepath:is-drive?

  ;; Operations on a file path, as a list of directories
  filepath:split-file-name filepath:take-file-name
  filepath:replace-file-name filepath:drop-file-name
  filepath:take-base-name filepath:replace-base-name
  filepath:take-directory filepath:replace-directory
  filepath:combine filepath:split-path filepath:join-path
  filepath:split-directories

  ;; Low-level procedures
  filepath:has-trailing-path-separator?
  filepath:add-trailing-path-separator
  filepath:drop-trailing-path-separator

  ;; File name manipulators
  filepath:normalise filepath:path-equal?
  filepath:make-relative filepath:is-relative? filepath:is-absolute?
  filepath:is-valid? filepath:make-valid
  
  )

  (import scheme chicken data-structures )
  (require-extension srfi-1 srfi-13 srfi-14)
  (require-extension matchable)

;; Utility list procedures

(define (scatter p lst)
  (define (break1 p lst)
    (let-values (((hd tl)  (break p lst)))
		(list hd tl)))
  (let loop ((lst lst) (ax (list)))
    (match (break1 p lst)
	   ((() ())  (reverse ax))
	   ((hd ())  (reverse (cons hd ax)))
	   ((() tl)  (loop (cdr tl) (cons (list) ax)))
	   ((hd tl)  (loop (cdr tl) (cons hd ax))))))

(define (prefix? p lst)
  (let loop ((p p)  (lst lst))
    (cond ((null? p)   #t)
	  ((null? lst) #f)
	  ((eq? (first p) (first lst))
	   (loop (cdr p) (cdr lst)))
	  (else #f))))
	

;; Utility char procedures

(define (is-letter? c)  (char-set-contains? char-set:letter c))

;; (define char-upcase-map
;;   (zip (char-set->list char-set:lower-case)
;;        (char-set->list char-set:upper-case)))

;; (define (char-upcase c) (or (safe-car (alist-ref c char-upcase-map)) c))

;; (define char-downcase-map
;;   (zip (char-set->list char-set:upper-case)
;;        (char-set->list char-set:lower-case)))

;; (define (char-downcase c) (or (safe-car (alist-ref c char-downcase-map)) c))


;; Is the operating system environment POSIX or Windows like

(define filepath:posix 
  (make-parameter 
   (or (equal? (software-type) 'unix)
       (equal? (software-type) 'macosx))))

(define (is-posix?) (filepath:posix))
  
(define (is-windows?) (not (is-posix?)))
  
  
;;  Default path separator character. In the case where more than one
;;  separator is possible, path-separator is the most commonly used
;;  one.
;;

(define (path-separator)
  (cond ((is-posix?)     #\/)
	((is-windows?)   #\\)
	(else (error 'path-separator "unknown system environment"))))

;; The set of all possible separators.

(define (path-separator-set)
  (cond ((is-posix?)    (list->char-set (list #\/)))
	((is-windows?)  (list->char-set (list #\\ #\/)))
	(else         (error 'path-separator-set "unknown system environment"))))
  
(define (is-path-separator? x)
  (char-set-contains? (path-separator-set) x))

;; The character that is used to separate the entries in the $PATH
;; environment variable.

(define (search-path-separator)
  (cond ((is-posix?)    #\:)
	((is-windows?)  #\;)
	(else         (error 'search-path-separator "unknown system environment"))))
	
(define (is-search-path-separator? x)
  (equal? x (search-path-separator)))

;; File extension character

(define (ext-separator) #\.)
  
(define (is-ext-separator? x)
  (equal? x (ext-separator)))


;;  Splits a string it on the search-path-separator character.
;;
;;   Follows the recommendations in
;;   <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html>
;;

(define (split-search-path s)
  (let ((cs (if (string? s) (string->list s) s)))
    (filter-map (lambda (x) (match x (()  (and (is-posix?) (list #\.))) (else x)))
		(scatter is-search-path-separator? cs))))

(define (get-search-path)  (split-search-path (get-environment-variable "PATH")))

;; Splits a file path string on the extension. 

(define (split-extension p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (match-let (((a b)  (split-file-name pcs)))
      (let-values (((c d)  (break is-ext-separator? (reverse b))))
	  (match d
		 (()       (list pcs (list)))
		 ((y . ys)  (list (append a (reverse ys)) (cons y (reverse c)))))))))

(define (take-extension p)
  (second (split-extension p)))

(define (drop-extension p)
  (first (split-extension p)))

(define (replace-extension p ext)
  (add-extension (drop-extension p) ext))

(define (add-extension p ext)
  (let ((ecs (if (string? ext) (string->list ext) ext))
	(pcs (if (string? p) (string->list p) p)))
    (if (null? ecs) pcs
	(if (prefix? (list (ext-separator)) ecs)
	    (append pcs ecs)
	    (append pcs (list (ext-separator)) ecs)))))

(define (has-extension? p)  (member (ext-separator) p))

(define (split-all-extensions p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (match-let (((a b)  (split-file-name pcs)))
	       (match (scatter is-ext-separator? b)
		      ((c d . e) (list (concatenate (append a (list c))) 
				       (concatenate (intersperse (cons d e) (list (ext-separator))))))
		      (else       (list pcs (list)))))))

(define (drop-all-extensions p)
  (first (split-all-extensions p)))

(define (take-all-extensions p)
  (second (split-all-extensions p)))

(define (split-drive p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (or (and (is-posix?) 
	     (let-values (((pre rest) (span is-path-separator? pcs)))
			 (and (not (null? pre)) (list pre rest))))
	(read-drive-letter pcs)
	(read-drive-unc pcs)
	(read-drive-share pcs)
	(list (list) pcs))))

(define (add-slash a xs)
  (let ((xcs (if (string? xs) (string->list xs) xs))
	(acs (if (string? a) (string->list a) a)))
    (let-values (((c d) (span is-path-separator? xcs)))
		(list (append acs c) d))))

;; http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/naming_a_file.asp
;; "\\?\D:\<path>" or "\\?\UNC\<server>\<share>"
;; a is "\\?\"

(define (read-drive-unc p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (match pcs 
	   (((and s1 (? is-path-separator?)) 
	     (and s2 (? is-path-separator?))
	     #\? (and s3 (? is-path-separator?)) . xs)
	    (let ((us (map char-upcase xs)))
	      (match us 
		     ((#\U #\N #\C (and s4 (? is-path-separator?)) . _)
		      (match (read-drive-share-name (drop xs 4))
			     ((a b)  (list (cons* s1 s2 #\? s3 (append (take xs 4) a)) b))))
		     (else
		      (match (read-drive-letter xs)
			     ((a b) (list (cons* s1 s2 #\? s3 a) b))
			     (else  #f))))))
	   (else #f))))


(define (read-drive-letter p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (match pcs
	   (((and x (? is-letter?))  #\: (and y (? is-path-separator?)) . xs)
	    (add-slash (list x #\:) (cons y xs)))
	   (((and x (? is-letter?)) #\: . xs)  
	    (list (list x #\:) xs))
	   (else #f))))

(define (read-drive-share p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (match pcs
	   (((and s1 (? is-path-separator?)) (and s2 (? is-path-separator?)) . xs)
	    (match-let (((a b)  (read-drive-share-name xs)))
		       (list (cons* s1 s2 a) b)))
	   (else #f))))

(define (read-drive-share-name n)
  (let ((ncs (if (string? n) (string->list n) n)))
    (let-values (((a b) (break is-path-separator? ncs)))
		(and (not (null? a)) (add-slash a b)))))


;; Join a drive and the rest of the path.
;;  Windows: joinDrive "C:" "foo" == "C:foo"
;;  Windows: joinDrive "C:\\" "bar" == "C:\\bar"
;;  Windows: joinDrive "\\\\share" "foo" == "\\\\share\\foo"
;;  Windows: joinDrive "/:" "foo" == "/:\\foo"

(define (join-drive a b)
  (let ((acs (if (string? a) (string->list a) a))
	(bcs (if (string? b) (string->list b) b)))
  (cond ((is-posix?)    (append acs bcs))
	((null? acs)  bcs)
	((null? bcs)  acs)
	((is-path-separator? (last acs)) (append acs bcs))
	(else (match acs
		     (((and a1 (? is-letter?)) #\:) (append acs bcs))
		     (else (append acs (list (path-separator)) bcs)))))))


(define (take-drive p)  (first (split-drive p)))

(define (drop-drive p)  (second (split-drive p)))

(define (has-drive? p)  (not (null? (take-drive p))))

(define (is-drive? p)   (null? (drop-drive p)))


;; Operations on a filepath, as a list of directories

;;; Split a filename into directory and file. 'combine' is the inverse.
(define (split-file-name p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (match-let (((c d)  (split-drive pcs)))
      (let-values (((a b)  (break is-path-separator? (reverse d))))
		  (list (append c (reverse b)) (reverse a))))))

(define (replace-file-name p r)
  (drop-file-name (combine p r)))

(define (drop-file-name p) (first (split-file-name p)))

(define (take-file-name p) (second (split-file-name p)))

(define (take-base-name p) (drop-extension (take-file-name p)))

(define (replace-base-name p name) 
  (let ((ncs (if (string? name) (string->list name) name)))
    (match-let (((a b) (split-file-name p)))
       (let* ((ext (take-extension b))
	      (ext (if (prefix? (list (ext-separator)) ext) ext 
		       (cons (ext-separator) ext))))
	 (combine-always a (append ncs ext))))))

;; Is an item either a directory or the last character a path separator?
(define (has-trailing-path-separator? p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (and (not (null? pcs)) (is-path-separator? (last pcs)))))
  
(define (add-trailing-path-separator p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (if (has-trailing-path-separator? pcs) pcs 
	(append pcs (list (path-separator))))))

(define (drop-trailing-path-separator p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (if (and (has-trailing-path-separator? pcs) (not (is-drive? pcs)))
	(reverse (drop-while is-path-separator? (reverse pcs))) pcs)))

(define (take-directory p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (let* ((fn   (drop-file-name pcs))
	   (res  (reverse (drop-while is-path-separator? (reverse fn)))))
      (if (is-drive? fn) fn
	  (if (and (null? res) (not (null? fn))) fn
	      res)))))
	   
(define (replace-directory p dir)
  (let ((pcs (if (string? p) (string->list p) p))
	(dcs (if (string? dir) (string->list dir) dir)))
    (combine-always dir (take-file-name pcs))))


;; Combine two paths, if the second path 'isAbsolute', then it returns the second.
;;
;; Posix:   combine "/" "test" == "/test"
;; Posix:   combine "home" "bob" == "home/bob"
;; Windows: combine "home" "bob" == "home\\bob"
;; Windows: combine "home" "/bob" == "/bob"

(define (combine a b)
  (let ((acs (if (string? a) (string->list a) a))
	(bcs (if (string? b) (string->list b) b)))
    (cond ((or (has-drive? bcs) (and (not (null? bcs)) (is-path-separator? (first bcs)))) bcs)
	  (else (combine-always acs bcs)))))

;; Combine two paths, assuming rhs is NOT absolute.
(define (combine-always a b)
  (let ((acs (if (string? a) (string->list a) a))
	(bcs (if (string? b) (string->list b) b)))
    (cond ((null? acs) bcs)
	  ((null? bcs) acs)
	  ((is-path-separator? (last acs))  (append acs bcs))
	  ((is-drive? acs)  (join-drive acs bcs))
	  (else (append acs (list (path-separator)) bcs)))))


;; Split a path by the directory separator.
;; splitPath "test//item/" == ["test//","item/"]
;; splitPath "test/item/file" == ["test/","item/","file"]
;; splitPath "" == []
;; Windows: splitPath "c:\\test\\path" == ["c:\\","test\\","path"]
;; Posix:   splitPath "/file/test" == ["/","file/","test"]

(define (split-path p )
  (define (f y) 
    (if (null? y) y
	(let*-values (((a b) (break is-path-separator? y))
		      ((c d) (break (lambda (x) (not (is-path-separator? x))) b)))
	  (cons (append a c) (f d)))))
  (let ((pcs (if (string? p) (string->list p) p)))
    (match-let (((drive path)  (split-drive pcs)))
	       (append (if (null? drive) (list) (list drive)) (f path)))))

;; Just as 'splitPath', but don't add the trailing slashes to each element.
;; splitDirectories "test/file" == ["test","file"]
;; splitDirectories "/test/file" == ["/","test","file"]
;; splitDirectories "" == []

(define (split-directories p)
  (define (g x)
    (let ((res (take-while (lambda (x) (not (is-path-separator? x))) x)))
      (if (null? res) x res)))
  (let* ((pcs (if (string? p) (string->list p) p))
	 (path-components (split-path pcs)))
    (if (has-drive? pcs) 
	(cons (car path-components) (map g (cdr path-components)))
	(map g path-components))))


;; Join path elements back together.
;; joinPath [] == ""
;; Posix: joinPath ["test","file","path"] == "test/file/path"

(define (join-path p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (fold-right combine (list) pcs)))


;; Equality of two 'FilePath's.
;;  Note that this doesn't follow symlinks or DOSNAM~1s.
;;   Posix:   equalFilePath "foo" "foo/"
;;   Posix:   not (equalFilePath "foo" "/foo")
;;   Posix:   not (equalFilePath "foo" "FOO")
;;   Windows: equalFilePath "foo" "FOO"

(define (path-equal? a b)
  (define (f x) 
    (if (is-windows?) (drop-trail-slash (map char-downcase (normalise x)))
	(drop-trail-slash (normalise x))))
  (define (drop-trail-slash x)
    (if (and (>= (length x) 2) (is-path-separator? (last x)))
	(drop-right x 1) x))
  (let ((acs (if (string? a) (string->list a) a))
	(bcs (if (string? b) (string->list b) b)))
    (equal? (f acs) (f bcs))))


;; Contract a filename, based on a relative path.
;; Windows: makeRelative "C:\\Home" "c:\\home\\bob" == "bob"
;; Windows: makeRelative "C:\\Home" "c:/home/bob" == "bob"
;; Windows: makeRelative "C:\\Home" "D:\\Home\\Bob" == "D:\\Home\\Bob"
;; Windows: makeRelative "C:\\Home" "C:Home\\Bob" == "C:Home\\Bob"
;; Windows: makeRelative "/Home" "/home/bob" == "bob"
;; Posix:   makeRelative "/Home" "/home/bob" == "/home/bob"
;; Posix:   makeRelative "/home/" "/home/bob/foo/bar" == "bob/foo/bar"
;; Posix:   makeRelative "/fred" "bob" == "bob"
;; Posix:   makeRelative "/file/test" "/file/test/fred" == "fred"
;; Posix:   makeRelative "/file/test" "/file/test/fred/" == "fred/"
;; Posix:   makeRelative "some/path" "some/path/a/b/c" == "a/b/c"

(define (make-relative root path)
  (define (drop-abs p) 
    (match p (((and x (? is-path-separator?)) . xs) xs)
	   (else (drop-drive p))))

  (define (take-abs p)
    (match p (((and x (? is-path-separator?)) . xs) (list (path-separator)))
	   (else (map (lambda (y) (if (is-path-separator? y) (path-separator) (char-downcase y))) 
		      (take-drive p)))))

  (define (f x y pcs)
    (if (null? x) (drop-while is-path-separator? y)
	(match-let (((x1 x2)  (g x))
		    ((y1 y2)  (g y)))
		   (if (path-equal? x1 y1) (f x2 y2 pcs) pcs))))

  (define (g x)
    (let-values (((a b)  (break is-path-separator? (drop-while is-path-separator? x))))
       (list (drop-while is-path-separator? a) (drop-while is-path-separator? b) )))

  (let ((pcs (if (string? path) (string->list path) path))
	(rcs (if (string? root) (string->list root) root)))

    (cond ((path-equal? rcs pcs)  (list #\.))
	  ((not (equal? (take-abs rcs) (take-abs pcs)))  pcs)
	  (else (f (drop-abs rcs) (drop-abs pcs) pcs)))))

;; Normalise a file
;;  Posix:   normalise "/file/\\test////" == "/file/\\test/"
;;  Posix:   normalise "/file/./test" == "/file/test"
;;  Posix:   normalise "/test/file/../bob/fred/" == "/test/file/../bob/fred/"
;;  Posix:   normalise "../bob/fred/" == "../bob/fred/"
;;  Posix:   normalise "./bob/fred/" == "bob/fred/"
;;  Posix:   normalise "./" == "./"
;;  Windows: normalise "c:\\file/bob\\" == "C:\\file\\bob\\"
;;  Windows: normalise "c:\\" == "C:\\"
;;  Windows: normalise "\\\\server\\test" == "\\\\server\\test"
;;  Windows: normalise "c:/file" == "C:\\file"
;;           normalise "." == "."

(define (normalise path)
  (define (drop-dots ax)
    (lambda (xs)
      (match xs
	     (((#\.) . (and rest (? pair?))) ((drop-dots ax) rest))
	     ((x . rest)  ((drop-dots (cons x ax)) rest))
	     (()          (reverse ax)))))

  (define (prop-sep xs)
    (match xs 
	   (((and a (? is-path-separator?)) (and b (? is-path-separator?)) . rest)  
	    (prop-sep (cons a rest)))
	   (((and a (? is-path-separator?)) . rest)
	    (cons (path-separator) (prop-sep rest)))
	   ((x . rest)
	    (cons x (prop-sep rest)))
	   (() xs)))

  (define f (compose join-path (drop-dots (list)) split-directories prop-sep))

  (match-let (((drv pth)  (split-drive path)))
     (append (join-drive (normalise-drive drv) (f pth))
	     (if (and (not (null? pth)) (is-path-separator? (last pth)))
		 (list (path-separator)) (list)))))

(define (normalise-drive drive)
  (define (rep-slash x) (if (is-path-separator? x) (path-separator) x))
  (if (is-posix?) drive
      (let ((x2 (map rep-slash drive)))
	(if (read-drive-letter x2) (map char-upcase x2) drive))))
	


;; information for validity functions on Windows
;; see http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/naming_a_file.asp

(define bad-chars (list->char-set (string->list ":*?><|")))

(define (is-bad-char? c)  (char-set-contains? bad-chars c))

(define bad-elems
  (map string->list
       (list "CON"  "PRN"  "AUX"  "NUL"  "COM1" "COM2" 
	     "COM3" "COM4" "COM5" "COM6" "COM7" "COM8"
	     "COM9" "LPT1" "LPT2" "LPT3" "LPT4" "LPT5"
	     "LPT6" "LPT7" "LPT8" "LPT9" "CLOCK$")))


(define (is-bad-elem? x) 
  (let ((x1 (map char-upcase (drop-all-extensions x))))
    (member x1 bad-elems)))

(define (is-valid? p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (and (not (null? pcs))
	 (or (is-posix?)
	     (let ((pcs1 (drop-drive pcs)))
	       (and (not (any is-bad-char? pcs1))
		    (not (any is-bad-elem? (split-directories pcs1)))
		    (not (and (>= (length pcs) 2) 
			      (every is-path-separator? pcs)))))))))
		    

;; Take a FilePath and make it valid; does not change already valid FilePaths.
;; isValid x ==> makeValid x == x
;; makeValid "" == "_"
;; Windows: makeValid "c:\\test:of_test" == "c:\\test_of_test"
;; Windows: makeValid "test*" == "test_"
;; Windows: makeValid "c:\\test\\nul" == "c:\\test\\nul_"
;; Windows: makeValid "c:\\test\\prn.txt" == "c:\\test\\prn_.txt"
;; Windows: makeValid "c:\\test/prn.txt" == "c:\\test/prn_.txt"
;; Windows: makeValid "c:\\nul\\file" == "c:\\nul_\\file"

(define (valid-chars p)
  (map (lambda (x) (if (is-bad-char? x) #\_ x)) p))

(define (valid-elements p)
  (define (g x) 
    (let-values (((a b) (span is-path-separator? (reverse x))))
		(append (h (reverse b)) (reverse a))))
  (define (h x)
    (match-let ((( a b) (split-all-extensions x)))
	       (if (is-bad-elem? a) (add-extension (append a (list #\_)) b) x)))
  (join-path (map g (split-path p))))

(define (make-valid p)
  (let ((pcs (if (string? p) (string->list p) p)))
    (cond ((null? pcs)  (list #\_))
	  ((is-posix?)    pcs)
	  ((and (>= (length pcs) 2) (every is-path-separator? pcs))
	   (append (take pcs 2) (string->list "drive")))
	  (else (match-let (((drv pth) (split-drive pcs)))
			   (join-drive drv (valid-elements (valid-chars pth))))))))


;; Is a path relative, or is it fixed to the root?
;; Windows: isRelative "path\\test" == True
;; Windows: isRelative "c:\\test" == False
;; Windows: isRelative "c:test" == True
;; Windows: isRelative "c:" == True
;; Windows: isRelative "\\\\foo" == False
;; Windows: isRelative "/foo" == True
;; Posix:   isRelative "test/path" == True
;; Posix:   isRelative "/test" == False

(define (is-relative? p)
  (is-relative-drive? (take-drive p)))


(define (is-relative-drive? p)
  (or (null? p)
      (let ((p1 (read-drive-letter p)))
	(not (is-path-separator? (or (and p1 (last (first p1)))
				     (first p))))
	)))

(define (is-absolute? p)
  (not (is-relative? p)))


;;; Exported interface

(define filepath:is-posix?    is-posix?)
(define filepath:is-windows?  is-windows?)

(define filepath:path-separator path-separator)
(define filepath:path-separator-set path-separator-set)
(define filepath:is-path-separator? is-path-separator?)

(define filepath:search-path-separator search-path-separator)
(define filepath:is-search-path-separator? is-search-path-separator?)
(define filepath:ext-separator ext-separator)
(define filepath:is-ext-separator? is-ext-separator?)

;; wrappers that take a char-list result from an internal procedure
;; and convert it to a string

(define (list-wrapper lst) 
  (map list->string lst))

  ;; Path methods (environment $PATH)
(define filepath:split-search-path (compose list-wrapper split-search-path))
(define filepath:get-search-path   (compose list-wrapper get-search-path))

  ;; Extension procedures
(define filepath:split-extension    (compose list-wrapper split-extension))
(define filepath:take-extension     (compose list->string  take-extension))
(define filepath:drop-extension     (compose list->string drop-extension))
(define filepath:replace-extension  (compose list->string replace-extension))
(define filepath:add-extension      (compose list->string add-extension))
(define filepath:split-all-extensions   (compose list-wrapper split-all-extensions))
(define filepath:drop-all-extensions    (compose list->string drop-all-extensions))
(define filepath:take-all-extensions    (compose list->string take-all-extensions))
(define filepath:has-extension? has-extension?)

;; Drive procedures
(define filepath:split-drive  (compose list-wrapper split-drive))
(define filepath:join-drive   (compose list->string join-drive))
(define filepath:take-drive   (compose list->string take-drive))
(define filepath:drop-drive   (compose list->string drop-drive)) 
(define filepath:has-drive?   has-drive?)
(define filepath:is-drive?    is-drive?)

;; Operations on a file path, as a list of directories
(define filepath:split-file-name   (compose list-wrapper split-file-name))
(define filepath:take-file-name    (compose list->string take-file-name))
(define filepath:replace-file-name (compose list->string replace-file-name))
(define filepath:drop-file-name    (compose list->string drop-file-name))
(define filepath:take-base-name    (compose list->string take-base-name))
(define filepath:replace-base-name (compose list->string replace-base-name))
(define filepath:take-directory    (compose list->string take-directory))
(define filepath:replace-directory (compose list->string replace-directory))
(define filepath:combine           (compose list->string combine))
(define filepath:split-path        (compose list-wrapper split-path))
(define filepath:join-path         (compose list->string join-path))
(define filepath:split-directories (compose list-wrapper split-directories))

;; Low-level procedures
(define filepath:add-trailing-path-separator (compose list->string add-trailing-path-separator))
(define filepath:drop-trailing-path-separator (compose list->string drop-trailing-path-separator))
(define filepath:has-trailing-path-separator? has-trailing-path-separator?)

;; File name manipulators
(define filepath:normalise      (compose list->string normalise ))
(define filepath:make-relative  (compose list->string make-relative ))
(define filepath:make-valid     (compose list->string make-valid ))
(define filepath:is-valid?      is-valid? )
(define filepath:path-equal?    path-equal?)
(define filepath:is-relative?   is-relative?) 
(define filepath:is-absolute?   is-absolute?)

)
