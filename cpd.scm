(define-module lisp-cpd.cpd
  (use srfi-1)
  (use srfi-26)
  (use srfi-42)
  (use util.match)
  (use file.util)
  (use rfc.md5)
  (use util.combinations)
;;  (use util.lcs)
  (export
   lisp-cpd
   )
  )

(select-module lisp-cpd.cpd)

(define-constant +least-tokes+ 5) ;; larger-than 1

(define undef (if #f #t))
(define (car* x . defaul)
  (if (pair? x)
      (car x)
      (car* defaul undef)))

(define-macro (if-bind var test then . else)
  `(let1 ,var ,test
     (if ,var ,then ,(car* else))))

(define-macro (aif test then . else)
  (macroexpand `(if-bind it ,test ,then ,@else)))

(define (map-tree proc tree)
  (cond ((pair? tree)
	 (cons (map-tree proc (car tree))
	       (map-tree proc (cdr tree))))
	((null? tree)
	 '())
	(else (proc tree))))

(define (leaf<num tree num)
  (cond ((pair? tree)
	 (aif (leaf<num (car tree) num)
	   (leaf<num (cdr tree) it)
	   #f))
	((null? tree)
	 num)
	(else (if (= (- num 1) 0) #f (- num 1)))))

(define (scheme-tree-fold proc knil tree)
  (define (loop position knil tree)
    (cond ((pair? tree)
	   (let lp ((position (cons #f position))
		    (knil (loop (cons #t position) (proc position knil tree) (car tree)))
		    (treed (cdr tree)))
	     (cond ((pair? treed)
		    (lp (cons #f position) (loop (cons #t position) knil (car treed)) (cdr treed)))
		   ((null? treed)
		    knil)
		   (else (proc position knil treed)))))
	  ((null? tree)
	   knil)
	  (else (proc position knil tree))))
  (loop '() knil tree))

(define (scheme-tree-fold/no-duplicate proc knil tree)
  (define (loop position knil tree)
    (cond ((pair? tree)
	   (let ((knil-p knil)
		 (knil (proc position knil tree)))
	     (if (eq? knil-p knil)
		 (let lp ((position (cons #f position))
			  (knil (loop (cons #t position) knil (car tree)))
			  (treed (cdr tree)))
		   (cond ((pair? treed)
			  (lp (cons #f position) (loop (cons #t position) knil (car treed)) (cdr treed)))
			 ((null? treed)
			  knil)
			 (else (proc position knil treed))))
		 knil)))
	  ((null? tree)
	   knil)
	  (else (proc position knil tree))))
  (loop '() knil tree))

(define (tree->subtree-digests )
  1)

(define (lisp-cpd . paths)
  (define (make-digest str)
    (if (string? str)
	(md5-digest-string str)
	str))
  (map (lambda (lis-ab)
	 (match-let1 ((patha . sexpa) (pathb . sexpb)) lis-ab
	   (cons* patha pathb (cpd sexpa sexpb))))
       (combinations (map (lambda (path) (cons path (file->sexp-list path))) paths) 2)))

(define (cpd treea treeb)
  (scheme-tree-fold/no-duplicate
   (lambda (posa knil sub-treea)
     (if (leaf<num sub-treea +least-tokes+)
	 knil
	 (aif (scheme-tree-fold
	       (lambda (posb knil sub-treeb)
		 (or knil (and (equal? sub-treea sub-treeb) (list sub-treea posa posb))))
	       #f treeb)
	   (cons it knil)
	   knil)))
   '() treea))
;; (cpd '((((a b ) a b) (a b)) (c d) (c d)) '((c d) (((a b) a b) (a b))))

