(define-module lisp-cpd.main
  (use file.util)
  (use rfc.md5)
;;  (use util.lcs)
  (export
   lisp-cpd
   )
  )

(select-module lisp-cpd.main)

(define-constant +max-depth+ 5)
(define-constant +least-tokes+ 5)

(define (car* x . default)
  (if (pair? x)
      (car x)
      (car* default undef)))

(define-macro (if-bind var test then . else)
  `(let1 ,var ,test
     (if ,var ,then ,(car* else))))

(define-macro (aif test then . else)
  (macroexpand `(if-bind it ,test ,then ,@else)))

(define (map-tree proc tree)
  (cond ((pair? tree)
	 (cons (map-tree proc (car tree))
	       (map-tree proc (cdr tree))))
	;; nilはどうしようか
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

;; (leaf<num '(1 2 3 4) 3)

(define (lisp-cpd paths)
  (define (make-digest str)
    (if (string? str)
	(md5-digest-string str)
	str))
  (let ((path-token-table (make-hash-table 'string=?))
	(skip-num1 0)
	(skip-num2 1))
    (dolist (path paths)
      (hash-table-put! path-token-table path (map-tree make-digest (file->sexp-list path))))
    (hash-table-for-each path-token-table
      (lambda (patha treea)
	(inc! skip-num1)
	(set! skip-num2 skip-num1)
	(hash-table-for-each path-token-table
	  (lambda (pathb treeb)
	    (if (> (dec! skip-num2) -1)
		#t
		(format #t "~a ~a ~s~%" patha  pathb (diff treea treeb)))))))))

(define (walk-tree position tree proc)
  (define (tree-before-push! tree-before sexp)
    (dotimes (i (- (vector-length tree-before) 1))
      (vector-set! tree-before i (vector-ref tree-before (+ i 1))))
    (vector-set! tree-before (- (vector-length tree-before) 1) (cons (not (leaf<num sexp +least-tokes+)) sexp)))
  (let1 tree-before (make-vector +max-depth+ #f)
    (define (loop position tree)
      (tree-before-push! tree-before tree)
      (if (pair? tree)
	  (or (loop (cons #t position) (car tree))
	      (walk-tree (cons #f position) (cdr tree) proc)) ;;新規tree-before
	  (let1 proc-ret #f
	    (dotimes (i (vector-length tree-before) proc-ret)
	      (or proc-ret
		  (aif (vector-ref tree-before i)
		    (set! proc-ret (proc it position))
		    #f))))))
    (loop position tree)))

(define (diff treea treeb)
  (let ((ret '()))
    (walk-tree '()
	       treea
	       (lambda (sub-treea posa)
		 (walk-tree '()
			    treeb
			    (lambda (sub-treeb posb)
			      (if (and (car sub-treea)
				       (car sub-treeb)
				       (equal? (cdr sub-treea) (cdr sub-treeb)))
				  (begin (push! ret (list (cdr sub-treea) posa posb))
					 #t)
				  #f)))))
    ret))
;;   (lcs-with-positions treea

;; (lisp-cpd '("/home/tyt/base.scm" "/home/tyt/macro-old.scm"))
