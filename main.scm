;; -*- encoding: utf-8 -*-
(use lisp-cpd.cpd)

(define (main args)
  (for-each (lambda (y)
	      (print (car y) (cadr y) ":")
	      (for-each (lambda (x) (format #t "	~s~%" (car x))) (cddr y)))
	    (apply lisp-cpd (cdr args))))

