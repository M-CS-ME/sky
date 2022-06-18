;; -*- Scheme -*-
;; This is a lambda-calculus evaluator using beta-reduction as an evaluation
;; strategy. This is part of my series on the Lambda-Calculus. Do whatever you
;; want with it.
;; 
;; NO WARRANTY IN ANY WAY. USE AT YOUR OWN RISK.
;;
;; Almost everything here is just an implementation of the basic definitions of
;; the lambda-calculus as described in my notes. There should be a detailed
;; explanation of this implementation over there.
;;
;; TODO:
;; 1. alpha-equiv
;; 2. substitution for lambdas
;; 3. single-step beta-reduction
;; 4. multi-step beta-reduction
;; 5. interface
;;     1. File IO
;;     2. REPL
;;
;; Contribute: https://github.com/M-CS-ME/sky

(define (in? obj lst)
  (cond ((null? lst) #f)
        ((eq? (car lst) obj) #t)
        (else (in? obj (cdr lst)))))

(define *variables* ; Most infinite list in the world
  '(a b c d e f g h i j k l m n o p q r s t u v w x y z
      A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

(define (variable? x)
  (in? x *variables*))
(define (lambda? exp) (eq? 'lambda (car exp)))
(define (application? exp) (lambda? (car exp)))

(define (make-lambda bound exp) `(lambda ,bound ,exp))
(define (lambda-exp l) (caadr l))
(define (lambda-params l) (cadr l))
(define (free-var? l v)
  (not (member v (lambda-params v))))

(define (free-vars l)
  (let loop ((vars *variables*) (acc '()))
    (cond ((null? vars) acc)
	      ((variable? (lambda-params l))
	       (delq (lambda-params l) *variables*))
	      ((not (in? (car vars) (lambda-params l)))
	       (loop (cdr vars) (append acc `(,(car vars)))))
	      (else (loop (cdr vars) acc)))))

(define (next-free-var l x)
  (cadr (member x (free-vars l))))

(define (rename m x y)
  (cond ((variable? m)
	     (if (eq? m x) y m))
	    ((lambda? m)
	     (make-lambda y
	       (rename (lambda-params m) x y)))
	    ((application? m)
	     (map (lambda (p)
		        (rename p x y)) m))))

(define (substitute m n x)
  (cond ((eq? m x) n)
        ((variable? m) m)
        ((lambda? m) ...)
        ((application? m)
         `(,(substitute (car m) n x)
           ,(substitute (cadr m) n x)))))
