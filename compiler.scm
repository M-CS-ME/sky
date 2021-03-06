;; Turn this whole compiler into a "data-oriented" program. Table look up > adding variables. 

(define (compile expr)
  (cond ((true? expr) '(λ (a b) a))
        ((false? expr) '(λ (a b) b))
        ((not? expr) '(λ (a) (a (λ (a b) b) (λ (a b) a))))
        ((and? expr) '(λ (a b) (a b (λ (a b) b))))
        ((or? expr) '(λ (a b) (a (λ (a b) a) b)))
        ((if? expr) '(λ (a b c) (a b c)))
        ((number? expr) (church-enc expr))
        ((inc? expr) '(λ (f m x) (m (f m x))))
        ((symbol? expr) expr)
        ((lambda? expr) `(λ ,(cadr expr) ,(compile (caddr expr))))
        ((application? expr) (cons (compile (car expr))
                                   (map compile (cdr expr))))))

(define (true? expr) (equal? expr 'true))
(define (false? expr) (equal? expr 'false))
(define (not? expr) (equal? expr 'not))
(define (and? expr) (equal? expr 'and))
(define (or? expr) (equal? expr 'or))
(define (if? expr) (equal? expr 'if))
(define (application? expr) (list? expr))
(define (inc? expr) (equal? 'inc expr))
(define (lambda? expr) (equal? (car expr) 'lambda))
(define (church-enc n)
  (if (= n 0) '(λ (s o) o)
      `(λ (s o) (s ,(caddr (church-enc (- n 1)))))))
