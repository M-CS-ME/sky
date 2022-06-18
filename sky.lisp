(defun bound-var (expr) (cadr expr))
(defun body (expr) (caddr expr))
(defun lambdap (expr)
  (if (listp expr) (eq (car expr) 'lambda) nil))

(defun free-vars (M)
  (cond ((atom M) `(,M))
        ((lambdap M) (set-difference (free-vars (body M)) `(,(bound-var M))))
        (t (union (free-vars (car M)) (free-vars (cadr M))))))
(defun free-var-p (expr var) (member var (free-vars expr)))

(defun relabel (M y x)
  (cond ((eq M x) y)
        ((atom M) M)
        ((lambdap M)
         (cond ((eq (bound-var M) x) `(lambda ,y ,(relabel (body M) y x)))
               (t (let ((z (gensym))) `(lambda ,z ,(relabel (body M) y x))))))
        (t (mapcar (lambda (e) (relabel e y x)) M))))

(defun alpha-eq (a b)
  (if (or (atom a) (atom b)) (equal a b)
      (equal a `(lambda ,(bound-var a)
                   ,(relabel (body b) (bound-var a) (bound-var b))))))

(defun substit (M N x)
  (cond ((eq M x) N)
        ((atom M) M)
        ((lambdap M)
         (cond ((eq (bound-var M) x) M)
               ((free-var-p N (bound-var M))
                (let ((y (gensym)))
                  `(lambda ,y ,(substit (relabel (body M) y (bound-var M)) N x))))
               (t `(lambda ,(bound-var M) ,(substit (body M) N x)))))
        (t (mapcar (lambda (e) (substit e N x)) M))))

(defun single-step-reduce (M)
  (cond ((atom M) M)
        ((lambdap M) `(lambda ,(bound-var M) ,(single-step-reduce (body M))))
        ((lambdap (car M))
         (append (substit (body (car M)) (cadr M) (bound-var (car M))) (cddr M)))
        (t (mapcar #'single-step-reduce M))))

(defun beta-reduce (expr)
  (labels ((iter (curr prev)
             (if (equal curr prev)
                 prev
                 (iter (single-step-reduce curr) curr))))
    (iter (single-step-reduce expr) expr)))

#| Tests

(defparameter successor '(lambda f (lambda m (lambda x (m (f m x))))))
(defparameter one '(lambda s (lambda o o)))

(single-step-reduce (single-step-reduce `(,successor ,one)))

(defparameter true '(lambda x (lambda y x)))
(defparameter false '(lambda x (lambda y y)))
(defparameter lambda-not `(lambda x ((x ,false) ,true)))

(beta-reduce `(,lambda-not ,true))
(beta-reduce `(,lambda-not ,false))

so the issue is that ((lambda s (lambda o o)) m x) isn't getting
evaluated properly. 

(cddr '((lambda s (lambda o o)) m x))

(substit '(lambda o o) 'm 'o)

|#
