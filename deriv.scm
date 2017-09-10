(define (equal? a b)
  (if (pair? a)
    (if (pair? b)
      (and 
        (equal? (car a) (car b))
        (equal? (cdr a) (cdr b)))
      false)
    (if (pair? b)
      false
      (eq? a b))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (multiplicand exp)
                         (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
           (exponent exp)
           (make-product
             (make-exponentiation (base exp) (- (exponent exp) 1))
             (deriv (base exp) var))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        ((sum? b) (cons '+ (cons a (cdr b))))
        (else (list '+ a b))))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b) (* a b)))
        ((product? b) (cons '* (cons a (cdr b))))
        (else (list '* a b))))

(define (make-exponentiation base exponent)
  (cond ((or (=number? base 1) (=number? exponent 0)) 1)
        ((=number? base 0) 0)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (sum? a)
  (and (pair? a) (eq? (car a) '+)))

(define (product? a)
  (and (pair? a) (eq? (car a) '*)))

(define (exponentiation? a)
  (and (pair? a) (eq? (car a) '**)))

(define (addend s) (cadr s))
(define (augend s) 
  (if (null? (cdddr s)) (caddr s) (cons '+ (cddr s))))

(define (multiplier s) (cadr s))
(define (multiplicand s) 
  (if (null? (cdddr s)) (caddr s) (cons '* (cddr s))))

(define (base s) (cadr s))
(define (exponent s) (caddr s))

