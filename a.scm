(define (fast-expt b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* b a)))))

(define (accumulate combiner null-value term next a b)
  (if (> a b) 
      null-value 
      (accumulate combiner (combiner null-value (term a)) term next (next a) b)))

(define (sum term next a b)
  (accumulate + 0 term next a b))

(define (product term next a b)
  (accumulate * 1 term next a b))


(define (inc n) (+ n 1))
(define (cube x) (* x x x))

(define (integrate f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (/ (* h (sum term inc 0 n)) 3))


(define (fixed-point f guess)
  (define (good-enough? guess)
    (< (abs (- (f guess) guess)) 0.00001))
  (define (try guess)
    (if (good-enough? guess) guess (try (f guess))))
  (try guess))

(define (cont-frac n d k)
  (define (cont-frac-iter n d k r)
    (if (= k 0) r (cont-frac-iter n d (- k 1) (/ (n k)(+ (d k) r)))))
  (define (cont-frac-recur n d k i)
    (if (< k i) 0 (/ (n i) (+ (d i) (cont-frac-recur n d k (+ i 1))))))
  (cont-frac-recur n d k 1))


(define (const i) 1.0)

(define (d-for-e i)
  (if (= (remainder i 3) 2) (* (ceiling (/ i 3)) 2) 1))


(define (tan-cf x k)
  (define (n-for-tan i)
    (if (= i 1.0) x (- 0.0 (* x x))))
  (define (d-for-tan i) 
    (- (* 2 i) 1.0))
  (cont-frac n-for-tan d-for-tan k))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))


(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform f)
  (lambda (x) (- x (/ (f x) ((deriv f) x)))))

(define (newtons-method f guess)
  (fixed-point (newton-transform f) guess))


(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

(define (double f)
  (lambda (x) (f (f x))))


(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f n)
  (if (= n 0) (lambda (x) x) (compose f (repeated f (- n 1)))))

(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (nth-smooth f n)
  ((repeated smooth n) f))

