(define (make-rand)
  (define seed (random 100))
  (let ((a 1103515245)
        (b 12345)
        (m 4294967296))
    (define (rand symbol)
      (cond ((eq? symbol 'generate)
             (set! seed (remainder (+ (* seed a) b) m))
             (remainder (quotient seed 65536) 32768))
            ((eq? symbol 'reset)
             (lambda (new-seed)
               (set! seed new-seed)))
            (else
              (error "Unsupported operation -- RAND" symbol))))
    rand))

(define rand (make-rand))


(define (make-f)
  (define state 0)
  (define (f n)
    (let ((temp state))
     (begin
       (set! state n)
       temp)
     ))
  f)

(define f (make-f))

(define (even-odd x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

