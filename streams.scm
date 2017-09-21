(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref stream n)
  (if (= 0 n)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (< low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (integers-from n) (cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))

(define factorials (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

(define (stream-take n s)
  (if (= n 0)
      the-empty-stream
      (cons-stream (stream-car s)
                   (stream-take (- n 1) (stream-cdr s)))))

(define (display-stream s)
  (display (stream->list s)))

(define (partial-sums stream)
  (if (stream-null? stream)
      the-empty-stream
      (let ((first (stream-car stream)))
       (cons-stream first
                    (stream-map (lambda (x) (+ x first))
                                (partial-sums (stream-cdr stream)))))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))

(define (multiply n)
  (lambda (x) (* x n)))

(define S (cons-stream 1 (merge (stream-map (multiply 2) S)
                                (merge (stream-map (multiply 3) S)
                                       (stream-map (multiply 5) S)))))


(define (integrate-series series)
  (stream-map / series integers))

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (stream-map (multiply (stream-car s1))
                                        (stream-cdr s2))
                            (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (define x (cons-stream 1 (stream-map - (mul-series (stream-cdr s) x))))
  x)

(define (scale-series n series)
  (stream-map (lambda (x) (* x n)) series))

(define (div-series s1 s2)
  (cond ((stream-null? s2) (error "Invalid stream"))
        ((= 0 (stream-car s2))
         (error "Can't divide by stream if constant term is 0"))
        (else
          (let ((scale (/ (stream-car s2))))
           (mul-series (scale-series scale s1)
                       (invert-unit-series (scale-series scale s2)))))))

(define (stream-limit stream tolerance)
  (if (or (stream-null? stream)
          (stream-null? (stream-cdr stream)))
      (error "Empty stream to stream-limit")
      (let ((first (stream-car stream))
            (second (stream-car (stream-cdr stream))))
        (if (< (abs (- first second)) tolerance)
            second
            (stream-limit (stream-cdr stream) tolerance)))))

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-series
  (partial-sums (ln2-summands 1)))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2
                               (stream-cdr s1)))))

(define (pairs s1 s2)
  (cons-stream
    (list (stream-car s1) (stream-car s2))
    (interleave
      (stream-map (lambda (x) (list (stream-car s1) x))
                  (stream-cdr s2))
      (pairs (stream-cdr s1) (stream-cdr s2)))))

(define (all-pairs s1 s2)
  (cons-stream
    (list (stream-car s1) (stream-car s2))
    (interleave
      (stream-map (lambda (x) (list (stream-car s1) x))
                  (stream-cdr s2))
      (all-pairs (stream-cdr s1) s2))))

