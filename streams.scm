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

(define (triples s t u)
  (cons-stream (list (stream-car s)
                     (stream-car t)
                     (stream-car u))
               (interleave
                 (stream-map (lambda (x) (cons (stream-car s) x))
                             (stream-cdr (pairs t u)))
                 (triples (stream-cdr s)
                          (stream-cdr t)
                          (stream-cdr u)))))


(define (is-pythagorean? x)
  (let ((i (car x))
        (j (cadr x))
        (k (caddr x)))
    (= (square k) (+ (square i) (square j)))))

(define pythagorean-triples
  (stream-filter is-pythagorean? (triples integers integers integers)))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let* ((s1car (stream-car s1))
                 (s2car (stream-car s2))
                 (s1w (weight s1car))
                 (s2w (weight s2car)))
            (cond ((< s1w s2w)
                   (cons-stream s1car 
                                (merge-weighted (stream-cdr s1) s2 weight)))
                  ((> s1w s2w)
                   (cons-stream s2car 
                                (merge-weighted s1 (stream-cdr s2) weight)))
                  (else
                    (cons-stream s1car
                                 (cons-stream s2car
                                              (merge-weighted (stream-cdr s1) 
                                                              (stream-cdr s2)
                                                              weight)))))))))


(define (weighted-pairs s1 s2 weight)
  (cons-stream
    (list (stream-car s1) (stream-car s2))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s1) x))
                  (stream-cdr s2))
      (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight)
      weight)))

(define ordbysum (weighted-pairs integers integers (lambda (x) (apply + x))))

(define (cube x) (* x x x))
(define (sumcube x) (+ (cube (car x)) (cube (cadr x))))
(define ordbysumcube (weighted-pairs integers integers sumcube))

(define (is-ramanujam-number? n)
  (define (loop n stream)
    (let ((a (stream-ref stream 0))
          (b (stream-ref stream 1)))
      (cond ((> (sumcube a) n) #f)
            ((< (sumcube a) n) (loop n (stream-cdr stream)))
            ((= (sumcube a) (sumcube b)) #t)
            (else
              (loop n (stream-cdr stream))))))
  (loop n ordbysumcube))

(define (uniq s)
  (let ((a (stream-ref s 0))
        (b (stream-ref s 1)))
    (if (= a b)
        (uniq (stream-cdr s))
        (cons-stream a (uniq (stream-cdr s))))))

(define ramanujam-possibs (uniq (stream-map sumcube ordbysumcube)))

(define ramanujam-numbers (stream-filter is-ramanujam-number? ramanujam-possibs))

(define (sumsq x) (+ (square (car x)) (square (cadr x))))
(define ordbysumsq (weighted-pairs integers integers sumsq))
(define (filter3sumsq stream)
  (let ((a (stream-ref stream 0))
        (b (stream-ref stream 1))
        (c (stream-ref stream 2))
        (rest (stream-cdr (stream-cdr (stream-cdr stream)))))
    (if (= (sumsq a) (sumsq b) (sumsq c))
        (cons-stream (list (sumsq a) (list a b c))
                     (filter3sumsq rest))
        (filter3sumsq (stream-cdr stream)))))
(define can-be-written-3-ways (filter3sumsq ordbysumsq))


(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-series dt integrand)
                              int)))
  int)

(define (RC R C dt)
  (define (voltage current initial-voltage)
    (add-streams
      (integral (scale-series (/ C) current) initial-value dt)
      (scale-series R current)))
  voltage)

(define (sign-change-detector a b)
  (if (= 0 (* a b))
      (if (>= (+ a b) 0) 0 1)
      (if (> (* a b) 0) 0 1)))

(define sense-data (cons-stream 1 (scale-series -1 sense-data)))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(define (smooth stream)
  (let ((a (stream-ref stream 0))
        (b (stream-ref stream 1)))
    (cons-stream (/ (+ a b) 2)
                 (smooth (stream-cdr stream)))))

(define (make-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
   (cons-stream (sign-change-detector avpt last-avg)
                (make-zero-crossings (stream-cdr input-stream)
                                     (stream-car input-stream)
                                     avpt))))
(define zero-crossings2
  (make-zero-crossings sense-data 0 0))

(define (make-zero-crossings2 input-stream initial-value)
  (let ((smoothed (smooth input-stream)))
   (stream-map sign-change-detector smoothed (cons-stream initial-value smoothed))))

(define zero-crossings3
  (make-zero-crossings2 sense-data 0))

(define (integral2 delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                (if (stream-null? integrand)
                    the-empty-stream
                    (integral2 (delay (stream-cdr integrand))
                              (+ (* dt (stream-car integrand))
                                 initial-value)
                              dt)))))

(define (solve-2nd a b dt y0 dy0)
  (define y (integral2 (delay dy) y0 dt))
  (define dy (integral2 (delay ddy) dy0 dt))
  (define ddy (add-streams
                (scale-series a dy)
                (scale-series b y)))
  y)

