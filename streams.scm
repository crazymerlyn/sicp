(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref stream n)
  (if (= 0 n)
      (stream-car stream)
      (stream-ref stream (- n 1))))

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
