(define (make-accumulator accum)
  (lambda (toadd)
    (set! accum (+ accum toadd))
    accum))

(define A (make-accumulator 5))

