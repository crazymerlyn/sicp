(define (make-accumulator accum)
  (lambda (toadd)
    (set! accum (+ accum toadd))
    accum))

(define (make-monitored f)
  (let ((count 0))
   (lambda (input)
     (cond
       ((eq? input 'how-many-calls?) count)
       ((eq? input 'reset-count)
        (set! count 0)
        count)
       (else
         (set! count (+ count 1))
         (f input))))))
