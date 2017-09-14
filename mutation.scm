(define (elem? element array)
  (if (null? array)
      #f
      (if (eq? element (car array))
          #t
          (elem? element (cdr array)))))
(define (count-pairs x)
  (define (count head seen)
    (if (null? head)
        0
        (if (elem? (cdr head) (cons head seen))
            1
            (+ 1 (count (cdr head) (cons head seen))))))
  (count x '()))
