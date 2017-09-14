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

(define (cycle? head)
  (define (race tortoise hare)
    (if (or (null? tortoise) (null? hare))
        #f
        (if (eq? tortoise hare)
            #t
            (if (null? (cdr hare))
                #f
                (cycle? (cdr tortoise) (cddr hare))))))
  (if (or (null? head) (null? (cdr head)))
      #f
      (race (cdr head) (cddr head))))
