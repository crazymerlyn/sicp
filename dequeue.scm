(define (make-dequeue)
  (define front-ptr '())
  (define rear-ptr '())
  (define (dispatch m)
    (cond ((eq? m 'front-ptr) (lambda () front-ptr))
          ((eq? m 'rear-ptr) (lambda () rear-ptr))
          ((eq? m 'set-front-ptr!) (lambda (item) (set! front-ptr item)))
          ((eq? m 'set-rear-ptr!) (lambda (item) (set! rear-ptr item)))
          (else (error "Invalid dispatch message to dequeue" m))
          ))
  dispatch)


(define (front-ptr dequeue) ((dequeue 'front-ptr)))
(define (rear-ptr dequeue) ((dequeue 'rear-ptr)))
(define (set-front-ptr! dequeue item) ((dequeue 'set-front-ptr!) item))
(define (set-rear-ptr! dequeue item) ((dequeue 'set-rear-ptr!) item))

(define (empty-dequeue? dequeue) (or (null? (front-ptr dequeue)) (null? (rear-ptr dequeue))))

(define (front-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "FRONT called with an empty dequeue" dequeue)
      (caar (front-ptr dequeue))))

(define (rear-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "REAR called with an empty dequeue" dequeue)
      (caar (rear-ptr dequeue))))

(define (front-insert-dequeue! dequeue item)
  (let ((new-pair (cons (cons item '()) (front-ptr dequeue))))
   (cond ((empty-dequeue? dequeue)
          (set-front-ptr! dequeue new-pair)
          (set-rear-ptr! dequeue new-pair)
          dequeue)
         (else
           (set-cdr! (car (front-ptr dequeue)) new-pair)
           (set-front-ptr! dequeue new-pair)
           dequeue))))

(define (rear-insert-dequeue! dequeue item)
  (let ((new-pair (cons (cons item (rear-ptr dequeue)) '())))
   (cond ((empty-dequeue? dequeue)
          (set-front-ptr! dequeue new-pair)
          (set-rear-ptr! dequeue new-pair)
          dequeue)
         (else
           (set-cdr! (rear-ptr dequeue) new-pair)
           (set-rear-ptr! dequeue new-pair)
           dequeue))))

(define (front-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
         (error "FRONT-DELETE! called with an empty dequeue" dequeue))
        (else
          (set-front-ptr! dequeue (cdr (front-ptr dequeue)))
          (if (null? (front-ptr dequeue))
              (set-rear-ptr! dequeue '())
              (set-cdr! (car (front-ptr dequeue)) '()))
          dequeue)))

(define (rear-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
         (error "REAR-DELETE! called with an empty dequeue" dequeue))
        (else
          (set-rear-ptr! dequeue (cdr (car (rear-ptr dequeue))))
          (if (null? (rear-ptr dequeue))
              (set-front-ptr! dequeue '())
              (set-cdr! (rear-ptr dequeue) '()))
          dequeue)))


(define (print-dequeue dequeue)
  (map car (front-ptr dequeue)))
