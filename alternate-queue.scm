(define (make-queue)
  (define front-ptr '())
  (define rear-ptr '())
  (define (dispatch m)
    (cond ((eq? m 'front-ptr) (lambda () front-ptr))
          ((eq? m 'rear-ptr) (lambda () rear-ptr))
          ((eq? m 'set-front-ptr!) (lambda (item) (set! front-ptr item)))
          ((eq? m 'set-rear-ptr!) (lambda (item) (set! rear-ptr item)))
          (else (error "Invalid dispatch message to queue" m))
          ))
  dispatch)


(define (front-ptr queue) ((queue 'front-ptr)))
(define (rear-ptr queue) ((queue 'rear-ptr)))
(define (set-front-ptr! queue item) ((queue 'set-front-ptr!) item))
(define (set-rear-ptr! queue item) ((queue 'set-rear-ptr!) item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
   (cond ((empty-queue? queue)
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
         (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))


(define (print-queue queue)
  (front-ptr queue))
