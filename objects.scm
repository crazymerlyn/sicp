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

(define (make-account balance password)
  (define failed-attemps 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? pass password)
        (begin
          (set! failed-attemps 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unkown request -- MAKE_ACCOUNT"
                             m))))
        (begin
          (set! failed-attemps (+ 1 failed-attemps))
          (if (>= failed-attemps 7)
              (call-the-cops)
              (error "Incorrect password")))))
  dispatch)


(define (make-joint account oldpass newpass)
  (define notoldpass (symbol-append oldpass 'wrong))
  (define (dispatch pass m)
    (if (eq? pass newpass) (account oldpass m) (account notoldpass m)))
  dispatch)

; For testing/debugging
; (define a (make-account 100 'pass))
; (define w (a 'pass 'withdraw))

; (define b (make-joint a 'pass 'newpass))
; (define w2 (b 'newpass 'withdraw))
