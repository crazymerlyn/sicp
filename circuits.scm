(define (invert input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
     (after-delay inverter-delay
                  (lambda ()
                    (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (and (get-signal a1) (get-signal a2))))
     (after-delay and-gate-delay
                  (lambda ()
                    (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (or (get-signal a1) (get-signal a2))))
     (after-delay or-gate-delay
                  (lambda ()
                    (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; Compound or gate composed of invert gate and and gates
; Delay = inverter-delay * 2 + and-gate-delay
(define (or-gate-compound a1 a2 output)
  (let ((i1 (make-wire))
        (i2 (make-wire))
        (c (make-wire)))
    (invert-input a1 i1)
    (invert-input a2 i2)
    (and-gate i1 i2 c)
    (invert-input c output)
    'ok))

