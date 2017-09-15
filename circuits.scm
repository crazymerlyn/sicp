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

; Delay = max(2 * and-gate-delay + inverter-delay, or-gate-delay + and-gate-delay)
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
   (or-gate a b d)
   (and-gate a b c)
   (invert c e)
   (and-gate d e s)
   'ok))

; Delay = half-adder-delay * 2 + or-gate-delay
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; Delay = n * full-adder-delay where n is no. of wires in as/bs/sums
(define (ripple-adder as bs sums c)
  (cond ((and (null? as) (null? bs) (null? sums)) 'ok)
        ((or (null? as) (null? bs) (null? sums))
         (error "unequal no. of wires in ripple-adder" as bs sums))
        (else
          (let ((c1 (make-wire)))
           (full-adder (car as) (car bs) c (car sums) c1)
           (ripple-adder (cdr as) (cdr bs) (cdr sums) c1)))))
