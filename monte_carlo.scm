(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
   (+ low (random range))))

(define (inside-rect x1 x2 y1 y2 P)
  (lambda ()
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials (inside-rect x1 x2 y1 y2 P)))

(define (make-circle-predicate x y r)
  (lambda (x1 y1)
    (<= (+ (expt (- x x1) 2) (expt (- y y1) 2)) (* r r))))


(define (estimate-pi trials)
  (define p (make-circle-predicate 0.0 0.0 1.0))
  (* 4.0 (estimate-integral p -1.0 1.0 -1.0 1.0 trials)))

