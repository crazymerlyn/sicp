(load "compiler.scm")

(define result
  (compile
    '(define (fact n)
       (if (= n 1)
           1
           (* (fact (- n 1)) n)))
    'val
    'next))

(define result2
  (compile
    '(define (fact n)
       (if (= n 1)
           1
           (* n (fact (- n 1)))))
    'val
    'next))


(newline)
(display (length (statements result)))
(newline)
(display (length (statements result2)))
