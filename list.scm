(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items) (cons (car items) result))))
  (reverse-iter items `()))


(define (same-parity x . xs)
  (define (desired y)
    (= (remainder (- x y) 2) 0))
  (if (null? xs)
      (list x)
      (cons x (filter desired xs))))


(define (for-each f items)
  (define (g item rest) (f item) (for-each f rest))
  (if (null? items)
      true
      (g (car items) (cdr items))))


(define (deep-reverse items)
    (reverse (map (lambda (item)
                    (if (pair? item)
                        (deep-reverse item)
                        item)) items)))


(define (fringe tree)
  (if (null? tree)
      tree
      (if (pair? (car tree))
          (append (fringe (car tree)) (fringe (cdr tree)))
          (cons (car tree) (fringe (cdr tree))))))


(define (make-mobile left right)
  (cons left right))

(define (make-branch len structure)
  (cons len structure))

(define (left-branch x) (car x))
(define (right-branch x) (cdr x))

(define (branch-length x) (car x))
(define (branch-structure x) (cdr x))


(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define (balanced? mobile)
  (cond ((null? mobile) true)
        ((not (pair? mobile)) true)
        (else (and
                (= (* (total-weight (branch-structure(left-branch mobile)))
                      (branch-length (left-branch mobile)))
                   (* (total-weight (branch-structure(right-branch mobile)))
                      (branch-length (right-branch mobile))))
                (balanced? (branch-structure (left-branch mobile)))
                (balanced? (branch-structure (right-branch mobile)))))))
                   


(define (tree-map f tree)
  (map (lambda (x)
         (if (pair? x)
           (tree-map f x)
           (f x)))
       tree))

(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (set) (cons (car s) set)) rest)))))


(define (square-tree tree)
  (tree-map square tree))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op initial (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))
 

(define (enumerator a b)
  (if (> a b) () (cons a (enumerator (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerator 1 (- i 1))))
           (enumerator 1 n)))


(define (unique-triples n)
  (flatmap (lambda (i) 
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerator 1 (- j 1))))
                      (enumerator 1 (- i 1))))
           (enumerator 1 n)))

(define (triple-sum-to-s s)
  (filter (lambda (x) (= s (accumulate + 0 x))) (unique-triples s)))


(define (repeat item n)
  (if (= n 0) () (cons item (repeat item (- n 1)))))

(define (nth-elem n items)
  (if (= n 1) (car items) (nth-elem (- n 1) (cdr items))))

(define (queens board-size)
  (define empty-board (repeat 0 board-size))
  
  (define (adjoin-position new-row k rest-of-queens)
    (if (= k 1)
      (cons new-row (cdr rest-of-queens))
      (cons 
        (car rest-of-queens) 
        (adjoin-position new-row (- k 1) (cdr rest-of-queens)))))
  
  (define (safe? k positions)
    (let ((new-row (nth-elem k positions)))
      (null?
        (filter
          (lambda (i)
            (let ((ith-row (nth-elem i positions)))
              (or 
                (= ith-row new-row)
                (= (abs (- ith-row new-row)) (- k i)))))
          (enumerator 1 (- k 1))))))
  
  (define (queens-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerator 1 board-size)))
          (queens-cols (- k 1))))))
  (queens-cols board-size))

