;Huffman Encoding

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (and (pair? object) (eq? (car object) 'leaf)))

(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))
  
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree) 
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))



(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
        ((= bit 1) (right-branch tree))
        (else (error "Invalid bit in CHOOSE-BRANCH" bit))))




(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (if (leaf? tree)
    (if (eq? s (symbol-leaf tree))
        '()
        (error "Symbol not in tree --ENCODE-SYMBOL" s))
    (let ((left (left-branch tree))
          (right (right-branch tree)))
      (cond ((element-list? s (symbols left))
             (cons 0 (encode-symbol s left)))
            ((element-list? s (symbols right))
             (cons 1 (encode-symbol s right)))
            (else (error "Symbol not in tree --ENCODE-SYMBOL" s))))))

(define (element-list? x xs)
  (cond ((null? xs) false)
        ((eq? x (car xs)) true)
        (else (element-list? x (cdr xs)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge node-set)
  (if (null? (cdr node-set))
      (car node-set)
      (let ((left (car node-set))
            (right (cadr node-set)))
        (successive-merge
          (adjoin-set (make-code-tree left right)
                      (cddr node-set))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-word '( a d a b b c a))


(define sample-pairs '((a 4) (b 2) (c 1) (d 1)))

