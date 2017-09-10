(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) 
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


(define (tree->list tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result)))))
  (copy-to-list tree '()))


(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (let ((this-entry (car non-left-elts))
                    (right-result (partial-tree (cdr non-left-elts)
                                                right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry left-tree right-tree)
                        remaining-elts))))))))
  (car (partial-tree elements (length elements))))


(define (intersection-set s1 s2)
  (define (intersection-list l1 l2)
    (cond ((or (null? l1) (null? l2)) '())
          ((= (car l1) (car l2))
           (cons (car l1) (intersection-list (cdr l1) (cdr l2))))
          ((< (car l1) (car l2))
           (intersection-list (cdr l1) l2))
          (else intersection-list l1 (cdr l2))))
  (list->tree (intersection-list (tree->list s1)
                                 (tree->list s2))))

(define (union-set s1 s2)
  (define (union-list l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((= (car l1) (car l2))
           (cons (car l1) (union-list (cdr l1) (cdr l2))))
          ((< (car l1) (car l2))
           (cons (car l1) (union-list (cdr l1) l2)))
          ((> (car l1) (car l2))
           (cons (car l2) (union-list l1 (cdr l2))))))
  (list->tree (union-list (tree->list s1)
                          (tree->list s2))))

