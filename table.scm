(define *table* 'TABLE)

(define (make-table same-key?)
  (let ((local-table (list *table*)))
   (define (lookup key-1 key-2)
     (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
      (if subtable
          (let ((record (assoc key-2 (cdr subtable) same-key?)))
           (if record
               (cdr record)
               false))
          false)))
   (define (insert! key-1 key-2 value)
     (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
      (if subtable
          (let ((record (assoc key-2 (cdr subtable) same-key?)))
           (if record
               (set-cdr! record value)
               (set-cdr! subtable
                         (cons (cons key-2 value)
                               (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
     'ok)
   (define (dispatch m)
     (cond ((eq? m 'lookup-proc) lookup)
           ((eq? m 'insert-proc!) insert!)
           (else (error "Unknown operation -- TABLE" m))))
   dispatch))

(define (make-generic-table)
  (let ((local-table (list *table*)))
   (define (lookup keys)
     (define (lookup1 table keys)
       (if (null? keys)
           (cdr table)
           (let ((record (assoc (car keys) (cdr table))))
            (if record
                (lookup1 record (cdr keys))
                false))))
     (lookup1 local-table keys))
   (define (insert! keys value)
     (define (insert1! table keys)
       (if (null? keys)
           (set-cdr! table value)
           (let ((record (assoc (car keys) (cdr table))))
            (if record
                (insert1! record (cdr keys))
                (begin
                  (set-cdr! table 
                            (cons (cons (car keys) '())
                                  (cdr table)))
                  (insert1! (cadr table) (cdr keys)))))))
     (insert1! local-table keys))
   (define (dispatch m)
     (cond ((eq? m 'lookup-proc) lookup)
           ((eq? m 'insert-proc!) insert!)
           (else (error "Unknown operation generic TABLE" m))))
   dispatch))
