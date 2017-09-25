(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
   (for-each (lambda (register-name)
               ((machine 'allocate-register) register-name))
             (register-names))
   ((machine 'install-operations) ops)
   ((machine 'install-instruction-sequence)
    (assembe controller-text machine))
   machine))

(define (make-register name)
  (let ((contents '*unassigned*))
   (define (dispatch m)
     (cond ((eq? m 'get) contents)
           ((eq? m 'set)
            (lambda (new-val) (set! contents new-val)))
           (else
             (error "Unknown request -- REGISTER" m))))
   dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register val)
  ((register 'set) val))


(define (make-stack)
  (let ((s '()))
   (define (push val)
     (set! s (cons val s)))
   (define (pop)
     (if (null? s)
         (error "Empty stack -- POP")
         (let ((top (car s)))
          (set! s (cdr s))
          top)))
   (define (initialize)
     (set! s '())
     'done)
   (define (dispatch m)
     (cond ((eq? m 'push) push)
           ((eq? m 'pop) (pop))
           ((eq? m 'initialize) (initialize))
           (else
             (error "Unknown request -- STACK" m))))
   dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))


(define (make-new-machine)
  (let ((pc (make-register))
        (flag (make-register))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register register-name)
        (if (assoc register-name register-table)
            (error "Multiply defined register:" register-name)
            (set! register-table
              (cons (list register-name (make-register))
                    register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((entry (assoc name register-table)))
         (if entry
             (cadr entry)
             (error "Unknown register -- LOOKUP-REGISTER" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
         (if (null? insts)
             'done
             (begin
               ((instruction-execution-proc (car insts)))
               (execute)))))
      (define (dispatch m)
        (cond ((eq? m 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? m 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? m 'allocate-register) allocate-register)
              ((eq? m 'get-register) lookup-register)
              ((eq? m 'install-operations)
               (lambda (ops) (set! the-ops (append the-opps ops))))
              ((eq? m 'stack) stack)
              ((eq? m 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" m)))))))

(define (start machine)
  (machine 'start))

(define (get-register machine register-name)
  ((machine 'get-register) register-name))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)


(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))


(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
        (cdr text)
        (lambda (insts labels)
          (let ((next-inst (car txt)))
           (if (symbol? next-inst)
               (receive insts
                        (cons-label (make-label-entry next-inst
                                                      insts)
                                    labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define (cons-label label labels)
  (if (contains? (label-name label)
                 (map label-name labels))
      (error "Multiply used label: " (label-name label))
      (cons label labels)))

(define (contains? name names)
  (if (null? names)
      #f
      (if (eq? (car names) name)
          #t
          (contains? name (cdr names)))))


(define (update-insts! insts labels machine)
  (let ((pc (get-register-contents machine 'pc))
        (flag (get-register-contents machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst) labels machine
            pc flag stack ops)))
      insts)))


(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))


(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-table labels label-name)
  (let ((val (assoc label-name labels)))
   (if val
       (cdr val)
       (error "Undefined label -- ASSEMBLE" label-name))))


