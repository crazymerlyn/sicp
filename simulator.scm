(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
   ((machine 'install-operations) ops)
   ((machine 'install-instruction-sequence)
    (assemble controller-text machine))
   machine))

(define (make-register name)
  (let ((contents '*unassigned*))
   (define (dispatch m)
     (cond ((eq? m 'get) contents)
           ((eq? m 'set)
            (lambda (new-val) (set! contents new-val)))
           ((eq? m 'name) name)
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
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0)
        (trace #f))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register register-name)
        (if (assoc register-name register-table)
            (error "Multiply defined register:" register-name)
            (set! register-table
              (cons (list register-name (make-register register-name))
                    register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((entry (assoc name register-table)))
         (if entry
             (cadr entry)
             (begin
               (allocate-register name)
               (cadr (assoc name register-table))))))
      (define (execute)
        (let ((insts (get-contents pc)))
         (if (null? insts)
             'done
             (begin
               (if trace (begin (display (caar insts)) (newline)))
               ((instruction-execution-proc (car insts)))
               (set! instruction-count (+ instruction-count 1))
               (execute)))))
      (define (dispatch m)
        (cond ((eq? m 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? m 'get-instruction-count) instruction-count)
              ((eq? m 'reset-instruction-count)
               (set! instruction-count 0))
              ((eq? m 'trace-on) (set! trace #t))
              ((eq? m 'trace-off) (set! trace #f))
              ((eq? m 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? m 'allocate-register) allocate-register)
              ((eq? m 'get-register) lookup-register)
              ((eq? m 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? m 'stack) stack)
              ((eq? m 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" m))))
      dispatch)))

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
          (let ((next-inst (car text)))
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
  (let ((pc (get-register machine 'pc))
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

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
   (if val
       (cdr val)
       (error "Undefined label -- ASSEMBLE" label-name))))



(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
          (error "Unknown instruction type -- ASSEMBLE"
                 inst))))


(define (make-assign inst machine labels ops pc)
  (let ((target
          (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
            (if (operation-exp? value-exp)
                (make-operation-exp
                  value-exp machine labels ops)
                (make-primitive-exp
                  (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc! pc)))))

(define (assign-reg-name inst) (cadr inst))
(define (assign-value-exp inst) (cddr inst))

(define (advance-pc! pc)
  (set-contents! pc (cdr (get-contents pc))))


(define (make-test inst machine labels ops flag pc)
  (let ((condition (test-condition inst)))
   (if (operation-exp? condition)
       (let ((condition-proc
               (make-operation-exp
                 condition machine lables ops)))
         (lambda ()
           (set-contents! flag (condition-proc))
           (advance-pc! pc)))
       (error "Bad test instruction -- ASSEMBLE" inst))))

(define (test-condition inst)
  (cdr inst))


(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
   (if (label-exp? dest)
       (let ((insts
               (lookup-label labels (label-exp-label dest))))
         (lambda ()
           (if (get-contents flag)
               (set-contents! pc insts)
               (advance-pc! pc))))
       (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define (branch-dest inst)
  (cadr inst))


(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
   (cond ((label-exp? dest)
          (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
            (lambda () (set-contents! pc insts))))
         ((register-exp? dest)
          (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
            (lambda ()
              (set-contents! pc (get-contents reg)))))
         (else
           (error "Bad GOTO instruction -- ASSEMBLE"
                  inst)))))

(define (goto-dest inst)
  (cadr inst))


(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc! pc))))

(define (make-restore inst machine stack)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc! pc))))

(define (stack-inst-reg-name inst)
  (cadr inst))


(define (make-perform inst machine labels ops pc)
  (let ((action (perform-action inst)))
   (if (operation-exp? action)
       (let ((action-proc
               (make-operation-exp
                 action machine labels ops)))
         (lambda ()
           (action-proc)
           (advance-pc! pc)))
       (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst)
  (cdr inst))


(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
          (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                 (lookup-label lables
                               (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((reg (get-register machine
                                  (register-exp-reg exp))))
           (lambda () (get-contents reg))))
        (else
          (error "Unknown expression type -- ASSEMBLE" exp))))

(define (tagged-list? list val)
  (and (pair? list)
       (eq? (car list) val)))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))


(define (make-operation-exp exp machine labels ops)
  (if (any label-exp? (operation-exp-operands exp))
      (error "Can't use labels with operations"))
  (let ((op (lookup-prim (operation-exp-op exp) ops))
        (aprocs
          (map (lambda (e)
                 (make-primitive-exp e machine labels))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))


(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op exp) (cadar exp))
(define (operation-exp-operands exp) (cdr exp))


(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
   (if val
       (cadr val)
       (error "Unknown operation -- ASSEMBLE" symbol))))


