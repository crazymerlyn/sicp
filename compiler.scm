(load "evaluator.scm")

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp)
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-action exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
          (error "Unknown expression type -- COMPILE" exp))))


(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))


(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence
           '(continue) '()
           '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
          (make-instruction-sequence
            '() '()
            `((goto (label ,linkage)))))))


(define (end-with-linkage linkage insts)
  (preserving '(continue)
              insts
              (compile-linkage linkage)))


(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
    linkage
    (make-instruction-sequence
      '() (list target)
      `((assign ,target (const ,exp))))))


(define (compile-quoted exp target linkage)
  (end-with-linkage
    linkage
    (make-instruction-sequence
      '() (list target)
      `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage
    linkage
    (make-instruction-sequence
      '(env) '(list target)
      `((assign ,target
                (op lookup-variable-value)
                (const ,exp)
                (reg env))))))


(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage
      linkage
      (preserving
        '(env)
        get-value-code
        (make-instruction-sequence
          '(env val) (list target)
          `((perform (op set-variable-value!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) 'val 'next)))
    (end-with-linkage
      linkage
      (preserving
        '(env)
        get-value-code
        (make-instruction-sequence
          '(env val) (list target)
          `((perform (op define-variable!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))


(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
            (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
              (compile
                (if-consequent exp) target consequent-linkage))
            (a-code
              (compile (if-alternative exp) target linkage)))
        (preserving
          '(env continue)
          p-code
          (append-instruction-sequences
            (make-instruction-sequence
              '(val) '()
              `((test (op false?) (reg val))
                (branch (label ,f-branch))))
            (parallel-instruction-sequences
              (append-instruction-sequences t-branch c-code)
              (append-instruction-sequences f-branch a-code))
            after-if))))))

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ label-counter 1))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))


(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
                  (compile (first-exp seq) target linkage)
                  (compile-sequence (rest-exps seq) target linkage))))


(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry proc) (cadr proc))
(define (compiled-procedure-env proc) (caddr proc))


(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
            (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage
            lambda-linkage
            (make-instruction-sequence
              '(env) (list target)
              `((assign ,target
                        (op make-compiled-procedure)
                        (label ,proc-entry)
                        (reg env)))))
          (compile-lambda-body exp proc-entry))
        after-lambda))))


(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-params exp)))
   (append-instruction-sequences
     (make-instruction-sequence
       '(env proc arg1) '(env)
       `(,proc-entry
          (assign env (op compiled-procedure-env) (reg proc))
          (assign env
                  (op extend-environment)
                  (const ,formals)
                  (reg arg1)
                  (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))


(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
          (map (lambda (op) (compile op 'val 'next))
               (operands exp))))
    (preserving
      '(env continue)
      proc-code
      (preserving
        '(proc continue)
        (construct-arg-list operand-codes)
        (compile-procedure-call target linkage)))))


(define (construct-arg-list operand-codes)
  (let ((operand-codes (reverse operand-codes)))
   (if (null? operand-codes)
       (make-instruction-sequence
         '() '(arg1)
         '((assign arg1 (const ()))))
       (let ((code-to-get-last-arg
               (append-instruction-sequences
                 (car operand-codes)
                 (make-instruction-sequence
                   '(val) '(arg1)
                   `((assign arg1 (op list) (reg val)))))))
         (if (null? (cdr operand-codes))
             code-to-get-last-arg
             (preserving
               '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                 (cdr operand-codes))))))))


(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving
            '(arg1)
            (car operand-codes)
            (make-instruction-sequence
              '(val arg1) '(arg1)
              `((assign arg1 (op cons) (reg val) (reg arg1)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving
          '(env)
          code-for-next-arg
          (code-to-get-rest-args (cdr operand-codes))))))


(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
            (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence
          '(proc) '()
          `((test (op primitive-procedure?) (reg proc))
            (branch (label ,primitive-branch))))
        (parallel-instruction-sequences
          (append-instruction-sequences
            compiled-branch
            (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage
              linkage
              (make-instruction-sequence
                '(proc arg1) (list target)
                `((assign ,target
                          (op apply-primitive-procedure)
                          (reg proc)
                          (reg arg1)))))))
        after-call))))

(define all-regs '(env proc arg1 val continue))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
           '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                     (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
          (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
           '(proc continue) all-regs
           `((assign val (op compiled-procedure-entry)
                     (reg proc))
             (goto (reg val)))))
        ((and (eq? linkage 'return) (not (eq? target 'val)))
         (error "return linkage, target is not val -- COMPILE"
                target))))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? s reg)
  (memq reg (registers-needed s)))
(define (modifies-register? s reg)
  (memq reg (registers-modified s)))


(define (append-instruction-sequences . seqs)
  (define (append-2-sequences s1 s2)
    (make-instruction-sequence
      (list-union (registers-needed s1)
                  (list-difference (registers-needed s2)
                                   (registers-modified s1)))
      (list-union (registers-modified s1)
                  (registers-modified s2))
      (append (statements s1) (statements s2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))


(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))


(define (preserving regs s1 s2)
  (if (null? regs)
      (append-instruction-sequences s1 s2)
      (let ((first-reg (car regs)))
       (if (and (needs-register? s2 first-reg)
                (modifies-register? s1 first-reg))
           (preserving
             (cdr regs)
             (make-instruction-sequence
               (list-union (list first-reg)
                           (registers-needed s1))
               (list-difference (registers-modified s1)
                                (list first-reg))
               (append `((save ,first-reg))
                       (statements s1)
                       `((restore ,first-reg))))
             s2)
           (preserving (cdr regs) s1 s2)))))


(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))))


(define (parallel-instruction-sequences s1 s2)
  (make-instruction-sequence
    (list-union (registers-needed s1)
                (registers-needed s2))
    (list-union (registers-modified s1)
                (registers-modified s2))
    (append (statements s1) (statements s2))))


