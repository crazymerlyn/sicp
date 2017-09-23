(define (evaln exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((assoc (car exp) eval-data-rules)
         ((cdr (assoc (car exp) eval-data-rules)) exp env))
        ((application? exp)
         (applyn (evaln (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (applyn procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (evaln (first-operand exp) env)
            (list-of-values (rest-operands exp) env))))


(define (eval-if exp env)
  (if (true? (evaln (if-predicate exp) env))
      (evaln (if-consequent exp) env)
      (evaln (if-alternative exp) env)))


(define (eval-and exp env)
  (evaln (expand-and (and-clauses exp)) env))

(define (and-clauses exp) (cdr exp))

(define (expand-and exps)
  (if (null? exps)
      'true
      (if (last-exp? exps)
          (first-exp exps)
          (make-if (first-exp exps)
                   (expand-and (rest-exps exps))
                   (first-exp exps)))))

(define (eval-or exp env)
  (evaln (expand-or (or-clauses exp)) env))

(define (or-clauses exp) (cdr exp))

(define (expand-or exps)
  (if (null? exps)
      'false
      (if (last-exp? exps)
          (first-exp exps)
          (make-if (first-exp exps)
                   (first-exp exps)
                   (expand-or (rest-exps exps))))))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (evaln (first-exp exps) env))
        (else (evaln (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (evaln (assignment-value exp) env)
                       env)
  'ok)


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (evaln (definition-value exp) env)
                    env)
  'ok)

(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let* ((left (evaln (first-operand exp) env))
             (right (list-of-values-l2r (rest-operands exp) env)))
        (cons left right))))


(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let* ((right (list-of-values-r2l (rest-operands exp) env))
             (left (evaln (first-operand exp) env)))
        (cons left right))))


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define (variable? exp)
  (symbol? exp))


(define (quoted? exp)
  (tagged-list? exp 'quote))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-params exp) (cadr exp))
(define (lambda-body exp) (cddr exp))


(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? cdddr exp))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))


(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (clause->exp first)
                     (expand-clauses rest))))))

(define (clause->exp clause)
  (cond ((null? clause) (error "Ill formed clause -- CLAUSE->EXP" clause))
        ((null? (cdr clause)) (car clause))
        ((eq? (cadr clause) '=>)
         (if (= (length clause) 3)
             ((caddr clause) (car clause))
             (error "Ill formed clause -- CLAUSE->EXP" clause)))
        (else
          (sequence->exp (cond-actions clause)))))


(define (let->combination exp)
  (cond ((named-let? exp)
         (sequence->exp
           (list
             (cons 'define
                   (cons (cons (named-let-name exp)
                               (named-let-parameters exp))
                         (named-let-body exp)))
             (cons (named-let-name exp)
                   (named-let-values exp)))))
        (else
          (cons (make-lambda (let-parameters exp) (let-body exp))
                (let-values exp)))))
(define (let-body exp) (cddr exp))
(define (let-parameters exp) (map car (cadr exp)))
(define (let-values exp) (map cadr (cadr exp)))
(define (named-let? exp) (symbol? (cadr exp)))
(define (named-let-body exp) (cdddr exp))
(define (named-let-parameters exp) (map car (caddr exp)))
(define (named-let-values exp) (map cadr (caddr exp)))
(define (named-let-name exp) (cadr exp))

(define (make-let parameters body)
  (cons 'let (cons parameters body)))

(define (let*-nested-lets exp)
  (if (null? (cdadr exp))
      (make-let (cadr exp) (cddr exp))
      (make-let (list (caadr exp))
                (list (let*-nested-lets
                        (cons 'let* (cons (cdadr exp) (cddr exp))))))))


(define (application-louis? exp) (tagged-list? exp 'call))
(define (operator-louis exp) (cadr exp))
(define (operands-louis exp) (cddr exp))

(define eval-data-rules
  (list
    (cons 'quote (lambda (exp env) (text-of-quotation env)))
    (cons 'set! eval-assignment)
    (cons 'define eval-definition)
    (cons 'if eval-if)
    (cons 'and eval-and)
    (cons 'or eval-or)
    (cons 'lambda (lambda (exp env)
                    (make-procedure (lambda-params exp)
                                    (lambda-body exp)
                                    env)))
    (cons 'let (lambda (exp env) (evaln (let->combination exp) env)))
    (cons 'let* (lambda (exp env) (evaln (let*-nested-lets exp) env)))
    (cons 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
    (cons 'cond (lambda (exp env) (evaln (cond->if exp) env)))))


(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (get-binding var frame)
  (define (loop vars vals)
    (cond ((null? vars) '())
          ((eq? var (car vars))
           (lambda (m)
             (cond ((eq? m 'value) (car vals))
                   ((eq? m 'set-value!)
                    (lambda (new-value) (set-car! vals new-value)))
                   (else (error "Unknown request BINDING" m)))))
          (else (loop (cdr vars) (cdr vals)))))
  (loop (frame-variables frame) (frame-values frame)))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


(define (lookup-variable-value var env)
  (if (eq? the-empty-environment env)
      (error "Unbound variable" var)
      (let ((binding (get-binding var (first-frame env))))
       (if (null? binding)
           (lookup-variable-value var (enclosing-environment env))
           (binding 'value)))))


(define (set-variable-value! var val env)
  (if (eq? the-empty-environment env)
      (error "Unbound variable -- SET!" var)
      (let ((binding (get-binding var (first-frame env))))
       (if (null? binding)
           (set-variable-value! var val (enclosing-environment env))
           ((binding 'set-value!) val)))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (get-binding var frame)))
    (if (null? binding)
        (add-binding-to-frame! var val frame)
        ((binding 'set-value!) val))))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedures)
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))


(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

