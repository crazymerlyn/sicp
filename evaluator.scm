(define (evaln exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((assoc (car exp) analyze-data-rules)
         ((cdr (assoc (car exp) analyze-data-rules)) exp))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (evaln (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
   (lambda (env) qval)))
(define (text-of-quotation exp)
  (cadr exp))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (actual-value pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-params exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))


(define (analyze-and exp)
  (analyze (expand-and (and-clauses exp))))

(define (and-clauses exp) (cdr exp))

(define (expand-and exps)
  (if (null? exps)
      'true
      (if (last-exp? exps)
          (first-exp exps)
          (make-if (first-exp exps)
                   (expand-and (rest-exps exps))
                   (first-exp exps)))))

(define (analyze-or exp)
  (analyze (expand-or (or-clauses exp))))

(define (or-clauses exp) (cdr exp))

(define (expand-or exps)
  (if (null? exps)
      'false
      (if (last-exp? exps)
          (first-exp exps)
          (make-if (first-exp exps)
                   (first-exp exps)
                   (expand-or (rest-exps exps))))))


(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
   (if (null? procs)
       (error "Empty sequence -- ANALYZE"))
   (loop (car procs) (cdr procs))))


(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))


(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (actual-value fproc env)
                           aprocs
                           env))))

(define (execute-application proc args env)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure
           proc
           (map (lambda (proc) (actual-value proc env)) args)))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              (procedure-evaluate-args proc args env)
                              (procedure-environment proc))))
        (else
          (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define (actual-value proc env)
  (force-it (proc env)))


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
  (if (not (null? (cdddr exp)))
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

(define analyze-data-rules
  (list
    (cons 'quote analyze-quoted)
    (cons 'set! analyze-assignment)
    (cons 'define analyze-definition)
    (cons 'if analyze-if)
    (cons 'and analyze-and)
    (cons 'or analyze-or)
    (cons 'lambda analyze-lambda)
    (cons 'let (lambda (exp) (analyze (let->combination exp))))
    (cons 'let* (lambda (exp) (analyze (let*-nested-lets exp))))
    (cons 'begin (lambda (exp) (analyze-sequence (begin-actions exp))))
    (cons 'cond (lambda (exp) (analyze (cond->if exp))))))


(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (map parameter-name (cadr p)))
(define (parameter-name param)
  (if (symbol? param) param (car param)))
(define (parameter-category param)
  (cond ((symbol? param) 'strict)
        (else (cadr param))))

(define (procedure-evaluate-args proc aprocs env)
  (define (make-arg category aproc)
    (cond ((eq? category 'strict) (actual-value aproc env))
          ((eq? category 'lazy) (delay-it aproc env #f))
          ((eq? category 'lazy-memo) (delay-it aproc env #t))
          (else
            (error "Unknown category of argument -- PROCEDURE-EVALUATE-ARGS" category))))
  (map make-arg (map parameter-category (cadr proc)) aprocs))

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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '> >)))

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


(define input-prompt ";;; L-Eval input:")
(define output-prompt  ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
   (let ((output (force-it (evaln input the-global-environment))))
    (announce-output output-prompt)
    (user-print output)))
  (driver-loop))

(define (prompt-for-input input-prompt)
  (newline) (newline) (display input-prompt) (newline))

(define (announce-output output-prompt)
  (newline) (display output-prompt) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value 
                         (thunk-proc obj)
                         (thunk-env obj))))
           (if (memoized-thunk? obj)
               (begin (set-car! obj 'evaluated-thunk)
                      (set-car! (cdr obj) result)
                      (set-cdr! (cdr obj) '())))
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

(define (delay-it proc env memoize)
  (if (true? memoize)
      (list 'thunk-memoize proc env)
      (list 'thunk proc env)))

(define (thunk? obj)
  (or (tagged-list? obj 'thunk)
      (tagged-list? obj 'thunk-memoize)))

(define (memoized-thunk? obj)
  (tagged-list? obj 'thunk-memoize))

(define (thunk-proc thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (evaluated-thunk? thunk)
  (tagged-list? thunk 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

