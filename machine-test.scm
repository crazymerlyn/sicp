(load "simulator.scm")

(define machine (make-machine
                  (list (list '- -)
                        (list '= =)
                        (list '* *))
                  '((assign continue (label fact-done))
                    fact-loop
                    (test (op =) (reg n) (const 1))
                    (branch (label base-case))
                    (save continue)
                    (save n)
                    (assign n (op -) (reg n) (const 1))
                    (assign continue (label after-fact))
                    (goto (label fact-loop))
                    after-fact
                    (restore n)
                    (restore continue)
                    (assign val (op *) (reg n) (reg val))
                    (goto (reg continue))
                    base-case
                    (assign val (const 1))
                    (goto (reg continue))
                    fact-done)))
(machine 'trace-on)
(set-register-contents! machine 'n 2)
(machine 'start)
(display "The factorial is: ")
(display (get-register-contents machine 'val))
(newline)
(display (list "computed in " (machine 'get-instruction-count) " instructions"))
(newline)

