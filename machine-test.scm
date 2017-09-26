(load "simulator.scm")

(define machine (make-machine
                  (list (list '+ +))
                  '((assign a (const 2))
                    (assign b (op +) (reg a) (reg a)))))
(machine 'trace-on)
(machine 'start)
(newline)
(display (get-register-contents machine 'a))
(display " ")
(display (get-register-contents machine 'b))
(newline)
(display (list "computed in " (machine 'get-instruction-count) " instructions"))

