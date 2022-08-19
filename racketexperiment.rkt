#lang racket

(define environment (lambda (y) (error 'lookup "unbound")))

(define get-code
  (lambda ()
    (let ((input (read)))
      (print (eval-expr input environment)))))

(define eval-expr
  (lambda (expr env)
    (cond ((symbol? expr) (env expr))
          ((number? expr) expr)
          ; there is a function application to take care of
          ((list? expr) (cond
                          ; built-in functions
                          ((equal? (car expr) 'plus) (+ (eval-expr (cadr expr) env) (eval-expr (caddr expr) env)))
                          ((equal? (car expr) 'minus) (- (eval-expr (cadr expr) env) (eval-expr (caddr expr) env)))
                          ((equal? (car expr) 'times) (* (eval-expr (cadr expr) env) (eval-expr (caddr expr) env)))
                          ((equal? (car expr) 'divide) (/ (eval-expr (cadr expr) env) (eval-expr (caddr expr) env)))
                          
                          ; abstraction
                          ((equal? (car expr) 'lambd) (lambda (arg)
                                                            (eval-expr (caddr expr) (lambda (y)
                                                                                      (if (eq? (cadr expr) y)
                                                                                          arg
                                                                                          (env y))))))
                        ; application
                        (#t ((eval-expr (car expr) env) (eval-expr (cadr expr) env)))))
          (#t #t))))

(get-code)
;((lambd x (plus x 3)) 7)