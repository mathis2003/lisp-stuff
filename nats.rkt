(require racket/match)

(require (for-syntax racket/base))
(require (for-syntax racket/match))

(define suc add1)
(define zero 0)

(define-syntax nat-num
  (lambda (stx)
    (match (syntax->datum stx)
      [(list _ nat-number)
       (datum->syntax stx
                      (letrec ([this (lambda (n)
                                       (if (< n 1)
                                           'zero
                                           (list 'suc (this (- n 1)))))])
                        (this nat-number)))])))

(nat-num 4)
