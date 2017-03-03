#lang racket
(require math/bigfloat
         "functions.rkt")

(provide bisection
         fixed-point
         newtons-method)

;Scott
(define (bisection num-iterations initial-guess input-string)
  void
  )

;Scott
(define (fixed-point num-iterations initial-guess input-string)
  void
  )

;Brad
;expects variable = x
;example:
;3x^2 - 1 @ -1 -> -1, -1.5, -1.347826, -1.325200, -1.324718...
;(newtons-method 4 -1 (list (list (bf 1) 'x (bf 3)) '- (list (bf 1) 'x (bf 1)) '+ (list (bf 1) 'x (bf 0))))
(define (newtons-method num-iterations initial-guess input-string)
  (define ret (bf initial-guess))
  ;(printf "0 iterations: ~a~n" ret)
  (define deriv (derivative input-string 'x))
  (for ([x num-iterations])
    (set! ret (bf- ret (bf/ (apply-func input-string (list (list 'x ret))) (apply-func deriv (list (list 'x ret))))))
    ;(printf "~a iterations: ~a~n" (add1 x) ret)
    )
  ret
  )