#lang racket
(require math/bigfloat
         "functions.rkt"
         "Bisection.rkt")

(provide bisection
         fixed-point
         newtons-method)

;Scott
;example:
;f(x)=3x^3-1, (-2, 2), 8 iterations
;(bisection 8 (list -2 2) (list (list (bf 3) 'x (bf 3)) '- (list (bf 1) 'x 0.bf)))
(define (bisection num-iterations initial-guess input-string)
  (define in (list (bf (first initial-guess)) (bf (second initial-guess))))
  (define (fx x)
    (apply-func input-string (list (list 'x x))))
  (bisectionProcess num-iterations fx in)
  )

;Scott
;example:
;f(x)=1x^3, 8 iterations, .5 initial guess
;(fixed-point 8 .5 (list (list (bf 1) 'x (bf 3))))
(define (fixed-point num-iterations initial-guess input-string)
  (define in (bf initial-guess))
  (define (fx x) (apply-func input-string (list (list 'x x))))
  (define (fixed-point-runner num-iterations x fx)
    (if (or (> 1 num-iterations) (equal? x (fx x)))
      x
      (fixed-point-runner (sub1 num-iterations) (fx x) fx)))
  (fixed-point-runner num-iterations in fx)
  )

;Brad
;expects variable = x
;example:
;3x^2 - 1 @ -1, 4 iterations
;(newtons-method 4 -1 (list (list (bf 3) 'x (bf 2)) '- (list (bf 1))))
(define (newtons-method num-iterations initial-guess input-string)
  (define ret (bf initial-guess))
  ;(printf "0 iterations: ~a~n" ret)
  (define deriv (derivative input-string 'x))
  (for ([x num-iterations])
    (set! ret (- ret (/ (apply-func input-string (list (list 'x ret))) (apply-func deriv (list (list 'x ret))))))
    ;(printf "~a iterations: ~a~n" (add1 x) ret)
    )
  ret
  )