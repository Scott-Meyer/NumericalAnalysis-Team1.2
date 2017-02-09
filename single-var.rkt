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
(define (newtons-method num-iterations initial-guess input-string)
  (define ret (bf initial-guess))
  (for ([x num-iterations])
    (set! ret (bf- ret (bf/ (apply-func input-string ret) (apply-func (derivative input-string) ret))))
    )
  ret
  )