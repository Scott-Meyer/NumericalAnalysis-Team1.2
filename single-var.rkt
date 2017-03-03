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
    (apply-func input-string x))
  (bisectionProcess num-iterations fx in)
  )

;Scott
(define (fixed-point num-iterations initial-guess input-string)
  void
  )

;Brad
;example:
;3x^2 - 1 @ -1 -> -1, -1.5, -1.347826, -1.325200, -1.324718...
;(newtons-method 4 -1 (list (list (bf 1) 'x (bf 3)) '- (list (bf 1) 'x (bf 1)) '+ (list (bf 1) 'x (bf 0))))
(define (newtons-method num-iterations initial-guess input-string)
  (define ret (bf initial-guess))
  ;(printf "0 iterations: ~a~n" ret)
  (for ([x num-iterations])
    (set! ret (bf- ret (bf/ (apply-func input-string ret) (apply-func (derivative input-string) ret))))
    ;(printf "~a iterations: ~a~n" (add1 x) ret)
    )
  ret
  )