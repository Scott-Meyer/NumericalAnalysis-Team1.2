#lang racket
(require math/bigfloat
         "functions.rkt")

(provide gaussian-elim
         lu-decomp
         jacobi
         sor
         multi-newtons
         broydens)

;Scott
(define (gaussian-elim A b) ;returns x in Ax=b
  void
  )

;Scott
(define (lu-decomp input-string)
  void
  )

;Scott
(define (jacobi input-string)
  void
  )

;Scott
(define (sor input-string)
  void
  )

(define (negate-matrix m)
  m
  )

(define (add-to-guess guess addit)
  guess
  )

;Brad
;ex. (multi-newtons 7 (list (list (list (bf 1) 'x (bf 1)) '- (list (bf 1) 'y (bf 3))) (list (list (bf 1) 'x (bf 2)) '+ (list (bf 1) 'y (bf 2)) '- (list (bf 1)))) (list (list 'x 1) (list 'y 2)))
(define (multi-newtons num-iterations system guess)
  (define x guess)
  (define df (jacobian system))
  (for ([_ num-iterations])
    (define s (gaussian-elim (apply-matrix df x) (negate-matrix (apply-matrix system x))))
    (set! x (add-to-guess x s))
    )
  x
  )

;Brad
(define (broydens input-string)
  void
  )