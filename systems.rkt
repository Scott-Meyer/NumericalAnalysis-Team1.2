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
(define (gaussian-elim Aug) ;returns x in Ax=b
  Aug
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

;(define g (list (list (list (bf 1)))
;                (list (list (bf 2)))
;                (list (list (bf 3)))
;                ))
;(negate-matrix g)
(define (negate-matrix m)
  (define ret (list))
  (for ([x (length m)])
    (set! ret (append ret (list (list (list (bf* (list-ref (list-ref (list-ref m x) 0) 0) (bf -1)))))))
    )
  ret
  )

;(add-to-guess (list (list 'x (bf 1)) (list 'y (bf 2)) (list 'z (bf 3))) g)
(define (add-to-guess guess addit)
  (define ret guess)
  (for ([x (length guess)])
    (set! ret (list-set ret x (list (list-ref (list-ref guess x) 0) (bf+ (list-ref (list-ref guess x) 1) (list-ref (list-ref (list-ref addit x) 0) 0)))))
    )
  ret
  )

(define (augment A b)
  (define ret A)
  (for ([row (length b)])
    (set! ret (list-set ret row (append (list-ref A row) (list-ref b row))))
    )
  ret
  )

;Brad
;(define sys (list (list (list (list (bf 1) 'x (bf 1)) '- (list (bf 1) 'y (bf 3))))
;                  (list (list (list (bf 1) 'x (bf 2)) '+ (list (bf 1) 'y (bf 2)) '- (list (bf 1))))
;                  ))
;ex. (multi-newtons 7 sys (list (list 'x (bf 1)) (list 'y (bf 2))))
(define (multi-newtons num-iterations system guess)
  (define x guess)
  (define df (jacobian system))
  (for ([_ num-iterations])
    (define s (gaussian-elim (augment (apply-matrix df x) (negate-matrix (apply-matrix system x)))))
    (set! x (add-to-guess x s))
    )
  x
  )

;Brad
;TODO
(define (broydens num-iterations system guess init-matrix)
  (define x guess)
  (define B init-matrix)
  (define F system)
  ;(set! x (-
  init-matrix
  )