#lang racket
(require math/bigfloat
         "functions.rkt"
         "Gaussian.rkt"
         "lu_decomposition.rkt"
         "Iterative.rkt")

(provide gaussian-elim
         lu-decomp
         jacobi
         sor
         multi-newtons
         broydens)

;Scott
;;returns x in Ax=b
; Takes A b (A = '((1 2)(3 4)) b='(8 9))
(define (gaussian-elim A b)
  (define fA (map flatten A))
  (define fb (flatten b))
  (define AB (map (λ(Ar bx) (append Ar (list bx))) fA fb))
  (map last (gaus AB))
  )

;Scott given A, return LUA in PA=LU,
;(lu-decomp '((1 3 5)(2 4 7)(1 1 0)))
;(A of the form '(((1 0 0) (.5 1 0) (.5 -1 1)) ((2 4 7) (0 1 1.5) (0 0 -2)) ((0 1 0) (1 0 0) (0 0 1)))
;return is form '(L U A)
(define (lu-decomp input-string)
  (LU_Decomposition input-string)
  )

;Scott given Ax=b (with initial guesses for x) return x
;(jacobi 10 '((2 1)(5 7)) '(1 1) '(11 13))
;(jacobi iter A x b
;returns final b '(7.3 8.8)
(define (jacobi num_iter A x b)
  (jacobiSc num_iter A x b)
  )

;Scott given Ax=b (with initial guesses for x) return x
;everything here is the same as jacobi, its just a different implementation
(define (sor num_iter A x b)
  (sorSc num_iter A x b)
  )

;(define g (list (list (list (bf 1)))
;                (list (list (bf 2)))
;                (list (list (bf 3)))
;                ))
;(negate-matrix g)
(define (negate-matrix m)
  (define ret (list))
  (for ([x (length m)])
    (set! ret (append ret (list (list (list (* (list-ref (list-ref (list-ref m x) 0) 0) (bf -1)))))))
    )
  ret
  )

;(add-to-guess (list (list 'x (bf 1)) (list 'y (bf 2)) (list 'z (bf 3))) g)
(define (add-to-guess guess addit)
  (define ret guess)
  (for ([x (length guess)])
    (set! ret (list-set ret x (list (list-ref (list-ref guess x) 0) (+ (list-ref (list-ref guess x) 1) (if (list? (list-ref addit x))(caar (list-ref addit x)) (list-ref addit x))))))
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
    (define s (gaussian-elim (apply-matrix df x) (negate-matrix (apply-matrix system x))))
    (set! x (add-to-guess x s))
    )
  x
  )

;ex (identity 5)
(define (identity size)
  (define ret (list))
  (for ([x size])
    (define row (make-list size (list 0.bf)))
    (set! row (list-set row x (list (bf 1))))
    (set! ret (append ret (list row)))
    )
  ret
  )

;ex. (row*col (list (list (bf 1)) (list (bf 2))) (list (list (bf 1)) (list (bf 0))))
(define (row*col a b)
  (define sum 0.bf)
  (for ([x (length a)])
    (set! sum (+ sum (* (list-ref (list-ref a x) 0) (list-ref (list-ref b x) 0))))
    )
  sum
  )

;ex (get-col Matrix row-num) --> column transverse
(define (get-col m x)
  (define row (list))
  (for ([c m])
    (set! row (append row (list (list-ref c x))))
    )
  row
  )

;ex. (dot (list (list (list (bf 1)) (list (bf 2))) (list (list (bf 3)) (list (bf 4))))(identity 2)) --> (list (list (list (bf 1)) (list (bf 2))) (list (list (bf 3)) (list (bf 4))))
(define (dot a b)
  (define ret (list))
  (for ([row (length a)])
    (define current-row (list))
    (for ([col (length (list-ref b 0))])
      (set! current-row (append current-row (list (list (row*col (list-ref a row) (get-col b col))))))
      )
    (set! ret (append ret (list current-row)))
    )
  ret
  )

;ex. (vector- (list (list (list 5.bf))(list (list 4.bf))(list (list 3.bf)))(list (list (list 5.bf))(list (list 4.bf))(list (list 3.bf))))
(define (vector- m1 m2)
  (define ret (list))
  (for ([row (length m1)])
    (set! ret (append ret (list (list (list (- (car (car (list-ref m1 row))) (car (car (list-ref m2 row)))))))))
    )
  ret
  )

(define (transverse M)
  (define ret (list))
  (for ([col (length (list-ref M 0))])
    (define current-row (list))
    (for ([row (length M)])
      (set! current-row (append current-row (list (list-ref (list-ref M row) col))))
      )
    (set! ret (append ret (list current-row)))
    )
  ret
  )

(define (matrix+ M1 M2)
  (define ret (list))
  (for ([row (length M1)])
    (define this-row (list))
    (for ([col (length (list-ref M1 row))])
      (set! this-row (append this-row (list (list (+ (car (list-ref (list-ref M1 row) col))(car (list-ref (list-ref M2 row) col)))))))
      )
    (set! ret (append ret (list this-row)))
    )
  ret
  )

(define (guess->vector g)
  (define ret (list))
  (for ([r g])
    (set! ret (append ret (list (list (list (list-ref r 1))))))
    )
  ret
  )

(define (subtract-guesses g1 g2)
  (define ret g1)
  (for ([row (length g1)])
    (define this-row (list-ref g1 row))
    (set! this-row (list-set this-row 1 (- (cadr (list-ref g1 row)) (cadr (list-ref g2 row)))))
    (set! ret (list-set ret row this-row))
    )
  ret
  )

(define (matrix*scalar m s)
  (define ret (list))
  (for ([row (length m)])
    (define this-row (list))
    (for ([col (length (list-ref m row))])
      (set! this-row (append this-row (list (list (* (car (list-ref (list-ref m row) col)) s)))))
      )
    (set! ret (append ret (list this-row)))
    )
  ret
  )

;Brad
;(define sys (list (list (list (list (bf 1) 'x (bf 1)) '- (list (bf 1) 'y (bf 3))))
;                  (list (list (list (bf 1) 'x (bf 2)) '+ (list (bf 1) 'y (bf 2)) '- (list (bf 1))))
;                  ))
;ex. (broydens 5 sys  (list (list 'x (bf 1)) (list 'y (bf 2))) (identity (length (get-vars sys))))
(define (broydens num-iterations system guesses init-matrix)
  (define x guesses)
  (define B init-matrix)
  (define F system)
  (for ([_ num-iterations])
    (define prev-x x)
    (set! x (add-to-guess x (negate-matrix (dot B (apply-matrix F x)))))
    (define d (guess->vector(subtract-guesses x prev-x)))
    (define Delta (vector-  (apply-matrix F x) (apply-matrix F prev-x)))
    (define top (dot (dot (vector- d (dot B Delta)) (transverse d)) B))
    (define bottom (dot (dot (transverse d) B) Delta))
    (set! B (matrix+ B (matrix*scalar top (/ 1.bf (caaar bottom)))))
    )
  x
  )