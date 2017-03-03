#lang racket
(define tA '((1 3 5)
             (2 4 7)
             (1 1 0)))

(define (LU_Decomposition A)
  (if (not (andmap (λ (x) (equal? (length A) (length x))) A))
      (error "Matrix not nXn")
      ("Hello")))

(define (rowOrder A)
  (if (or (andmap (λ (x) (e(empty? A))
      '()
      (cons
       (index-of (map first A) (apply max (map first A)))
       (rowOrder (cons '(0) (rest (map rest A)))))))