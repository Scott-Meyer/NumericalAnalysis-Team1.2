#lang racket
(define tA '((2 1)
              (5 7)))
(define tx '(1 1))
(define tb '(11 13))

(define (jacobi num_iter A x b)
  (define (diag-dom? A)
    (let* ([diag-index (range 0 (length A))])
      (andmap >
              (map list-ref A diag-index)
              (map (λ(x i) (foldl (λ(x acc) (+ acc (abs x))) 0 (delete-n x i))) A diag-index))))
  (define (converge? A)
    #f)
  (define (jacobi* num_iter A x b)
    (if (or (zero? num_iter) (converge? A))
        x
        (let ([x+1 (for/list ([i (range 0 (length A))])
                     (let* ([θ (foldl (λ(j acc)
                                        (if (not (equal? i x))
                                            (+ acc (* (list-ref (list-ref A i) j) (list-ref x j)))
                                            acc))
                                      0 (range 0 (length A)))])
                       (* (/ 1 (list-ref (list-ref A i) i)) (- (list-ref b i) θ))))])
          (jacobi* (- num_iter 1) A x+1 b))))
  (cond
    [(not (andmap (λ (x) (equal? (length A) (length x))) A)) (error "A not nXn")]
    [(not (equal? (length A) (length x))) (error "vector x wrong length")]
    [(not (equal? (length A) (length b))) (error "vector b wrong length")]
    [(not (diag-dom? A)) (error "A not diagonal dominant")]
    [else (jacobi* num_iter A x b)]))

(define (sor num_iter A x b)
  A)

;Given a list and an Index, return that list with the index removed
;(delete-n '(a b c d) 1) -> '(a c d)
(define (delete-n ls n)
  (if (empty? ls)
      empty
      (if (= n 0)
          (delete-n (rest ls) (- n 1))
          (cons (first ls) (delete-n (rest ls) (- n 1))))))