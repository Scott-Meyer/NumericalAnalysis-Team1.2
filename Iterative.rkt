#lang racket
(require math/bigfloat
         "functions.rkt")

(define tA (list (list (bf 2) (bf 1))
                 (list (bf 5) (bf 7))))
(define tx (list (bf 1) (bf 1)))
(define tb (list (bf 11) (bf 13)))

(provide jacobiSc
         sorSc)

(define (jacobiSc num_iter A x b)
  (reset-fp-op)
  (define (diag-dom? A)
    (let* ([diag-index (range 0 (length A))])
      (andmap bf>
              (map list-ref A diag-index)
              (map (λ(x i) (foldl (λ(x acc) (+ acc (bfabs x))) (bf 0) (delete-n x i))) A diag-index))))
  (define (converge? A)
    #f)
  (define (jacobi* num_iter A x b)
    (if (or (zero? num_iter) (converge? A))
        x
        (let ([x+1 (for/list ([i (range 0 (length A))])
                     (let ([σ (foldl (λ(j acc)
                                        (if (not (equal? i j))
                                            (+ acc (* (list-ref (list-ref A i) j) (list-ref x j)))
                                            acc))
                                      (bf 0) (range 0 (length A)))])
                       (* (/ (bf 1) (list-ref (list-ref A i) i)) (- (list-ref b i) σ))))])
          (jacobi* (sub1 num_iter) A x+1 b))))
  (cond
    [(not (andmap (λ (x) (equal? (length A) (length x))) A)) (error "A not nXn")]
    [(not (equal? (length A) (length x))) (error "vector x wrong length")]
    [(not (equal? (length A) (length b))) (error "vector b wrong length")]
    [(not (diag-dom? A)) (error "A not diagonal dominant")]
    [else (jacobi* num_iter A x b)]))

(define t2A '((16 3)
              (7 -11)))
(define t2b '(11 13))

(define (sorSc num_iter A x b)
  (reset-fp-op)
  (define (converge? A) #f)
  (cond
    [(< num_iter 1) x]
    [(converge? A) x]
    [else
     (let ([Φ (for/list ([i (range 0 (length A))])
                (let ([σ (foldl (λ(j acc) (if (not (equal? i j))
                                              (+ acc (* (list-ref (list-ref A i) j) (list-ref x j)))
                                              acc))
                                (bf 0) (range 0 (length A)))])
                (* (/ (bf 1) (list-ref (list-ref A i) i)) (- (list-ref b i) σ))))])
     (sorSc (sub1 num_iter) A Φ b))]))

;Given a list and an Index, return that list with the index removed
;(delete-n '(a b c d) 1) -> '(a c d)
(define (delete-n ls n)
  (if (empty? ls)
      empty
      (if (zero? n)
          (delete-n (rest ls) (sub1 n))
          (cons (first ls) (delete-n (rest ls) (sub1 n))))))