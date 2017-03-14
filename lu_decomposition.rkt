#lang racket
(require math/array
         math/bigfloat
         "functions.rkt")
(define tA (list (list (bf 1) (bf 3) (bf 5))
                 (list (bf 2) (bf 4) (bf 7))
                 (list (bf 1) (bf 1) (bf 0))))

(provide LU_Decomposition)

;;Logic and definition (for LU_Decomposition) copied from
;;http://rosettacode.org/wiki/LU_decomposition#Python
(define (LU_Decomposition A)
  (if (not (andmap (λ (x) (equal? (length A) (length x))) A))
      (error "Matrix not nXn")
      (let* ([n (length A)]
             [A2 (array->mutable-array (list*->array (map (λ(x) (list-ref A x)) (Order A)) bigfloat?))]
             ;[L (vector*->array (list->vector (map (λ(x) (for/list ([i (range 0 (length A))]) (if (equal? i x) 1 0))) (range 0 (length A))) vector?))]
             [L (array->mutable-array (make-array (vector n n) 0))]
             [U (array->mutable-array (make-array (vector n n) 0))])
        (print A2)
        (for* ([j (range 0 (length A))])
          (array-set! L (vector j j) 1)
          (for* ([i (range 0 (add1 j))])
            (let ([s1 (foldl (λ(k sum) (+ sum (* (array-ref U (vector k j)) (array-ref L (vector i k))))) (bf 0) (range 0 i))])
              (array-set! U (vector i j) (- (array-ref A2 (vector i j)) s1))))
          (for* ([i (range j n)])
            (let ([s2 (foldl (λ(k sum) (+ sum (* (array-ref U (vector k j)) (array-ref L (vector i k))))) (bf 0) (range 0 j))])
              (array-set! L (vector i j) (/ (- (array-ref A2 (vector i j)) s2)
                                      (array-ref U (vector j j)))))))
        (list (array->list* L)
              (array->list* U)
              (map (λ(x) (map bf x)) (P A))))))


;Given a matrix, return the P part of LU=PA
(define (P A)
  (define (Prow a n)
    (if (equal? n 0)
        empty
        (let ([rest (Prow (sub1 a) (sub1 n))])
          (if (equal? a 0) (cons 1 rest) (cons 0 rest)))))
  (map (λ(x) (Prow x (length A))) (Order A)))


;Given a matrix, return the correct index order after pivoting
;example: (Ordered '((1 3 5)(2 4 7)(1 1 0))) -> '(1 0 2)
(define (Order A)
  (define (Order* iA)
    (if (empty? iA)
        empty
        (let* ([firsts (map (λ(x) (first (second x))) iA)]
               [IndexMax (index-of firsts (apply bfmax firsts))]
               [OrIndex (first (list-ref iA IndexMax))]
               [removed (delete-n iA IndexMax)])
          (if (empty? removed)
              (list OrIndex)
              (cons OrIndex (Order* (map list (map first removed) (map (λ(x) (rest (second x))) removed))))))))
  (Order* (map-indexed A)))

;Given a list, return that list with the items paired with their indexes
;(map-indexed '(a b c d)) -> '((0 a) (1 b) (2 c) (3 d))
(define (map-indexed ls)
  (define (map-indexed2 n ls)
    (if (empty? ls)
        empty
        (cons (list n (first ls)) (map-indexed2 (add1 n) (rest ls)))))
  (map-indexed2 0 ls))

;Given a list and an Index, return that list with the index removed
;(delete-n '(a b c d) 1) -> '(a c d)
(define (delete-n ls n)
  (if (empty? ls)
      empty
      (if (= n 0)
          (delete-n (rest ls) (sub1 n))
          (cons (first ls) (delete-n (rest ls) (sub1 n))))))