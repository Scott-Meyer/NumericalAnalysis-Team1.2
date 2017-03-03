#lang racket
(require math/array)

(define t (mutable-array #[#[2 -2 -1 -2]
                           #[4 1 -2 1]
                           #[-2 1 -2 -3]]))
(define (row-count A)
  (vector-ref (array-shape A) 0))
(define (col-count A)
  (vector-ref (array-shape A) 1))

(define (gaus A)
  (define (gausdown a b)
    (if (not (or (>= a (row-count A)) (>= b (col-count A))))
        (let ([Aab (array-ref A (vector a b))])
          (begin (for* ([i (in-range b (col-count A))])
                   (array-set! A (vector a i) (/ (array-ref A (vector a i)) Aab)))
                 (for* ([i (in-range (+ a 1) (row-count A))])
                   (let ([Aib (array-ref A (vector i b))])
                     (for* ([j (in-range b (col-count A))])
                       (array-set! A (vector i j) (- (array-ref A (vector i j)) (* (array-ref A (vector a j)) Aib))))))
                 (gausdown (+ a 1) (+ b 1))))
        empty))
  (define (gausup a b)
    (if (not (or (< a 0) (< b 1)))
        (begin
          (for* ([i (in-range a -1 -1)])
            (let ([Aib (array-ref A (vector i b))])
              (for* ([j (in-range 0 (col-count A))])
                (array-set! A (vector i j) (- (array-ref A (vector i j)) (* (array-ref A (vector (+ a 1) j)) Aib))))))
          (gausup (- a 1) (- b 1)))
        empty))
  (begin
    (gausdown 0 0)
    (gausup (- (row-count A) 2) (- (col-count A) 2))))

;(define (gaus A)
;  (define (gaussdown a b)
;    (if (or (>= a (vector-length A)) (>= b (vector-length (vector-ref A 0))))
;        empty
;        (for* ([i (in-range b (vector-length (vector-ref A 0)))])
;          (vector-set! A a))
;  (gaussdown 4 0))