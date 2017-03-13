#lang racket
(require math/bigfloat)

(provide bisectionProcess)

(define fp-op 0)

;replace standard math
(define mnegative? bfnegative?)
(define mzero? bfzero?)
(define m+ (lambda lst
            (for ([_ (in-range 1 (length lst))])(set! fp-op (add1 fp-op)))
            (apply bf+ lst)))
(define m- (lambda lst
            (for ([_ (in-range 1 (length lst))])(set! fp-op (add1 fp-op)))
            (apply bf- lst)))
(define m/ (lambda lst
            (for ([_ (in-range 1 (length lst))])(set! fp-op (add1 fp-op)))
            (apply bf/ lst)))
(define m* (lambda lst
            (for ([_ (in-range 1 (length lst))])(set! fp-op (add1 fp-op)))
            (apply bf* lst)))
(define m> (lambda lst
            (for ([_ (in-range 1 (length lst))])(set! fp-op (add1 fp-op)))
            (apply bf> lst)))
(define m< (lambda lst
            (for ([_ (in-range 1 (length lst))])(set! fp-op (add1 fp-op)))
            (apply bf< lst)))
(define mabs (lambda lst
            (for ([_ (in-range 1 (length lst))])(set! fp-op (add1 fp-op)))
            (apply bfabs lst)))


;given a function x->f(x) and a value pair (x1, x2)
;where f(x1)<0<f(x2) OR f(x2)<0<f(x1) return c where f(c)=0
(define (bisectionProcess iter f x)
  (define start-time (current-inexact-milliseconds))
  ;Prop function used to determine if the value pair x is correct
  (define (prop? f x)
    (xor (mnegative? (f (car x)))
         (mnegative? (f (second x)))))
  ;Inner function to return c where f(c)=0
  (define (bisection1 iter f x1 x2)
    (cond
      [(mzero? (f x1)) (list x1 iter)]
      [(mzero? (f x2)) (list x2 iter)]
      [else
       (define c (mabs (m/(m+ x1 x2) (bf 2))))
       (if (or (mzero? (f c)) (mzero? (m- iter (bf 1)))) (list c (bf- iter (bf 1)))
           (if (m> (f c) (bf 0)) (bisection1 (m- iter (bf 1)) f x1 c)
                           (bisection1 (m- iter (bf 1)) f c x2)))]))
  ;Meat of bisection, do a bit of input checking, then call bisection1
  (let ([result (cond
                  [(not (pair? x)) (error "Need a range (x1,x2)")]
                  [(not (prop? f x)) (error "need positive/negative (f(x1),f(x2))")]
                  [#t (if (m> (f (car x)) (bf 0))
                          (bisection1 (bf iter) f (second x) (car x))
                          (bisection1 (bf iter) f (car x) (second x)))])])
    (list
     ;first, result of x
     (first result)
     ;second, iterations
     (bf- (bf iter) (second result))
     ;third, total floating point operations
     (bf fp-op)
     ;fourth, execution time
     (bf- (bf (current-inexact-milliseconds)) (bf start-time)))))