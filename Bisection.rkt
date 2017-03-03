#lang racket
(require math/bigfloat)

(provide bisectionProcess)

;replace standard math
(define negative? bfnegative?)
(define zero? bfzero?)
(define + bf+)
(define - bf-)
(define / bf/)
(define * bf*)
(define > bf>)
(define < bf<)
(define abs bfabs)


;given a function x->f(x) and a value pair (x1, x2)
;where f(x1)<0<f(x2) OR f(x2)<0<f(x1) return c where f(c)=0
(define (bisectionProcess iter f x)
  ;Prop function used to determine if the value pair x is correct
  (define (prop? f x)
    (xor (negative? (f (car x)))
         (negative? (f (second x)))))
  ;Inner function to return c where f(c)=0
  (define (bisection1 iter f x1 x2)
    (cond
      [(zero? (f x1)) x1]
      [(zero? (f x2)) x2]
      [#t
       (define c (abs (/(+ x1 x2) (bf 2))))
       (if (or (zero? (f c)) (zero? (- iter (bf 1)))) c
           (if (> (f c) (bf 0)) (bisection1 (- iter (bf 1)) f x1 c)
                           (bisection1 (- iter (bf 1)) f c x2)))]))
  ;Meat of bisection, do a bit of input checking, then call bisection1
  (cond
    [(not (pair? x)) (error "Need a range (x1,x2)")]
    [(not (prop? f x)) (error "need positive/negative (f(x1),f(x2))")]
    [#t (if (> (f (car x)) (bf 0))
              (bisection1 (bf iter) f (second x)) (car x))
              (bisection1 (bf iter) f (car x) (second x))]))