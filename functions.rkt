#lang racket
(require math/bigfloat)

(provide jacobian
         apply-matrix
         apply-func
         derivative)

(define (jacobian matrix) ;returns the jacobian matrix of 'matrix'
  void
  )

(define (apply-matrix matrix var-vals) ;apply each variable value to each function in the matrix
  matrix
  )

;ex. (apply-func (list (list (bf 1) 'x (bf 3)) '- (list (bf 1) 'x (bf 1)) '+ (list (bf 5) 'x (bf 2) 'y (bf 3))) (list (list 'x (bf 2)) (list 'y (bf 7))))
(define (apply-func func-string var-vals) ;applies each variables value to each term of a polynomial and returns the numeric result
  (define ret 0.bf)
  (define op '+)
  (define current-val (bf 0))
  (for ([val func-string])
    (if (list? val)
        (begin
          (set! current-val val)
          (for ([var var-vals])
            (set! current-val (parse-value current-val (cadr var) (car var)))
            )
          (case op
            [(+) (set! ret (bf+ ret (car current-val)))]
            [(-) (set! ret (bf- ret (car current-val)))]
            )
          )
        (begin
          (set! op val)
          )
        )
    )
  ret
  )

;ex. (parse-value (list (bf 5) 'x (bf 2) 'y (bf 3)) (bf 2) 'x)
(define (parse-value val var-val respect-to) ;returns the value of 'val' with 'respect-to' replaced by 'var-val'
  ;(printf "parse ~a with var ~a = ~a~n" val respect-to var-val)
  (define ret (list (car val)))
  (for ([x (in-range 1 (- (length val) 1) 2)])
    (if (equal? respect-to (list-ref val x))
        (begin
          (set! x (add1 x))
          (set! ret (list-set ret 0 (bf* (list-ref ret 0) (bfexpt var-val (list-ref val x)))))
          )
        (begin
          (set! ret (append ret (list (list-ref val x) (list-ref val (add1 x)))))
          )
        )
    )
  ret
  )

;ex. (derive (list (bf 5) 'x (bf 2) 'y (bf 3)) 'x)
(define (derive val respect-to) ;derivative of a single value (i.e. 7x^2y)
  (define contains-respect-to #false)
  (define deriv-ret (list (car val)))
  (for ([x (in-range 1 (- (length val) 1) 2)])
    (define var (list-ref val x))
    (set! x (add1 x))
    (define expo (list-ref val x))
    (if (equal? var respect-to)
        (begin
          (set! contains-respect-to #true)
          (set! deriv-ret (list-set deriv-ret 0 (bf* (list-ref deriv-ret 0) expo)))
          (set! deriv-ret (append deriv-ret (list var (bf- expo (bf 1)))))
          )
        (begin
          (set! deriv-ret (append deriv-ret (list var expo)))
          )
        )
    )
  (when (equal? (list-ref deriv-ret 0) (bf 0)) (set! contains-respect-to #false))
  (if contains-respect-to
      deriv-ret
      (list 0.bf)
      )
  )

;ex. (derivative (list (list (bf 1) 'x (bf 3)) '- (list (bf 1) 'x (bf 1)) '+ (list (bf 5) 'x (bf 2) 'y (bf 3))) 'x)
(define (derivative func-string respect-to) ;derivative of a polynomial (i.e. 7x^2 + 2xy - y)
  (define deriv-string '())
  (for ([val func-string])
    (when (list? val)
      (define this-deriv (derive val respect-to))
      (set! deriv-string (append deriv-string (list this-deriv)))
      )
    (unless (list? val)
      (set! deriv-string (append deriv-string (list val)))
      )
    )
  deriv-string
  )