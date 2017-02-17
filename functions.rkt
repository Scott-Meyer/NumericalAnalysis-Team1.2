#lang racket
(require math/bigfloat)

(provide apply-func
         derivative)

(define (apply-func func-string x-val);expecting only multiples of orders of x added or subtracted together
  (define ret 0.bf)
  (define op '+)
  (for ([val func-string])
    (if (list? val)
        (begin
          (case op
            [(+) (set! ret (bf+ ret (parse-value val x-val)))]
            [(-) (set! ret (bf- ret (parse-value val x-val)))]
            )
          )
        (begin
          (set! op val)
          )
        )
    )
  ret
  )

(define (parse-value val x-val)
  (bf* (car val) (bfexpt x-val (caddr val)))
  )

(define (derivative func-string);assuming single variable and bigfloat values
  (define deriv-string '())
  (for ([val func-string])
    (when (list? val)
      (define frst  (bf* (car val) (caddr val)))
      (define scnd (cadr val))
      (define thrd (bf- (caddr val) (bf 1)))
      (set! deriv-string (append deriv-string (list (list frst scnd thrd))))
      )
    (unless (list? val)
      (set! deriv-string (append deriv-string (list val)))
      )
    )
  deriv-string
  )