#lang racket/gui
(require math/bigfloat)

(bf-precision 128);sets the precision we want to use

(require racket/gui/base
         "single-var.rkt"
         "systems.rkt")

(define (process-val word)
  (if (equal? word "+")
      '+
      (if (equal? word "-")
          '-
          (list (bf (string->number (car (string-split word "x")))) 'x (if (equal? (length (string-split word "^")) 2) (bf (string->number (cadr (string-split word "^"))))
                                                                           (if (string-contains? word "x") (bf 1) (bf 0))))
          )
      )
  )

;This should process our user input into
;what we want to pass into our math functions
;example output: 3x^2 + 2x - 7 --> (list (list 3 x 2) '+ (list 2 x 1) '- (list 7 x 0))
(define (process-string input-string)
  (define output-list (list))
  (define word-start 0)
  (define current-word "")
  (for ([x input-string])
    (if (and (equal? #\space x) (not (equal? (process-val current-word) "")))
      (begin
        (set! output-list (append output-list (list (process-val current-word))))
        (set! current-word "")
        )
      (set! current-word (string-append current-word (string x)))
      )
    )
  (set! output-list (append output-list (list (process-val current-word))))
  output-list
  )

;~~~~~~~~~~~~FUNCTIONS~~~~~~~~~~~~~~~~~~~
;Single var:
; (bisection num-iterations initial-guess (process-string input-string))
; (fixed-point num-iterations initial-guess (process-string input-string))
; (newtons-method num-iterations initial-guess (process-string input-string))
;Systems:
; (gaussian-elim (process-string input-string))
; (lu-decomp (process-string input-string))
; (jacobi (process-string input-string))
; (sor (process-string input-string))
; (multi-newtons (process-string input-string))
; (broydens (process-string input-string))

;~~~~~~~~~~~~Jonathans GUI~~~~~~~~~~~~~~~

;/////////part a////////////////
;bisection num-iterations frame
(define bisection-frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Single Var"]
                   [width 1000]
                   [height 600]))

(define bis-in (new text-field%
                    [label "Number of iterations"]
                    [parent bisection-frame]
                    ))
(define bis-in1 (new text-field%
                     [label "list"]
                     [parent bisection-frame]))




;fixed-point num-iterations frame
(define fixed-point-frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Single Var"]
                   [width 1000]
                   [height 600]))

;newtons-method num-iterations tab
(define newtons-method-frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Single Var"]
                   [width 1000]
                   [height 600]))


;////////part b///////////
;gaussian-elim
(define gaussian-elim-frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Single Var"]
                   [width 1000]
                   [height 600]))

;lu-decomp
(define lu-decomp-frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Single Var"]
                   [width 1000]
                   [height 600]))

;jacobi
(define jacobi-frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Single Var"]
                   [width 1000]
                   [height 600]))

;sor
(define sor-frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Single Var"]
                   [width 1000]
                   [height 600]))

;multi-newtons
(define multi-newtons-frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Single Var"]
                   [width 1000]
                   [height 600]))

;broydens
(define broydens-frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Single Var"]
                   [width 1000]
                   [height 600]))





; Show the frame by calling its show method
(send bisection-frame show #t)

;(bisection 3 (list 0 1) (list (list (bf 1) 'x (bf 5)) '+ (list (bf 1) 'x (bf 1)) '- (list (bf 1) 'x 0.bf)))
