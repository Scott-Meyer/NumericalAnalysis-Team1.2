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
; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Group 1.2 Numerical Analysis Project"]))
 
; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
 
; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])

; Show the frame by calling its show method
(send frame show #t)
