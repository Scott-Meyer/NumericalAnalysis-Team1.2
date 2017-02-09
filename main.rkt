#lang racket/gui
(require math/bigfloat)

(bf-precision 128);sets the precision we want to use

(require racket/gui/base
         "single-var.rkt"
         "systems.rkt")

;This should process our user input into
;what we want to pass into our math functions
(define (process-string input-string)
  (define output-string input-string)
  output-string
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
 
; Show the frame by calling its show method
(send frame show #t)

; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
 
; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])