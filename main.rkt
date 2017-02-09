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
; (bisection input-string)
; (fixed-point input-string)
; (newtons-method input-string)
;Systems:
; (gaussian-elim input-string)
; (lu-decomp input-string)
; (jacobi input-string)
; (sor input-string)
; (multi-newtons input-string)
; (broydens input-string)

;~~~~~~~~~~~~Jonathans GUI~~~~~~~~~~~~~~~
; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Group 1.2 Numerical Analysis Project"]))
 
; Show the frame by calling its show method
(send frame show #t)