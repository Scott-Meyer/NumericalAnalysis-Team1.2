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
;this frame for part a-Single Var 
(define frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Single Var"]
                   [width 1000]
                   [height 600]))

;this for frame b-systems
(define sys-frame (new frame%
                   [label "Group 1.2 Numerical Analysis Project-Systems"]
                   [width 1000]
                   [height 600]))


;interface for adding tabs to the frame of part a
(define tab-panel (new tab-panel%
              [parent frame]
              [choices (list "bisection num-iterations" "fixed-point num-iterations" "newtons-method num-iterations")]
              (callback
               (lambda [tp e]
                 (case [send tp get-selection]
                   ;separates the differnt tabs and calls _-panel to fill each tab with info
                   ((0) (send tp change-children (lambda (children)
                                                   (list a-panel))))
                   ((1) (send tp change-children (lambda (children)
                                                   (list b-panel))))
                   ((2) (send tp change-children (lambda (children)
                                                   (list a-panel)))))))))

;interface for adding tabs to the frame of part b
(define sys-tab-panel (new tab-panel%
              [parent sys-frame]
              [choices (list "gaussian-elim" "lu-decomp" "jacobi" "sor" "multi-newtons" "broydens")]
              (callback
               (lambda [tp e]
                 (case [send tp get-selection]
                   ;separates the differnt tabs and calls _-panel to fill each tab with info
                   ((0) (send tp change-children (lambda (children)
                                                   (list a-panel))))
                   ((1) (send tp change-children (lambda (children)
                                                   (list b-panel))))
                   ((2) (send tp change-children (lambda (children)
                                                   (list a-panel)))))))))


;each block defines the _-panel and creates the information within each tab

;part a
;bisection num-iterations tab
(define a-panel (new panel%
                     [parent tab-panel]))
(define a-text (new message%
                    [parent a-panel]
                    [label "This is the first panel"]))

;fixed-point num-iterations tab
(define b-panel (new panel%
                     [parent tab-panel]))
(define b-text (new message%
                    [parent b-panel]
                    [label "This is the second panel"]))

;newtons-method num-iterations tab
(define c-panel (new panel%
                     [parent tab-panel]))
(define c-text (new message%
                    [parent c-panel]
                    [label "This is the third panel"]))


;part b
;gaussian-elim
(define d-panel (new panel%
                     [parent sys-tab-panel]))
(define d-text (new message%
                    [parent d-panel]
                    [label "This is the first panel"]))

;lu-decomp
(define e-panel (new panel%
                     [parent sys-tab-panel]))
(define e-text (new message%
                    [parent e-panel]
                    [label "This is the second panel"]))

;jacobi " "sor" "multi-newtons" "broydens
(define f-panel (new panel%
                     [parent sys-tab-panel]))
(define f-text (new message%
                    [parent f-panel]
                    [label "This is the third panel"]))

;sor" "multi-newtons" "broydens
(define g-panel (new panel%
                     [parent sys-tab-panel]))
(define g-text (new message%
                    [parent g-panel]
                    [label "This is the fourth panel"]))

;multi-newtons
(define h-panel (new panel%
                     [parent sys-tab-panel]))
(define h-text (new message%
                    [parent h-panel]
                    [label "This is the fifth panel"]))

;broydens
(define i-panel (new panel%
                     [parent sys-tab-panel]))
(define i-text (new message%
                    [parent i-panel]
                    [label "This is the sixth panel"]))


; Show the frame by calling its show method
;will show two frames/windows for each part
(send frame show #t)
(send sys-frame show #t)
