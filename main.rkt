#lang racket/gui
(require math/bigfloat)

(bf-precision 128);sets the precision we want to use

(require racket/gui/base
         "single-var.rkt"
         "systems.rkt"
         (only-in "functions.rkt"
                  fp-op
                  reset-fp-op))

;ex. (parse-coefficient "9x^2y^3") --> (list (bf 9) "x^2y^3")
(define (parse-coefficient word)
  (define coeff 0)
  (define rest "")
  (define done #f)
  (for ([x (string-length word)] #:break done)
    (define current-char-int (- (char->integer (string-ref word x)) (char->integer #\0)))
    (if (< current-char-int 10)
        (set! coeff (+ (* coeff 10) current-char-int))
        (begin
          (set! rest (substring word x))
          (set! done #t)
          )
        )
    )
  (when (zero? coeff) (set! coeff 1))
  (set! coeff (bf coeff))
  (list coeff rest)
  )

;ex. (parse-vars "x^2y^3") --> (list 'x (bf 2) 'y (bf 3))
(define (parse-vars word)
  (define ret (list))
  (define state 1);1=get var, 2=check for carrot, 3=get exponent
  (define x 0)
  (define current-char-int 0)
  (define exponent 0)
  (define (loop)
    (case state
      [(1) (begin
             (set! ret (append ret (list (string->symbol (string (string-ref word x))))))
             (set! state 2)
             )
           ]
      [(2) (begin
             (if (equal? (string-ref word x) #\^)
                 (set! state 3)
                 (begin
                   (set! state 1)
                   (set! x (sub1 x))
                   )
                 )
             )
           ]
      [(3) (begin
             (set! current-char-int (- (char->integer (string-ref word x)) (char->integer #\0)))
             (if (< current-char-int 10)
                 (begin
                   (set! exponent (+ (* exponent 10) current-char-int))
                   )
                 (begin
                   (set! state 1)
                   (set! x (sub1 x))
                   (when (equal? exponent 0) (set! exponent 1))
                   (set! ret (append ret (list (bf exponent))))
                   (set! exponent 0)
                   )
                 )
             )
           ]
      )
    (set! x (add1 x))
    (when (< x (string-length word)) (loop))
    )
  (when (> (string-length word) 0)
    (loop)
    (when (equal? state 3)
        (set! ret (append ret (list (bf exponent))))
        )
    )
  ret
  )

;ex. (process-val "9x^2y^3") --> (list (bf 9) 'x (bf 2) 'y (bf 3))
(define (process-val word)
  (define ret-list (list))
  (define temp-list (list))
  (if (equal? word "+")
      (set! ret-list '+)
      (if (equal? word "-")
          (set! ret-list '-)
          (begin (set! temp-list (parse-coefficient word))
                 (set! ret-list (append ret-list (list (car temp-list))))
                 (set! temp-list (parse-vars (cadr temp-list)))
                 (set! ret-list (append ret-list temp-list))
                 )
          )
      )
  ret-list
  )

;This should process our user input into
;what we want to pass into our math functions
;example output: 3x^2 + 2x - 7 --> (list (list 3 x 2) '+ (list 2 x 1) '- (list 7 x 0))
;ex. (process-string "4x^2y^3 - 8xz^2 + 7y^2z - 9xyz") --> (list (list (bf 4) 'x (bf 2) 'y (bf 3)) '- (list (bf 8) 'x 'z (bf 2)) '+ (list (bf 7) 'y (bf 2) 'z) '- (list (bf 9) 'x 'y 'z))
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
;Function Arguments (argument name --> example):
;   num-iterations --> 5
;   initial-guess --> 0.5
;   input-string --> (process-string "3x^2 + 2x - 7")
;   system --> (list (list (list (list (bf 1) 'x (bf 1)) '- (list (bf 1) 'y (bf 3))))
;                     (list (list (list (bf 1) 'x (bf 2)) '+ (list (bf 1) 'y (bf 2)) '- (list (bf 1))))
;                     ))
;   guesses --> (list (list 'x (bf 1)) (list 'y (bf 2)))
;   init-matrix --> (identity (length (get-vars sys)))
;
;Single var:
;   ~~~~~Scott's~~~~~~~~~~
;    (bisection num-iterations initial-guess (process-string input-string))
;    (fixed-point num-iterations initial-guess (process-string input-string))
;   ~~~~~Brad's~~~~~~~~~~
;    (newtons-method num-iterations initial-guess input-string)
;
;Systems:
;   ~~~~~Scott's~~~~~~~~~~
;    (gaussian-elim (process-string input-string))
;    (lu-decomp (process-string input-string))
;    (jacobi (process-string input-string))
;    (sor (process-string input-string))
;   ~~~~~Brad's~~~~~~~~~~
;    (multi-newtons num-iterations system guesses)
;    (broydens num-iterations system guesses init-matrix)

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
                                                                (list d-panel))))
                                ((1) (send tp change-children (lambda (children)
                                                                (list e-panel))))
                                ((2) (send tp change-children (lambda (children)
                                                                (list f-panel))))
                                ((3) (send tp change-children (lambda (children)
                                                                (list g-panel))))
                                ((4) (send tp change-children (lambda (children)
                                                                (list h-panel))))
                                ((5) (send tp change-children (lambda (children)
                                                                (list i-panel)))))))))
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

(send tab-panel change-children (lambda (children)
                                  (list a-panel)))
(send sys-tab-panel change-children (lambda (children)
                                      (list d-panel)))
