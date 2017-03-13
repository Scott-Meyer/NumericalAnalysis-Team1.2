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
  (when (and (zero? coeff) (not (equal? rest ""))) (set! coeff 1))
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
                   (when (equal? exponent 0) (set! exponent 1))
                   (set! ret (append ret (list (bf exponent))))
                   (set! exponent 0)
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
    (when (equal? state 2) (set! ret (append ret (list (bf 1)))))
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
                   [label "Group 1.2 Numerical Analysis Project"]
                   [width 1000]
                   [height 600]))

(define single-sys-tab (new tab-panel%
                            [parent frame]
                            [choices (list "Single Variable" "Systems")]
                            (callback
                             (lambda [tp e]
                               (case [send tp get-selection]
                                 ;separates the differnt tabs and calls _-panel to fill each tab with info
                                 ((0) (send tp change-children (lambda (children)
                                                                 (list tab-panel))))
                                 ((1) (send tp change-children (lambda (children)
                                                                 (list sys-tab-panel)))))))))


;interface for adding tabs to the frame of part a
(define tab-panel (new tab-panel%
                       [parent single-sys-tab]
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
                                                            (list c-panel)))))))))

;interface for adding tabs to the frame of part b
(define sys-tab-panel (new tab-panel%
                           [parent single-sys-tab]
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
                    [label ""]))

(define bisection-split (new horizontal-panel%
                             [parent a-panel]
                             [alignment '(left center)]
                             [style '(border)]))
(define bisection-main (new vertical-panel%
                            [parent bisection-split]
                            [alignment '(center center)]
                            [style '(border)]))
(define bis-num-iter (new text-field%
                          [label "Number of iterations:"]
                          [parent bisection-main]
                          ))
(define bis-init-guess-left (new text-field%
                                 [label "Initial left bracket:"]
                                 [parent bisection-main]
                                 ))
(define bis-init-guess-right (new text-field%
                                  [label "Initial right bracket:"]
                                  [parent bisection-main]
                                  ))
(define bis-equation (new text-field%
                          [label "f(x)="]
                          [parent bisection-main]
                          ))
(define bis-submit (new button%
                        [label "Submit"]
                        [parent bisection-main]
                        (callback
                         (lambda (_ ...)
                           (define num-iter (string->number (send (send bis-num-iter get-editor) get-text)))
                           (define init-left (string->number (send (send bis-init-guess-left get-editor) get-text)))
                           (define init-right (string->number (send (send bis-init-guess-right get-editor) get-text)))
                           (define in-string (send (send bis-equation get-editor) get-text))
                           (define result (bisection num-iter (list init-left init-right) (process-string in-string)))
                           (send bis-result set-value (bigfloat->string result))
                           ))))
(define bis-right (new vertical-panel%
                       [parent bisection-split]
                       [style '(border)]
                       [alignment '(center center)]))
(define bis-result (new text-field%
                        [parent bis-right]
                        [label "results"]))

;fixed-point num-iterations tab
(define b-panel (new panel%
                     [parent tab-panel]))
(define b-text (new message%
                    [parent b-panel]
                    [label ""]))
(define fix-split (new horizontal-panel%
                       [parent b-panel]
                       [alignment '(left center)]
                       [style '(border)]))
(define fix-main (new vertical-panel%
                      [parent fix-split]
                      [alignment '(center center)]
                      [style '(border)]))
(define fix-num-iter (new text-field%
                          [label "Number of iterations:"]
                          [parent fix-main]
                          ))
(define fix-init-guess (new text-field%
                            [label "Initial guess:"]
                            [parent fix-main]
                            ))
(define fix-equation (new text-field%
                          [label "f(x)="]
                          [parent fix-main]
                          ))
(define fix-submit (new button%
                        [label "Submit"]
                        [parent fix-main]
                        (callback
                         (lambda (_ ...)
                           (define num-iter (string->number (send (send fix-num-iter get-editor) get-text)))
                           (define init-guess (string->number (send (send fix-init-guess get-editor) get-text)))
                           (define in-string (send (send fix-equation get-editor) get-text))
                           (define result (fixed-point num-iter init-guess (process-string in-string)))
                           (send fix-result set-value (bigfloat->string result))
                           ))))
(define fix-right (new vertical-panel%
                       [parent fix-split]
                       [style '(border)]
                       [alignment '(center center)]))
(define fix-result (new text-field%
                        [parent fix-right]
                        [label "results"]))

;newtons-method num-iterations tab
(define c-panel (new panel%
                     [parent tab-panel]))
(define c-text (new message%
                    [parent c-panel]
                    [label ""]))
(define newt-split (new horizontal-panel%
                        [parent c-panel]
                        [alignment '(left center)]
                        [style '(border)]))
(define newt-main (new vertical-panel%
                       [parent newt-split]
                       [alignment '(center center)]
                       [style '(border)]))
(define newt-num-iter (new text-field%
                           [label "Number of iterations:"]
                           [parent newt-main]
                           ))
(define newt-init-guess (new text-field%
                             [label "Initial guess:"]
                             [parent newt-main]
                             ))
(define newt-equation (new text-field%
                           [label "f(x)="]
                           [parent newt-main]
                           ))
(define newt-submit (new button%
                         [label "Submit"]
                         [parent newt-main]
                         (callback
                          (lambda (_ ...)
                            (define num-iter (string->number (send (send newt-num-iter get-editor) get-text)))
                            (define init-guess (string->number (send (send newt-init-guess get-editor) get-text)))
                            (define in-string (send (send newt-equation get-editor) get-text))
                            (define result (newtons-method num-iter init-guess (process-string in-string)))
                            (send newt-result set-value (bigfloat->string result))
                            ))))
(define newt-right (new vertical-panel%
                        [parent newt-split]
                        [style '(border)]
                        [alignment '(center center)]))
(define newt-result (new text-field%
                         [parent newt-right]
                         [label "results"]))

;part b

(define (set-dimensions parent text-list dimen)
  (for ([x (length text-list)])
    (if (< x dimen)
        (if (send (list-ref text-list x) is-shown?)
            (begin
              (send parent delete-child (list-ref text-list x))
              (send parent add-child (list-ref text-list x)))
            (send parent add-child (list-ref text-list x)))
        (when (send (list-ref text-list x) is-shown?)
          (send parent delete-child (list-ref text-list x)))
        )
    )
  )

;gaussian-elim
(define d-panel (new panel%
                     [parent sys-tab-panel]))
(define d-text (new message%
                    [parent d-panel]
                    [label ""]))
(define gauss-split (new horizontal-panel%
                         [parent d-panel]
                         [alignment '(left center)]
                         [style '(border)]))
(define gauss-main (new vertical-panel%
                        [parent gauss-split]
                        [alignment '(center center)]
                        [style '(border)]))
(define gauss-num-iter (new text-field%
                            [label "Number of iterations:"]
                            [parent gauss-main]
                            ))
(define gauss-init-guess (new text-field%
                              [label "Initial guess:"]
                              [parent gauss-main]
                              ))
(define gauss-equation (new text-field%
                            [label "f(x)="]
                            [parent gauss-main]
                            ))
(define gauss-submit (new button%
                          [label "Submit"]
                          [parent gauss-main]
                          (callback
                           (lambda (_ ...)
                             (define num-iter (string->number (send (send gauss-num-iter get-editor) get-text)))
                             (define init-guess (string->number (send (send gauss-init-guess get-editor) get-text)))
                             (define in-string (send (send gauss-equation get-editor) get-text))
                             (define result (newtons-method num-iter init-guess (process-string in-string)))
                             (send gauss-result set-value (bigfloat->string result))
                             ))))
(define gauss-right (new vertical-panel%
                         [parent gauss-split]
                         [style '(border)]
                         [alignment '(center center)]))
(define gauss-result (new text-field%
                          [parent gauss-right]
                          [label "results"]))

;lu-decomp
(define e-panel (new panel%
                     [parent sys-tab-panel]))
(define e-text (new message%
                    [parent e-panel]
                    [label "This is the second panel"]))

;jacobi
(define f-panel (new panel%
                     [parent sys-tab-panel]))
(define f-text (new message%
                    [parent f-panel]
                    [label "This is the third panel"]))

;sor
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
                    [label ""]))
(define mnewt-split (new horizontal-panel%
                         [parent h-panel]
                         [alignment '(left center)]
                         [style '(border)]))
(define mnewt-main (new vertical-panel%
                        [parent mnewt-split]
                        [alignment '(center center)]
                        [style '(border)]))
(define mnewt-num-iter (new text-field%
                            [label "Number of iterations:"]
                            [parent mnewt-main]
                            ))
(define mnewt-slider (new slider%
                          [label "Number of unknowns:"]
                          [min-value 1]
                          [max-value 10]
                          [parent mnewt-main]
                          (callback
                           (lambda (_ ...)
                             (define val (send mnewt-slider get-value))
                             (set-dimensions mnewt-functions mnewt-functions-list val)
                             (set-dimensions mnewt-init-guess mnewt-guess-list val)
                             (set-dimensions mnewt-result mnewt-result-list val)
                             )
                           )))
(define mnewt-functions (new vertical-panel%
                             [parent mnewt-main]
                             ))
(define mnewt-functions-list (for/list ([x 20])
                               (new text-field%
                                    [parent mnewt-functions]
                                    [label (format "f~a=" (add1 x))]
                                    [style '(single deleted)]
                                    )))
(define mnewt-init-guess (new vertical-panel%
                              [parent mnewt-main]
                              ))
(define mnewt-guess-list (for/list ([x 20])
                           (new text-field%
                                [parent mnewt-init-guess]
                                [label (format "~a=" (integer->char (+ (char->integer #\a) x)))]
                                [style '(single deleted)]
                                )))
(define mnewt-submit (new button%
                          [label "Submit"]
                          [parent mnewt-main]
                          (callback
                           (lambda (_ ...)
                             (define num-iter (string->number (send (send mnewt-num-iter get-editor) get-text)))
                             (define val (send mnewt-slider get-value))
                             (define system (for/list ([x val])
                                              (list(process-string (send (send (list-ref mnewt-functions-list x) get-editor) get-text))))
                               )
                             (define guess (for/list ([x val])
                                             (list (string->symbol (format "~a" (integer->char (+ (char->integer #\a) x))))
                                                   (bf (string->number (send (send (list-ref mnewt-guess-list x) get-editor) get-text)))
                                                   )
                                             )
                               )
                             (printf "(multi-newtons ~a ~a ~a)~n" num-iter system guess)
                             (define result (multi-newtons num-iter system guess))
                             (printf "result: ~a~n" result)
                             (for ([x val])
                               (send (list-ref mnewt-result-list x) set-value (bigfloat->string (cadr (list-ref result x))))
                               )
                             void
                             ))))
(define mnewt-right (new vertical-panel%
                         [parent mnewt-split]
                         [style '(border)]
                         [alignment '(center center)]))
(define mnewt-result-label (new message%
                                [parent mnewt-right]
                                [label "results:"]))
(define mnewt-result (new vertical-panel%
                          [parent mnewt-right]
                          ))
(define mnewt-result-list (for/list ([x 20])
                            (new text-field%
                                 [parent mnewt-result]
                                 [label (format "~a=" (integer->char (+ (char->integer #\a) x)))]
                                 [style '(single deleted)]
                                 )))

(set-dimensions mnewt-functions mnewt-functions-list 1)
(set-dimensions mnewt-init-guess mnewt-guess-list 1)
(set-dimensions mnewt-result mnewt-result-list 1)

;broydens
(define i-panel (new panel%
                     [parent sys-tab-panel]))
(define i-text (new message%
                    [parent i-panel]
                    [label ""]))
(define broydens-split (new horizontal-panel%
                            [parent i-panel]
                            [alignment '(left center)]
                            [style '(border)]))
(define broydens-main (new vertical-panel%
                           [parent broydens-split]
                           [alignment '(center center)]
                           [style '(border)]))
(define broydens-num-iter (new text-field%
                               [label "Number of iterations:"]
                               [parent broydens-main]
                               ))
(define broydens-slider (new slider%
                             [label "Number of unknowns:"]
                             [min-value 1]
                             [max-value 6]
                             [parent broydens-main]
                             (callback
                              (lambda (_ ...)
                                (define val (send broydens-slider get-value))
                                (set-dimensions broydens-functions broydens-functions-list val)
                                (set-dimensions broydens-init-guess broydens-guess-list val)
                                (set-dimensions broydens-result broydens-result-list val)
                                (set-dimensions broydens-init-matrix broydens-matrix-vert val)
                                (for ([x (length broydens-matrix-hor)])
                                  (set-dimensions (list-ref broydens-matrix-vert x) (list-ref broydens-matrix-hor x) val))
                                )
                              )))
(define broydens-functions (new vertical-panel%
                                [parent broydens-main]
                                ))
(define broydens-functions-list (for/list ([x 6])
                                  (new text-field%
                                       [parent broydens-functions]
                                       [label (format "f~a=" (add1 x))]
                                       [style '(single deleted)]
                                       )))
(define broydens-init-guess (new vertical-panel%
                                 [parent broydens-main]
                                 ))
(define broydens-guess-list (for/list ([x 6])
                              (new text-field%
                                   [parent broydens-init-guess]
                                   [label (format "~a=" (integer->char (+ (char->integer #\a) x)))]
                                   [style '(single deleted)]
                                   )))
(define broydens-matrix-lable (new message% [parent broydens-main] [label "Initial Matrix:"]))
(define broydens-init-matrix (new vertical-panel%
                                  [parent broydens-main]
                                  ))
(define broydens-matrix-vert (for/list ([_ 6])
                               (new horizontal-panel%
                                    [parent broydens-init-matrix])
                               ))
(define broydens-matrix-hor (for/list ([x broydens-matrix-vert])
                              (for/list([y 6])
                                (new text-field%
                                     [parent x]
                                     [label #f]
                                     [style '(single deleted)]
                                     [min-width 1]
                                     )
                                )))
                              

(define broydens-submit (new button%
                             [label "Submit"]
                             [parent broydens-main]
                             (callback
                              (lambda (_ ...)
                                (define num-iter (string->number (send (send broydens-num-iter get-editor) get-text)))
                                (define val (send broydens-slider get-value))
                                (define system (for/list ([x val])
                                                 (list(process-string (send (send (list-ref broydens-functions-list x) get-editor) get-text))))
                                  )
                                (define guess (for/list ([x val])
                                                (list (string->symbol (format "~a" (integer->char (+ (char->integer #\a) x))))
                                                      (bf (string->number (send (send (list-ref broydens-guess-list x) get-editor) get-text)))
                                                      )
                                                )
                                  )
                                (define init-matrix (for/list ([x val])
                                                            (for/list ([y val])
                                                              (car(process-string (send (send (list-ref (list-ref broydens-matrix-hor x) y) get-editor) get-text)))
                                                              ))
                                  )
                                                      
                              
                                (printf "(broydens ~a ~a ~a ~a)~n" num-iter system guess init-matrix)
                                (define result (broydens num-iter system guess init-matrix))
                                (printf "result: ~a~n" result)
                                (for ([x val])
                                  (send (list-ref broydens-result-list x) set-value (bigfloat->string (cadr (list-ref result x))))
                                  )
                                void
                                ))))
(define broydens-right (new vertical-panel%
                            [parent broydens-split]
                            [style '(border)]
                            [alignment '(center center)]))
(define broydens-result-label (new message%
                                   [parent broydens-right]
                                   [label "results:"]))
(define broydens-result (new vertical-panel%
                             [parent broydens-right]
                             ))
(define broydens-result-list (for/list ([x 6])
                               (new text-field%
                                    [parent broydens-result]
                                    [label (format "~a=" (integer->char (+ (char->integer #\a) x)))]
                                    [style '(single deleted)]
                                    )))

(set-dimensions broydens-functions broydens-functions-list 1)
(set-dimensions broydens-init-guess broydens-guess-list 1)
(set-dimensions broydens-result broydens-result-list 1)
(set-dimensions broydens-init-matrix broydens-matrix-vert 1)
(for ([x (length broydens-matrix-hor)])
  (set-dimensions (list-ref broydens-matrix-vert x) (list-ref broydens-matrix-hor x) 1))


; Show the frame by calling its show method
(send frame show #t)
(send single-sys-tab change-children (lambda (children)
                                       (list tab-panel)))
(send tab-panel change-children (lambda (children)
                                  (list a-panel)))
(send sys-tab-panel change-children (lambda (children)
                                      (list d-panel)))
