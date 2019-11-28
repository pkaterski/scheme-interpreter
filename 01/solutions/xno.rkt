#lang racket

(provide winner
         play)

(require "matrix.rkt")
; You can use your matrix functions below, thanks to the "require" invocation above.

(define (id x) x)

; takes a symbol `sym`
; returns a function which checks if all the symbols in a list are equal to `sym`
(define (all-sym sym) (lambda (xs) (all? (lambda (x) (equal? x sym)) xs)))

; takes a board `b` and a symbol `sym`
; checks if there is a winning streak of the given symbol somewhere in the board
(define (win-streak b sym)
  (define (or* a b) (or a b))
  (foldr or* #f (append 
                  (map (all-sym sym) (rows b)) 
                  (map (all-sym sym) (cols b)) 
                  (map (all-sym sym) (diags b)))))

; takes a board `b`
; checks if there are any empty spots on the board
(define (any-empty b)
  (any? id (map (lambda (xs) (any? (lambda (x) (equal? x #f)) xs)) b)))


(define (winner b)
  (cond ((win-streak b "X") "X")
        ((win-streak b "O") "O")
        ((any-empty b) #f)
        (else "D")))


; takes a board `b`
; returns all the possible free positions as a list of pairs
(define (gen-positions b)
  (define (helper i j)
    (cond ((= i (length b)) '())
          ((= j (length b)) (helper (+ i 1) 0))
          ((equal? (matrix-ref b i j) #f) (cons (cons i j) (helper i (+ j 1))))
          (else (helper i (+ j 1)))))
  (helper 0 0))


; takes a board `b` and a symbol `sym`
; returns a list of all the possible ways the sym can be placed on the board once
(define (gen-boards b sym)
    (map 
      (lambda (pair) (place b (car pair) (cdr pair) sym))
      (gen-positions b)))


; takes a `sym`
; returns the opposing symbol of `sym`
(define (opponent sym) 
  (cond 
    ((equal? sym "X") "O") 
    ((equal? sym "O") "X")))


; takes a board and a "maximazing symbol" `max-sym`
; returns a score* if the game is over or #f otherwise
; * the score is 1 if `max-sym` has won, -1 if it has lost or 0 is its a draw
(define (gen-score b max-sym) 
  (let ((win (winner b)))
    (cond ((equal? win max-sym) 1)
          ((equal? win (opponent max-sym)) -1)
          ((equal? win "D") 0)
          (else #f))))


; :P 
(define (maximum-on f xs)
  (cdr (foldr 
         (lambda (x rec) 
           (let 
             ((val (f x))) 
             (if (> val (car rec)) 
                 (cons val x) 
                 rec))) 
         '(-inf.0 . ()) 
         xs)))


; a minimax implementation for a given position
; takes a postion `pos` as a board, a "maximazing symbol" `max-sym` and a current symbol `curr-sym`
; returns an estimated score of the given position, based on the maximazing player and the current player
(define (minimax pos max-sym curr-sym)
  (if (not (equal? (gen-score pos max-sym) #f))
    (gen-score pos max-sym)
    (maximum-on 
      (lambda (x) (if (equal? max-sym curr-sym) x (- 0 x))) 
      (map 
        (lambda (b) (minimax b max-sym (opponent curr-sym))) 
        (gen-boards pos curr-sym)))))
  

; minimax with alpha beta prunning
(define (minimax-fast pos alpha beta max-sym curr-sym)
  (define (helper poss ex-eval alpha beta is-max)
    (if (or (null? poss) (<= beta alpha))
          ex-eval
          (let ((evalu (minimax-fast 
                               (car poss) 
                               alpha 
                               beta 
                               max-sym 
                               (opponent curr-sym))))
                  (helper 
                    (cdr poss) 
                    (if is-max (max ex-eval evalu) (min ex-eval evalu))
                    (if is-max (max alpha evalu) alpha) 
                    (if is-max beta (min beta evalu))
                    is-max))))
  (if (not (equal? (gen-score pos max-sym) #f))
    (gen-score pos max-sym)
    (if (equal? max-sym curr-sym)
      (helper (gen-boards pos curr-sym) -inf.0 alpha beta #t)
      (helper (gen-boards pos curr-sym) +inf.0 alpha beta #f)
    )))


; generator of a list of position-score pairs
; takes a board `b` and a current symbol `cur-sym`
; returns a list of pairs, each of which has a position as their first 
; element and a score, based on that position, as the second element
(define (gen-pos-score b curr-sym)
  (map 
    (lambda (pair) 
         (cons
           pair
           (minimax-fast 
             (place b (car pair) (cdr pair) curr-sym) 
             -inf.0
             +inf.0
             curr-sym 
             (opponent curr-sym)))) 
    (gen-positions b)))


(define (play b curr-sym)
  (car (maximum-on cdr (gen-pos-score b curr-sym))))

; old way; without maximum-on; predi da pogledna hinta :D
;(define (play b curr-sym)
;  (car (foldr 
;    (lambda (x rec) (if (>= (cdr rec) (cdr x)) rec x))
;    '((-1 . -1) . -100) 
;    (gen-pos-score b curr-sym))))
;
