#lang racket

(provide winner
         play)

(require "matrix.rkt")
; You can use your matrix functions below, thanks to the "require" invocation above.

(define (id x) x)

; winner implementation that only detects draws right now.
; Put your own implementation here!
(define (winner b)
  (define (all-sym sym) (lambda (xs) (all? (lambda (x) (equal? x sym)) xs)))
  (if (any? id (append 
                 (map (all-sym "X") (rows b)) 
                 (map (all-sym "X") (cols b)) 
                 (map (all-sym "X") (diags b))))
    "X"
      
    (if (any? id (append 
                   (map (all-sym "O") (rows b)) 
                   (map (all-sym "O") (cols b)) 
                   (map (all-sym "O") (diags b))))
      "O"
      (if (any? id (map 
                     (lambda (xs) 
                       (any? (lambda (x) (equal? x #f)) xs)) 
                     b))
          #f
          "D"))))

;(winner '((#f #f #f)
;          (#f #f #f)
;          (#f #f #f))) ;-- #f
;
; (winner '(("X" "O" "X")
;           ("O" "X" "O")
;           ("X" "O" "X"))) ;-- "X"
;
; (winner '(("O" "O" "O")
;           ("X" "X" #f)
;           (#f #f "X"))) ;-- "O"
;
; (winner '(("O" #f "O")
;           (#f "O" #f)
;           ("X" "X" "X"))) ;-- "X"
;
; (winner '(("X" "O" "O")
;           ("O" "X" "O")
;           ("O" "X" "X"))) ;-- "X"
;
; (winner '(("X" "O" "X")
;           ("O" "X" "O")
;           ("O" "X" "O"))) ;-- "D"
;
; (winner '(("X" "O" "X")
;           ("O" "O" "X")
;           ("O" "X" #f))) ;-- #f

; "Dumb" "AI", plays the "next" free spot, going left-to-right, top-to-bottom.
; Put your own implementation here!

(define (gen-positions b)
  (define (helper i j)
    (cond ((= i (length b)) '())
          ((= j (length b)) (helper (+ i 1) 0))
          ((equal? (matrix-ref b i j) #f) (cons (cons i j) (helper i (+ j 1))))
          (else (helper i (+ j 1)))))
  (helper 0 0))


;  (gen-positions '((#f "O" "X")
;                   ("O" #f "X")
;                   ("O" "X" #f)))


(define (gen-boards b sym)
    (foldr 
      (lambda (pair rec) 
        (cons (place b (car pair) (cdr pair) sym) rec)) 
      '() 
      (gen-positions b)))


;  (gen-boards '((#f #f "X")
;                ("O" #f "X")
;                ("O" #f #f)) "O")

(define (oponent sym) 
  (cond 
    ((equal? sym "X") "O") 
    ((equal? sym "O") "X")))

(define (gen-score b max-sym) 
  (let ((win (winner b)))
    (cond ((equal? win max-sym) 1)
          ((equal? win (oponent max-sym)) -1)
          ((equal? win "D") 0)
          (else #f))))

; (print "-----------------")
; (newline)

;  (gen-score '((#f #f "X")
;                ("O" #f "X")
;                ("O" #f #f)) "X")

(define (minimax pos max-sym curr-sym)
  (if (not (equal? (gen-score pos max-sym) #f))
           (gen-score pos max-sym)
           (if (equal? max-sym curr-sym)
               (foldl 
                 (lambda (x acc) (max acc (minimax x max-sym (oponent max-sym)))) 
                 -100 
                 (gen-boards pos curr-sym))
               (foldl 
                 (lambda (x acc) (min acc (minimax x max-sym max-sym))) 
                 100 
                 (gen-boards pos curr-sym))
               )))
  
; (minimax '((#f #f #f)
;            (#f #f #f)
;            (#f #f #f)) "X" "O")


(define (gen-pos-score b curr-sym)
  (map 
    (lambda (pair) 
         (cons
           pair
           (minimax 
             (place 
               b 
               (car pair) 
               (cdr pair) 
               curr-sym) 
             curr-sym 
             (oponent curr-sym)))) 
    (gen-positions b)))

 (gen-pos-score '(("X" #f #f)
                  (#f #f #f)
                  (#f #f "O")) "X")

(define (play b curr-sym)
  (car (foldr 
    (lambda (x rec) (if (>= (cdr rec) (cdr x)) rec x))
    '((-1 . -1) . -100) 
    (gen-pos-score b curr-sym))))

;  (play'(("X" "X" #f)
;                   (#f #f #f)
;                   (#f #f "O")) "X")

;  (play! '((#f #f #f)
;             (#f #f #f)
;             (#f #f #f)) "X")

; (define (play curr-board curr-sign)
;   (define (helper i j)
;     (cond ((> i 2) #f)
;           ((> j 2) (helper (+ i 1) 0))
;           ((not (list-ref (list-ref curr-board i) j)) (cons i j))
;           (else (helper i (+ j 1)))))
;   (helper 0 0))
