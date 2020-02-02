#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix)
; the provide "exports" these functions

; 00.
(define (all? p? xs)
  (define (and* a b) (and a b))
  (foldr and* #t (map p? xs)))

; 01.
(define (any? p? xs)
  (define (not* f) (lambda (x) (not (f x))))
  (not (all? (not* p?) xs)))

; 02.
(define (concat xss) 
  (foldr append '() xss))

; 03.
(define (rows xss) 
  xss)


; 04.
(define (cols xss) 
  (if (or (null? xss) (any? null? xss)) 
      '() 
      (cons (map car xss) (cols (map cdr xss)))))


; 05.
(define (matrix-ref xss i j) 
  (list-ref (list-ref xss i) j))


; 06.
(define (set xs i x) 
  (if (= i 0)
      (cons x (cdr xs))
      (cons 
        (car xs) 
        (set (cdr xs) (- i 1) x))))


; 07.
(define (place xss i j x) 
  (set xss i (set (list-ref xss i) j x)))


; 08.
(define (diag xss) 
  (define (go i n)
    (if (>= i n)
        '()
        (cons (matrix-ref xss i i) (go (+ i 1) n))))
  (go 0 (length xss)))


; 09.
(define (diags xss)
  (cons
    (diag xss)
    (cons (diag (map reverse xss)) '())))


; 10.
(define (map-matrix f xss) 
  (map (lambda (xs) (map f xs)) xss))


; 11.
(define (filter-matrix p? xss)
  (map (lambda (xs) (filter p? xs)) xss))


; 12.
(define (zip-with f xs ys) 
  (if (or (null? xs) (null? ys))
      '()
      (cons
        (f (car xs) (car ys))
        (zip-with f (cdr xs) (cdr ys)))))


; 13.
(define (zip xs ys) (zip-with cons xs ys))

(define (zip-matrix xss yss) 
  (zip-with zip xss yss))


