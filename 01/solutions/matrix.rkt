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
  (if (null? xs)
      #t
      (and (p? (car xs)) (all? p? (cdr xs)))))

; 01.
(define (any? p? xs) 
  (if (null? xs)
      #f
      (or (p? (car xs)) (any? p? (cdr xs)))))

; 02.
(define (concat xss) 
  (if (null? xss)
      '()
      (append (car xss) (concat (cdr xss)))))

; 03.
(define (rows xss) 
  xss)

;(rows '((1 2 3)
;        (4 5 6)
;        (7 8 9))) ;-- '((1 2 3) (4 5 6) (7 8 9))

; 04.
(define (cols xss) 
  (define (carList xss)
    (foldr (lambda (x rec) (if (null? x) rec (cons (car x) rec))) '() xss))
  (define (cdrList xss)
    (foldr (lambda (x rec) (if (null? x) rec (cons (cdr x) rec))) '() xss))
  (define (skoba x) (if (null? x) x (cons x '())))
  (define (helper xss acc)
    (if (null? xss)
        acc
        (helper (cdrList xss) (append acc (skoba (carList xss)))
                )))
  (helper xss '()))

;(cols '((1 2 3)
;        (4 5 6)
;        (7 8 9))) ;-- '((1 2 3) (4 5 6) (7 8 9))



; 05.
(define (matrix-ref xss i j) 
  (if (= i 0)
      (if (= j 0)
          (caar xss)
          (matrix-ref (cons (cdr (car xss)) (cdr xss)) i (- j 1)))
      (matrix-ref (cdr xss) (- i 1) j)))

;(matrix-ref '((1 2 3)
;              (4 5 6)
;              (7 8 9)) 1 1) ;-- 5
;(matrix-ref '((1 2 3)
;              (4 5 6)
;              (7 8 9)) 1 0) ;-- 4
;(matrix-ref '((1 2 3)
;              (4 5 6)
;              (7 8 9)) 0 2) ;-- 3


; 06.
(define (set xs i x) 
  (if (= i 0)
      (cons x (cdr xs))
      (cons 
        (car xs) 
        (set (cdr xs) (- i 1) x))))

;(set '(1 2 3) 2 1337)   ;-- '(1 2 1337)
;(set '(1 2 3 4 5) 0 42) ;-- '(42 2 3 4 5)

; 07.
(define (place xss i j x) 
  (if (= i 0)
      (cons (set (car xss) j x) (cdr xss))
      (cons (car xss) (place (cdr xss) (- i 1) j x))))

;(place '((1 2 3)
;         (4 5 6)
;         (7 8 9)) 1 1 42)   ;-- '((1 2 3)
;                            ;     (4 42 6)
;                            ;     (7 8 9))
;(place '((1 2 3)
;         (4 5 6)
;         (7 8 9)) 1 0 69)   ;-- '((1 2 3)
;                            ;     (69 5 6)
;                            ;     (7 8 9))
;(place '((1 2 3)
;         (4 5 6)
;         (7 8 9)) 0 2 1337) ;-- '((1 2 1337)
;                            ;     (4 5 6)
;                            ;     (7 8 9))

; 08.
(define (diag xss) 
  (define (go i n)
    (if (>= i n)
        '()
        (cons (matrix-ref xss i i) (go (+ i 1) n))))
  (go 0 (length xss)))

;(diag '((1 2 3)
;        (4 5 6)
;        (7 8 9))) ;-- '(1 5 9)
;(diag '((1))) ;-- '(1)
;(diag '((1 0)
;        (0 1))) ;-- '(1 1)

; 09.
(define (diags xss) 
  (cons
    (diag xss)
    (cons 
      (diag 
        (map (lambda (xs) (foldl cons '() xs)) xss)) ; reverse each row 
      '())))

;(diags '((1 0)
;         (0 1)))   ;-- '((1 1) (0 0))
;(diags '((1 2 3)
;         (4 5 6)
;         (7 8 9))) ;-- '((1 5 9) (3 5 7))

; 10.
(define (map-matrix f xss) 
  (if (null? xss)
      '()
      (cons (map f (car xss)) (map-matrix f (cdr xss)))))

;(define (const x) (lambda (y) x))
;(define (1+ x) (+ x 1))
;(define (id x) x)
;(map-matrix id         '((1337)))  ;-- '((1337))
;(map-matrix 1+         '((1 0)
;                         (0 1)))   ;-- '((2 1)
;                                   ;     (1 2))
;(map-matrix (const 69) '((1 2 3)
;                         (4 5 6)
;                         (7 8 9))) ;-- '((69 69 69)
;                                   ;     (69 69 69)
;                                   ;     (69 69 69))

; 11.
(define (filter-matrix p? xss)
  (if (null? xss)
      '()
      (cons (filter p? (car xss)) (filter-matrix p? (cdr xss)))))

;(filter-matrix odd?  '((1 2 3)
;                       (4 5 6)
;                       (7 8 9))) ;-- '((1 3) (5) (7 9))
;(filter-matrix zero? '((1 0)
;                       (0 1)))   ;-- '((0) (0))
;(filter-matrix zero? '((1 2 3)
;                       (4 5 6)
;                       (7 8 9))) ;-- '(() () ())

; 12.
(define (zip-with f xs ys) 
  (if (or (null? xs) (null? ys))
      '()
      (cons
        (f (car xs) (car ys))
        (zip-with f (cdr xs) (cdr ys)))))

;(zip-with cons '(1 2 3) '(4 5 6))              ;-- '((1 . 4) (2 . 5) (3 . 6))
;(zip-with cons '(1) '(4 5 6))                  ;-- '((1 . 4))
;(zip-with + '(60 1300 40) '(9 37 2))           ;-- '(69 1337 42)
;(zip-with string-append '("y" "y") '("o" "o")) ;-- '("yo" "yo")

; 13.
(define (zip-matrix xss yss) 
  (zip-with (lambda (xs ys) (zip-with cons xs ys)) xss yss))

;(zip-matrix '((1 2))
;            '((3 4))) ;-- '(((1 . 3) (2 . 4)))
;(zip-matrix '((1 0)
;              (0 1))
;            '((6 9)
;              (9 6))) ;-- '(((1 . 6) (0 . 9))
;                      ;     ((0 . 9) (1 . 6)))

