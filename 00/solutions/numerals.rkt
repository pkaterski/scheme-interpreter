#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))

(define (succ n)
  (lambda (f v)
    (f (n f v))))


(define (to-numeral n)
  (if (= n 0)
      zero
      (lambda (f x) (f ((to-numeral (+ n -1)) f x)))))


(define (from-numeral n)
  (n
   (lambda (x) (+ 1 x))
   0))


(define (plus n m)
  (m succ n))

; vtori nachin
;(define (plus n m)
;  (lambda (s z) (m s (n s z))))

(define (mult n m)
  (lambda (f x) (n (lambda (y) (m f y)) x)))

(define (pair f g)
  (lambda (select) (select f g)))

(define (car f g) f)
(define (cdr f g) g)

(define (succPair xs)
  (pair
   (succ (xs car))
   (xs car)))


(define (pred n)
  ((n succPair (pair zero zero)) cdr))



