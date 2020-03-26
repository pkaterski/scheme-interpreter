(define zero (lambda (f v) v))

(define (succ n)
  (lambda (f v)
    (f (n f v))))


(define (toNumeral n)
  (if (eq? n 0)
      zero
      (lambda (f x) (f ((toNumeral (+ n -1)) f x)))))


(define (fromNumeral n)
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

(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))


(fromNumeral one)
(fromNumeral two)
(fromNumeral three)
(fromNumeral four)
(fromNumeral five)

(fromNumeral (mult five five))
(fromNumeral (plus five three))
(fromNumeral (pred (mult five five)))
(fromNumeral (mult (pred (mult five five)) (pred (mult four four))))
