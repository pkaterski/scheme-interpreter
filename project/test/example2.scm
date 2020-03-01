
(define (fact n)
  (if (eq? 0 n)
      1
      (* n (fact (+ n -1)))))

(fact 5)

;  (define (lul x) x)

(define (f x) (* x x))
(define x 10)

(f x)

(if (eq? x 10) "ivancho" #t)

(cond
    ((eq? (+ x 10) 0) "musagenitsa")
    (#t 2))

(define (g x y) (+ x y))

(g 101 x)

(define (c f g x) (f (g x)))
(define (h x) (+ x 2))

(c f h 5)

(define (pow n m)
  (if (eq? m 0)
      1
      (* n (pow n (+ m -1)))))

(pow 4 3)

(define (A m n)
    (cond
        ((eq? m 0) (+ n 1))
        ((eq? n 0) (A (+ m -1) 1))
        (else (A (+ m -1) (A m (+ n -1))))))

(A 3 2)

(car '(1 2 3))
(cdr '(1 2 3))

(cdr (cdr '(4.0 9 define)))

(cons 5 (cons 1.9 '()))

(define (range n)
  (if (eq? n 0)
      '()
      (cons n (range (+ n -1)))))

(range 10)


(define (append xs ys)
  (if (eq? xs '())
      ys
      (cons (car xs) (append (cdr xs) ys))))

(append '(1 2 3) '(5 6))

(define (reverse xs)
  (if (eq? xs '())
      '()
      (append (reverse (cdr xs)) (cons (car xs) '()))))

(reverse '(a b c))

(define t (lambda (x) (x 5)))
;(define (t x) (x 5))
(t (lambda (x) (+ x 11)))


(define (loc x) 
  (define y 10) 
  (define z 7) 
  (+ x y z))
(loc 10)
(define o 9)
o
; z ; local var is undefined here

(define (plusn n) (lambda (x) (+ n x)))
((plusn 3) 5)

(define (ax x) (lambda (x) x))
((ax 33) 77)

; scheme evaluetes this to 10, but we to 88
(define (j x) (define x 10) x)
(j 88)

(define k (+ -11 -12))
(define test ((lambda (x) (x 10)) (lambda (x) (+ x 11 k))))
test

(define add (lambda (x) (lambda (y) (+ x y))))
((add 1) 2)
((lambda (x) ((lambda (x) ((add 3) x)) x)) 10)

((lambda (x) ((add 3) x)) 10)







