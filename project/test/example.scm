(define (fact n)
  (if (eq? 0.0 n)
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
  (if (eq? m 0.0)
      1
      (* n (pow n (+ m -1)))))

(pow 4 3)

(define (A m n)
    (cond
        ((eq? m 0.0) (+ n 1))
        ((eq? n 0.0) (A (+ m -1) 1))
        (else (A (+ m -1) (A m (+ n -1))))))

(A 3 2)

(car '(1 2 3))
(cdr '(1 2 3))

(cdr (cdr '(4.0 9 define)))

(cons 5 (cons 1.9 '()))


