
(define (lul x) x)

(define (f x) (* x x))
(define x 10)

(f x) 

(if (eq? x 10) "ivancho" #t)

(cond
    ((eq? (- x 10) 0) "musagenitsa")
    (#t 2))

(define g (lambda (x y) (+ x y)))

(g 101 x)

(define (c f g x) (f (g x)))
(define (h x) (+ x 2))

(c f h 5)

