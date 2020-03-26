(define (range n)
    (define (helper i)
    (if (eq? i n)
        (cons n '())
        (cons i (helper (+ i 1)))))
  (helper 1))

(define (append xs ys)
  (if (eq? xs '())
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (reverse xs)
  (if (eq? xs '())
      '()
      (append (reverse (cdr xs)) (cons (car xs) '()))))

(define (foldl f nv xs)
    (if (eq? xs '())
        nv
        (foldl f (f nv (car xs)) (cdr xs))))

(define (foldr f nv xs)
    (if (eq? xs '())
        nv
        (f (car xs) (foldr f nv (cdr xs)))))

(foldl (lambda (acc x) (+ acc x)) 0 (range 100))
(foldr (lambda (x rec) (+ x rec)) 0 (range 100))

(foldl (lambda (acc x) (cons x acc)) '() (range 10))
(foldr (lambda (x rec) (append rec (cons x '()))) '() (range 10))

; (append (reverse (range 5)) (range 4))

(define (map g xs)
    (foldr 
        (lambda (x rec) (cons (g x) rec))
        '()
        xs))

(define (filter p xs)
    (foldr
        (lambda (x rec)
            (if (p x)
                (cons x rec)
                rec))
        '()
        xs))

(reverse (map (lambda (x) (* x x)) (range 10)))
(reverse (filter (lambda (x) (eq? x 5)) (range 10)))