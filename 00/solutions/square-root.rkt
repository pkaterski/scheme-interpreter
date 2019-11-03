#lang racket

(provide my-sqrt)


(define (my-sqrt x)
  (define (newtonche i)
    (if (<
         (abs (- (* i i) x))
         0.0001) ; tochnostta
        i
        (newtonche
         (-
          i
          (/
           (- (* i i) x)
           (* 2 i))))))
  (newtonche 200))

