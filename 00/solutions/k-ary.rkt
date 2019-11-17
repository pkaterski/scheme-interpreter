#lang racket

(provide from-k-ary
         to-k-ary)

(define (from-k-ary n k)
            (if (< n 10)
                n
                (+
                 (remainder n 10)
                 (* k (from-k-ary
                       (/
                        (- n (remainder n 10))
                        10)
                       k)))))

(define (to-k-ary n k)
            (if (< n k)
                n
                (+
                 (remainder n k)
                 (* 10 (to-k-ary
                       (/
                        (- n (remainder n k))
                        k)
                       k)))))

