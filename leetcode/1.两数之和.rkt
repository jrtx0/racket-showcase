#lang racket

(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define table (make-hash))
  (for/last ([i (in-naturals)]
        [num (in-list nums)]
        #:final (hash-has-key? table (- target num)))
    (when (not (hash-has-key? table num))
        (hash-set! table num i))
    (list (hash-ref table (- target num) '()) i))
  )


(two-sum (list 2 7 11 15) 9)
(two-sum (list 3 2 4) 6)
(two-sum (list 3 3) 6)