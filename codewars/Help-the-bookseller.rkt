#lang racket

(provide stock-list)

(define ur1 '("BBAR 150" "CDXE 515" "BKWR 250" "BTSQ 890" "DRTY 600"))
;(define ur1 '())

(define vr1 '("A" "B" "C" "D"))
;(define vr5 '("B" "R" "D" "X"))


(define (stock-list list-of-books list-of-cat)
  (if (or (null? list-of-books) (null? list-of-cat))
      '()
      (let ([h (make-hash)])
        (for ([cat list-of-cat])
          (hash-set! h cat 0))
        (for ([book list-of-books])
          (when (hash-ref h (string (string-ref (list-ref (string-split book) 0) 0)) #f)
            (hash-update! h
                          (string (string-ref (list-ref (string-split book) 0) 0))
                          (lambda (val) (+ val (string->number (list-ref (string-split book) 1)))))))
        (for/list ([cat list-of-cat])
          (cons cat (hash-ref h cat))))))
