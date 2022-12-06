#lang racket

(define input (file->lines "Data/Day6.txt"))

(define (scan-for-non-repeated-block-i g i l) (if (check-duplicates g)
                                                  (scan-for-non-repeated-block-i
                                                   (append (cdr g) (list (car l))) (+ i 1) (cdr l))
                                                  i))

(define (scan-for-non-repeated-block i l) (scan-for-non-repeated-block-i (take l i) i (drop l i)))

(define (part-one i) (scan-for-non-repeated-block 4 (string->list (car i))))

(define (part-two i) (scan-for-non-repeated-block 14 (string->list (car i))))