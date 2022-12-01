#lang racket

(define input (file->lines "Data/Day1.txt"))

(define (chunkInput_i i l ll) (
                               if (eq? i '())
                                  (if (eq? l '()) ll (cons l ll))
                                  (if (equal? "" (car i))
                                      (chunkInput_i (cdr i) '() (cons l ll))
                                      (chunkInput_i (cdr i) (cons (car i) l) ll))
                               ))

(define (chunkInput i) (chunkInput_i i '() '()))

(define (sum l) (foldl + 0 l))

(define (sum_l l) (sum (map string->number l)))

(define (sum_all_sublists l) (map sum_l l))

(define part_one (foldl max 0 (sum_all_sublists (chunkInput input))))

(define (last_ents_i x n sl l) (if (eq? l '())
                                sl
                                (if (= x n)
                                    (last_ents_i x n (append (cdr sl) (cons (car l) '())) (cdr l))
                                    (last_ents_i (+ x 1) n (append sl (cons (car l) '())) (cdr l))
                                    )))
                            

(define (last_ents n l) (last_ents_i 0 n '() l))

(define part_two (sum (last_ents 3 (sort (sum_all_sublists (chunkInput input)) <))))