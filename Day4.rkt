#lang racket

(define input (file->lines "Data/Day4.txt"))
(define (parse_line l) (map (lambda (x) (map string->number (string-split x "-"))) (string-split l ",")))

(define (contains? a b) (let ([start_a (car a)]
                              [end_a (cadr a)]
                              [start_b (car b)]
                              [end_b (cadr b)])
                          (if (< start_a start_b )
                              (if (>= end_a end_b) #t #f)
                              (if (= start_a start_b) #t
                                  (if (>= end_b end_a) #t #f)))))

(define (count i l) (foldl + 0
                           (map (lambda b (if (car b) 1 0))
                                (filter (lambda a (if (eq? i (car a)) #t #f)) l))))

(define (part_one i) (count #t (map (lambda a (contains? (caar a) (cadar a))) i)))

(define (overlaps? a b) (let ([start_a (car a)]
                              [end_a (cadr a)]
                              [start_b (car b)]
                              [end_b (cadr b)])
                          (if (< start_a start_b)
                              (if (<= start_b end_a) #t #f)
                              (if (= start_a start_b) #t
                                  (if (<= start_a end_b) #t #f)))))

(define (part_two i) (count #t (map (lambda a (overlaps? (caar a) (cadar a))) i)))

(define output (let ([ip (map parse_line input)])
                 (print (string-append "Part one " (number->string (part_one ip))))
                 (print (string-append "Part two " (number->string (part_two ip))))))