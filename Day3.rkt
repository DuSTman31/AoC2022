#lang racket

(define input (file->lines "Data/Day3.txt"))

(define (sum x) (foldl + 0 x))
(define (transform_and_sum tx l) (sum (map tx l)))

(define (split_in_two s) (cons (substring s 0 (/ (string-length s) 2)) (substring s (/ (string-length s) 2) (string-length s))))
(define (split_all_in_two l) (map split_in_two l))
(define (is_char_in_string c s) (foldl (lambda (a b) (if a #t b)) #f (map (lambda (a) (eq? a c)) s)))
(define (dedup_i p l) (if (eq? l '()) (cons p '())
                          (if (eq? p (car l))
                              (dedup_i p (cdr l))
                              (cons p (dedup_i (car l) (cdr l))))))
(define (dedup l) (dedup_i (car l) (cdr l)))
(define (intersection_of_strings s1 s2) (dedup (sort (filter (lambda (c) (is_char_in_string c s2)) s1) (lambda (a b) (< (char->integer a) (char->integer b))))))
(define (find_intersections l) (map (lambda (a) (intersection_of_strings (string->list (car a)) (string->list (cdr a)))) l))
(define (char_to_priority c) (let ([cc (char->integer c)]) (if (>= cc 97) (+ (- cc 97) 1) (if (>= cc 65) (+ (- cc 65) 27) c))))


(define (part_one i) (transform_and_sum (lambda (a) (char_to_priority (car a))) (find_intersections (split_all_in_two i))))

(define (intersection_of_three_strings a b c) (intersection_of_strings (string->list a) (intersection_of_strings (string->list b) (string->list c))))

(define (group_into_threes_i n g l) (if (eq? l '())
                                        (cons g '())
                                        (if (= n 2)
                                            (cons g (group_into_threes_i 0 (list (car l)) (cdr l)))
                                            (group_into_threes_i (+ 1 n) (append (list (car l)) g) (cdr l))
                                            )))
(define (group_into_threes l) (group_into_threes_i  0 (list (car l)) (cdr l)))

(define (part_two i) (transform_and_sum (lambda (a) (char_to_priority (car a)))
                                        (map (lambda (a) (intersection_of_three_strings (car a) (cadr a) (caddr a)))
                                             (group_into_threes i))))

(define output (let ([ip input])
                 (print (string-append "Part one " (number->string (part_one ip))))
                 (print (string-append "Part two " (number->string (part_two ip))))))