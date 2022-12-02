#lang racket

(define input (file->lines "Data/Day2.txt"))

(define (decode s) (cons (string-ref s 0) (string-ref s 2)))
(define (decode-all l) (map decode l))

(define (shape_score s) (case s
                          [(#\X) 1]
                          [(#\Y) 2]
                          [(#\Z) 3]))

(define (win_loss_score a b) (case a
                         [(#\A) (case b
                                  [(#\X) 3]
                                  [(#\Y) 6]
                                  [(#\Z) 0])]
                         [(#\B) (case b
                                  [(#\X) 0]
                                  [(#\Y) 3]
                                  [(#\Z) 6])]
                         [(#\C) (case b
                                  [(#\X) 6]
                                  [(#\Y) 0]
                                  [(#\Z) 3])]))

(define (sum x) (foldl + 0 x))
(define (transform_and_sum tx l) (sum (map tx l)))

(define (part_one i) (transform_and_sum (lambda (x) (+ (shape_score (cdr x)) (win_loss_score (car x) (cdr x)))) i ))

(define (intended_outcome a b) (case a
                         [(#\A) (case b
                                  [(#\X) #\Z]
                                  [(#\Y) #\X]
                                  [(#\Z) #\Y])]
                         [(#\B) (case b
                                  [(#\X) #\X]
                                  [(#\Y) #\Y]
                                  [(#\Z) #\Z])]
                         [(#\C) (case b
                                  [(#\X) #\Y]
                                  [(#\Y) #\Z]
                                  [(#\Z) #\X])]))

(define (part_two i) (transform_and_sum (lambda (x) (let ([fst (car x)] [snd (cdr x)])
                                                      (+ (shape_score (intended_outcome fst snd))
                                                         (win_loss_score fst (intended_outcome fst snd))))) i ))

(define output (let ([ip (decode-all input)])
                 (print (string-append "Part one " (number->string (part_one ip))))
                 (print (string-append "Part two " (number->string (part_two ip))))))