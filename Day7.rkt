#lang racket

(define input (file->lines "Data/Day7.txt"))

(define (sum x) (foldl + 0 x))
(define (transform-and-sum tx l) (sum (map tx l)))

(define (process-lines-i path i l) (if (eq? l '())
                                       (cons (cons path (cons i '())) '())
                                       (let ([line (car l)]
                                             [mr (regexp-match-positions #rx"^[$] (.*)$" (car l))])
                                         (if (eq? mr #f)
                                             (let ([fr (regexp-match-positions #rx"^([0-9]+) ([0-9a-zA-Z.]*)$" line)])
                                               (if (eq? fr #f)
                                                   (process-lines-i path i (cdr l))
                                                   (process-lines-i path (+ i (string->number (substring line (caadr fr) (cdadr fr)))) (cdr l))))
                                             (let ([dr (regexp-match-positions #rx"^cd (.*)$" (substring line (caadr mr) (cdadr mr)))])
                                               (if (eq? dr #f)
                                                   (process-lines-i path i (cdr l))
                                                   (let ([dir (substring (substring line (caadr mr) (cdadr mr) ) (caadr dr) (cdadr dr))])
                                                     (if (equal? dir "..")
                                                         (cons (cons path (cons i '())) (process-lines-i (drop-right path 1) 0 (cdr l)))
                                                         (cons (cons path (cons i '())) (process-lines-i (append path (cons dir '())) 0 (cdr l)))))))))))
(define (process-lines l) (process-lines-i '() 0 l))

(define (starts-with? l1 l2) (if (eq? l1 '())
                                 #t
                                 (if (eq? l2 '())
                                     #f
                                     (if (equal? (car l1) (car l2))
                                         (starts-with? (cdr l1) (cdr l2))
                                         #f))))

(define (filter-subtrees lr l) (filter (lambda (x) (if (starts-with? lr (car x)) #t #f)) l))


(define (choose-non-zero-i line l) (if (eq? l '()) line
                                       (if (> (cadar l) (cadr line))
                                           (choose-non-zero-i (car l) (cdr l))
                                           (choose-non-zero-i line (cdr l)))))
(define (choose-non-zero l) (choose-non-zero-i (car l) (cdr l)))

(define (filter-duplicates l) (if (eq? l '()) '()
                                  (cons (choose-non-zero (filter (lambda (x) (if (equal? (caar l) (car x)) #t #f)) l))
                                       (filter-duplicates (filter (lambda (x) (if (equal? (caar l) (car x)) #f #t)) l)))))

(define (add-subdir-totals l) (map (lambda (x) (cons (car x) (cons (cadr x) (cons (sum (map (lambda (y) (cadr y)) (filter-subtrees (car x) l))) '()))) )l))

(define (process-input l) (add-subdir-totals (filter-duplicates (process-lines l))))

(define (part-one ip) (sum  (map caddr (filter (lambda (x) (if (< (caddr x) 100000) #t #f)) (process-input ip)))))

(define (space-to-free l) (- 30000000 (- 70000000 (caddar (sort l (lambda (x y) (if (> (caddr x) (caddr y)) #t #f)))))))

(define (part-two ip) (let ([pip (process-input ip)]) (caddar (sort (filter (lambda (x) (if (> (caddr x) (space-to-free pip)) #t #f)) pip) (lambda (x y) (if (< (caddr x) (caddr y)) #t #f))))))
