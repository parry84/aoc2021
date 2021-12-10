#lang racket

(define (is-ortogonal? x)
  (match x
    [(list (list x1 y1) (list x2 y2))
     (or (= x1 x2) (= y1 y2))]))

(define (etl x)
  (list (map string->number (string-split (car x) ",")) (map string->number (string-split (third x) ","))))

(define input (filter is-ortogonal? (map etl  (map string-split (file->lines "input.txt")))))

(define (flatmap f lst)
  (apply append (map f lst)))

(define test (list (list (list 1 2) (list 2 2)) (list (list 1 2) (list 2 2)) (list (list 4 5) (list 4 10))))

(define occurrences (make-hash))

(define (hl y x1 x2) 
  (for*/list ([x (range (min x1 x2) (add1 (max x1 x2)))])
    (hash-set! occurrences (list x y) 0)
    (append (list x y))))

(define (vl x y1 y2) 
  (for*/list ([y (range (min y1 y2) (add1 (max y1 y2)))])
    (hash-set! occurrences (list x y) 0)
    (append (list x y))))

(define (expand-lines x)
  (match x
    [(list (list x1 y1) (list x2 y2)) #:when (= x1 x2) (hl x1 y1 y2)]
    [(list (list x1 y1) (list x2 y2)) #:when (= y1 y2) (vl y1 x1 x2)]))

(define points (flatmap expand-lines input))

(for-each (λ (p)
            (hash-update! occurrences p add1)
            ) points) 

(define (is-dangerous? p) (> (hash-ref occurrences p) 1))

(count is-dangerous? (remove-duplicates points))

