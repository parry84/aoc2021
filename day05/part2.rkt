#lang racket

(define (etl x)
  (list (map string->number (string-split (car x) ",")) (map string->number (string-split (third x) ","))))

(define input (map etl (map string-split (file->lines "input.txt"))))

(define (flatmap f lst)
  (apply append (map f lst)))

(define occurrences (make-hash))

(define (append-point x y) 
  (hash-update! occurrences (list x y) add1 0))

(define (expand-lines x)
  (match x
    [(list (list x1 y1) (list x2 y2))
     (for*/list ([i (range 0 (add1 (max (abs (- x1 x2)) (abs (- y1 y2)))))])
       (define x (cond
                   [(= x2 x1) x1]
                   [(> x2 x1) (+ x1 i)]
                   [(< x2 x1) (- x1 i)]))
       (define y (cond
                   [(= y2 y1) y1]
                   [(> y2 y1) (+ y1 i)]
                   [(< y2 y1) (- y1 i)]))
       (append-point x y))]))

(define _points (flatmap expand-lines input))

(for/sum ([occurrence (in-hash-values occurrences)] #:when (>= occurrence 2)) 1)
