#lang racket

(require racket/file)

(define real-input (car (file->lines "input.txt")))
(define test "16,1,2,0,4,2,7,1,2,14")
(define input (map string->number (string-split real-input ",")))

(define (max1 xs)
  (define (max x1 x2)
    (if (> x1 x2) x1 x2))
  (foldl max (first xs) (rest xs)))

(define (min1 xs)
  (define (max x1 x2)
    (if (< x1 x2) x1 x2))
  (foldl min (first xs) (rest xs)))

(define (fuel-rule x) (floor (/ (* x (add1 x)) 2)))

(min1 (for/list ((position (range (min1 input) (add1 (max1 input)))))
  (for/sum ((j input)) (fuel-rule (abs (- position j))))))
