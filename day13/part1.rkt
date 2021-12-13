#lang racket

(require racket/file)

(define input (file->lines "input.txt"))

(define grid
  (list->set (values (map (λ (l) (map string->number (string-split l ","))) (take input (index-of input ""))))))

(define folds
  (map (λ (l) (string-split (last (string-split l)) "=")) (cdr (drop input (index-of input "")))))

(define (fold-f n dist)
  (if (< n dist)
      n
      (- (* 2 dist) n)))

(define axis (car (first folds)))
(define dist (string->number (cadr (first folds))))

(time (set-count
 (list->set
  (set-map grid
           (λ (p)
             (define x (car p))
             (define y (cadr p))
             (if (equal? "x" axis)
                 (list (fold-f x dist) y)
                 (list x (fold-f y dist))))))))
