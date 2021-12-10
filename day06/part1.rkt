#lang racket

(require racket/file)

(define input (map string->number (string-split (car (file->lines "input.txt")) ",")))

(define test (list 3 4 3 1 2))

(define population input)
(define new-fishes 0)

(define (g) 
           (set! new-fishes (add1 new-fishes))
           6)

(define (f x) (
               if (= 0 x) (g)
                    (sub1 x)))

(for-each (λ (y)
   (set! new-fishes 0)
   (define old (map f population))
   (set! population (append old (make-list new-fishes 8)))
  ) (range 0 80))

(length population)
