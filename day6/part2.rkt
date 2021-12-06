#lang racket

(require racket/file)

(define initial-population
  (map string->number (string-split (car (file->lines "input.txt")) ",")))

(define occurrences (make-hash))
(for-each (λ (x) (hash-set! occurrences x 0)) (range 0 9))
(for-each (λ (x) (hash-update! occurrences x add1 0)) initial-population)

(define (f k v)
  (cond ((= 0 k)
         (hash-update! occurrences 6 (λ (x) (+ x v)))
         (hash-update! occurrences 8 (λ (x) (+ x v))))
        (else 
         (hash-update! occurrences (sub1 k) (λ (x) (+ x v)))
         )))

(for-each (λ (day)
            (define occurrences-c (hash-copy occurrences))
            (for-each (λ (x) (hash-set! occurrences x 0)) (range 0 9))
            (hash-for-each occurrences-c f)
            ) (range 0 256))

(apply + (hash-values occurrences))
