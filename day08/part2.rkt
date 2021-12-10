#lang racket

(require racket/file)
(require racket/set)

(define displays (map (λ (l) (string-split l " | "))  (file->lines "input.txt")))

(define wires (string->list "abcdefg"))
(define digits (make-immutable-hash (map cons (map string->list
                                                   (list "abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"))
                                         (range 0 10))))
(define (read-digit segments) (hash-ref digits segments))
(define (read-digits digits) (foldl (λ (d acc) (+ d (* 10 acc))) 0 digits))
(define wirings (map make-immutable-hash (map (λ (p) (map cons wires p)) (permutations wires))))
(define (rewire w x) (sort (map (λ (x) (hash-ref w x)) (string->list x)) char<?))

(define (all p xs) (foldr (λ (x acc) (and acc (p x))) #t xs))

(for/sum ((d displays) )
  (define inputs  (string-split (car d)))
  (define outputs (string-split (cadr d)))

  (define rewiring (findf (λ (w)
                            (all (λ (y) (hash-has-key? digits y))
                                 (for/list ((i inputs)) (rewire w i))))
                          wirings))

  (read-digits (map (λ (x) (read-digit (rewire rewiring x))) outputs)))  
