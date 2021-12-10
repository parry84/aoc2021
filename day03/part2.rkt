#lang racket

(require racket/file)

(define input (map string->list (file->lines "input.txt")))

(define (criteria nums pos op)
  (define (occurrences x) (count (λ (e) (eq? x (list-ref e pos))) nums))
  (define zeroes (occurrences #\0))
  (define ones   (occurrences #\1))
  (cond
    [(eq? 'mc1 op) (if (>= ones zeroes) #\1 #\0)]
    [(eq? 'lc0 op) (if (<= zeroes ones) #\0 #\1)]
    [else (error "wrong criteria")]))

(define (go pos nums op)
  (if (eq? 1 (length nums))
      (car nums)
      (go (+ 1 pos)
          (filter (λ (e) (eq? (criteria nums pos op) (list-ref e pos))) nums)
          op)))

(define (bin->dec n)
  (foldl (λ (x acc) (+ (* 2 acc) (string->number (string x)))) 0 n))

(define oxygen-generator-rating (bin->dec (go 0 input 'mc1)))
(define co2-scrubber-rating     (bin->dec (go 0 input 'lc0)))

(* co2-scrubber-rating oxygen-generator-rating)