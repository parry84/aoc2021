#lang racket

(require racket/file)

(define (update-edges h e)
  (define h* (hash-update h (car e) (λ (v) (append v (list (cadr e)))) (list)))
  (hash-update h* (cadr e) (λ (v) (append v (list (car e)))) (list)))

(define edges (foldl
               (λ (e acc) (update-edges acc e))
               (make-immutable-hash)
               (map (λ (l) (string-split l "-")) (file->lines "input.txt"))))

(define (small-cave? v) (equal? (string-downcase v) v))

(define (visit v visited)
  (if (equal? "end" v)
      1
      (for/sum ((v* (hash-ref edges v)) #:unless (set-member? visited v*))
        (visit v* (if (small-cave? v*) (set-add visited v*) visited)))))

(visit "start" (set "start"))