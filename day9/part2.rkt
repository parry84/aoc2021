#lang racket

(require math/array)

(define input (map (λ (l) (map (λ (c) (string->number (string c))) l))
                   (map string->list (file->lines "input.txt"))))

(define rows (length input))
(define columns (length (cadr input)))
(define arr (list->array (vector rows columns) (apply append input)))

(define (sx   r c) (list (sub1 r) c))
(define (dx   r c) (list (add1 r) c))
(define (up   r c) (list r (sub1 c)))
(define (down r c) (list r (add1 c)))

(define (depth r c)
  (if (and (>= r 0) (<= r (sub1 rows)) (>= c 0) (<= c (sub1 columns)))
      (array-ref arr (vector r c))
      +inf.0))

(define (adjacents p)
  (define r (car p))
  (define c (cadr p))
  (list (sx r c) (dx r c) (up r c) (down r c)))

(define centers
  (filter-not empty?
              (for*/list ((r (range 0 rows))
                          (c (range 0 columns)))
                (define x (array-ref arr (vector r c)))
                (if (andmap
                     (λ (p) (< x (depth (car p) (cadr p))))
                     (adjacents (list r c)))
                    (list r c)
                    '()))))

(define (go center visited)
  (define as
       (filter (λ (p) (and (>= (car p) 0) (>= (cadr p) 0) (< (depth (car p) (cadr p)) 9)))
           (adjacents center)))
  (define new-as (filter-not (λ (p) (set-member? visited p)) as))
  (for-each (λ (p) (set-add! visited p)) as)
  (if (empty? new-as)
      (add1 (length as))
      (for/sum ((a new-as)) (go a visited))))

(apply * (take (sort (for/list ((center centers))
  (define visited (mutable-set center))
  (go center visited)
  (set-count visited)) >) 3))
