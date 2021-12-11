#lang racket

(require racket/file)

(define input (file->lines "input.txt"))

(define grid (make-immutable-hash (append*
                                   (for/list ((r input)
                                              (i (in-naturals)))
                                     (for/list ((c (string->list r))
                                                (j (in-naturals)))
                                       (cons (cons i j) (string->number (string c))))))))

(define rs (length input))
(define cs (string-length (car input)))

(define (neighbors p)
  (define r (car p))
  (define c (cdr p))
  (for*/list ([row* (in-inclusive-range (sub1 r) (add1 r))]
              [col* (in-inclusive-range (sub1 c) (add1 c))]
              #:when (and (>= row* 0) (>= col* 0) (< row* rs) (< col* cs))
              #:unless (and (= r row*) (= c col*)))
    (cons row* col*)))

(define (flash p grid)
  (define energy (hash-ref grid p))
  (cond [(>= energy 9)
         (foldl flash (hash-set grid p 0) (neighbors p))]
        [(>= energy 1)
         (hash-set grid p (add1 energy))]
        [else grid]))

(define (step grid)
  (define grid-plus1 (make-immutable-hash (hash-map grid (λ (k v) (cons k (add1 v))))))
  (define flashing (for/fold ([acc '()])
                             ([(k v) (in-hash grid)]
                              #:when (= v 9))
                     (append acc (list k))))
  (foldl flash grid-plus1 flashing))

(for/fold ([grid* grid]
           [s 0]
           #:result s)
          ([i (in-naturals)]
           #:final (= (* cs rs) (count (λ (v) (= 0 v)) (hash-values grid*))))
  (values (step grid*) i))

