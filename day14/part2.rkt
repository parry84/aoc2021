#lang racket

(require racket/file)

(define input (file->lines "input.txt"))
(define polymer-s (string->list (car (take input (index-of input "")))))
(define rules (make-immutable-hash (map
                                    (λ (rule) (match (string-split rule)
                                                ((list left "->" right) (cons left right))))
                                    (cdr (drop input (index-of input ""))))))

(define polymer
  (foldl
   (λ (e acc) (hash-update acc e add1 0))
   (make-immutable-hash)
   (for/list ([c0 polymer-s]
              [c1 (cdr polymer-s)])
     (list c0 c1))))

(define result
  (for/fold ([polymer* polymer])
        ([i (in-range 0 40)])
          (foldl (λ (k acc)
                   (define cr (string->list (hash-ref rules (list->string k))))    
                   (define pair1 (hash-update acc (flatten (list (car k) cr)) (λ (v) (+ v (hash-ref polymer* k))) 0))
                   (hash-update pair1 (flatten (list cr (cdr k))) (λ (v) (+ v (hash-ref polymer* k))) 0))
                 (make-immutable-hash)
                 (hash-keys polymer*))))

(define counts
  (foldl
   (λ (k acc) (hash-update acc (cadr k) (λ (v) (+ v (hash-ref result k))) 0))
   (make-immutable-hash)
   (hash-keys result)))

(define most-frequent-element (apply max (hash-values counts)))
(define least-frequent-element (apply min (hash-values counts)))

(- most-frequent-element least-frequent-element)
