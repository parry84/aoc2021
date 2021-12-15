#lang racket

(require racket/file)

(define input (file->lines "input.txt"))
(define polymer (string->list (car (take input (index-of input "")))))
(define rules (make-immutable-hash (map
                                    (λ (rule) (match (string-split rule) ((list left "->" right) (cons left right))))
                                    (cdr (drop input (index-of input ""))))))

(define result (for/fold ([polymer* polymer])
                         ([i (in-range 0 10)])
                 (flatten
                  (cons
                   (list (car polymer*))
                   (for/list ([c0 (take polymer* (sub1 (length polymer*)))]
                              [c1 (drop polymer* 1)])
                     (list
                      (string->list (hash-ref rules (list->string (list c0 c1))))
                      c1))))))

(define (most-frequent-element xs)
  (define ht (make-hash))
  (for ([x xs]) (hash-update! ht x add1 0))
  (argmax cdr (hash->list ht)))

(define (least-frequent-element xs)
  (define ht (make-hash))
  (for ([x xs]) (hash-update! ht x add1 0))
  (argmin cdr (hash->list ht)))

(time (- (cdr (most-frequent-element result)) (cdr (least-frequent-element result))))
