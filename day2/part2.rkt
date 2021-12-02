#lang racket

(require racket/file)

(define input (map string-split (file->lines "input.txt")))

(define aim 0)
(define depth 0)
(define position 0)

(define f (lambda (e) (match e
    [(list "forward" x) (set! position (+ position (string->number x))) (set! depth (+ depth (* aim (string->number x))))]
    [(list "up" x) (set! aim (- aim (string->number x)))]
    [(list "down" x) (set! aim (+ aim (string->number x)))])))

(for-each f input)

(display (* depth position))
