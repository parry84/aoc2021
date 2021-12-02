#lang racket

(require racket/file)

(define input (map string->number (file->lines "input.txt")))

(define zip (lambda (l1 l2) (map list l1 l2)))

(define pairs (zip input (append (cdr input) (list 0))))

(define increase? (lambda (p) (< (car p) (cadr p))))

(count increase? pairs)

