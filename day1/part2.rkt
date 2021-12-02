#lang racket

(require racket/file)

(define input (map string->number (file->lines "input.txt")))

(define (prefix-sums xs)
  (define-values (sums sum)
    (for/fold ([sums '()] [sum 0]) ([x xs])
      (values (cons sum sums) (+ sum x))))
  (list->vector (reverse (cons sum sums))))

(define (sum3 xs i)
  (- (vector-ref xs (+ i 3))
     (vector-ref xs    i)))

(define (sum3s xs)
  (for/list ([i (- (vector-length xs) 2)])
    (sum3 (prefix-sums xs) i)))

(define windows (sum3s (list->vector input)))

(define zip (lambda (l1 l2) (map list l1 l2)))

(define pairs (zip windows (append (cdr windows) (list 0))))

(define increase? (lambda (p) (< (car p) (cadr p))))

(count increase? pairs)
