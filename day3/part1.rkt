#lang racket

(require racket/file)

(define input (map string->list (file->lines "input.txt")))

(define ones (make-vector 12))
(vector-fill! ones 0)
(define zeroes (make-vector 12))
(vector-fill! zeroes 0)

(define (f x)
  (for-each (lambda (pos) (vector-set! zeroes pos (+ 1 (vector-ref zeroes pos)))) (indexes-of x #\0))
  (for-each (lambda (pos) (vector-set! ones pos (+ 1 (vector-ref ones pos)))) (indexes-of x #\1)))

(for-each f input)

(define (bin->dec n)
 (foldl (lambda (x acc) (+ (* 2 acc) x)) 0 n))

(define gamma (bin->dec (map (lambda (i)
         (if (< (vector-ref zeroes i) (vector-ref ones i)) 1 0))
       (range 0 12))))

(define epsilon (bin->dec (map (lambda (i)
         (if (> (vector-ref zeroes i) (vector-ref ones i)) 1 0))
       (range 0 12))))

(* gamma epsilon)

