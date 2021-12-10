#lang racket

(require math/array)

(define input (map (λ (l) (map (λ (c) (string->number (string c))) l))
                   (map string->list (file->lines "input.txt"))))

(define rows (length input))
(define columns (length (cadr input)))
(define arr (list->array (vector rows columns) (apply append input)))

(define (sx r c)
  (if (= r 0)
      +inf.0
      (array-ref arr (vector (sub1 r) c))))

(define (dx r c)
  (if (= r (sub1 rows))
      +inf.0
      (array-ref arr (vector (add1 r) c))))

(define (up r c)
  (if (= c 0)
      +inf.0
      (array-ref arr (vector r (sub1 c)))))

(define (down r c)
  (if (= c (sub1 columns))
      +inf.0
      (array-ref arr (vector r (add1 c)))))

(for*/sum ((r (range 0 rows))
           (c (range 0 columns)))
  (define x (array-ref arr (vector r c)))
  (if (and (< x (sx r c)) (< x (dx r c)) (< x (up r c)) (< x (down r c)))
      (add1 x)
      0))

