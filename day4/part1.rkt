#lang racket

(require racket/file)

(define (split-by lst n)
  (if (not (empty? lst))
      (cons (take lst n) (split-by (drop lst n) n))
      '() ))

(define (map-inside f l) (map (λ (x) (map f x)) l))
(define (list-to-nums l) (map string->number l))

(define game (call-with-input-file  "input.txt"
               (λ (input)
                 (define nums (map string->number (string-split (read-line input) ",")))
                 (define rows (let loop
                                ((l (read-line input))
                                 (r '()))
                                (if (eof-object? l)
                                    (reverse r)
                                    (loop (read-line input) (cons l r)))))
                 (list nums (split-by
                             (filter-not empty?
                                         (map list-to-nums
                                              (map string-split
                                                   (filter-not eof-object? rows)))) 5)))))

(define nums (car game))
(define boards (cadr game))
(define boards-count (length boards))
(define boards-vector (list->vector (map list->vector (map-inside list->vector boards))))

(define rows-sums (make-hash))
(define columns-sums (make-hash))
(for* ([i (range 0 boards-count)]
      [j (range 0 5)])
    (hash-set! rows-sums (list i j) 0)
    (hash-set! columns-sums (list i j) 0))

(define unmarked (list->vector (map append* boards)))

(define (sum elemList)
  (if
   (null? elemList)
   0
   (+ (car elemList) (sum (cdr elemList)))
   )
  )

(define (vector-update! vec pos f) (vector-set! vec pos (f (vector-ref vec pos))))

(for-each (λ (num)
            (for-each (λ (b)
                        (for-each (λ (r)
                                    (define marked-column? (vector-member num (vector-ref (vector-ref boards-vector b) r)))
                                    (when marked-column?
                                      (vector-update! unmarked b (λ (x) (remove num x)))
                                      
                                      (hash-update! rows-sums (list b r) add1) 
                                      (when (= 5 (hash-ref rows-sums (list b r)))
                                        (display (* num (sum (vector-ref unmarked b))))
                                        (exit))
                                      
                                      (hash-update! columns-sums (list b marked-column?) add1) 
                                      (when (= 5 (hash-ref columns-sums (list b marked-column?)))
                                        (display (* num (sum (vector-ref unmarked b))))
                                        (exit))
                                      '()
                                      )
                                    ) (range 0 5))
                        ) (range 0 boards-count))
            ) nums)