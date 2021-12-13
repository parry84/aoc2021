#lang racket

(require racket/file)

(define input (file->lines "input.txt"))

(define grid
  (list->set (values (map (λ (l) (map string->number (string-split l ","))) (take input (index-of input ""))))))

(define folds
  (map (λ (l) (string-split (last (string-split l)) "=")) (cdr (drop input (index-of input "")))))

(define (fold-point n dist)
  (if (< n dist)
      n
      (- (* 2 dist) n)))

(define (display-grid grid)
  (define max-y (apply max (map car (set->list grid))))
  (define max-x (apply max (map cadr (set->list grid))))
  (display
   (list->string
    (flatten
     (for*/list ([x (inclusive-range 0 max-x)]
                 [y (inclusive-range 0 max-y)])
       (define fill (if (set-member? grid (list y x)) #\█ #\.))
       (if (= y max-y) (list fill #\newline) fill))))))

(time
 (display-grid
  (for/fold ([acc grid])
            ([fold* (in-set folds)])
    (list->set
     (set-map acc
              (λ (p)
                (match p
                  [(list x y)
                   (match fold*
                     [(list "x" dist) (list (fold-point x (string->number dist)) y)]
                     [(list "y" dist) (list x (fold-point y (string->number dist)))])])))))))                
