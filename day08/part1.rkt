#lang racket

(require racket/file)

(define input (file->lines "input.txt"))

(length (filter (λ (x) (member x (list 2 3 4 7)))
                (for*/list ((line input)
                            (j (string-split (cadr (string-split line "|")))))
                  (string-length j) )))
