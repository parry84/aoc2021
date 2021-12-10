#lang racket

(require racket/file)

(define input (map string->list (file->lines "input.txt")))

(define (make-stack)
  (let ((stack '()))
    (lambda (msg . args)
      (cond 
        [(eq? msg 'pop!)
         (define top (car stack))
         (set! stack (cdr stack))
         top]
        [(eq? msg 'push!) (set! stack (append (reverse args) stack))]
        [(eq? msg 'stack) stack]
        [else "Invalid operation!"]))))

(define open-brackets (list #\[ #\( #\{ #\<))

(define (validate open closed)
  (or (and (eq? open #\[) (eq? closed #\]))
      (and (eq? open #\() (eq? closed #\)))
      (and (eq? open #\{) (eq? closed #\}))
      (and (eq? open #\<) (eq? closed #\>))))

(define (complete l)
  (foldl (λ (character score)
           (+ (* score 5)
              (cond ((eq? character #\() 1)
                    ((eq? character #\[) 2)
                    ((eq? character #\{) 3)
                    ((eq? character #\<) 4)
                    (else 0)))) 0 l))
  

(define scores
  (sort
   (map complete
        (filter list?
                (for/list ((line input))
                  (define s (make-stack))
                  (when (andmap
                         (λ (character) (if (member character open-brackets)
                                                 (s 'push! character)
                                                 (when (not (validate (s 'pop!) character))
                                                   #f)
                                                 )) line ) (s 'stack))))) <))

(list-ref scores (floor (/ (length scores) 2)))
