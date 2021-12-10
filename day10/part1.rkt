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

(apply + (flatten (for/list ((line input))
                    (define s (make-stack))
                    (filter (λ (x) (number? x))
                            (for/list ((character line))
                              (if (member character open-brackets)
                                  (s 'push! character)
                                  (when (not (validate (s 'pop!) character))
                                    (cond ((eq? character #\)) 3)
                                          ((eq? character #\]) 57)
                                          ((eq? character #\}) 1197)
                                          ((eq? character #\>) 25137)))))))))

