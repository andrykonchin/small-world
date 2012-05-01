#lang racket

(require rackunit)

(provide remove-nth) 

(define (remove-nth lst n) 
  (append (take lst n) 
          (drop lst (add1 n))))


;; Test

(check-equal? (remove-nth '(0 1 2 3) 0) '(1 2 3))
(check-equal? (remove-nth '(0 1 2 3) 1) '(0 2 3))
(check-equal? (remove-nth '(0 1 2 3) 2) '(0 1 3))
(check-equal? (remove-nth '(0 1 2 3) 3) '(0 1 2))
