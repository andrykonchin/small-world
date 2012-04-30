#lang racket

(provide vector-drop-nth drop-nth) 

(define (vector-drop-nth v n) 
  (vector-append (vector-take v n)
                 (vector-drop v (add1 n))))

(define (drop-nth v n) 
  (append (take v n) 
          (drop v (add1 n))))