#lang racket

(provide vector-drop-nth) 

(define (vector-drop-nth v n) 
  (vector-append (vector-take v n)
                 (vector-drop v (add1 n))))