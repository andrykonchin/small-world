#lang racket

(provide (all-defined-out))

(define (alchemist %)
  (class %
    (super-new)
    
    (define/override (initial-tokens) 
      (+ (super initial-tokens) 4))
    
    ))