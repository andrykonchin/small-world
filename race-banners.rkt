#lang racket

(provide (all-defined-out))

(define (ghouls %)
  (class %
    (super-new)
    
    (define/override (initial-tokens) 
      (+ (super initial-tokens) 5))
    
    (define/override (can-conquer?)
      #t)
    
    (define/override (flip-tokens region)
      (void))
    ))