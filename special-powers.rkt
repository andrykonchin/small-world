#lang racket

(provide (all-defined-out))


(define (alchemist %)
  (class %
    (super-new)
    
    (define/override (initial-tokens) 
      (+ (super initial-tokens) 4))
    
    ))


(define (berserk %)
  (class %
    (super-new)
    ))


(define (bivouacking %)
  (class %
    (super-new)
    ))


(define (commando %)
  (class %
    (super-new)
    ))


(define (diplomat %)
  (class %
    (super-new)
    ))


(define (dragon-master %)
  (class %
    (super-new)
    ))


(define (flying %)
  (class %
    (super-new)
    ))


(define (forest %)
  (class %
    (super-new)
    ))


(define (fortified %)
  (class %
    (super-new)
    ))


(define (heroic %)
  (class %
    (super-new)
    ))


(define (hill %)
  (class %
    (super-new)
    ))


(define (merchant %)
  (class %
    (super-new)
    ))


(define (mounted %)
  (class %
    (super-new)
    ))


(define (pillaging %)
  (class %
    (super-new)
    ))


(define (seafaring %)
  (class %
    (super-new)
    ))


(define (spirit %)
  (class %
    (super-new)
    ))


(define (stout %)
  (class %
    (super-new)
    ))


(define (swamp %)
  (class %
    (super-new)
    ))


(define (underworld %)
  (class %
    (super-new)
    ))


(define (wealthy %)
  (class %
    (super-new)
    ))


(define all-special-powers
  (list alchemist berserk bivouacking commando diplomat
        dragon-master flying forest fortified heroic
        hill merchant mounted pillaging seafaring
        spirit stout swamp underworld wealthy))
