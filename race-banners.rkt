#lang racket

(provide (all-defined-out))


(define (amazons %)
  (class %
    (super-new)
    ))


(define (dwarves %)
  (class %
    (super-new)
    ))


(define (elves %)
  (class %
    (super-new)
    ))


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


(define (giants %)
  (class %
    (super-new)
    ))


(define (halflings %)
  (class %
    (super-new)
    ))


(define (humans %)
  (class %
    (super-new)
    ))


(define (orcs %)
  (class %
    (super-new)
    ))


(define (ratmen %)
  (class %
    (super-new)
    ))


(define (skeletons %)
  (class %
    (super-new)
    ))


(define (sorcerers %)
  (class %
    (super-new)
    ))


(define (tritons %)
  (class %
    (super-new)
    ))


(define (trolls %)
  (class %
    (super-new)
    ))


(define (wizards %)
  (class %
    (super-new)
    ))


(define all-race-banners
  (list amazons dwarves elves ghouls giants
        halflings humans orcs ratmen skeletons
        sorcerers tritons trolls wizards))
