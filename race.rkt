#lang racket

(require rackunit)
(require "world.rkt")
(require "region.rkt")

(provide race%
         new-race
         race-active?
         race-in-decline?
         race-can-conquer?
         all-race-banners
         all-special-powers
         ghouls)

(define race%
  (class object%
    (super-new)
    (field [coins 0])
    (field [in-decline #f])
    (field [tokens-in-hand 0])
    
    (define/public (can-conquer?)
      (not in-decline))
    
    (define/public (decline!)
      (set! in-decline #t))
    
    (define/public (withdraw! token-count)
      (check >= token-count 1)
      (set! tokens-in-hand (+ tokens-in-hand token-count -1))
      (not in-decline))
    
    (define/public (can-conquer-region? region)
      (>= tokens-in-hand 
          (send region tokens-to-conquer)))
    
    (define/public (conquer! region)
      (let ([tokens-to-conquer (send region tokens-to-conquer)])
        (when (can-conquer-region? region)
          (set! tokens-in-hand (- tokens-in-hand tokens-to-conquer))
          (send region occupy! this tokens-to-conquer))))
    ))

(define (new-race special-power race-banner)
  (new race%))

(define race-in-decline? (class-field-accessor race% in-decline))
(define race-active? (negate race-in-decline?))

(define (race-can-conquer? race)
  (send race can-conquer?))

(define all-race-banners
  '(amazons dwarves elves ghouls giants
            halflings humans orcs ratmen skeletons
            sourcerers tritons trolls wizards))

(define all-special-powers
  '(alchemist berserk bivouacking commando diplomat
              dragon-master flying forest fortified heroic
              hill merchant mounted pillaging seafaring
              spirit stout swamp underworld wealthy))

(define (ghouls %)
  (class %
    (super-new)
    (define/override (can-conquer?)
      #t)
    ))
