#lang racket

(require "world.rkt")
(require "region.rkt")

(provide race%
         new-race
         race-active?
         all-race-banners
         all-special-powers
         ghouls)

(define race%
  (class object%
    (super-new)
    (field [coins 0])
    (field [in-decline #f])
    (field [tokens-in-hand 0])
    (field [occupied-regions '()])
    
    (define/public (add-coin!)
      (set! coins (add1 coins)))
    
    (define/public (take-coins!)
      (let ([coins-to-take coins])
        (set! coins 0)
        coins-to-take))
    
    (define/public (can-conquer?)
      (not in-decline))
    
    (define/public (decline!)
      (set! in-decline #t))
    
    (define/public (withdraw! region)
      (set! occupied-regions (remove region occupied-regions))
      (set! tokens-in-hand
            (+ tokens-in-hand (get-field occupant-count region) -1)))
    
    (define/public (can-conquer-region? region)
      (>= tokens-in-hand 
          (send region tokens-to-conquer)))
    
    (define/public (conquer! region)
      (let ([tokens-to-conquer (send region tokens-to-conquer)])
        (when (can-conquer-region? region)
          (set! tokens-in-hand (- tokens-in-hand tokens-to-conquer))
          (send region occupy! this tokens-to-conquer)
          (set! occupied-regions (cons region occupied-regions)))))
    
    (define/public (score-coins)
      (for/sum ([region occupied-regions])
               (score-coins-for-region region)))

    (define/public (score-coins-for-region region)
      1)
    ))

(define (new-race special-power race-banner)
  (new race%))

(define race-in-decline? (class-field-accessor race% in-decline))
(define race-active? (negate race-in-decline?))

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
