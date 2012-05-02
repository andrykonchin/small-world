#lang racket

(require rackunit)
(require "world.rkt")

(provide new-race
         race-coins
         race-active?
         race-in-decline?
         race-can-conquer?
         all-race-banners
         all-special-powers)

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
      (let ([tokens-to-conquer (region-tokens-to-conquer region)])
        (when (can-conquer-region? region)
          (set! tokens-in-hand (- tokens-in-hand tokens-to-conquer))
          (region-occupy! region this tokens-to-conquer))))
    ))

(define (new-race special-power race-banner)
  (new race%))

(define race-coins (class-field-accessor race% coins))
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


;; Tests

(let ([r (new-race 'berserk 'amazons)])
  (check-equal? (race-coins r) 0)
  (check-false (race-in-decline? r)))

; can-conquer?
(let ([r (new-race 'berserk 'amazons)])
  (check-true (race-can-conquer? r))
  (send r decline!)
  (check-false (race-can-conquer? r)))

(let ([rg (new (ghouls race%))])
  (check-true (race-can-conquer? rg))
  (send rg decline!)
  (check-true (race-can-conquer? rg)))

; can-conquer-region?
(let ([race (new-race 'berserk 'amazons)]
      [region (new (class object% 
                     (super-new) 
                     (define/public (tokens-to-conquer) 5)))])
  (set-field! tokens-in-hand race 4)
  (check-false (send race can-conquer-region? region))
  (set-field! tokens-in-hand race 5)
  (check-true (send race can-conquer-region? region)))
