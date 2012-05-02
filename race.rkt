#lang racket

(require rackunit)
(require "world.rkt")

(provide new-race 
         race-race-banner
         race-special-power
         race-coins
         race-active?
         race-in-decline?
         race-can-conquer?
         race-decline!
         race-withdraw!
         all-race-banners
         all-special-powers)

(define race%
  (class* object% (writable<%>)
    (super-new)
    (init-field race-banner)
    (init-field special-power)
    (field [coins 0])
    (field [in-decline #f])
    (field [tokens-in-hand 0])
    
    (define/public (custom-write port)
      (write (list special-power race-banner) port))
    
    (define/public (custom-display port)
      (display (list special-power race-banner) port))
    
    (define/public (can-conquer?)
      (not in-decline))
    
    (define/public (withdraw! token-count)
      (check >= token-count 1)
      (set! tokens-in-hand (+ tokens-in-hand token-count -1))
      (not in-decline))
    
    (define/public (conquer! region)
      (let ([tokens-to-conquer (region-tokens-to-conquer region)])
        (when (region-occupant-race region)
          (race-withdraw! (region-occupant-race region)
                          (region-occupant-count region)))
        (set! tokens-in-hand (- tokens-in-hand tokens-to-conquer))
        (region-occupy! region this tokens-to-conquer)))
    ))

(define (new-race special-power race-banner)
  (new race% [race-banner race-banner] [special-power special-power]))

(define race-race-banner (class-field-accessor race% race-banner))
(define race-special-power (class-field-accessor race% special-power))
(define race-coins (class-field-accessor race% coins))
(define race-in-decline? (class-field-accessor race% in-decline))
(define race-active? (negate race-in-decline?))

(define (race-can-conquer? race)
  (send race can-conquer?))

(define (race-decline! race)
  (set-field! in-decline race #t))

(define (race-withdraw! race token-count)
  (send race withdraw! token-count))

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
  (race-decline! r)
  (check-false (race-can-conquer? r)))

(let ([rg (new (ghouls race%) [race-banner #f] [special-power #f])])
  (check-true (race-can-conquer? rg))
  (race-decline! rg)
  (check-true (race-can-conquer? rg)))
