#lang racket

(require rackunit)

(provide new-race 
         race-race-banner
         race-special-power
         race-coins
         race-in-decline
         set-race-in-decline!
         all-race-banners
         all-special-powers)

(define race%
  (class object%
    (super-new)
    (init-field race-banner)
    (init-field special-power)
    (field [coins 0])
    (field [in-decline #f])
    ))

(define (new-race race-banner special-power)
  (new race% [race-banner race-banner] [special-power special-power]))

(define race-race-banner (class-field-accessor race% race-banner))
(define race-special-power (class-field-accessor race% special-power))
(define race-coins (class-field-accessor race% coins))
(define race-in-decline (class-field-accessor race% in-decline))
(define set-race-in-decline! (class-field-mutator race% in-decline))

(define all-race-banners
  '(amazons dwarves elves ghouls giants
            halflings humans orcs ratmen skeletons
            sourcerers tritons trolls wizards))

(define all-special-powers
  '(alchemist berserk bivouacking commando diplomat
              dragon-master flying forest fortified heroic
              hill merchant mounted pillaging seafaring
              spirit stout swamp underworld wealthy))
