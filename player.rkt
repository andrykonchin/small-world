#lang racket

(require rackunit)
(require "utils.rkt")
(require "maps.rkt")
(require "world.rkt")
(require "race.rkt")


(provide new-player
         player-name
         player-points
         player-races)

(define player%
  (class object%
    (super-new)
    
    (init-field name)
    (init-field strategy)
    (field [game #f])
    (field [points 5])
    (field [races '()])
    
    (define/public (get-active-race)
      (findf race-active? races))
    
    (define/public (add-race! race)
      (set! races (append races (list race))))
    
    (define (pick-a-race)
      (let* ([race-index (send strategy pick-a-race this)])
        (add-race! (send game take-race race-index))))
    
    (define/public (conquer)
      (for ([race races] #:when (race-can-conquer? race))
        (for ([r (send strategy conquer this race)])
          (let ([region (get-region (send game get-world) r)])
            (send race conquer! region)))))
    
    (define (redeploy)
      #f)
    
    (define/public (play-turn)
      (when (not (get-active-race))
        (pick-a-race))
      (conquer)
      (redeploy))
    
    ))

(define (new-player name strategy)
  (new player% [name name] [strategy strategy]))

(define player-name (class-field-accessor player% name))
(define player-strategy (class-field-accessor player% strategy))
(define player-points (class-field-accessor player% points))
(define player-races (class-field-accessor player% races))
