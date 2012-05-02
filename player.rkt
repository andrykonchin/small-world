#lang racket

(require "race.rkt")
(require "strategy.rkt")


(provide new-player)

(define player%
  (class object%
    (super-new)
    
    (init-field name)
    (init-field strategy)
    (field [game #f])
    (field [world #f])
    (field [coins 5])
    (field [races '()])
    
    (set-field! player strategy this)
    
    (define (add-points! x)
      (set! coins (+ coins x)))
    
    (define/public (join-game! g)
      (set! game g)
      (set! world (get-field world g)))
    
    (define/public (get-active-race)
      (findf race-active? races))
    
    (define/public (add-race! race)
      (add-points! (send race take-coins!))
      (set! races (append races (list race))))
    
    (define/public (pick-a-race!)
      (let* ([race-index (send strategy pick-a-race)])
        (add-points! (- race-index))
        (add-race! (send game take-race! race-index))))
    
    (define/public (conquer)
      (for ([race races] #:when (send race can-conquer?))
        (for ([r (send strategy conquer race)])
          (let ([region (send world get-region r)])
            (send race conquer! region)))))
    
    (define (redeploy)
      #f)
    
    (define/public (play-turn)
      (when (not (get-active-race))
        (pick-a-race!))
      (conquer)
      (redeploy))
    
    ))

(define (new-player name [strategy (new strategy%)])
  (new player% [name name] [strategy strategy]))
