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
    (field [points 5])
    (field [races '()])
    
    (set-field! player strategy this)
    
    (define/public (get-active-race)
      (findf race-active? races))
    
    (define/public (add-race! race)
      (set! races (append races (list race))))
    
    (define (pick-a-race)
      (let* ([race-index (send strategy pick-a-race)])
        (add-race! (send game take-race race-index))))
    
    (define/public (conquer)
      (for ([race races] #:when (send race can-conquer?))
        (for ([r (send strategy conquer race)])
          (let ([region (send (send game get-world) get-region r)])
            (send race conquer! region)))))
    
    (define (redeploy)
      #f)
    
    (define/public (play-turn)
      (when (not (get-active-race))
        (pick-a-race))
      (conquer)
      (redeploy))
    
    ))

(define (new-player name [strategy (new strategy%)])
  (new player% [name name] [strategy strategy]))
