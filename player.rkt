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
    
    (define (add-coins! x)
      (set! coins (+ coins x)))
    
    (define/public (join-game! g)
      (set! game g)
      (set! world (get-field world g)))
    
    (define/public (get-active-race)
      (findf race-active? races))
    
    (define/public (add-race! race)
      (add-coins! (send race take-coins!))
      (set! races (append races (list race))))
    
    (define/public (pick-a-race!)
      (let* ([race-index (send strategy pick-a-race)])
        (add-coins! (- race-index))
        (add-race! (send game take-race! race-index))))
    
    (define/public (ready-troops!)
      (for ([race races] #:when (send race can-conquer?))
        (for ([pair (send strategy ready-troops race)])
          (let ([region (send world get-region (car pair))]
                [token-count (cdr pair)])
            (send race ready-troops! region token-count)))))
    
    (define/public (conquer!)
      (for ([race races] #:when (send race can-conquer?))
        (for ([r (send strategy conquer race)])
          (let ([region (send world get-region r)])
            (send race conquer! region)))))
    
    (define/public (redeploy!)
      #f)
    
    (define/public (score-victory-coins!)
      (for ([race races])
        (add-coins! (send race score-coins))))
    
    (define/public (play-turn!)
      (let ([active-race (get-active-race)])
        (if (and active-race
                 (send strategy go-into-decline? active-race))
            (send active-race decline!)
            (begin
              (if (not active-race)
                  (pick-a-race!)
                  (ready-troops!))
              (conquer!)
              (redeploy!))))
      (score-victory-coins!))
    ))

(define (new-player name [strategy (new strategy%)])
  (new player% [name name] [strategy strategy]))
