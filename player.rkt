#lang racket

(require rackunit)
(require "utils.rkt")
(require "maps.rkt")
(require "world.rkt")
(require "race.rkt")


(provide new-player
         player-name
         player-points
         player-races
         new-strategy)

(define player%
  (class object%
    (super-new)
    
    (init-field name)
    (init-field strategy)
    (field [game #f])
    (field [points 5])
    (field [races '()])
    
    (define/public (get-active-race)
      (findf active? races))
    
    (define/public (add-race! race)
      (set! races (append races (list race))))
    
    (define (pick-a-race)
      (let* ([race-index ((strategy-pick-a-race strategy) this)])
        (add-race! (send game take-race race-index))))
    
    (define/public (conquer)
      (for ([race races] #:when (can-conquer? race))
        (for ([r ((strategy-conquer strategy) this race)])
          (let* ([region (get-region (send game get-world) r)]
                 [tokens-to-conquer (region-tokens-to-conquer region)])
            (when (region-occupant-race region)
              (race-withdraw! (region-occupant-race region)
                              (region-occupant-count region)))
            (set-field! tokens-in-hand race (- (get-field tokens-in-hand race) 
                                               tokens-to-conquer))
            (region-occupy! region race tokens-to-conquer)))))
    
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


;; Strategy

(struct strategy (pick-a-race ready-troops conquer redeploy go-into-decline))

(define (new-strategy #:pick-a-race [pick-a-race (lambda (player) 0)]
                      #:ready-troops [ready-troops (lambda (player) '())]
                      #:conquer [conquer (lambda (player race) '())]
                      #:redeploy [redeploy (lambda (player) #f)]
                      #:go-into-decline [go-into-decline (lambda (player) #f)])
  (strategy pick-a-race ready-troops conquer redeploy go-into-decline))


;; Tests

(let ([p (new-player "Vasya" #f)])
  (check-equal? (player-name p) "Vasya"))

(let ([p (new-player "Vasya" #f)]
      [r (new-race 'berserk 'amazons)])
  (check-false (send p get-active-race))
  (send p add-race! r)
  (check-equal? (send p get-active-race) r)
  (decline! r)
  (check-false (send p get-active-race)))

; add-race!
(let* (
       [p (new-player "Vasya" #f)]
       [r1 (new-race 'berserk 'amazons)]
       [r2 (new-race 'alchemist 'dwarves)])
  (send p add-race! r1)
  (send p add-race! r2)
  (check-equal? (player-races p) (list r1 r2)))
