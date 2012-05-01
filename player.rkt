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
      (set! races (cons race races)))
    
    (define (pick-a-race)
      (let* ([race-index ((strategy-pick-a-race strategy) this)])
        (add-race! (send game take-race race-index))))
    
    (define (conquer)
      (for ([r ((strategy-conquer strategy) this)])
        (let ([race (get-active-race)]
              [tokens-to-conquer (+ 2 (length (get-tokens (send game get-world) r)))])
          (set-tokens! (send game get-world)
                       r
                       (make-list tokens-to-conquer (race-race-banner race))))))
    
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
                      #:conquer [conquer (lambda (player) '())]
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