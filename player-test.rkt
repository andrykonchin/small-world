#lang racket

(require rackunit)
(require "player.rkt")
(require "race.rkt")
(require "world.rkt")
(require "maps.rkt")
(require "strategy.rkt")
(require "game.rkt")

(provide player-test-suite) 

(define player-test-suite
  (test-suite "player"
    (test-case "name"
      (let ([p (new-player "Vasya")])
        (check-equal? (get-field name p) "Vasya")))
    
    (test-case "points"
      (let ([p (new-player "Vasya")])
        (check-equal? (get-field points p) 5)))
    
    (test-case "add-race!"
      (let* ([p (new-player "Vasya")]
             [r1 (new-race 'berserk 'amazons)]
             [r2 (new-race 'alchemist 'dwarves)])
        (send r1 add-coin!)
        (send r1 add-coin!)
        (check-equal? (get-field points p) 5)
        
        (send p add-race! r1)
        (check-equal? (get-field points p) 7)

        (send p add-race! r2)
        (check-equal? (get-field races p) (list r1 r2))))
    
    (test-case "get-active-race"
      (let ([p (new-player "Vasya")]
            [r (new-race 'berserk 'amazons)])
        (check-false (send p get-active-race))
        (send p add-race! r)
        (check-equal? (send p get-active-race) r)
        (send r decline!)
        (check-false (send p get-active-race))))
    
    (test-case "pick-a-race"
      (let* ([p (new-player "Vasya" (new (class strategy%
                                          (super-new)
                                          (define/override (pick-a-race) 2))))]
             [g (new-game)]
             [race (list-ref (get-field races g) 2)])
        (send p join-game! g)
        (check-false (send p get-active-race))
        (check-equal? (get-field points p) 5)
        (check-equal? (get-field coins (first (get-field races g))) 0)
        (check-equal? (get-field coins (second (get-field races g))) 0)

        (send p pick-a-race!)
        (check-equal? (send p get-active-race) race)
        (check-equal? (get-field points p) 3)
        (check-equal? (get-field coins (first (get-field races g))) 1)
        (check-equal? (get-field coins (second (get-field races g))) 1)
        ))
    
    (test-case "conquer"
      (let* ([count 0]
             [p (new-player "Vasya" (new (class strategy%
                                           (super-new)
                                           (define/override (conquer race) 
                                             (set! count (add1 count))
                                             '(2)))))]
             [r1 (new-race 'berserk 'amazons)]
             [r2 (new-race 'alchemist 'dwarves)])
        (set-field! world p (new-world two-player-map))
        (send p add-race! r1)
        (send p add-race! r2)
        (send p conquer)
        (check-equal? count 2)))
    ))
