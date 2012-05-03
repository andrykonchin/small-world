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
    
    (test-case "coins"
      (let ([p (new-player "Vasya")])
        (check-equal? (get-field coins p) 5)))
    
    (test-case "add-race!"
      (let* ([p (new-player "Vasya")]
             [r1 (new-race 'berserk 'amazons)]
             [r2 (new-race 'alchemist 'dwarves)])
        (send r1 add-coin!)
        (send r1 add-coin!)
        (check-equal? (get-field coins p) 5)
        
        (send p add-race! r1)
        (check-equal? (get-field coins p) 7)
        
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
      (let* ([p (new-player "Vasya" (new (class strategy% (super-new)
                                           (define/override (pick-a-race) 2))))]
             [g (new-game)]
             [race (list-ref (get-field races g) 2)])
        (send p join-game! g)
        (check-false (send p get-active-race))
        (check-equal? (get-field coins p) 5)
        (check-equal? (get-field coins (first (get-field races g))) 0)
        (check-equal? (get-field coins (second (get-field races g))) 0)
        
        (send p pick-a-race!)
        (check-equal? (send p get-active-race) race)
        (check-equal? (get-field coins p) 3)
        (check-equal? (get-field coins (first (get-field races g))) 1)
        (check-equal? (get-field coins (second (get-field races g))) 1)
        ))
    
    (test-case "conquer"
      (let* ([world (new-world two-player-map)]
             [p (new-player "Vasya" (new strategy%))]
             [r1 (new-race 'berserk 'amazons)]
             [r2 (new-race 'alchemist 'dwarves)])
        (set-field! world p world)
        (set-field! world r1 world)
        (set-field! world r2 world)
        (set-field! tokens-in-hand r1 2)
        (set-field! tokens-in-hand r2 4)
        (send p add-race! r1)
        (send p add-race! r2)
        (send p conquer!)
        (check-equal? (length (get-field occupied-regions r1)) 0)
        (check-equal? (length (get-field occupied-regions r2)) 1)))
    
    (test-case "score-victory-coins!"
      (let* ([p (new-player "Vasya")]
             [r1 (new (class race% (super-new)
                        (define/override (score-coins) 3)))]
             [r2 (new (class race% (super-new)
                        (define/override (score-coins) 4)))])
        (send p add-race! r1)
        (send p add-race! r2)
        (check-equal? (get-field coins p) 5)
        (send p score-victory-coins!)
        (check-equal? (get-field coins p) 12)))
    
    (test-case "play-turn!"
      (let* ([p (new-player "Vasya")]
             [g (new-game)])
        (send p join-game! g)
        (send p play-turn!)
        (send p play-turn!)
        (send p play-turn!)))
    ))
