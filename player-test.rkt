#lang racket

(require rackunit)
(require "player.rkt")
(require "race.rkt")
(require "world.rkt")
(require "maps.rkt")
(require "strategy.rkt")

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
        (send p add-race! r1)
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
