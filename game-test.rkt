#lang racket

(require rackunit)
(require "game.rkt")
(require "race-banners.rkt")
(require "special-powers.rkt")

(provide game-test-suite) 

(define game-test-suite
  (test-suite "game"
    (test-case "play-turn"
      (let ([g (new-game)]
            [count 0])
        (send g add-player! (new (class object% 
                                   (super-new) 
                                   (define/public (play-turn) 
                                     (set! count (add1 count))))))
        (check-equal? (get-field turn g) 1)
        (send g play-turn!)
        (check-equal? (get-field turn g) 2)
        (check-equal? count 1)))
    
    (test-case "races"
      (let ([g (new-game)])
        (check-equal? (length (get-field races g)) 6)
        (check-equal? (length (get-field race-banners g)) (- 14 6))
        (check-equal? (first (get-field race-banners g)) humans)
        (check-equal? (length (get-field special-powers g)) (- 20 6))
        (check-equal? (first (get-field special-powers g)) flying)
        ))

    (test-case "take-race!"
      (let* ([g (new-game)]
             [r1 (first (get-field races g))]
             [r2 (second (get-field races g))]
             [r3 (third (get-field races g))])
        (check-equal? (get-field coins r1) 0)
        (check-equal? (length (get-field races g)) 6)
        
        (let ([r (send g take-race! 1)])
          (check-equal? r r2)
          (check-equal? (get-field coins r1) 1)
          (check-equal? (second (get-field races g)) r3)
          (check-equal? (length (get-field races g)) 6))))
    ))
