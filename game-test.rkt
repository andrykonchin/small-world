#lang racket

(require rackunit)
(require "game.rkt")
(require "player.rkt")

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
        (check-equal? (send g get-turn) 1)
        (send g play-turn)
        (check-equal? (send g get-turn) 2)
        (check-equal? count 1)))
    
    (test-case "races"
      (let ([g (new-game)])
        (check-equal? (length (send g get-races)) 6)
        (check-equal? (length (get-field race-banners g)) 8)
        (check-equal? (first (get-field race-banners g)) 'humans)))
    ))
