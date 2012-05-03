#lang racket

(require "utils.rkt")
(require "maps.rkt")
(require "world.rkt")
(require "race.rkt")

(provide new-game)

 
(define game%
  (class object%
    (super-new)
    
    (field [world (new-world two-player-map)])

    (init-field players)
    (for ([p players])
      (send p join-game! this))
    
    (field [race-banners all-race-banners])
    (field [special-powers all-special-powers])

    (define (add-new-race!)
      (let ([race (new-race (first special-powers) (first race-banners))])
        (set-field! world race world)
        (set! races (append races (list race)))
        (set! race-banners (rest race-banners))
        (set! special-powers (rest special-powers))))
    
    (field [races '()])
    (for ([i (in-range 6)])
      (add-new-race!))
    
    (field [turn 1])
    
    (define/public (add-player! player)
      (set! players (append players (list player))))

    (define/public (take-race! index)
      (for ([skipped-race (take races index)])
        (send skipped-race add-coin!))
      (let ([race (list-ref races index)])
        (set! races (remove-nth races index))
        (add-new-race!)
        race))
    
    (define/public (play-turn!)
      (for ([p players])
        (send p play-turn))
      (set! turn (add1 turn)))
    ))

(define (new-game . players)
  (make-object game% players))
