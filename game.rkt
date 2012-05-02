#lang racket

(require rackunit)
(require "utils.rkt")
(require "maps.rkt")
(require "world.rkt")
(require "race.rkt")
(require "player.rkt")


(define game%
  (class object%
    (super-new)
    
    (init-field players)
    (define/public (get-players) players)
    (for ([p players])
      (set-field! game p this))
    
    (define world (new-world two-player-map))
    (define/public (get-world) world)
    
    (define race-banners all-race-banners)
    (define/public (get-race-banners) race-banners)
    
    (define special-powers all-special-powers)
    (define/public (get-special-powers) special-powers)
    
    (define turn 1)
    (define/public (get-turn) turn)
    
    (define (add-new-race!)
      (let ([race (new-race (first special-powers) (first race-banners))])
        (set! races (append races (list race)))
        (set! race-banners (rest race-banners))
        (set! special-powers (rest special-powers))))
    
    (define races '())
    
    (for ([i (in-range 6)])
      (add-new-race!))
    
    (define/public (get-races) races)
    
    (define/public (take-race index)
      (let ([race (list-ref races index)])
        (set! races (remove-nth races index))
        (add-new-race!)
        race))
    
    (define/public (play-turn)
      (for ([p players])
        (send p play-turn))
      (set! turn (add1 turn)))
    ))


;; Tests

(define p1 (new-player "Vasya" (new-strategy #:conquer (lambda (player race) '(2)))))
(define p2 (new-player "Petya" (new-strategy)))

(define g (new game% [players (list p1 p2)]))
(check-equal? (length (send g get-races)) 6)

(check-equal? (length (send g get-players)) 2)
(check-eq? (first (send g get-players)) p1)
(check-eq? (second (send g get-players)) p2)
(check-equal? (send g get-turn) 1)
(check-equal? (player-points p1) 5)
(check-equal? (player-points p2) 5)

(define r1 (first (send g get-races)))
(check-equal? (race-special-power r1) 'alchemist)
(check-equal? (race-race-banner r1) 'amazons)

(define r2 (second (send g get-races)))
(check-equal? (race-special-power r2) 'berserk)
(check-equal? (race-race-banner r2) 'dwarves)

(check-equal? (length (send g get-race-banners)) 8)
(check-equal? (first (send g get-race-banners)) 'humans)

(define w (send g get-world))

(send g play-turn)
(check-equal? (player-races p1) (list r1))
(check-equal? (player-races p2) (list r2))
(check-equal? (region-occupant-count (get-region w 2)) 2)
(check-equal? (send g get-turn) 2)
(check-equal? (length (send g get-races)) 6)

; conquer
(let* ([count 0]
       [p (new-player "Vasya" (new-strategy #:conquer (lambda (player race) 
                                                        (set! count (add1 count)) 
                                                        '(2))))]
       [r1 (new-race 'berserk 'amazons)]
       [r2 (new-race 'alchemist 'dwarves)]
       [g (new game% [players (list p)])])
  (send p add-race! r1)
  (send p add-race! r2)
  (send p conquer)
  (check-equal? count 2))

