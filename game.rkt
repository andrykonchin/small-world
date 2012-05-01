#lang racket

(require rackunit)
(require "utils.rkt")
(require "maps.rkt")
(require "world.rkt")


;; Races

(struct race (race-banner special-power coins in-decline) #:transparent)

(define (new-race race-banner special-power)
  (race race-banner special-power 0 #f))


;; Game
(define game% 
  (class object% 
    (init players)
    
    (super-new)
    
    (define current-players players)
    (define/public (get-players) current-players) 
    
    (define world (new-world two-player-map))
    (define/public (get-world) world)
    
    (define race-banners all-race-banners)
    (define/public (get-race-banners) race-banners)
    
    (define special-powers all-special-powers)
    (define/public (get-special-powers) special-powers)
    
    (define turn 1)
    (define/public (get-turn) turn)
    
    (define (add-new-race!) 
      (let ([race (new-race (first race-banners) (first special-powers))])
        (set! races (append races (list race)))
        (set! race-banners (rest race-banners))
        (set! special-powers (rest special-powers))))
    
    (define races '()) 
    
    (for ([i (in-range 6)]) 
      (add-new-race!))
    
    (define/public (get-races) races)
    
    (define/public (play-turn)
      (for ([p current-players])
        (let* ([race-index ((strategy-pick-a-race (player-strategy p)) this p)]
               [race (list-ref races race-index)])
          (set-player-races! p (cons race (player-races p)))
          (set! races (remove-nth races race-index))
          (add-new-race!))
        
        (map (lambda (r)
               (let ([race (get-active-race p)]
                     [tokens-to-conquer (+ 2 (length (get-tokens world r)))])
                 (set-tokens! world r (make-list tokens-to-conquer (race-race-banner race))))) 
             ((strategy-conquer (player-strategy p)) this p)))
      
      (set! turn (add1 turn)))
    
    ))

(define all-race-banners 
  '(amazons dwarves elves ghouls giants
            halflings humans orcs ratmen skeletons
            sourcerers tritons trolls wizards))

(define all-special-powers
  '(alchemist berserk bivouacking commando diplomat
              dragon-master flying forest fortified heroic
              hill merchant mounted pillaging seafaring
              spirit stout swamp underworld wealthy))


;; Player

(struct player (name points [races #:mutable] strategy))

(define (new-player name strategy)
  (player name 5 '() strategy))

(define (get-active-race player) 
  (first (player-races player)))


;; Strategy

(struct strategy (pick-a-race ready-troops conquer redeploy go-into-decline))

(define (new-strategy #:pick-a-race [pick-a-race (lambda (game player) 0)]
                      #:ready-troops [ready-troops (lambda (game player) '())]
                      #:conquer [conquer (lambda (game player) '())]
                      #:redeploy [redeploy (lambda (game player) #f)]
                      #:go-into-decline [go-into-decline (lambda (game player) #f)])
  (strategy pick-a-race ready-troops conquer redeploy go-into-decline))


;; Tests

(define p1 (new-player "Vasya" (new-strategy #:conquer (lambda (game player) '(2)))))
(check-equal? (player-name p1) "Vasya")

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
(check-equal? (get-tokens w 2) '(amazons amazons))
(check-equal? (send g get-turn) 2)
(check-equal? (length (send g get-races)) 6)
