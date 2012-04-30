#lang racket

(require rackunit)
(require "utils.rkt")

;; Region

(struct region (terrain-type places adjacent-regions [tokens #:mutable]))

(define (new-region terrain-type places adjacent-regions . tokens)
  (region terrain-type places adjacent-regions tokens))


;; World (map)

(struct world (regions))

(define (new-world region-data)
  (let* ([regions (map (lambda (x)
                         (apply new-region x))
                       region-data)] 
         [w (world regions)]
         [e (get-errors w)])
    (if e
        (error "Invalid world map" e)
        w)))

(define (get-region world index)
  (list-ref (world-regions world) index))

(define (create-world-for-two-players)
  (new-world '((border () (1 2 3 4 5 6 11 12 16 17 18 19 20 21 22 23))
               (sea-or-lake () (0 2 6))
               (farmlands (magic-source) (0 1 3 6 7))
               (forest (mine) (0 2 4 7 8 9))
               (swamp (cavern) (0 3 5 9 10) lost-tribe)
               (hills () (0 4 10 11))
               (mountains (cavern mine) (0 1 2 7 12) mountain)
               (hills () (2 3 6 8 12 13) lost-tribe)
               (sea-or-lake () (3 7 9 13 14))
               (mountains () (3 4 8 10 14 15) mountain)
               (farmlands () (4 5 9 11 15))
               (forest (magic-source) (0 5 10 15 16) lost-tribe)
               (farmlands () (0 6 7 13 17 18) lost-tribe)
               (forest () (7 8 12 14 18 19) lost-tribe)
               (farmland (magic-source) (8 9 13 15 19 20 21) lost-tribe)
               (hills (cavern) (9 10 11 14 16 21 22) lost-tribe)
               (mountains (mine) (0 11 15 22 23) mountain)
               (swamp (magic-source) (0 12 18) lost-tribe)
               (hills (cavern) (0 12 13 17 19))
               (swamp (mine) (0 13 14 18 20) lost-tribe)
               (mountains () (0 14 19 21) mountain)
               (swamp () (0 14 15 20 22 23))
               (forest () (0 15 16 21 23))
               (sea-or-lake () (0 16 21 22)))))

(define (get-terrain-type world r)
  (region-terrain-type (get-region world r)))

(define (get-adjacent-regions world r)
  (region-adjacent-regions (get-region world r)))

(define (get-tokens world r) 
  (region-tokens (get-region world r)))

(define (get-errors world)
  (for/or ([r (in-range (length (world-regions world)))])
    (ormap (lambda (ar)
             (if (member r (get-adjacent-regions world ar))
                 #f
                 (list 'adjacency-error r)))
           (get-adjacent-regions world r))))


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
    
    (define world (create-world-for-two-players))
    (define/public (get-world) world)
    
    (define race-banners (list->vector all-race-banners))
    (define/public (get-race-banners) race-banners)
    
    (define special-powers (list->vector all-special-powers))
    (define/public (get-special-powers) special-powers)
    
    (define turn 1)
    (define/public (get-turn) turn)
    
    (define (add-new-race!) 
      (let ([race (new-race (vector-ref race-banners 0)
                            (vector-ref special-powers 0))])
        (set! races (append races (list race)))
        (set! race-banners (vector-drop race-banners 1))
        (set! special-powers (vector-drop special-powers 1))))
    
    (define races '()) 
    
    (for ([i (in-range 6)]) 
      (add-new-race!))
    
    (define/public (get-races) races)
    
    (define/public (play-turn)
      (for ([p current-players])
        (let* ([race-index ((strategy-pick-a-race (player-strategy p)) this p)]
               [race (list-ref races race-index)])
          (set-player-races! p (cons race (player-races p)))
          (set! races (drop-nth races race-index))
          (add-new-race!))
        
        (map (lambda (r)
               (let ([race (get-active-race p)]
                     [tokens-to-conquer (+ 2 (length (get-tokens world r)))] 
                     [region (get-region world r)])
                 (set-region-tokens! region (make-list tokens-to-conquer (race-race-banner race))))) 
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

(check-equal? (vector-length (send g get-race-banners)) 8)
(check-equal? (vector-ref (send g get-race-banners) 0) 'humans)

(define w (send g get-world))

(send g play-turn)
(check-equal? (player-races p1) (list r1))
(check-equal? (player-races p2) (list r2))
(check-equal? (get-tokens w 2) '(amazons amazons))
(check-equal? (send g get-turn) 2)
(check-equal? (length (send g get-races)) 6)

(check-equal? (get-terrain-type w 1) 'sea-or-lake)
(check-equal? (get-adjacent-regions w 1) '(0 2 6))
(check-equal? (get-terrain-type w 7) 'hills)
(check-equal? (get-adjacent-regions w 7) '(2 3 6 8 12 13))

(define w0 (world (list (new-region 'border '() '()))))
(check-equal? (get-errors w0) #f)

(define m2 (world (list (new-region 'border '() '())
                        (new-region 'sea-or-lake '() '(0 1))
                        (new-region 'mountain '() '(0)))))
(check-equal? (get-errors m2) '(adjacency-error 1))
