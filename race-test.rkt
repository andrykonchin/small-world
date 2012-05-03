#lang racket

(require rackunit)
(require "race.rkt")
(require "region.rkt")
(require "race-banners.rkt")
(require "special-powers.rkt")
(require "world.rkt")

(provide race-test-suite) 


(define race-test-suite
  (test-suite "race"
    (test-case "init"
      (let ([r (new-race 'berserk 'amazons)])
        (check-equal? (get-field coins r) 0)
        (check-false (get-field in-decline r))))
    
    (test-case "can-conquer?"
      (let ([r (new-race 'berserk 'amazons)])
        (check-true (send r can-conquer?))
        (send r decline!)
        (check-false (send r can-conquer?)))
      
      (let ([rg (new (ghouls race%))])
        (check-true (send rg can-conquer?))
        (send rg decline!)
        (check-true (send rg can-conquer?))))
    
    (test-case "conquer!"
      (let ([race1 (new-race 'berserk 'amazons)]
            [race2 (new-race 'alchemist 'dwarves)]
            [region (new-region 'hills '() '())])
        (send region occupy! race1 3)
        (set-field! occupied-regions race1 (list region))
        (set-field! tokens-in-hand race1 0)
        (set-field! tokens-in-hand race2 15)
        
        (send race2 conquer! region)
        (check-equal? (get-field occupied-regions race1) '())
        (check-equal? (get-field occupied-regions race2) (list region))
        (check-equal? (get-field tokens-in-hand race1) 2)
        (check-equal? (get-field tokens-in-hand race2) 10)))
    
    (test-case "score-coins"
      (let ([race (new-race 'berserk 'amazons)])
        (check-equal? (send race score-coins) 0)
        (set-field! occupied-regions race '(r1 r2 r3))
        (check-equal? (send race score-coins) 3)))
    
    (test-case "decline!"
      (let ([race (new-race 'berserk 'amazons)]
            [region1 (new-region 'hills '() '())]
            [region2 (new-region 'hills '() '())])
        (set-field! occupied-regions race (list region1 region2))
        (send region1 occupy! race 3)
        (send region2 occupy! race 4)
        (check-false (get-field in-decline race))
        (check-equal? (get-field occupant-count region1) 3)
        (check-equal? (get-field occupant-count region2) 4)
        
        (send race decline!)
        (check-true (get-field in-decline race))
        (check-equal? (get-field occupant-count region1) 1)
        (check-equal? (get-field occupant-count region2) 1)))
    
    (test-case "ghouls" 
      (let ([race (new (alchemist (ghouls race%)))])
        (check-equal? (send race initial-tokens) 9)))
    
    (test-case "get-conquerable-regions"
      (let ([race (new (alchemist (ghouls race%)))]
            [world (new-world '((border () (1 2))
                                (sea-or-lake () (0 2))
                                (farmlands () (0 1 3 4))
                                (hills () (2 3 4))
                                (swamp () (2 3))))])
        (set-field! world race world)
        (check-equal? (send race get-conquerable-regions) '(1 2))
        
        (let ([r3 (send world get-region 3)]
              [r4 (send world get-region 4)])
          (send r3 occupy! race 8) 
          (send r4 occupy! race 9) 
          (set-field! occupied-regions race (list r3 r4)))
        (check-equal? (send race get-conquerable-regions) '(2))))
    ))
