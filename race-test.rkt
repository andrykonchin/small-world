#lang racket

(require rackunit)
(require "race.rkt")
(require "region.rkt")

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
    
    (test-case "can-conquer-region?"
      (let ([race (new-race 'berserk 'amazons)]
            [region (new (class object% 
                           (super-new) 
                           (define/public (tokens-to-conquer) 5)))])
        (set-field! tokens-in-hand race 4)
        (check-false (send race can-conquer-region? region))
        (set-field! tokens-in-hand race 5)
        (check-true (send race can-conquer-region? region))))
    
    (test-case "conquer!"
      (let* ([race1 (new-race 'berserk 'amazons)]
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
      (let* ([race (new-race 'berserk 'amazons)])
        (check-equal? (send race score-coins) 0)
        (set-field! occupied-regions race '(r1 r2 r3))
        (check-equal? (send race score-coins) 3)))
    ))
