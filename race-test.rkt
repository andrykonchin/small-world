#lang racket

(require rackunit)
(require "race.rkt")

(provide race-test-suite) 

(define race-test-suite
  (test-suite "race"
    (test-case "init"
      (let ([r (new-race 'berserk 'amazons)])
        (check-equal? (get-field coins r) 0)
        (check-false (race-in-decline? r))))
    
    (test-case "can-conquer?"
      (let ([r (new-race 'berserk 'amazons)])
        (check-true (race-can-conquer? r))
        (send r decline!)
        (check-false (race-can-conquer? r)))
      
      (let ([rg (new (ghouls race%))])
        (check-true (race-can-conquer? rg))
        (send rg decline!)
        (check-true (race-can-conquer? rg))))
    
    (test-case "can-conquer-region?"
      (let ([race (new-race 'berserk 'amazons)]
            [region (new (class object% 
                           (super-new) 
                           (define/public (tokens-to-conquer) 5)))])
        (set-field! tokens-in-hand race 4)
        (check-false (send race can-conquer-region? region))
        (set-field! tokens-in-hand race 5)
        (check-true (send race can-conquer-region? region))))
    ))
