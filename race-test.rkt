#lang racket

(require rackunit)
(require "race.rkt")

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
    ))
