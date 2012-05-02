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
        (set-field! tokens-in-hand race1 0)
        (set-field! tokens-in-hand race2 15)
        (send race2 conquer! region)
        (check-equal? (get-field tokens-in-hand race1) 2)
        (check-equal? (get-field tokens-in-hand race2) 10)))
    ))


#;(let* ([p (new-player "Vasya" (new (class strategy%
                                       (super-new)
                                       (define/override (conquer race)
                                         '(2)))))]
         [r1 (new-race 'berserk 'amazons)]
         [r2 (new-race 'alchemist 'dwarves)]
         [g (new game% [players (list p)])])
    (send (send (get-field world g) get-region 2) occupy! r1 3)
    (send p add-race! r2)
    (set-field! tokens-in-hand r1 0)
    (set-field! tokens-in-hand r2 15)
    (send p conquer)
    (check-equal? (get-field tokens-in-hand r1) 2)
    (check-equal? (get-field tokens-in-hand r2) 10))
