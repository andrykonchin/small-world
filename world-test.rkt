#lang racket

(require rackunit)
(require "world.rkt")

(provide world-test-suite) 


(define world-test-suite
  (test-suite "world"
    (test-case "init fail"
      (check-exn exn:fail? (lambda () (new-world '((border () ())
                                             (sea-or-lake () (0 1)))))))
    
    (test-case "init"
      (let ([w (new-world '((border () (1 2 3))
                            (sea-or-lake () (0 2))
                            (mountain (mine) (0 1 3))
                            (hills () (0 2) lost-tribe)))])
        (check-equal? (get-field terrain-type (send w get-region 1)) 'sea-or-lake)
        (check-equal? (get-field terrain-type (send w get-region 2)) 'mountain)
        
        (check-equal? (get-adjacent-regions w 1) '(0 2))
        (check-equal? (get-adjacent-regions w 2) '(0 1 3))
        
        (check-equal? (get-tokens w 1) '())
        (check-equal? (get-tokens w 3) '(lost-tribe))))
    ))
