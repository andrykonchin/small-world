#lang racket

(require rackunit)
(require "region.rkt")

(provide region-test-suite) 

(define region-test-suite
  (test-suite "region"
    (test-case "occupy!"
      (let ([region (new-region 'farmlands '() '())]
            [race 'some-race])
        (send region occupy! race 2)
        (check-equal? (region-occupant-race region) 'some-race)
        (check-equal? (region-occupant-count region) 2)))
    
    (test-case "tokens-to-conquer"
      (let ([r1 (new-region 'farmlands '() '())]
            [r2 (new-region 'mountains '() '() 'mountain)])
        (check-equal? (send r1 tokens-to-conquer) 2)
        (check-equal? (send r2 tokens-to-conquer) 3)
        (send r1 occupy! 'amazons 3)
        (check-equal? (send r1 tokens-to-conquer) 5)))    
    ))
