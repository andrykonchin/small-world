#lang racket

(require rackunit)
(require "region.rkt")

(provide region-test-suite) 

(define region-test-suite
  (test-suite "region"
    (test-case "occupy!"
      (let ([region (new-region 'farmlands '() '())]
            [race 'some-race])
        (region-occupy! region race 2)
        (check-equal? (region-occupant-race region) 'some-race)
        (check-equal? (region-occupant-count region) 2)))
    
    (test-case "tokens-to-conquer"
      (let ([r1 (new-region 'farmlands '() '())]
            [r2 (new-region 'mountains '() '() 'mountain)])
        (check-equal? (region-tokens-to-conquer r1) 2)
        (check-equal? (region-tokens-to-conquer r2) 3)
        (region-occupy! r1 'amazons 3)
        (check-equal? (region-tokens-to-conquer r1) 5)))    
    ))
