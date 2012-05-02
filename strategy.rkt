#lang racket

(provide strategy%)

(define strategy%
  (class object%
    (super-new)
    (define/public (pick-a-race player) 0)
    (define/public (ready-troops player) '())
    (define/public (conquer player race) '())
    (define/public (redeploy player race) '())
    (define/public (go-into-decline player race) #f)
    ))