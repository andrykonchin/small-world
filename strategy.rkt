#lang racket

(provide strategy%)

(define strategy%
  (class object%
    (super-new)
    
    (field [player #f])
    
    (define/public (pick-a-race) 0)
    (define/public (ready-troops race) '())
    (define/public (conquer race) 
      (send race get-conquerable-regions))
    (define/public (redeploy race) '())
    (define/public (go-into-decline? race) #f)
    ))