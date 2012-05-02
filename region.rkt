#lang racket

(provide new-region
         region-tokens
         region-adjacent-regions)


(define region%
  (class object%
    (super-new)
    (init-field terrain-type)
    (init-field places)
    (init-field adjacent-regions)
    (init-field tokens)
    (field [occupant-race #f])
    (field [occupant-count 0])
    
    (define/public (occupy! race token-count)
      (when occupant-race
        (send occupant-race withdraw! this))
      (set! occupant-race race)
      (set! occupant-count token-count))

    (define/public (tokens-to-conquer)
      (+ 2 occupant-count (length tokens)))
    ))
    
(define (new-region terrain-type places adjacent-regions . tokens)
  (make-object region% terrain-type places adjacent-regions tokens))

(define region-adjacent-regions (class-field-accessor region% adjacent-regions))
(define region-tokens (class-field-accessor region% tokens))
