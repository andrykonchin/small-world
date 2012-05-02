#lang racket

(provide new-region
         region-tokens
         region-adjacent-regions
         region-occupant-race
         region-occupant-count
         region-tokens-to-conquer
         region-occupy!)


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
        (send occupant-race withdraw! occupant-count))
      (set! occupant-race race)
      (set! occupant-count token-count))

    (define/public (tokens-to-conquer)
      (+ 2 occupant-count (length tokens)))
    ))
    
(define (new-region terrain-type places adjacent-regions . tokens)
  (make-object region% terrain-type places adjacent-regions tokens))

(define region-terrain-type (class-field-accessor region% terrain-type))
(define region-adjacent-regions (class-field-accessor region% adjacent-regions))
(define region-tokens (class-field-accessor region% tokens))
(define region-occupant-race (class-field-accessor region% occupant-race))
(define region-occupant-count (class-field-accessor region% occupant-count))

(define (region-occupy! region race token-count)
  (send region occupy! race token-count))

(define (region-tokens-to-conquer region)
  (send region tokens-to-conquer))
