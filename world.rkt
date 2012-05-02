#lang racket

(require "region.rkt")

(provide new-world
         get-adjacent-regions
         get-tokens)


(define world%
  (class object%
    (super-new)

    (init region-data)

    (field [regions (map (curry apply new-region) region-data)])

    (for* ([r (in-range (length regions))]
           [ar (get-adjacent-regions this r)])
      (when (not (member r (get-adjacent-regions this ar)))
        (error "Adjacency error" r)))

    (define/public (get-region index)
      (list-ref regions index))
    ))

(define (new-world region-data)
  (new world% [region-data region-data]))

(define world-regions (class-field-accessor world% regions))

(define (get-adjacent-regions world r)
  (region-adjacent-regions (send world get-region r)))

(define (get-tokens world r)
  (region-tokens (send world get-region r)))
