#lang racket

(require rackunit)
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


;; Tests

(check-exn exn:fail? (lambda () (new-world '((border () ())
                                             (sea-or-lake () (0 1))))))

(define w (new-world '((border () (1 2 3))
                       (sea-or-lake () (0 2))
                       (mountain (mine) (0 1 3))
                       (hills () (0 2) lost-tribe))))

(check-equal? (get-field terrain-type (send w get-region 1)) 'sea-or-lake)
(check-equal? (get-field terrain-type (send w get-region 2)) 'mountain)

(check-equal? (get-adjacent-regions w 1) '(0 2))
(check-equal? (get-adjacent-regions w 2) '(0 1 3))

(check-equal? (get-tokens w 1) '())
(check-equal? (get-tokens w 3) '(lost-tribe))
