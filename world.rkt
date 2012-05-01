#lang racket

(require rackunit)

(provide new-world
         get-terrain-type
         get-adjacent-regions
         get-tokens
         set-tokens!)


;; Region

(struct region (terrain-type places adjacent-regions [tokens #:mutable]))

(define (new-region terrain-type places adjacent-regions . tokens)
  (region terrain-type places adjacent-regions tokens))


;; World (map)

(define world%
  (class object%
    (super-new)

    (init region-data)

    (field [regions (map (curry apply new-region) region-data)])

    (for* ([r (in-range (length regions))]
           [ar (get-adjacent-regions this r)])
      (when (not (member r (get-adjacent-regions this ar)))
        (error "Adjacency error" r)))

    ))

(define (new-world region-data)
  (new world% [region-data region-data]))

(define world-regions (class-field-accessor world% regions))

(define (get-region world index)
  (list-ref (world-regions world) index))

(define (get-terrain-type world r)
  (region-terrain-type (get-region world r)))

(define (get-adjacent-regions world r)
  (region-adjacent-regions (get-region world r)))

(define (get-tokens world r)
  (region-tokens (get-region world r)))

(define (set-tokens! world r tokens)
  (set-region-tokens! (get-region world r) tokens))


;; Tests

(check-exn exn:fail? (lambda () (new-world '((border () ())
                                             (sea-or-lake () (0 1))))))

(define w (new-world '((border () (1 2 3))
                       (sea-or-lake () (0 2))
                       (mountain (mine) (0 1 3))
                       (hills () (0 2) lost-tribe))))

(check-equal? (get-terrain-type w 1) 'sea-or-lake)
(check-equal? (get-terrain-type w 2) 'mountain)

(check-equal? (get-adjacent-regions w 1) '(0 2))
(check-equal? (get-adjacent-regions w 2) '(0 1 3))

(check-equal? (get-tokens w 1) '())
(check-equal? (get-tokens w 3) '(lost-tribe))

(set-tokens! w 3 '(amazon amazon amazon))
(check-equal? (get-tokens w 3) '(amazon amazon amazon))

