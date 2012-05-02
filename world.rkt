#lang racket

(require rackunit)

(provide new-world
         get-region
         get-terrain-type
         get-adjacent-regions
         get-tokens
         region-tokens
         region-occupant-race
         region-occupant-count
         region-tokens-to-conquer
         region-occupy!)


;; Region
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

; region-occupy!
(let ([region (new-region 'farmlands '() '())]
      [race 'some-race])
  (region-occupy! region race 2)
  (check-equal? (region-occupant-race region) 'some-race)
  (check-equal? (region-occupant-count region) 2))

;region-tokens-to-conquer
(let ([r1 (new-region 'farmlands '() '())]
      [r2 (new-region 'mountains '() '() 'mountain)])
  (check-equal? (region-tokens-to-conquer r1) 2)
  (check-equal? (region-tokens-to-conquer r2) 3)
  (region-occupy! r1 'amazons 3)
  (check-equal? (region-tokens-to-conquer r1) 5))
  