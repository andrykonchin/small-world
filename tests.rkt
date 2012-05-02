#lang racket

(require rackunit)
(require rackunit/gui)
(require rackunit/text-ui)
(require "region-test.rkt")
(require "player-test.rkt")

(define small-world-test-suite
  (test-suite "small-world"
    region-test-suite
    player-test-suite))

(run-tests small-world-test-suite)

;; Uncomment to run tests in a GUI window:
;(test/gui small-world-test-suite)

