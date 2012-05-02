#lang racket

(require rackunit)
(require rackunit/gui)
(require rackunit/text-ui)
(require "game-test.rkt")
(require "race-test.rkt")
(require "region-test.rkt")
(require "player-test.rkt")

(define small-world-test-suite
  (test-suite "small-world"
    game-test-suite
    race-test-suite
    region-test-suite
    player-test-suite))

;; Uncomment to run tests in console:
;(run-tests small-world-test-suite)

;; Uncomment to run tests in a GUI window:
(test/gui small-world-test-suite)

