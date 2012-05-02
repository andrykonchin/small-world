#lang racket

(require rackunit)
(require rackunit/gui)
(require rackunit/text-ui)
(require "player-test.rkt")

(define small-world-test-suite
  (test-suite 
   "small-world"
   player-test-suite))

(run-tests small-world-test-suite)

;; Uncomment to run tests in a GUI window:
;(test/gui small-world-test-suite)

