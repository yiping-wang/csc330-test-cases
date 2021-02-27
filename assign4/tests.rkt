#lang racket
; ref: https://docs.racket-lang.org/test-engine/index.html#%28part._top%29

(require "functions.rkt")
(require test-engine/racket-tests)

#|Test project to define some tests outside the scope of main. There is a slew of test options
in require test-engine/racket-tests that cannot work in tandem with rackunit. One such test will
check for the proper error. feel free to play around with this for additional testing - Greay|#

(check-error (add-pointwise '(3 4) 3) "illegal parameter")
(check-error (add-pointwise 3 '(3 4)) "illegal parameter")

(test)
