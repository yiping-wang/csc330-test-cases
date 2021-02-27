#lang racket

(require "functions.rkt")
(require test-engine/racket-tests)

#|Test project to define some tests outside the scope of main. There is a slew of test options
in require test-engine/racket-tests that cannot work in tandem with rackunit. One such test will
check for the proper error. feel free to play around with this for additional testing - Greay|#


(create-stream squares using (lambda (x) (* x x)) 
          starting at  5
          with increment 2)

;------------------------add-pointwise tests--------------------------

;(Greay tests)

(check-error (add-pointwise '(3 4) 3) "illegal parameter")
(check-error (add-pointwise 3 '(3 4)) "illegal parameter")
   
;-------------------add-pointwise-lists tests------------------------------

;(Greay tests)
(check-error (add-pointwise-lists '((1 1) (2 2 2 2) 3 ())) "illegal parameter")

;-------------------add-pointwise-lists-2 tests------------------------------
   

;-------------------stream-for-n-steps tests------------------------------


;--------------------------fibo-stream tests------------------------------
  

;--------------------------filter-stream tests------------------------------
  

;-----------------------palyndromic-numbers tests---------------------------
  

;-------------------------------squares tests------------------------------
  

;--------------------------vector-assoc tests------------------------------
   

;--------------------------cached-assoc tests------------------------------
   
(print "-----------tests.rkt-----------")
(newline)
(newline)
(test)
(newline)
(newline)
(print "-----------main.rkt------------")
(newline)
(newline)
