#lang racket
;;
;;
(require "functions.rkt")

(require rackunit)

;; test create-stream
(create-stream squares using (lambda (x) (* x x)) 
          starting at  5
          with increment 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"

   ;------------------------add-pointwise tests--------------------------
   
   (check-equal? (add-pointwise '(1 2 3) '(4 2))
                 (list 5 4 3)
                 "add-pointwise test")
   ;(Greay tests)
   (check-equal? (add-pointwise '() '()) null "add-pointwise test")
   (check-equal? (add-pointwise '(1 2) '(4 2 3)) (list 5 4 3) "add-pointwise test")
   (check-equal? (add-pointwise '(1 2) '()) (list 1 2) "add-pointwise test")
   (check-equal? (add-pointwise '() '(1 2)) (list 1 2) "add-pointwise test")
   (check-equal? (add-pointwise '(3 4) '(1 2)) (list 4 6) "add-pointwise test")
   (check-exn exn:fail? (lambda () (add-pointwise 3)) "add-pointwise test")
   
   ;-------------------add-pointwise-lists tests------------------------------

   (check-equal? (add-pointwise-lists '((1 1) (2 2 2 2) (3) ()))
                 '(6 3 2 2)
                 "add-pointwise-list test")

   ;-------------------add-pointwise-lists-2 tests------------------------------
   
   (check-equal? (add-pointwise-lists-2 '((1 1) (2 2 2 2) (3) ()))
                 '(6 3 2 2)
                 "add-pointwise-list test")

   ;-------------------stream-for-n-steps tests------------------------------

  (check-equal? (stream-for-n-steps nat-num-stream 10)
                 '(0 1 2 3 4 5 6 7 8 9)
                 "stream-for-n-steps test")

  ;--------------------------fibo-stream tests------------------------------
  
  (check-equal? (stream-for-n-steps fibo-stream 10)
                 '(0 1 1 2 3 5 8 13 21 34)
                 "fibo-stream test")

  ;--------------------------filter-stream tests------------------------------
  
  (check-equal? (stream-for-n-steps 
                  (filter-stream (lambda (i) (> i 5)) nat-num-stream) 5)
                 '(6 7 8 9 10)
                 "filter stream test")

  ;-----------------------palyndromic-numbers tests---------------------------
  
  (check-equal? (stream-for-n-steps palyndromic-numbers 20)
                 '(0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101)
                 "palyndromic-numbers test")

  ;-------------------------------squares tests------------------------------
  
   (check-equal? (stream-for-n-steps squares 5)
                 '(25 49 81 121 169)
                 "stream defined using a macro. only tests is return value")

  ;--------------------------vector-assoc tests------------------------------
   
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)))
                 (cons 4 1)
                 "vector-assoc test")
   ;(Greay tests)
   (check-equal? (vector-assoc 4 (vector #f #f #f #f #f #f)) #f "vector-assoc test")

  ;--------------------------cached-assoc tests------------------------------
   
   ;; note that the following test tests functionality but not performance
   (check-equal?
    (let
        [(cache (cached-assoc  (list (cons 1 2) (cons 3 4)) 3) )]
           (cache 3))
    (cons 3 4)
    "cached-assoc test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
