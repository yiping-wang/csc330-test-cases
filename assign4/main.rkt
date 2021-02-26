#lang racket
;;
;;
(require "functions.rkt")

(require rackunit)

;; test create-stream
;(create-stream squares using (lambda (x) (* x x)) 
;          starting at  5
;          with increment 2)

(define tests
    (test-suite
        "Sample tests for Assignment 4"


   	(check-equal? (add-pointwise '(1 2 3) '(4 2)) (list 5 4 3) "add-pointwise test")
   	(check-equal? (add-pointwise '(1 2 3 4 5 6 7 8 9 10) '(0 5 10 15 20)) (list 1 7 13 19 25 6 7 8 9 10) "add-pointwise test ep1")
	(check-equal? (add-pointwise '(1 2 3 4 5 6 7 8 9 10) '(-1 -2 -3 -4 -5 -6)) (list 0 0 0 0 0 0 7 8 9 10) "add-pointwise test ep2")
	(check-equal? (add-pointwise '(1 2 3 4 5 6 7 8 9 10) '(-2 10 15 20)) (list -1 12 18 24 5 6 7 8 9 10) "add-pointwise test ep3")
	(check-equal? (add-pointwise '(0 5 10 15 20) '(-1 -2 -3 -4 -5 -6)) (list -1 3 7 11 15 -6) "add-pointwise test ep4")
	(check-equal? (add-pointwise '(0 5 10 15 20) '(-2 10 15 20)) (list -2 15 25 35 20) "add-pointwise test ep5")
	(check-equal? (add-pointwise '(-1 -2 -3 -4 -5 -6) '(-2 10 15 20)) (list -3 8 12 16 -5 -6) "add-pointwise test ep6")
   

   	(check-equal? (add-pointwise-lists '((1 1) (2 2 2 2) (3) ())) '(6 3 2 2) "add-pointwise-list test"))
   	(check-equal? (add-pointwise-lists '((1 2 3 4 5 6 7 8 9 10) (0 5 10 15 20) (-1 -2 -3 -4 -5 -6)) '(0 5 10 15 20 0 7 8 9 10) "add-pointwise-list test ep1"))
	(check-equal? (add-pointwise-lists '((1 2 3 4 5 6 7 8 9 10) (0 5 10 15 20) (-1 -2 -3 -4 -5 -6) (-2 10 15 20) (-100 -200 300 400 600)) '(-102 -185 325 435 620 0 7 8 9 10) "add-pointwise-list test ep2"))
	(check-equal? (add-pointwise-lists '((1 2 3 4 5 6 7 8 9 10) (-1 -2 -3 -4 -5 -6) (-1 -2 -3 -4 -5 -6) (-1 -2 -3 -4 -5 -6)) '(-2 -4 -6 -8 -10 -12 7 8 9 10) "add-pointwise-list test ep3"))
	(check-equal? (add-pointwise-lists '((1 2 3 4 5 6 7 8 9 10)) '(1 2 3 4 5 6 7 8 9 10) "add-pointwise-list test ep4"))
	(check-equal? (add-pointwise-lists '((1 2 3 4 5 6 7 8 9 10) (-2 10 15 20) (-100 -200 300 400 600)) '(-101 -188 318 424 605 6 7 8 9 10) "add-pointwise-list test ep5"))
	(check-equal? (add-pointwise-lists '((1 2 3 4 5 6 7 8 9 10) (-1 -2 -3 -4 -5 -6) (-2 10 15 20) (-2 10 15 20)) '(-4 20 30 40 0 0 7 8 9 10) "add-pointwise-list test ep6"))


	(check-equal? (add-pointwise-lists-2 '((1 1) (2 2 2 2) (3) ())) '(6 3 2 2) "add-pointwise-list test"))
   	(check-equal? (add-pointwise-lists-2 '((1 2 3 4 5 6 7 8 9 10) (0 5 10 15 20) (-1 -2 -3 -4 -5 -6)) '(0 5 10 15 20 0 7 8 9 10) "add-pointwise-list test ep1"))
	(check-equal? (add-pointwise-lists-2 '((1 2 3 4 5 6 7 8 9 10) (0 5 10 15 20) (-1 -2 -3 -4 -5 -6) (-2 10 15 20) (-100 -200 300 400 600)) '(-102 -185 325 435 620 0 7 8 9 10) "add-pointwise-list test ep2"))
	(check-equal? (add-pointwise-lists-2 '((1 2 3 4 5 6 7 8 9 10) (-1 -2 -3 -4 -5 -6) (-1 -2 -3 -4 -5 -6) (-1 -2 -3 -4 -5 -6)) '(-2 -4 -6 -8 -10 -12 7 8 9 10) "add-pointwise-list test ep3"))
	(check-equal? (add-pointwise-lists-2 '((1 2 3 4 5 6 7 8 9 10)) '(1 2 3 4 5 6 7 8 9 10) "add-pointwise-list test ep4"))
	(check-equal? (add-pointwise-lists-2 '((1 2 3 4 5 6 7 8 9 10) (-2 10 15 20) (-100 -200 300 400 600)) '(-101 -188 318 424 605 6 7 8 9 10) "add-pointwise-list test ep5"))
	(check-equal? (add-pointwise-lists-2 '((1 2 3 4 5 6 7 8 9 10) (-1 -2 -3 -4 -5 -6) (-2 10 15 20) (-2 10 15 20)) '(-4 20 30 40 0 0 7 8 9 10) "add-pointwise-list test ep6"))


   	(check-equal? (stream-for-n-steps nat-num-stream 10) '(0 1 2 3 4 5 6 7 8 9) "stream-for-n-steps test")
   	(check-equal? (stream-for-n-steps nat-num-stream 15) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14) "nat-num-stream test ep1")
   	(check-equal? (stream-for-n-steps nat-num-stream 20) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) "nat-num-stream test ep2")
   	(check-equal? (stream-for-n-steps nat-num-stream 25) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24) "nat-num-stream test ep3")
   	(check-equal? (stream-for-n-steps nat-num-stream 30) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29) "nat-num-stream test ep4")


   	(check-equal? (stream-for-n-steps fibo-stream 10) '(0 1 1 2 3 5 8 13 21 34) "fibo-stream test")
   	(check-equal? (stream-for-n-steps fibo-stream 15) '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377) "fibo-stream test ep2")
    (check-equal? (stream-for-n-steps fibo-stream 20) '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181) "fibo-stream test ep2")
    (check-equal? (stream-for-n-steps fibo-stream 25) '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368) "fibo-stream test ep3")
    (check-equal? (stream-for-n-steps fibo-stream 30) '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229) "fibo-stream test ep4")


    (check-equal? (stream-for-n-steps (filter-stream (lambda (i) (> i 5)) nat-num-stream) 5) '(6 7 8 9 10) "filter nat-num-stream test")
    (check-equal? (stream-for-n-steps (filter-stream (lambda (i) (integer? (/ i 3))) 5) '(0 3 6 9 12) "filter nat-num-stream test")
    (check-equal? (stream-for-n-steps (filter-stream (lambda (i) (> i 5)) fibo-stream) 15) '(8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765) "filter fibo-stream test ep1")    
    (check-equal? (stream-for-n-steps (filter-stream (lambda (i) (> i 10)) palyndromic-numbers) 11) '(11 22 33 44 55 66 77 88 99 101 111) "filter palyndromic-numbers test ep1")    


    (check-equal? (stream-for-n-steps palyndromic-numbers 20) '(0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101) "palyndromic-numbers test")
    (check-equal? (stream-for-n-steps palyndromic-numbers 15) '(0 1 2 3 4 5 6 7 8 9 11 22 33 44 55) "palyndromic-numbers test ep1")



    (check-equal? (stream-for-n-steps squares 5) '(25 49 81 121 169) "stream defined using a macro. only tests is return value")

    (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")

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
