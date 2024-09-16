#lang racket

(require "../src/lib.rkt")

(function (fib-demand Number) unit-lat)
(function (fib Number) Number)

(run-action! (fib-demand 20))

(rule ((fib-demand n)
       (> n 1))
      ((fib-demand (- n 1))
       (fib-demand (- n 2))))

(rule ()
      ((set! (fib 0) 0)
       (set! (fib 1) 1)))

(rule ((fib-demand n)
       (= (fib (- n 1)) f1)
       (= (fib (- n 2)) f2))
      ((set! (fib n) (+ f1 f2))))

(saturate)

(check (= (fib 18) 2584))
(check (= (fib 19) 4181))
(check (= (fib 20) 6765))
