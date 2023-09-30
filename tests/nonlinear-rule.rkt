#lang racket

(require "../src/lib.rkt")

(sort Math)
(function (Num i64) Math)
(function (Sub Math Math) Math)
(rewrite (Sub a a) (Num 0))

(run-action! (Sub (Num 1) (Num 2)))
(run 1)
(define result (run-query (= (Sub (Num 1) (Num 2)) (Num 0))))
(unless (null? result)
  (error "non-linear rule is applied without respecting equality constraitns"))