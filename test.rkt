#lang racket/base

(require "./lib.rkt")

;; sort
(sort S)

(function (add S S) S)

(ruleset main
         (define R
           (rule
            ([= ab (add a b)]
             [= r1 (add ab c )])
            ([let bc (add b c)]
             [let r2 (add a bc)]
             [union r1 r2]))))