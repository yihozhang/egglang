#lang racket/base

(require "./lib.rkt")

;; sort
(sort S)

(define R (rule
  ([= ab (+ a b)]
   [= r1 (+ ab c )])
  ([let bc (+ b c)]
   [let r2 (+ a bc)]
   [union r1 r2])))
(flatten-rule R)