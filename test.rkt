#lang racket/base

(require "./lib.rkt")

;; sort
(sort S)

(function (add S S) S)
(function (num i64) S)

(require "egraph.rkt")
(require "macros.rkt")

(rule ()
      ((let x (add (num 1) (num 2)))
       (let y (add (num 2) (num 1)))
       (union x y)
       ))
(run1 (current-egraph) (current-ruleset))