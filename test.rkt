#lang racket/base

(require "./lib.rkt")

;; sort
(sort S)

(function (add S S) S)


; (define (R)
;   (rule
;    ([= ab (add a b)]
;     [= r1 (add ab c )])
;    ([let ab (add b c)]
;     [let r2 (add a ab)]
;     [union r1 r2])))

(define (R)
  (rule
   ([= r r])
   ([let r (add b c)]
    [let r (add r r)]
    [let r (add r r)]
    [union r1 r2])))
(ruleset main
         (R))

(require "egraph.rkt")

(run1 (current-egraph) 'main)