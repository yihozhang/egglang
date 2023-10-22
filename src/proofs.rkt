#lang racket/base

(require racket/match
         "core.rkt")

(provide (struct-out justification)
         termify)

(struct proof-manager
  ())

(struct justification
  (cause
   [context #:mutable]))

;; Replace every sort variable in the context
;; with corresponding term
(define (termify egraph m)
  m
  )
