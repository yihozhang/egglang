#lang racket/base

(require racket/match)

(provide (all-defined-out))

(define i64 'i64)
(define u64 'u64)
(struct semilattice (dom ⊥ ∨))

(define (base-type? type)
  (match type
    [(or 'i64) #t]
    [_ #f]))

(define min-nat (semilattice u64 0 +))
