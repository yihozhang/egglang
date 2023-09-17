#lang racket/base

(require racket/match)

(provide i64 u64 semilattice sort
         show-base-type base-type?)

(define i64 'i64)
(define u64 'u64)
(struct semilattice (name dom bot join))
(struct sort (name) #:transparent)

(define (base-type? type)
  (match type
    [(or 'i64) #t]
    [_ #f]))

(define (show-base-type type)
  (match type
    [(semilattice name dom bot join) `(semilattice ,name)]
    ['i64 'i64]
    ['u64 'u64]
    [(sort name) `(sort ,name)]))

(define min-nat (semilattice 'min-nat u64 0 +))
