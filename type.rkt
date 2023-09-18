#lang racket/base

(require racket/match)

(provide i64 u64 unit semilattice sort
         show-base-type base-type? literal?
         function show-function function-name False)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base types

(define i64 'i64)
(define u64 'u64)
(define unit '())
(struct semilattice (name dom bot join))
(struct sort (name) #:transparent)

(define (literal? l)
  (or (number? l)
      ))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(struct function
  (name
   ;; a pair of input types and output type
   types
   )
  #:transparent)

(define False (function 'False (cons '() unit)))

(define (show-function func)
  (define name (function-name func))
  (define in (car function-types))
  (define out (cdr function-types))
  `(function (,name ,@(map show-base-type in)) ,(show-base-type out)))


(define min-nat (semilattice 'min-nat u64 0 +))
