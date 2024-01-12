#lang racket/base

(require racket/match
         "core.rkt")

(provide (struct-out justification)
         (rename-out [user-defined-jus make-user-defined-jus])
         termify)

(struct user-defined-jus
  (cause))

(struct justification
  (cause
   [context #:mutable]))

(struct proof-manager
  (jus-existence
   jus-equivalence))

(define (make-proof-manager)
  (proof-manager (make-hash)
                 (make-hash)))

(define (justify-existence proof-manager term jus)
  (define jus-existence (proof-manager-jus-existence proof-manager))
  (hash-set! jus-existence term jus))

(define (justify-equivalence proof-manager term1 term2 jus)
  (define jus-equivalence (proof-manager-jus-equivalence proof-manager))
  (hash-set! jus-equivalence (cons term1 term2) jus))

;; Replace every sort variable in the context
;; with corresponding term
(define (termify egraph m)
  m
  )


;; Basically, the algorithm is
;; 1. Termify the substitution, get a justification
;; 2. Every time make changes to the database or the union find, 
;;    use the justification
;; 3. ?? Apply the congruence rule: run congruence rule over the term graph? 