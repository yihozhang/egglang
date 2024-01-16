#lang racket/base

(require racket/match
         "core.rkt")

(provide (struct-out user-jus)
         (struct-out rule-jus)
         (struct-out cong-jus)
         (struct-out join-jus)
         make-proof-manager
         add-term-proof
         add-fact-proof
         add-equiv-proof
         termify)
;; There are four kinds of justifications:
;;  1. User-defined justification
;;  2. Rule-based justification
;;  3. Congruence
;;  4. Lattice-join justification
(struct user-jus (cause))
(struct rule-jus (cause [context #:mutable]))
(struct cong-jus (fun args1 args2))
;; vals is a list of lattice values (cons val1 val2)
(struct join-jus (vals))

(struct proof-manager
  (jus-term-existence
   jus-fact-existence
   jus-equivalence))

(define (make-proof-manager)
  (proof-manager (make-hash)
                 (make-hash)
                 (make-hash)))

(define (add-term-proof proof-manager term jus)
  (define jus-term-existence (proof-manager-jus-term-existence proof-manager))
  (hash-set! jus-term-existence term jus))

(define (add-fact-proof proof-manager fun args val jus)
  (define jus-fact-existence (proof-manager-jus-fact-existence proof-manager))
  (hash-set! jus-fact-existence (list fun args val) jus))

(define (add-equiv-proof proof-manager term1 term2 jus)
  (define jus-equivalence (proof-manager-jus-equivalence proof-manager))
  (hash-set! jus-equivalence (cons term1 term2) jus))

;; Replace every sort variable in the context
;; with corresponding term
(define (termify egraph m)
  m
  )
