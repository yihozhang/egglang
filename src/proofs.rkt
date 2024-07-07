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

         get-existence-proof
         )
;; There are four kinds of justifications:
;;  1. User-defined justification
;;  2. Rule-based justification
;;  3. Congruence
;;  4. Lattice-join justification
(struct user-jus (cause) #:transparent)
; a context in a rule justification is a list of pairs of names and
; representative terms
(struct rule-jus (cause [context #:mutable]) #:transparent)
(struct cong-jus (fun args1 args2) #:transparent)
;; vals is a list of lattice values (cons val1 val2)
(struct join-jus (vals) #:transparent)

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

(define (get-existence-proof proof-manager term)
  (define result (make-hash))
  (get-existence-proof-impl proof-manager term result)
  result)

(define (get-existence-proof-impl proof-manager term result)
  (hash-ref! result term
             (lambda ()
               (define jus-term-existence (proof-manager-jus-term-existence proof-manager))
               (define proof (hash-ref jus-term-existence term #f))
               (displayln jus-term-existence)

               (if (not proof)
                   (error 'get-existence-proof "No proof for term ~a" term)
                   (begin
                     (cond [(rule-jus? proof)
                            (define context (rule-jus-context proof))
                            (for ([binding context])
                              (get-existence-proof-impl proof-manager (cdr binding) result))
                            ]
                           [(cong-jus? proof) (error 'get-existence-proof-impl "ugh")]
                           [(join-jus? proof) (error 'get-existence-proof-impl "ugh")]
                           [(user-jus? proof) #f])
                     proof)))
             )
  )
