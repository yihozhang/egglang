#lang racket/base

(provide current-egraph current-ruleset
         register-function register-sort register-rule
         egraph-functions egraph-sorts egraph-rulesets
         run1)
(require data/gvector
         racket/function
         "./core.rkt"
         "./ast.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; egraph

(struct egraph
  (functions
   sorts
   rulesets))

(define (make-egraph)
  (egraph
   (make-hash)
   (make-gvector)
   (make-hash)))

(define (register-function egraph function)
  (define functions (egraph-functions egraph))
  (hash-set! functions function (make-table))
  )

(define (register-sort egraph sort)
  (define sorts (egraph-sorts egraph))
  (gvector-add! sorts sort))

(define (register-rule egraph rule #:ruleset ruleset)
  (define rulesets (egraph-rulesets egraph))
  (define rules (hash-ref! rulesets ruleset (thunk (make-gvector))))
  (gvector-add! rules rule))

(define current-egraph (make-parameter (make-egraph)))

(define current-ruleset (make-parameter 'main))

(define (run1 egraph ruleset)
  (define rules (hash-ref (egraph-rulesets egraph) ruleset))
  (define compiled-rules (for/list ([rule (in-gvector rules)]) (compile rule egraph)))

  (define matches (map (compose (curry run-query egraph) core-rule-query) compiled-rules))
  (map (lambda (rule ms) (map (curry run-actions egraph (core-rule-actions rule)) ms)) compiled-rules matches)
  )

(define (run-query egraph query) '())
(define (run-actions egraph actions m) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table
(struct table
  (entries))

(define (make-table)
  (table (make-gvector)))
