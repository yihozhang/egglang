#lang racket/base

(require "./lib.rkt")

(struct core-atom (fun args) #:transparent)
(struct core-let-action (var fun args) #:transparent)
(struct core-set-action (fun args expr) #:transparent)
(struct core-union-action (v1 v2) #:transparent)

(struct core-query (atoms))
(struct core-actions (actions))
(struct core-rule (query actions))


;; compilation
(define (flatten-query-expr e)
  (displayln e)
  (cond
    [(call? e) (let* ([flattened (map flatten-query-expr (call-args e))]
                      [children-args (map car flattened)]
                      [children-atoms (apply append (map cdr flattened))]

                      [o (gensym)]
                      [args (append children-args (list o))]
                      [atom (core-atom (call-fun e) args)])
                 (cons o (cons atom children-atoms)))]
     
    [#t (cons e '())]))

(define (flatten-atom a)
  (cond
    [(value-eq? a) (let* ([flattened-lhs (flatten-query-expr (value-eq-lhs a))]
                          [flattened-rhs (flatten-query-expr (value-eq-rhs a))]
                          [lhs-var (car flattened-lhs)]
                          [rhs-var (car flattened-rhs)]
                          [atoms (reverse (cons (core-atom 'value-eq (list lhs-var rhs-var))
                                                (append (cdr flattened-lhs) (cdr flattened-rhs))))])
                     atoms)]
    [#t (cdr (flatten-query-expr a))]))
    
                     

(flatten-atom (define-atom (= (f (g (h x))) (g (h x)))))


;(define (flatten-query q)
;  
;
;(define (compile-query q)
;  (define vars (collect-vars q))
;  )
