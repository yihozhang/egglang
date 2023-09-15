#lang racket/base

(require racket/match)
(require racket/list)
(require "./lib.rkt")


;; 
(struct core-atom (fun args) #:transparent)
(struct core-let-atom-action (var fun args) #:transparent)
(struct core-let-val-action (var val) #:transparent)
(struct core-set-action (fun args expr) #:transparent)
(struct core-union-action (v1 v2) #:transparent)

(struct core-query (atoms) #:transparent)
(struct core-actions (actions) #:transparent)
(struct core-rule (query actions) #:transparent)


;; compilation

;; constructor takes function name, args, and output argument
;; and returns the constructed object
(define (flatten-expr-generic e constructor)
  (define (go e) (flatten-expr-generic e constructor))
  (match e
    [(call fun args) (let* ([flattened (map go args)]
                            [children-args (map car flattened)]
                            [children-atoms (map cdr flattened)]
                            
                            [o (gensym)]
                            [atom (constructor fun children-args o)]
                            
                            [atoms (foldr append (list atom) children-atoms)])
                       (cons o atoms))]
     
    [_ (cons e '())]))


(define (flatten-query-expr e)
  (flatten-expr-generic
   e
   (λ (f args out)
     (let ([args+ (append args (list out))])
       (core-atom f args+)))))


(define (flatten-action-expr e)
  (flatten-expr-generic
   e
   (λ (f args out) (core-let-atom-action out f args))))

(define (flatten-atom a)
  (match a
    [(value-eq lhs rhs)
     (let* ([flattened-lhs (flatten-query-expr lhs)]
            [flattened-rhs (flatten-query-expr rhs)]
            [lhs-var (car flattened-lhs)]
            [rhs-var (car flattened-rhs)]
            [atoms (append (append (cdr flattened-lhs) (cdr flattened-rhs))
                           (list (core-atom 'value-eq (list lhs-var rhs-var))))])
       atoms)]
    [_ (cdr (flatten-query-expr a))]))
    
(define (flatten-query q)
  (define atoms-list (map flatten-atom (query-atoms q)))
  (core-query (append* atoms-list)))

(define (flatten-action a)
  (match a
    [(let-action v e)
     (match-let ([(cons v2 actions) (flatten-action-expr e)])
       (append actions (list (core-let-val-action v v2))))]
    [(union-action e1 e2)
     (match-let ([(cons v1 actions1) (flatten-action-expr e1)]
                 [(cons v2 actions2) (flatten-action-expr e2)])
       (append actions1
               actions2
               (list (core-union-action v1 v2))))]
    [(set-action fun args e)
     (match-let* ([vas (map flatten-action-expr args)]
                  [args+ (map car vas)]
                  [args-actions (map cdr vas)]
                  [(cons e+ expr-action) (map flatten-action-expr e)])
       (append args-actions
               (list expr-action)
               (list (core-set-action fun args+ e+))))]))

(define (flatten-actions as)
  (define actions (actions-actions as))
  (core-actions (append* (map flatten-action actions))))

(define (flatten-rule rule)
  (core-rule (flatten-query (rule-query rule))
             (flatten-actions (rule-actions rule))))


(flatten-atom (define-atom (= (f (g (h x))) (g (h x)))))

(flatten-query (define-query ((= (f (g (h x))) (g (h x))))))


;; sort
(define-sort sort)

(define-rule R
  ([= ab (+ a b)]
   [= r1 (+ ab c )])
  ([let bc (+ b c)]
   [let r2 (+ a bc)]
   [union r1 r2]))
(flatten-rule R)