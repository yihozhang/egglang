#lang racket/base

(require racket/match
         racket/list
         "ast.rkt"
         "type.rkt")
(provide (all-defined-out))

;; atoms
(struct core-atom (fun args) #:transparent)
(struct core-value-eq (lhs rhs) #:transparent)

;; actions
(struct core-let-atom-action (var fun args) #:transparent)
(struct core-let-val-action (var val) #:transparent)
(struct core-set-action (fun args expr) #:transparent)
(struct core-union-action (v1 v2) #:transparent)

(struct core-query (atoms) #:transparent)
(struct core-actions (actions) #:transparent)
(struct core-rule (query actions) #:transparent)

;; constant
(define false-atom (core-atom False '()))

;; lowering to the core

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
                           (list (core-value-eq lhs-var rhs-var)))])
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

;; substitution
(define (subst-core-atom atom v e)
  (define (replace arg) (if (equal? arg v) e arg))
  (match atom
    [(core-atom fun args)
     (core-atom fun (map replace args))]
    [(core-value-eq lhs rhs)
     (core-value-eq (replace lhs) (replace rhs))]))

(define (subst-core-action action v e)
  (define (replace arg) (if (equal? arg v) e arg))
  (match action
    [(core-let-atom-action var fun args)
     (when (equal? var v) (error "let-bound vars conflict"))
     (core-let-atom-action var fun (map replace args))]
    [(core-let-val-action var val)
     (when (equal? var v) (error "let-bound vars conflict"))
     (core-let-val-action var (replace val))]
    [(core-set-action fun args expr)
     (core-set-action fun (map replace args) (replace expr))]
    [(core-union-action v1 v2)
     (core-union-action (replace v1) (replace v2))]))


;; canonicalize query
(define (canonicalize-value-eq rule)
  (let loop ([rule rule])
    (match-define
      (core-rule (core-query atoms)
                 (core-actions actions))
      rule)

    (define core-value-eq-atom (findf core-value-eq? atoms))
    (if core-value-eq-atom
        (match-let ([(core-value-eq lhs rhs) core-value-eq-atom])
          (define (go v e)
            (let* ([atoms+ (remove* (list (core-value-eq e e))
                                    (map (lambda (atom) (subst-core-atom atom v e)) atoms))]
                   [actions+ (map (lambda (action) (subst-core-action action v e)) actions)])
              (core-rule (core-query atoms+) (core-actions actions+))))

          (define rule+
            (cond [(equal? lhs rhs) ; (= x x): remove the atom
                   (let ([atoms+ (remove core-value-eq-atom atoms)])
                     (core-rule (core-query atoms+)) (core-actions actions))]
                  [(symbol? lhs) (go lhs rhs)] ; (= x y) where x is a symbol: subst x y
                  [(symbol? rhs) (go rhs lhs)] ; (= x y) where y is a symbol: subst y x
                  [(and (literal? lhs) ; (= l1 l2) where (!= l1 l2): replace with false
                        (literal? rhs))
                   (let ([index (index-of atoms core-value-eq-atom)])
                     (list-set atoms index false-atom))]))

          (loop rule+))
        rule)))

;; TODO show core rules
(define (rcompose . args)
  (apply compose (reverse args)))

(define (compile rule egraph)
  (define passes
    (rcompose flatten-rule
              canonicalize-value-eq
              ))
  (passes rule))