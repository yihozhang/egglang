#lang racket/base

(require "./type.rkt")

(provide (all-defined-out))

(struct rule (query actions) #:transparent)
(struct query (query) #:transparent)
(struct actions (actions) #:transparent)

(struct value-eq (lhs rhs) #:transparent)
(struct call (fun args) #:transparent)

(struct let-action (var expr) #:transparent)
(struct set-action (fun args expr) #:transparent)
(struct union-action (e1 e2) #:transparent)

(struct sort (name) #:transparent)
(struct function
  (name
   ;; a pair of input types and output type
   types
   ;; one of 'union, 'first, or an expression
   ;; TODO: better design here, should be implied by the type declaration of output types
   ;; merge-expr
   )
  #:transparent)

(define-syntax define-atom
  (syntax-rules (=)
    [(define-atom (= lhs rhs))
     (value-eq (define-atom lhs)
               (define-atom rhs))]
    [(define-atom expr) (define-expr expr)]))

(define-syntax define-query
  (syntax-rules ()
    [(define-query (atom ...))
     (query (list (define-atom atom) ...))]))

(define-syntax define-expr
  (syntax-rules ()
    [(define-atom (fun args ...))
     (call (quote fun) (list (define-atom args) ...))]
    ;; TODO: we need to make sure that a is a symbol or number etc
    [(define-atom a) (quote a)]))

(define-syntax define-action
  (syntax-rules (let set union)
    ;; require v to be a symbol
    [(define-action (let v e)) (let-action (quote v) (define-expr e))]
    ;; require fun to be a symbol
    [(define-action (set (fun args ...) e)) (set-action (quote fun)
                                                             (list (define-expr args) ...)
                                                             (define-expr e))]
    [(define-action (union e1 e2)) (union-action (define-expr e1) (define-expr e2))]
    [(define-action e) (define-expr e)]
  ))

(define-syntax define-actions
  (syntax-rules ()
    [(define-actions (action ...))
     (actions (list (define-action action) ...))]))

(define-syntax define-rule
  (syntax-rules ()
    [(define-rule query actions)
     (rule (define-query query)
           (define-actions actions))]))

(define-syntax define-sort
  (syntax-rules ()
    [(define-sort sort-name) (sort (quote sort-name))]))

(define-syntax define-function
  (syntax-rules ()
    [(define-function (name inputs ...) output)
     (function (quote name) (cons (list inputs ...) output))]))

;; sort
(define-sort sort)
(define-function (R 'integer 'integer) min-nat)

(define-rule
  ([= ab (+ a b)]
   [= r1 (+ ab c )])
  ([let bc (+ b c)]
   [let r2 (+ a bc)]
   [union r1 r2]))

   


