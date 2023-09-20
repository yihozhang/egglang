#lang racket/base

(require "egraph.rkt"
         "ast.rkt"
         "type.rkt")

(provide (all-defined-out))

;; macros for making functions, rules, atoms
(define-syntax make-atom
  (syntax-rules (=)
    [(make-atom (= lhs rhs))
     (value-eq (make-atom lhs)
               (make-atom rhs))]
    [(make-atom expr) (make-expr expr)]))

(define-syntax make-query
  (syntax-rules ()
    [(make-query (atom ...))
     (query (list (make-atom atom) ...))]))

(define-syntax make-expr
  (syntax-rules ()
    [(make-atom (fun args ...))
     (call fun (list (make-atom args) ...))]
    ;; TODO: we need to make sure that a is a symbol or number etc
    [(make-atom a) (quote a)]))

(define-syntax make-action
  (syntax-rules (let set union)
    ;; require v to be a symbol
    [(make-action (let v e)) (let-action (quote v) (make-expr e))]
    ;; require fun to be a symbol
    [(make-action (set (fun args ...) e)) (set-action fun
                                                      (list (make-expr args) ...)
                                                      (make-expr e))]
    [(make-action (union e1 e2)) (union-action (make-expr e1) (make-expr e2))]
    [(make-action e) (make-expr e)]
    ))

(define-syntax make-actions
  (syntax-rules ()
    [(make-actions (action ...))
     (actions (list (make-action action) ...))]))

(define-syntax make-rule
  (syntax-rules ()
    [(make-rule query actions)
     (let* ([r (rule (make-query query)
                     (make-actions actions))]
            [_ (register-rule (current-egraph) r #:ruleset (current-ruleset))])
       r)]))

(define-syntax make-ruleset
  (syntax-rules ()
    [(make-ruleset ruleset exprs ...)
     (parameterize ([current-ruleset (quote ruleset)])
       exprs ...
       (void))]))


(define-syntax make-sort
  (syntax-rules ()
    [(make-sort sort-name)
     (define sort-name (let* ([s (sort (quote sort-name))]
                              [_ (register-sort (current-egraph) s)])
                         s))]))

(define-syntax make-function
  (syntax-rules ()
    [(make-function (name inputs ...) output)
     (define name (let* ([f (function (quote name) (cons (list inputs ...) output))]
                         [_ (register-function (current-egraph) f)])
                    f))]))
