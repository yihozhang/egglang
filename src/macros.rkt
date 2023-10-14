#lang racket/base

(require (for-syntax racket/base))
(require "egraph.rkt"
         "ast.rkt"
         "type.rkt")

(provide (all-defined-out))

;; macros for making functions, rules, atoms
(define-syntax make-atom
  (syntax-rules (=)
    [(make-atom (= lhs rhs))
     (value-eq (make-expr lhs)
               (make-expr rhs))]
    [(make-atom expr) (make-expr expr)]))

(define-syntax make-query
  (syntax-rules ()
    [(make-query (atom ...))
     (query (list (make-atom atom) ...))]))

(define-syntax make-head
  (syntax-rules ()
    [(make-head head)
     (let ([head+ head])
       (cond [(procedure? head+) (computed-function (quote head) head+)]
             [(function? head+) head+]))]))

(define-syntax make-expr
  (syntax-rules ()
    [(make-expr (fun args ...))
     (call (make-head fun) (list (make-atom args) ...))]
    ;; TODO: we need to make sure that a is a symbol or number etc
    [(make-expr a) (quote a)]))

(define-syntax make-action
  (syntax-rules (let set! union!)
    ;; require v to be a symbol
    [(make-action (let v e)) (let-action (quote v) (make-expr e))]
    ;; require fun to be a symbol
    [(make-action (set! (fun args ...) e)) (set-action fun
                                                       (list (make-expr args) ...)
                                                       (make-expr e))]
    [(make-action (union! e1 e2)) (union-action (make-expr e1) (make-expr e2))]
    [(make-action e) (make-expr e)]
    ))

(define-syntax make-actions
  (syntax-rules ()
    [(make-actions (action ...))
     (actions (list (make-action action) ...))]))

(define-syntax make-rule
  (syntax-rules (:name)
    [(make-rule query actions :name name)
     (let* ([r (rule (quote name)
                     (make-query query)
                     (make-actions actions))]
            [_ (register-rule (current-egraph) r #:ruleset (current-ruleset))])
       (void))]
    [(make-rule query actions)
     (make-rule query actions :name (rule query actions))]))

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

(define-syntax make-term
  (syntax-rules ()
    [(make-term term-name)
     (define term-name (let* ([s (term (quote term-name))]
                              [_ (register-term (current-egraph) s)])
                         s))]))

(define-syntax make-function
  (syntax-rules ()
    [(make-function (name inputs ...) output)
     (define name (let* ([f (function (quote name) (cons (list inputs ...) output) #f)]
                         [_ (register-function (current-egraph) f)])
                    f))]))

(define-syntax make-constructor
  (syntax-rules ()
    [(make-function (name inputs ...) output)
     (define name (let* ([f (function (quote name) (cons (list inputs ...) output) #t)]
                         [_ (register-constructor (current-egraph) f)])
                    f))]))

(define-syntax make-relation
  (syntax-rules ()
    [(make-relation (name inputs ...))
     (make-function (name inputs ...) unit)]))

(define-syntax make-rewrite
  (syntax-rules (:when :name)
    [(make-rewrite lhs rhs :when (cond ...) :name name)
     (make-rule (lhs cond ...) ((set! lhs rhs)) :name name)]
    [(make-rewrite lhs rhs :when (cond ...))
     (make-rewrite lhs rhs :when (cond ...)
                   :name (rewrite lhs rhs :when (cond ...)))]
    [(make-rewrite lhs rhs :name name)
     (make-rewrite lhs rhs :when () :name name)]
    [(make-rewrite lhs rhs)
     (make-rewrite lhs rhs :when () :name (rewrite lhs rhs))]))

(define-syntax make-datatype
  (syntax-rules ()
    [(make-datatype D (func args ...) ...)
     (define-values (D func ...)
       (let ()
         (make-sort D)
         (make-constructor (func args ...) D)
         ...
         (values D func ...)))]))

(define-syntax make-run-action!
  (syntax-rules ()
    [(make-run-action! action)
     (run-action! (make-action action))]))

(define-syntax declare-const
  (syntax-rules ()
    [(declare-const name type)
     (define name
       (let ()
         (make-function (name) type)
         (make-run-action! (name))
         name))]))

(define-syntax define-const
  (syntax-rules ()
    [(define-const name type value)
     (define name
       (let ()
         (declare-const name type)
         (make-run-action! (union! (name) value))
         name))
     ]))

(define-syntax make-check
  (syntax-rules ()
    [(make-check atom ...)
     (let* ([query (make-query (atom ...))]
            [result (run-query query)])
       (when (null? result)
         (raise (format "check failure: ~a" query))))]))

(define-syntax make-run-query
  (syntax-rules ()
    [(run-query atom ...)
     (run-query (make-query (atom ...)))]))