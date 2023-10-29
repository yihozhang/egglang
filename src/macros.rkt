#lang racket/base

(require (for-syntax racket/base racket/syntax))
(require "egraph.rkt"
         "ast.rkt"
         "type.rkt"
         "terms.rkt")

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
            [_ (register-rule r)])
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
     (begin
       (define sort-name (sort (quote sort-name)))
       (register-sort (current-egraph) sort-name))]))

(define-syntax make-term
  (syntax-rules ()
    [(make-term term-name)
     (begin
       (define term-name (term (quote term-name)))
       (register-term (current-egraph) term-name))]))

(define-syntax make-function
  (syntax-rules ()
    [(make-function (name inputs ...) output)
     (begin
       (define name (function (quote name) (cons (list inputs ...) output) #f))
       (register-function (current-egraph) name))]))

(define-syntax make-constructor
  (syntax-rules ()
    [(make-constructor (name inputs ...) output)
     (begin
       (define name (function (quote name) (cons (list inputs ...) output) #t))
       (register-constructor (current-egraph) name))]))

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

(define-syntax (make-datatype stx)
  (syntax-case stx ()
    [(make-datatype D (func args ...) ...)
     (let ([@ (lambda (stx) (format-id #'D "~a@" stx))])
       (with-syntax ([D@ (@ #'D)]
                     [(func+ ...) (map @ (syntax->list #'(func ...)))]
                     [D:=>@ (format-id #'D "~a:=>@" #'D)]
                     [D:@=> (format-id #'D "~a:@=>" #'D)])
         #'(begin
             (make-sort D)
             (make-term D@)
             (make-function (D:=>@ D) D@)
             (make-function (D:@=> D@) D)
             (register-sort-term-pair D D@ D:=>@ D:@=>)
             (make-constructor (func args ...) D)
             ...
             (make-constructor (func+ (get-term-or-id args) ...) D@)
             ...
             (make-@-rules D D:@=> D:=>@ (list (list func func+ args ...) ...))
             )))])
  )

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
     (begin
       (declare-const name type)
       (make-run-action! (union! (name) value)))
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
