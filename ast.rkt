#lang racket/base

(require racket/string
         racket/match
         "egraph.rkt"
         "type.rkt")
(provide (all-defined-out))

(struct rule (query actions) #:transparent)
(struct query (atoms) #:transparent)
(struct actions (actions) #:transparent)

(struct value-eq (lhs rhs) #:transparent)
(struct call (fun args) #:transparent)

(struct let-action (var expr) #:transparent)
(struct set-action (fun args expr) #:transparent)
(struct union-action (e1 e2) #:transparent)

(struct function
  (name
   ;; a pair of input types and output type
   types
   )
  #:transparent)

;; formatting
(define (show-function func)
  (define name (function-name func))
  (define in (car function-types))
  (define out (cdr function-types))
  `(function (,name ,@(map show-base-type in)) ,(show-base-type out)))

(define (show-rule rule)
  (define (show-expr expr)
    (cond
      [(call? expr)
       (define func-name (function-name (call-fun expr)))
       (define args (call-args expr))
       `(,func-name ,@(map show-rule args))]
      [#t expr]))

  (define (show-query query)
    (define atoms (query-atoms query))
    (define (show-atom atom)
      (match atom
        [(value-eq lhs rhs) `(= ,(show-atom lhs) ,(show-atom rhs))]
        [e (show-expr e)]))
    (map show-atom atoms))

  (define (show-actions actions)
    (define actions+ (actions-actions actions))
    (define (show-action action)
      (match action
        [(let-action var expr) `(let ,var ,(show-expr expr))]
        [(set-action fun args expr) `(set (,(function-name fun) ,@(map show-expr args))
                                          ,(show-expr expr))]
        [(union-action e1 e2) `(union ,(show-expr e1) ,(show-expr e2))]
        ))
    (map show-actions actions+))

  `(rule ,(show-query (rule-query rule)) ,(show-actions (rule-actions actions))))

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
     (parameterize ([current-ruleset (quote rule-set)])
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
